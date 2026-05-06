import argparse
import csv
import json
import zipfile
from collections import defaultdict
from datetime import date
from pathlib import Path

EXPECTED_FOLDER_SEGMENT = Path('DI_CONNECT') / 'DI-Connect-Wellness'
LIFESTYLE_FILENAME_SUFFIX = 'LifestyleLogging.json'


def parse_arguments():
    parser = argparse.ArgumentParser(
        description='Extract Garmin LifestyleLogging JSON into a pivoted CSV file.'
    )
    parser.add_argument(
        '--input', '-i',
        required=True,
        help='Path to a LifestyleLogging JSON file, GarminUserData folder, or ZIP archive.'
    )
    parser.add_argument(
        '--output', '-o',
        help='Optional output CSV path. If omitted, writes to Out/<date>_<garminnumber>_LifestyleLogging.csv next to the script.'
    )
    return parser.parse_args()


def find_lifestyle_file_in_dir(root: Path):
    if not root.exists() or not root.is_dir():
        return None

    def candidates_in_folder(folder: Path):
        path = folder / EXPECTED_FOLDER_SEGMENT
        if path.is_dir():
            return sorted(path.glob(f'*{LIFESTYLE_FILENAME_SUFFIX}'))
        return []

    candidates = candidates_in_folder(root)
    if candidates:
        return candidates

    for child in root.iterdir():
        if child.is_dir():
            candidates = candidates_in_folder(child)
            if candidates:
                return candidates

    return sorted(root.rglob(f'*{LIFESTYLE_FILENAME_SUFFIX}'))


def find_lifestyle_file_in_zip(zip_path: Path):
    with zipfile.ZipFile(zip_path, 'r') as archive:
        candidates = []
        fallback = []
        for entry in archive.namelist():
            normalized = entry.replace('\\', '/').lstrip('/')
            if normalized.endswith(LIFESTYLE_FILENAME_SUFFIX):
                if '/'.join(EXPECTED_FOLDER_SEGMENT.parts) in normalized:
                    candidates.append(normalized)
                else:
                    fallback.append(normalized)

        if len(candidates) == 1:
            return candidates[0]
        if len(candidates) > 1:
            raise ValueError(
                'Multiple LifestyleLogging entries found in ZIP archive:\n' + '\n'.join(candidates)
            )
        if len(fallback) == 1:
            return fallback[0]
        if len(fallback) > 1:
            raise ValueError(
                'Multiple LifestyleLogging entries found in ZIP archive (fallback search):\n' + '\n'.join(fallback)
            )

    return None


def load_json(path: Path):
    with path.open('r', encoding='utf-8') as handle:
        return json.load(handle)


def load_json_from_zip(zip_path: Path, entry_name: str):
    with zipfile.ZipFile(zip_path, 'r') as archive:
        with archive.open(entry_name) as handle:
            return json.load(handle)


def extract_lifestyle_json(input_path: Path):
    if input_path.is_file():
        if zipfile.is_zipfile(input_path):
            entry_name = find_lifestyle_file_in_zip(input_path)
            if entry_name is None:
                raise FileNotFoundError(
                    f'No LifestyleLogging JSON found inside ZIP archive: {input_path}'
                )
            return load_json_from_zip(input_path, entry_name), Path(entry_name).name

        if input_path.name.endswith(LIFESTYLE_FILENAME_SUFFIX):
            return load_json(input_path), input_path.name

        raise FileNotFoundError(
            f'Input file is not a LifestyleLogging JSON nor a ZIP archive: {input_path}'
        )

    if input_path.is_dir():
        candidates = find_lifestyle_file_in_dir(input_path)
        if not candidates:
            raise FileNotFoundError(
                f'No LifestyleLogging JSON found under directory: {input_path}'
            )
        if len(candidates) > 1:
            raise FileExistsError(
                'Multiple LifestyleLogging JSON files found. Please specify one directly.\n' +
                '\n'.join(str(item) for item in candidates)
            )
        return load_json(candidates[0]), candidates[0].name

    raise FileNotFoundError(f'Input path does not exist: {input_path}')


def normalize_behavior_name(name):
    if name is None:
        return 'unknown'
    return str(name).strip()


def parse_date_field(value):
    if not isinstance(value, (list, tuple)) or len(value) < 3:
        return None
    try:
        return date(value[0], value[1], value[2]).isoformat()
    except Exception:
        return None


def find_daily_log_list(data):
    if isinstance(data, dict):
        if 'dailyLogList' in data and isinstance(data['dailyLogList'], list):
            return data['dailyLogList']
        for value in data.values():
            found = find_daily_log_list(value)
            if found is not None:
                return found
    elif isinstance(data, list):
        for item in data:
            if isinstance(item, dict) and 'dailyLogList' in item and isinstance(item['dailyLogList'], list):
                return item['dailyLogList']
            found = find_daily_log_list(item)
            if found is not None:
                return found
    return None


def normalize_daily_logs(data):
    daily_logs = None
    if isinstance(data, dict) and 'dailyLogList' in data:
        daily_logs = data['dailyLogList']
    else:
        daily_logs = find_daily_log_list(data)

    if daily_logs is None and isinstance(data, list):
        daily_logs = data

    if daily_logs is None:
        raise ValueError('Unable to locate dailyLogList in the LifestyleLogging JSON.')

    rows = defaultdict(dict)
    behavior_names = set()

    for entry in daily_logs:
        if not isinstance(entry, dict):
            continue

        date_value = entry.get('calendarDate') or entry.get('date') or entry.get('logDate')
        row_date = parse_date_field(date_value)
        if row_date is None:
            continue

        behavior_name = normalize_behavior_name(
            entry.get('behaviourName') or entry.get('behaviorName') or entry.get('name') or entry.get('label')
        )
        if behavior_name == 'unknown':
            continue

        status = entry.get('status')
        if status is None:
            status = entry.get('value')

        rows[row_date][behavior_name] = status
        behavior_names.add(behavior_name)

    if not rows:
        raise ValueError('No valid lifestyle log rows were found in the JSON data.')

    return rows, sorted(behavior_names)


def build_output_path(output_arg, json_filename, rows):
    if output_arg:
        output_path = Path(output_arg)
        if output_path.exists() and output_path.is_dir():
            filename = build_default_filename(json_filename, rows)
            return output_path / filename
        if output_path.suffix.lower() == '.csv':
            return output_path
        return output_path

    return Path(__file__).resolve().parent / 'Out' / build_default_filename(json_filename, rows)


def build_default_filename(json_filename, rows):
    garmin_number = 'unknown'
    if json_filename and '_' in json_filename:
        garmin_number = json_filename.split('_', 1)[0]

    latest_date = max(rows.keys())
    return f'{latest_date}_{garmin_number}_LifestyleLogging.csv'


def write_csv(output_path: Path, rows, behavior_names):
    output_path.parent.mkdir(parents=True, exist_ok=True)
    sorted_dates = sorted(rows.keys())

    with output_path.open('w', newline='', encoding='utf-8') as handle:
        writer = csv.writer(handle)
        writer.writerow(['date', *behavior_names])
        for row_date in sorted_dates:
            row = [row_date] + [rows[row_date].get(name, '') for name in behavior_names]
            writer.writerow(row)

    print(f'Wrote CSV: {output_path}')
    print(f'Dates: {len(sorted_dates)}, behaviors: {len(behavior_names)}')


def main():
    args = parse_arguments()
    input_path = Path(args.input)
    data, filename = extract_lifestyle_json(input_path)
    rows, behavior_names = normalize_daily_logs(data)
    output_path = build_output_path(args.output, filename, rows)
    write_csv(output_path, rows, behavior_names)


if __name__ == '__main__':
    main()
