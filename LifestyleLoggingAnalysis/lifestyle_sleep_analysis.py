#!/usr/bin/env python3
"""Analyse Garmin LifestyleLogging against Garmin sleep metrics.

The input is a Garmin export directory or ZIP archive.  The script is
deliberately independent from the R analysis and only reads the export.
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import math
import re
import statistics
import zipfile
from dataclasses import dataclass
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Any, Iterable, Mapping

try:
    import yaml
except ImportError as exc:  # pragma: no cover - exercised by installation docs
    raise SystemExit("Install dependencies with: pip install -r LifestyleLoggingAnalysis/requirements.txt") from exc

try:
    from scipy import stats
except ImportError as exc:  # pragma: no cover - exercised by installation docs
    raise SystemExit("Install dependencies with: pip install -r LifestyleLoggingAnalysis/requirements.txt") from exc


LIFESTYLE_SUFFIX = "LifestyleLogging.json"
DEFAULT_METRICS: dict[str, list[str]] = {
    "Sleep_Score": ["Score", "Sleep Score", "sleepScore", "overallSleepScore"],
    "Sleep_Duration": ["Dauer", "Sleep Duration", "sleepDuration", "totalSleepTime"],
    "HRV": ["HFV-Status", "HRV", "avgOvernightHrv", "averageOvernightHrv"],
    "RHR": ["Ruheherzfrequenz", "Resting Heart Rate", "restingHeartRate", "restingHr"],
}
DATE_ALIASES = {"date", "datum", "sleep score 4 wochen", "sleep date", "calendar date"}
TRUE_VALUES = {"true", "yes", "y", "1", "done", "completed", "complete", "ja", "gemacht"}
FALSE_VALUES = {"false", "no", "n", "0", "not_done", "not done", "nicht gemacht", "nein"}


def norm(value: Any) -> str:
    return re.sub(r"\s+", " ", str(value or "").strip().lower().replace("_", " "))


def parse_date(value: Any) -> date | None:
    if isinstance(value, (list, tuple)) and len(value) >= 3:
        try:
            return date(int(value[0]), int(value[1]), int(value[2]))
        except (TypeError, ValueError):
            return None
    if isinstance(value, datetime):
        return value.date()
    if isinstance(value, date):
        return value
    text = str(value or "").strip()
    if not text:
        return None
    for fmt in ("%Y-%m-%d", "%d.%m.%Y", "%m/%d/%Y"):
        try:
            return datetime.strptime(text[:10], fmt).date()
        except ValueError:
            pass
    return None


def flatten_dict(value: Any, prefix: str = "") -> dict[str, Any]:
    if not isinstance(value, dict):
        return {prefix: value} if prefix else {}
    result: dict[str, Any] = {}
    for key, item in value.items():
        name = f"{prefix}.{key}" if prefix else str(key)
        if isinstance(item, dict):
            result.update(flatten_dict(item, name))
        else:
            result[name] = item
    return result


def find_daily_logs(value: Any) -> list[dict[str, Any]] | None:
    if isinstance(value, dict):
        if isinstance(value.get("dailyLogList"), list):
            return [x for x in value["dailyLogList"] if isinstance(x, dict)]
        for item in value.values():
            found = find_daily_logs(item)
            if found is not None:
                return found
    elif isinstance(value, list):
        for item in value:
            found = find_daily_logs(item)
            if found is not None:
                return found
    return None


def normalize_status(value: Any) -> bool | None:
    if isinstance(value, bool):
        return value
    text = norm(value)
    if text in TRUE_VALUES:
        return True
    if text in FALSE_VALUES:
        return False
    return None


def lifestyle_rows(payload: Any) -> dict[date, dict[str, bool]]:
    logs = find_daily_logs(payload) or (payload if isinstance(payload, list) else [])
    rows: dict[date, dict[str, bool]] = {}
    for entry in logs:
        day = parse_date(entry.get("calendarDate") or entry.get("date") or entry.get("logDate"))
        name = str(entry.get("behaviourName") or entry.get("behaviorName") or entry.get("name") or entry.get("label") or "").strip()
        status = normalize_status(entry.get("status", entry.get("value")))
        if day is None or not name or status is None:
            continue
        # Multiple records on one day resolve to positive if any record is positive.
        rows.setdefault(day, {})[name] = rows.setdefault(day, {}).get(name, False) or status
    return rows


class ExportReader:
    def __init__(self, source: str | Path, sleep_source: str | Path | None = None):
        self.source = Path(source)
        self.archive: zipfile.ZipFile | None = None
        self.sleep_reader: ExportReader | None = None
        self._names: list[str] = []
        if self.source.is_file() and zipfile.is_zipfile(self.source):
            self.archive = zipfile.ZipFile(self.source)
            self._names = [n for n in self.archive.namelist() if not n.endswith("/")]
        elif self.source.is_file() and self.source.name.endswith(LIFESTYLE_SUFFIX):
            self._names = [self.source.name]
        elif self.source.is_dir():
            self._names = [str(p.relative_to(self.source)) for p in self.source.rglob("*") if p.is_file()]
        else:
            raise FileNotFoundError(f"Garmin folder, ZIP, or LifestyleLogging JSON not found: {self.source}")
        if sleep_source is not None:
            sleep_path = Path(sleep_source)
            if sleep_path.resolve() != self.source.resolve():
                self.sleep_reader = ExportReader(sleep_path)

    def close(self) -> None:
        if self.archive:
            self.archive.close()
        if self.sleep_reader:
            self.sleep_reader.close()

    def read_bytes(self, name: str) -> bytes:
        if self.sleep_reader and name in self.sleep_reader.csv_names():
            return self.sleep_reader.read_bytes(name)
        if self.archive:
            return self.archive.read(name)
        if self.source.is_file():
            return self.source.read_bytes()
        return (self.source / name).read_bytes()

    def lifestyle_payloads(self) -> list[Any]:
        names = [n for n in self._names if n.replace("\\", "/").endswith(LIFESTYLE_SUFFIX)]
        if not names:
            raise FileNotFoundError("No LifestyleLogging.json found in Garmin export")
        return [json.loads(self.read_bytes(name).decode("utf-8-sig")) for name in names]

    def csv_names(self) -> list[str]:
        names = [n for n in self._names if n.lower().endswith(".csv")]
        if self.sleep_reader:
            names.extend(self.sleep_reader.csv_names())
        return names


def parse_number(value: Any) -> float | None:
    if value is None:
        return None
    text = str(value).strip().replace("\u00a0", " ")
    if not text or text in {"--", "-", "n/a", "NA"}:
        return None
    text = re.sub(r"[^0-9,.-]", "", text)
    if not text:
        return None
    if "," in text and "." in text:
        text = text.replace(".", "").replace(",", ".")
    else:
        text = text.replace(",", ".")
    try:
        return float(text)
    except ValueError:
        return None


def duration_to_hours(value: Any) -> float | None:
    number = parse_number(value)
    if number is not None and not re.search(r"[hHmMs]", str(value)):
        return number
    match = re.search(r"(?:(\d+)\s*h)?\s*(?:(\d+)\s*min?)?", str(value or ""), re.I)
    if match and (match.group(1) or match.group(2)):
        return (int(match.group(1) or 0) * 60 + int(match.group(2) or 0)) / 60
    return None


def metric_specs(config: Mapping[str, Any]) -> dict[str, list[str]]:
    configured = config.get("sleep_metrics", list(DEFAULT_METRICS))
    result: dict[str, list[str]] = {}
    if isinstance(configured, dict):
        for name, aliases in configured.items():
            result[str(name)] = [str(x) for x in aliases] if isinstance(aliases, list) else [str(aliases)]
    else:
        for name in configured:
            result[str(name)] = DEFAULT_METRICS.get(str(name), [str(name)])
    return result


def sleep_rows(reader: ExportReader, specs: Mapping[str, list[str]]) -> dict[date, dict[str, float]]:
    alias_map = {norm(alias): metric for metric, aliases in specs.items() for alias in aliases}
    result: dict[date, dict[str, float]] = {}
    for name in reader.csv_names():
        try:
            text = reader.read_bytes(name).decode("utf-8-sig")
            sample = text[:4096]
            dialect = csv.Sniffer().sniff(sample, delimiters=",;\t")
        except (UnicodeDecodeError, csv.Error):
            continue
        rows = csv.DictReader(io.StringIO(text), dialect=dialect)
        if not rows.fieldnames:
            continue
        date_col = next((col for col in rows.fieldnames if norm(col) in DATE_ALIASES), None)
        if not date_col:
            continue
        columns = {norm(col): col for col in rows.fieldnames}
        matched: dict[str, str] = {}
        for metric, aliases in specs.items():
            for alias in aliases:
                column = columns.get(norm(alias))
                if column is not None:
                    matched[metric] = column
                    break
        if not matched:
            continue
        for row in rows:
            day = parse_date(row.get(date_col))
            if day is None:
                continue
            target = result.setdefault(day, {})
            for metric, col in matched.items():
                if col is None:
                    continue
                value = duration_to_hours(row.get(col)) if metric == "Sleep_Duration" else parse_number(row.get(col))
                if value is not None and metric not in target:
                    target[metric] = value
    if not result:
        raise ValueError(
            "No compatible Garmin sleep CSV found. Provide a Garmin export folder/ZIP "
            "or configure --sleep-input/sleep_input when --input is a direct LifestyleLogging JSON."
        )
    return result


def quantile(values: list[float], probability: float) -> float:
    ordered = sorted(values)
    if len(ordered) == 1:
        return ordered[0]
    position = (len(ordered) - 1) * probability
    lower, upper = math.floor(position), math.ceil(position)
    if lower == upper:
        return ordered[lower]
    return ordered[lower] + (ordered[upper] - ordered[lower]) * (position - lower)


def group_stats(values: list[float], interval: float) -> dict[str, Any]:
    if not values:
        return {"n": 0, "mean": None, "median": None, "sd": None, "interval_low": None, "interval_high": None}
    low = (1 - interval) / 2
    return {"n": len(values), "mean": statistics.mean(values), "median": statistics.median(values),
            "sd": statistics.stdev(values) if len(values) > 1 else None,
            "interval_low": quantile(values, low), "interval_high": quantile(values, 1 - low)}


def analyse(config: Mapping[str, Any], reader: ExportReader) -> dict[str, Any]:
    start = parse_date(config.get("start_date"))
    end = parse_date(config.get("end_date"))
    if start is None or end is None or end < start:
        raise ValueError("Config requires valid start_date and end_date with end_date >= start_date")
    interval = float(config.get("value_interval", 0.80))
    confidence = float(config.get("confidence_level", 0.95))
    alpha = float(config.get("significance_level", 0.05))
    if not 0 < interval <= 1 or not 0 < confidence < 1 or not 0 < alpha < 1:
        raise ValueError("value_interval must be in (0,1], confidence_level and significance_level in (0,1)")
    payloads = reader.lifestyle_payloads()
    lifestyle: dict[date, dict[str, bool]] = {}
    for payload in payloads:
        for day, activities in lifestyle_rows(payload).items():
            for activity, done in activities.items():
                lifestyle.setdefault(day, {})[activity] = lifestyle.setdefault(day, {}).get(activity, False) or done
    specs = metric_specs(config)
    sleep = sleep_rows(reader, specs)
    excluded = {norm(x) for x in config.get("excluded_activities", [])}
    overrides = {norm(k): bool(v) for k, v in (config.get("missing_activity_is_no_by_activity", {}) or {}).items()}
    default_missing_no = bool(config.get("missing_activity_is_no", True))
    configured_activities = [str(x) for x in config.get("activities", [])]
    activities = set(configured_activities) | {a for values in lifestyle.values() for a in values}
    activities = {a for a in activities if norm(a) not in excluded}
    days = [start + timedelta(days=i) for i in range((end - start).days + 1)]
    results: list[dict[str, Any]] = []
    for activity in sorted(activities, key=norm):
        missing_no = overrides.get(norm(activity), default_missing_no)
        for metric in specs:
            done_values: list[float] = []
            not_done_values: list[float] = []
            for day in days:
                value = sleep.get(day, {}).get(metric)
                if value is None:
                    continue
                status = lifestyle.get(day, {}).get(activity)
                if status is None:
                    if not missing_no:
                        continue
                    status = False
                (done_values if status else not_done_values).append(value)
            done = group_stats(done_values, interval)
            not_done = group_stats(not_done_values, interval)
            row: dict[str, Any] = {"activity": activity, "metric": metric, "missing_activity_is_no": missing_no,
                                   **{f"done_{k}": v for k, v in done.items()},
                                   **{f"not_done_{k}": v for k, v in not_done.items()}}
            delta = None
            ci_low = ci_high = p_value = None
            if len(done_values) >= 2 and len(not_done_values) >= 2:
                delta = statistics.mean(done_values) - statistics.mean(not_done_values)
                test = stats.ttest_ind(done_values, not_done_values, equal_var=False, alternative="two-sided")
                p_value = float(test.pvalue)
                se = math.sqrt(statistics.variance(done_values) / len(done_values) + statistics.variance(not_done_values) / len(not_done_values))
                if se > 0 and math.isfinite(se):
                    df_num = (statistics.variance(done_values) / len(done_values) + statistics.variance(not_done_values) / len(not_done_values)) ** 2
                    df_den = (statistics.variance(done_values) ** 2 / (len(done_values) ** 2 * (len(done_values) - 1)) + statistics.variance(not_done_values) ** 2 / (len(not_done_values) ** 2 * (len(not_done_values) - 1)))
                    critical = float(stats.t.ppf((1 + confidence) / 2, df_num / df_den))
                    ci_low, ci_high = delta - critical * se, delta + critical * se
            row.update({"delta": delta, "delta_ci_low": ci_low, "delta_ci_high": ci_high, "p_value": p_value,
                        "significant": bool(p_value is not None and p_value < alpha and delta != 0),
                        "classification": "significant_positive" if p_value is not None and p_value < alpha and delta > 0 else "significant_negative" if p_value is not None and p_value < alpha and delta < 0 else "not_significant"})
            results.append(row)
    return {"metadata": {"start_date": start.isoformat(), "end_date": end.isoformat(), "value_interval": interval,
                          "confidence_level": confidence, "significance_level": alpha, "method": "Welch two-sample t-test",
                          "delta_definition": "mean(done) - mean(not_done)",
                          "input_statistics": {
                              "lifestyle_json_files": len(payloads),
                              "lifestyle_dates": sum(1 for day in lifestyle if start <= day <= end),
                              "sleep_csv_files_considered": len(reader.csv_names()),
                              "sleep_dates": sum(1 for day in sleep if start <= day <= end),
                              "date_count": len(days),
                              "activities_found": len({a for values in lifestyle.values() for a in values}),
                              "activities_excluded": len({a for a in {a for values in lifestyle.values() for a in values} if norm(a) in excluded}),
                              "activities_analysed": len(activities),
                          }}, "results": results}


def write_outputs(result: Mapping[str, Any], output_dir: Path, config: Mapping[str, Any]) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    groups = {"significant_positive": "significant_positive.csv", "significant_negative": "significant_negative.csv", "not_significant": "not_significant.csv"}
    rows = result["results"]
    fields = list(rows[0].keys()) if rows else ["activity", "metric"]
    for classification, filename in groups.items():
        selected = [r for r in rows if r["classification"] == classification]
        selected.sort(key=lambda r: (r["delta"] is None, -(r["delta"] or 0)))
        if classification == "significant_negative":
            selected.sort(key=lambda r: (r["delta"] is None, r["delta"] if r["delta"] is not None else 0))
        with (output_dir / filename).open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=fields)
            writer.writeheader()
            writer.writerows(selected)
    full = dict(result)
    full["config"] = dict(config)
    (output_dir / "lifestyle_sleep_analysis.json").write_text(json.dumps(full, ensure_ascii=False, indent=2, default=str), encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", "-i", help="Optional override for config input_path")
    parser.add_argument("--sleep-input", help="Optional Garmin folder/ZIP containing sleep CSV files")
    parser.add_argument("--config", "-c", required=True, help="YAML configuration")
    args = parser.parse_args()
    config = yaml.safe_load(Path(args.config).read_text(encoding="utf-8")) or {}
    input_path = args.input or config.get("input_path")
    if not input_path:
        parser.error("Set input_path in the config or provide --input")
    sleep_input = args.sleep_input or config.get("sleep_input") or None
    reader = ExportReader(input_path, sleep_input)
    try:
        result = analyse(config, reader)
        write_outputs(result, Path(config.get("output_dir", "LifestyleLoggingAnalysis/Out")), config)
        print(f"Analysed {len(result['results'])} activity/metric combinations")
    finally:
        reader.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
