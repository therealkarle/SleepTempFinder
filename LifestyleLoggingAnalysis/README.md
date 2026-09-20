# LifestyleLogging Extractor

## Independent lifestyle/sleep analysis

The independent analysis script compares each Garmin lifestyle activity with
configured sleep metrics. It accepts an extracted Garmin export directory or a
ZIP export and writes three ranked CSV tables plus a JSON result file.

Install its dependencies once:

```sh
python -m pip install -r LifestyleLoggingAnalysis/requirements.txt
```

Use `LifestyleLoggingAnalysis/GarminLifestyleAnalysisConfig.yaml` for your
personal dates, exclusions, metrics, and output settings. It is a copy of the
template `GarminLifestyleAnalysisConfig.yaml.example` and is intentionally
ignored by Git.

Run the analysis with:

```sh
python LifestyleLoggingAnalysis/lifestyle_sleep_analysis.py \
  --input GarminUserData/2026.01 \
  --config LifestyleLoggingAnalysis/GarminLifestyleAnalysisConfig.yaml
```

The result directory contains `significant_positive.csv`,
`significant_negative.csv`, `not_significant.csv`, and
`lifestyle_sleep_analysis.json`. The delta is always `mean(done) -
mean(not_done)`. The test is a two-sided Welch t-test; all confidence,
significance, missing-activity, date, exclusion, and metric choices are in the
YAML config.

Dieses kleine Python-Skript extrahiert das Garmin `LifestyleLogging.json` aus einem Garmin-Export und schreibt es als pivotierte CSV-Datei.

## Nutzung

```sh
python LifestyleLoggingAnalysis/extract_lifestyle_logging.py --input <path> [--output <output.csv>]
```

### Eingabeoptionen

- `--input` / `-i`:
  - Pfad zu einer direkten `*LifestyleLogging.json`-Datei
  - Pfad zu einem Garmin-Export-Ordner (`GarminUserData/...`)
  - Pfad zu einer ZIP-Datei mit Garmin-Export

### Ausgabe

- `--output` / `-o`:
  - Optionaler Zielpfad für die CSV-Datei
  - Wird kein Pfad angegeben, wird die Datei im Ordner `LifestyleLoggingAnalysis/Out/` abgelegt
  - Standarddateiname: `YYYY-MM-DD_<garminnumber>_LifestyleLogging.csv`

## Dateien

- `extract_lifestyle_logging.py` — Das Python-Skript
- `README.md` — Nutzungshinweise

## Beispiele

```sh
python LifestyleLoggingAnalysis/extract_lifestyle_logging.py --input GarminUserData/2026.04
python LifestyleLoggingAnalysis/extract_lifestyle_logging.py --input GarminUserData/2026.04/DI_CONNECT/DI-Connect-Wellness/108826033_LifestyleLogging.json
python LifestyleLoggingAnalysis/extract_lifestyle_logging.py --input export.zip
python LifestyleLoggingAnalysis/extract_lifestyle_logging.py --input export.zip --output results.csv
python LifestyleLoggingAnalysis/extract_lifestyle_logging.py --input GarminUserData/2026.04 --output LifestyleLogging.csv
python LifestyleLoggingAnalysis/extract_lifestyle_logging.py --input GarminUserData/2026.04 --output C:\Users\flori\Downloads
```
