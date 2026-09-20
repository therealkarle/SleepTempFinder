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

Set `input_path` in the config to either the Garmin export folder, a ZIP file,
or a direct `LifestyleLogging.json` file. Run the analysis with:

```sh
python LifestyleLoggingAnalysis/lifestyle_sleep_analysis.py \
  --config LifestyleLoggingAnalysis/GarminLifestyleAnalysisConfig.yaml
```

The same analysis is available as an R script. It requires the R packages
`yaml` and `jsonlite` and uses the same YAML configuration and output files:

```sh
Rscript LifestyleLoggingAnalysis/lifestyle_sleep_analysis.R \
  --config LifestyleLoggingAnalysis/GarminLifestyleAnalysisConfig.yaml
```

In RStudio, the file can also be run with **Source**. The script then searches
automatically for `GarminLifestyleAnalysisConfig.yaml` next to the script or
in the project directory:

```r
source("LifestyleLoggingAnalysis/lifestyle_sleep_analysis.R")
```

If the sleep CSV files are stored elsewhere, set `sleep_input` in the config:

```sh
python LifestyleLoggingAnalysis/lifestyle_sleep_analysis.py \
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
