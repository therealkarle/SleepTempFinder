# LifestyleLogging Extractor

Dieses kleine Python-Skript extrahiert das Garmin `LifestyleLogging.json` aus einem Garmin-Export und schreibt es als pivotierte CSV-Datei.

## Nutzung

```sh
python LifestyleLogingExtractor/extract_lifestyle_logging.py --input <path> [--output <output.csv>]
```

### Eingabeoptionen

- `--input` / `-i`:
  - Pfad zu einer direkten `*LifestyleLogging.json`-Datei
  - Pfad zu einem Garmin-Export-Ordner (`GarminUserData/...`)
  - Pfad zu einer ZIP-Datei mit Garmin-Export

### Ausgabe

- `--output` / `-o`:
  - Optionaler Zielpfad für die CSV-Datei
  - Wird kein Pfad angegeben, wird die Datei im Ordner `LifestyleLogingExtractor/Out/` abgelegt
  - Standarddateiname: `YYYY-MM-DD_<garminnumber>_LifestyleLogging.csv`

## Dateien

- `extract_lifestyle_logging.py` — Das Python-Skript
- `README.md` — Nutzungshinweise

## Beispiele

```sh
python LifestyleLogingExtractor/extract_lifestyle_logging.py --input GarminUserData/2026.04
python LifestyleLogingExtractor/extract_lifestyle_logging.py --input GarminUserData/2026.04/DI_CONNECT/DI-Connect-Wellness/108826033_LifestyleLogging.json
python LifestyleLogingExtractor/extract_lifestyle_logging.py --input export.zip
python LifestyleLogingExtractor/extract_lifestyle_logging.py --input export.zip --output results.csv
```