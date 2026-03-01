# SleepTempFinder AI coding guide

## Big picture
- Single R analysis script orchestrates the pipeline: read Garmin sleep CSVs + room sensor CSVs, align by bedtime/waketime, compute nightly averages, audit exclusions, then run stats and plots. See [RScript/SleepTempFinder.R](RScript/SleepTempFinder.R).
- Configuration is data-driven via YAML; most edits should be made in [RScript/config.yaml](RScript/config.yaml) rather than changing the R logic.
- Data lives in date-stamped folders under data/ (e.g., data/2026.01.22) with Garmin sleep CSVs and sensor exports.

## Data flow & key components
- Ingestion: `read_garmin_fixed()` cleans odd CSV headers/decimal commas; `clean_val_final()` normalizes mixed numeric/time strings.
- Sensor imports use `read_delim(..., locale(decimal_mark = ","))` and rename columns via config.
- Nightly mapping: sleep records use `bedtime`/`waketime` windows; room metrics are averaged from `sensor_raw` within those windows.
- Outlier filtering is optional and stage-specific (`sensor` vs `nightly`), with removed nights tracked and printed.

## Developer workflow (R)
- Run from RStudio; the script sets working directory to its own location when interactive.
- Execute the whole script to regenerate analysis, audits, and plots (console output + ggplot windows).
- Packages are auto-installed on first run (`tidyverse`, `lubridate`, `yaml`, `broom`, `GGally`, `gridExtra`, `grid`, `scales`).

## Project-specific conventions
- Config-driven mappings:
  - `column_names` for Garmin column headers.
  - `sensor_files` (previously called `temp_files`) + `usage_timeline` for sensor definitions and time ranges.
  - `sleep_data_sources` list of Garmin CSVs.
  - `outlier_filter` for method, thresholds, and stage.
- Date handling: nightly grouping uses `as.Date(timestamp - hours(12))` to align nights crossing midnight.
- RHR optimization inverts values before peak detection (`RHR` treated as “lower is better”).

## When changing code
- Keep pipeline sections in order (ENV SETUP → DATA CLEANING → AUDIT → ANALYSIS → PLOTS) to avoid breaking downstream assumptions.
- Prefer updating config and data files over altering hard-coded column names in the script.
