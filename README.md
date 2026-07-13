# SleepTempFinder

SleepTempFinder is an R-based analysis project for combining Garmin sleep exports with room temperature and humidity sensor data.

## What It Does

The project:

- imports Garmin sleep CSV files
- imports room sensor CSV files
- matches sensor readings to each sleep period
- calculates nightly environmental averages and standard deviations
- applies optional tag, sensor, date, and numeric filters
- produces tables and plots for analysis

## Main Entry Points

The core logic lives in:

- `RScript/SleepTempFinder.R` - main analysis script for import, cleanup, matching, filtering, and visualization
- `RScript/studio_commands.R` - interactive helper functions for RStudio, especially `run_analysis()`
- `RScript/config.yaml` - central configuration for file formats, sensors, calendar settings, filters, and plots

## Requirements

- R installed
- The following packages are installed automatically on first run if needed:
  - `tidyverse`
  - `lubridate`
  - `yaml`
  - `broom`
  - `GGally`
  - `gridExtra`
  - `grid`
  - `scales`
  - `httpgd` for browser plot mode

## Project Structure

- `data/` - CSV exports and date-based subfolders
- `PlotOutput/` - optional output directory for saved plot images
- `RScript/` - project scripts and configuration files
- `RScript/config.yaml` - main project configuration
- `RScript/config.private.yaml` - optional local override for personal settings

## Configuration

The most important file is `RScript/config.yaml`.

Useful sections:

- `data_directory` - base folder for your datasets
- `parse_orders` - date and time formats for Garmin and sensor CSV files
- `locale` - especially `decimal_mark: ","` for sensor CSV files that use commas as decimal separators
- `sensor_files` - sensor definitions with file paths, column names, and optional nicknames
- `calendar_default_sensor` - fallback sensor if the calendar does not specify one
- `analysis_filter` - default filter settings loaded at startup
- `outlier_filter` - rules for excluding outlier nights
- `plot.output_mode` - `rstudio`, `browser`, or `both`
- `plot.export.enabled` - when `true`, plot images are written to `plot.export.output_dir`

If you need personal settings, create `RScript/config.private.yaml` and override values there without changing the repository defaults.

## Running From The Command Line

From the project root:

```sh
Rscript RScript/SleepTempFinder.R
```

With a filter:

```sh
Rscript RScript/SleepTempFinder.R --filter="2026;Sensors=WohnwagenSensor"
```

Dry run, which suppresses plot output:

```sh
Rscript RScript/SleepTempFinder.R --dry-run
```

Verbose mode for additional debug output:

```sh
Rscript RScript/SleepTempFinder.R --verbose
```

## Filter Syntax

Filters are passed as a semicolon-separated string.

Supported parts:

- Date selection: `YYYY`, `qN.YYYY`, `MM.YYYY`, `YYYY.MM`, `DD.MM.YYYY`, `DD.MM.YYYY,DD.MM.YYYY`
- Sensors: `Sensors=Name` with multiple values separated by `,` or `|`
- Tags: `Tags=Tag1|Tag2` for OR or `Tags=Tag1,Tag2` for AND
- Complex tag expressions: `TagsExpr=` with `!`, `,`, `&`, `*`, and parentheses
- Numeric expressions: for example `temp>18`, `SleepScore>80`, `18<temp<22`

Examples:

```sh
Rscript RScript/SleepTempFinder.R --filter="q1.2026;Tags=Hochlitten"
Rscript RScript/SleepTempFinder.R --filter="01.2026;Sensors=WohnwagenSensor"
Rscript RScript/SleepTempFinder.R --filter="temp>18"
Rscript RScript/SleepTempFinder.R --filter="SleepScore>80"
Rscript RScript/SleepTempFinder.R --filter="TagsExpr=(Urlaub,Wohnmobil) & !Hochlitten"
```

Notes:

- `filter` overrides the other command arguments.
- The analysis filter can also target numeric columns such as `Avg_Temp`, `Sleep_Score`, `HRV`, and `RHR`.
- Outlier handling is configured in `RScript/config.yaml`.

## Interactive Use In RStudio

1. Open the project in RStudio.
2. Source the helper script:

```r
source("RScript/studio_commands.R")
```

3. Run an analysis with `run_analysis()`.

## Helper Functions

`run_analysis(date = NULL, tags = NULL, sensors = NULL, dry_run = FALSE, filter = NULL, flags = NULL)`

- Main helper for interactive use.
- Builds the command-line style arguments used by the main script.
- Accepts date, tag, and sensor filters either positionally or by name.
- Accepts a raw `filter` string and uses it instead of the other arguments.
- Supports unquoted logical expressions such as `run_analysis(temp > 18)`.
- Accepts `flags` as an alias for `tags`.

`run_clear_filter()`

- Clears the temporary `_STF_ARGS_` environment variable.
- Useful if you want to remove a previously set interactive filter.

`setwd_to_active()`

- Sets the working directory to the folder of the currently active document in RStudio.
- Helpful when you want relative paths to resolve correctly before calling `run_analysis()`.

Preset helpers:

- `run_winter2026()` -> `run_analysis("02.2026")`
- `run_wohnwagen()` -> `run_analysis("Sensors=Wohnwagen")`
- `run_hochlitten()` -> `run_analysis("Tags=Hochlitten")`

## `run_analysis()` Examples

```r
run_analysis("2026")
run_analysis(tags = "Hochlitten")
run_analysis(sensors = "WohnwagenSensor")
run_analysis(filter = "temp>18")
run_analysis(filter = "SleepScore>80")
run_analysis(tags = "(Urlaub, Wohnmobil) & !Hochlitten")
run_analysis(tags != "Hochlitten")
```

## Sensor And Calendar Data

Sensors are defined in `RScript/config.yaml` under `sensor_files`. Each sensor entry can contain:

- `nickname` - alternative names used for calendar matching
- `path` - sensor CSV file under `data/` or in a subfolder
- `col_time`, `col_temp`, `col_hum` - column names in the sensor CSV
- `default: true` - optional default sensor

Calendar events can define the sensor and tags in `SUMMARY` or `DESCRIPTION`, for example:

```text
sensor=Wohnwagen; tags=Hochlitten
```

If no sensor is specified, `calendar_default_sensor` or the configured default sensor is used.

## Data Sources

- Sleep data: Garmin CSV files inside `data/`
- Sensor data: CSV files detected through `sensor_files`
- Calendar data: either the configured Google Calendar feed or a local calendar file, depending on `calendar_source.mode`

## Output

The script produces:

- a reviewed table with matched nights
- audit information for excluded or cleaned nights
- plots in the RStudio graphics pane or in the browser
- optional plot images in `PlotOutput/`

## Tips

- Use `Sensors=<Name>` to focus on a specific sensor.
- Use `TagsExpr=` or `run_analysis(tags = ...)` for complex boolean tag logic.
- Create `RScript/config.private.yaml` if you want local configuration overrides.

## Example Sensors In The Current Setup

- `FlorianZimmerSensor` (default, `ThermometerZimmerFlorian_data.csv`)
- `WohnwagenSensor` (`Wohnwagen_data.csv`)
- `WohnmobilAussen` (`Wohnmobil Außen_data.csv`)
- `WohnmobilInnen` (`Wohnmobil Innen_data.csv`)

## Example Tags

- `Urlaub`
- `Wohnmobil`
- `Trainingslager`
- `Hochlitten`

## Quick Start

1. Open the project in RStudio or an R console.
2. Run `source("RScript/studio_commands.R")`.
3. Run `run_analysis("2026")` or `run_analysis(filter = "temp>18")`.

## How The Values Are Calculated

This section describes the values that appear in the nightly output and in the summary tables.

### Sleep And Metadata Fields

- `Date` - taken from the Garmin sleep export row for the night.
- `Sensor` - the sensor assigned from the calendar entry, or the default sensor if the calendar does not specify one.
- `Flags` - tags or flags parsed from the calendar entry.
- `Sensor_File` - the sensor file that was actually used for the night.
- `Outlier_Reason` - a text reason if the night was marked as an outlier.

### Sleep Metrics

- `Sleep_Score` - read from the Garmin sleep export and renamed from the configured Garmin sleep score column.
- `HRV` - read from the Garmin sleep export and renamed from the configured HRV column.
- `RHR` - read from the Garmin sleep export and renamed from the configured resting heart rate column.
- `Sleep_Duration` - read from the Garmin sleep export and renamed from the configured duration column. It is displayed in `hh:mm` format in the final output.

### Sensor Metrics

For each night, sensor readings are selected from the chosen sensor file within the sleep window:

- start = bedtime minus `matching_padding_minutes`
- end = wake time plus `matching_padding_minutes`

The script then uses only the sensor rows whose timestamps fall inside that window.

- `Avg_Temp` - mean of all selected room temperature readings
- `Temp_SD` - standard deviation of the selected room temperature readings
- `Avg_Rel_Hum` - mean of all selected relative humidity readings
- `Rel_Hum_SD` - standard deviation of the selected relative humidity readings
- `Avg_Abs_Hum` - mean of all selected absolute humidity readings
- `Abs_Hum_SD` - standard deviation of the selected absolute humidity readings
- `Raw_N_Readings` - number of sensor readings used for the night
- `Sensor_Files` - list of source sensor files contributing readings
- `Sensor_Names` - list of sensor names contributing readings

### Filtering And Summary Statistics

- Nights with missing sleep metrics can be excluded before analysis.
- Nights with no usable sensor data can be excluded before analysis.
- Outlier filtering is applied before the final analysis table is built.
- The analysis filter from the config is applied after outlier filtering.
- Summary statistics are calculated from the remaining nights.

For each selected metric in the summary table:

- `mean` - arithmetic mean of the non-missing values
- `median` - median of the non-missing values
- `std_dev` - standard deviation, or `NA` if only one value is available
- `[lower;upper]` - the configured summary interval, based on quantiles of the filtered values

`Sleep_Duration` is formatted as hours and minutes in the summary output, while the other numeric values are rounded for display.

---

If you want, this README can also be extended with a dedicated CSV column reference or an example `config.private.yaml`.
