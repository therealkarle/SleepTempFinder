# SleepMetaModel.R
# Builds meta-regression models for sleep metrics using room sensor and lifestyle logging data.
# This script uses helper logic from SleepTempFinder_shared.R and does not modify the original
# SleepTempFinder.R script.

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args)
  if (length(m) > 0) return(normalizePath(sub("^--file=", "", args[m][1])))
  if (!is.null(sys.frame(1)$ofile)) return(normalizePath(sys.frame(1)$ofile))
  NULL
}
script_path <- get_script_path()
script_dir <- if (!is.null(script_path)) dirname(script_path) else getwd()
source(file.path(script_dir, "SleepTempFinder_shared.R"))

load_meta_config <- function(script_dir) {
  cfg_path <- file.path(script_dir, "config.meta.yaml")
  private_path <- file.path(script_dir, "config.meta.private.yaml")
  meta_cfg <- list()
  if (file.exists(cfg_path)) {
    meta_cfg <- read_yaml(cfg_path)
  }
  if (file.exists(private_path)) {
    private_cfg <- read_yaml(private_path)
    for (k in names(private_cfg)) meta_cfg[[k]] <- private_cfg[[k]]
  }
  meta_cfg
}

meta_cfg <- load_meta_config(script_dir)

args <- commandArgs(trailingOnly = TRUE)
cli_lifestyle <- NULL
dry_run <- FALSE
verbose_run <- FALSE
for (a in args) {
  if (a == "--dry-run") {
    dry_run <- TRUE
  } else if (a == "--verbose") {
    verbose_run <- TRUE
  } else if (startsWith(a, "--lifestyle=")) {
    cli_lifestyle <- sub("^--lifestyle=", "", a)
  }
}

if (verbose_run) message("SleepMetaModel.R: verbose mode enabled")

config_data_dir <- resolve_path(config$data_directory %||% "../data")
if (!dir.exists(config_data_dir)) stop(sprintf("Data directory not found: %s", config_data_dir))

message("Loading lifestyle data...")
lifestyle_raw <- load_lifestyle_data(cli_lifestyle, meta_cfg)
if (nrow(lifestyle_raw) == 0) stop("Lifestyle dataset is empty.")
if (!(meta_cfg$lifestyle$csv_date_col %||% "date") %in% names(lifestyle_raw)) stop(sprintf("Lifestyle CSV must contain a '%s' column.", meta_cfg$lifestyle$csv_date_col %||% "date"))

date_col <- meta_cfg$lifestyle$csv_date_col %||% "date"
lifestyle_df <- lifestyle_raw %>%
  mutate(Date = as.Date(!!sym(date_col))) %>%
  select(-all_of(date_col)) %>%
  build_lifestyle_features(meta_cfg)

message(sprintf("Lifestyle rows loaded: %d", nrow(lifestyle_df)))

all_data_files <- list_csv_files(config_data_dir, recursive = isTRUE(config$scan_recursive))
message(sprintf("Found %d CSV files under %s", length(all_data_files), config_data_dir))

sleep_candidates <- all_data_files[sapply(all_data_files, is_sleep_csv, mapping = config$column_names)]
sensor_candidates <- all_data_files[sapply(all_data_files, is_sensor_csv, sensor_files = config$sensor_files)]
message(sprintf("Sleep candidate files: %d", length(sleep_candidates)))
message(sprintf("Sensor candidate files: %d", length(sensor_candidates)))

if (length(sleep_candidates) == 0) stop("No sleep CSV files detected.")
if (length(sensor_candidates) == 0) stop("No sensor CSV files detected.")

read_sleep_file <- function(f) {
  df <- read_garmin_fixed(f)
  hdr <- names(df)
  date_col <- config$column_names$garmin_date
  if (!(date_col %in% hdr)) {
    alt <- hdr[str_detect(hdr, regex("^(Sleep Score|Datum)", ignore_case = TRUE))]
    if (length(alt) >= 1) {
      date_col <- alt[[1]]
      message(sprintf("Using alternate sleep date column '%s' for %s", date_col, f))
    }
  }
  df %>%
    mutate(Source_File = f, Source_Name = canonical_basename(f)) %>%
    mutate(Date = as.Date(!!sym(date_col)),
           bedtime = if (config$column_names$garmin_bedtime %in% hdr) parse_datetime_safe(!!sym(config$column_names$garmin_bedtime), type = "garmin_time") else as.POSIXct(NA),
           waketime = if (config$column_names$garmin_waketime %in% hdr) parse_datetime_safe(!!sym(config$column_names$garmin_waketime), type = "garmin_time") else as.POSIXct(NA)) %>%
    mutate(waketime = update(waketime, year = year(Date), month = month(Date), mday = day(Date)),
           bedtime = update(bedtime, year = year(Date), month = month(Date), mday = day(Date))) %>%
    mutate(.bedtime_orig = bedtime,
           .waketime_orig = waketime,
           .swap_flag = (!is.na(.bedtime_orig) & !is.na(.waketime_orig) &
                            (hour(.bedtime_orig) <= 12 & hour(.waketime_orig) > 12 & .bedtime_orig < .waketime_orig))) %>%
    mutate(bedtime = if_else(.swap_flag, .waketime_orig, .bedtime_orig),
           waketime = if_else(.swap_flag, .bedtime_orig, .waketime_orig)) %>%
    mutate(.missing_window = is.na(bedtime) & is.na(waketime),
           bedtime = if_else(.missing_window, as.POSIXct(Date) - hours(12), bedtime),
           waketime = if_else(.missing_window, as.POSIXct(Date) + hours(12), waketime)) %>%
    mutate(bedtime = if_else(bedtime > waketime, bedtime - days(1), bedtime)) %>%
    select(-.bedtime_orig, -.waketime_orig, -.swap_flag, -.missing_window) %>%
    mutate(across(any_of(unlist(config$column_names[4:length(config$column_names)])), clean_val_final))
}

read_sensor_file <- function(fp) {
  f_info <- get_sensor_file_info(fp)
  df <- suppressMessages(suppressWarnings(read_delim(fp, delim = ",", locale = locale(decimal_mark = config$locale$decimal_mark %||% ","), show_col_types = FALSE, name_repair = "unique")))
  names(df)[names(df) == f_info$col_time] <- "timestamp"
  names(df)[names(df) == f_info$col_temp] <- "room_temp"
  names(df)[names(df) == f_info$col_hum] <- "rel_hum"
  if ("Abs Humidity(g/m³)" %in% names(df)) names(df)[names(df) == "Abs Humidity(g/m³)"] <- "abs_hum"
  df <- df %>% mutate(timestamp = parse_datetime_safe(timestamp, type = "sensor_timestamp")) %>%
    mutate(Source_File = fp,
           Source_Name = canonical_basename(fp),
           Sensor_ID = identify_sensor_id(fp))
  if (!"abs_hum" %in% names(df)) df$abs_hum <- NA_real_
  df
}

sleep_df_raw <- map_df(sleep_candidates, read_sleep_file)
sensor_raw <- map_df(sensor_candidates, read_sensor_file)

if (nrow(sensor_raw) == 0) stop("No sensor rows could be read.")

calendar_daily_raw <- load_calendar_daily(config$calendar_source, config$calendar_parser)
calendar_daily <- apply_calendar_three_day_rule(calendar_daily_raw, config$calendar_assignment)
if (nrow(calendar_daily) > 0) {
  calendar_daily$Sensor_Raw <- calendar_daily$Sensor
  calendar_daily$Sensor <- sapply(calendar_daily$Sensor_Raw, resolve_sensor_label, cfg = config, USE.NAMES = FALSE)
}

default_sensor_id <- {
  ids <- names(config$sensor_files)[sapply(config$sensor_files, function(x) isTRUE(x$default))]
  if (length(ids) > 0) ids[[1]] else config$calendar_default_sensor %||% NA_character_
}
if (!is.na(default_sensor_id) && nrow(calendar_daily) > 0) {
  calendar_daily$Sensor[is.na(calendar_daily$Sensor) & is.na(calendar_daily$Sensor_Raw)] <- default_sensor_id
}

mapping <- config$column_names
sleep_complete <- {
  hdr <- names(sleep_df_raw)
  rename_map <- list()
  sleep_col <- resolve_sleep_col(mapping, hdr, "garmin_sleep_score")
  hrv_col <- resolve_sleep_col(mapping, hdr, "garmin_hrv", "garmin_hrv_alt")
  rhr_col <- resolve_sleep_col(mapping, hdr, "garmin_rhr")
  duration_col <- resolve_sleep_col(mapping, hdr, "garmin_duration")
  if (!is.null(sleep_col)) rename_map[["Sleep_Score"]] <- sleep_col
  if (!is.null(hrv_col)) rename_map[["HRV"]] <- hrv_col
  if (!is.null(rhr_col)) rename_map[["RHR"]] <- rhr_col
  if (!is.null(duration_col)) rename_map[["Sleep_Duration"]] <- duration_col
  sleep_df_raw %>% rename(!!!rename_map) %>%
    mutate(across(any_of(c("Sleep_Score", "HRV", "RHR", "Sleep_Duration")), clean_val_final))
}

sleep_rows <- sleep_complete %>%
  filter(!is.na(Sleep_Score), !is.na(HRV), !is.na(RHR)) %>%
  left_join(calendar_daily %>% select(Date, Sensor, Sensor_Raw, Flags, Flags_List), by = "Date") %>%
  mutate(Sensor = ifelse(is.na(Sensor) & is.na(Sensor_Raw) & !is.na(default_sensor_id), default_sensor_id, Sensor))

if (nrow(sleep_rows) == 0) stop("No complete sleep nights available after initial filtering.")

if (any(duplicated(sleep_rows$Date))) {
  dup_dates <- sleep_rows %>% count(Date) %>% filter(n > 1) %>% pull(Date)
  warning(sprintf("Duplicate sleep rows detected for dates: %s", paste(format(dup_dates, "%Y-%m-%d"), collapse = ", ")))
  sleep_rows <- sleep_rows %>% arrange(Date) %>% distinct(Date, .keep_all = TRUE)
}

sensor_rows <- map_dfr(seq_len(nrow(sleep_rows)), function(i) {
  compute_nightly_sensor_summary(sleep_rows[i, ], sensor_raw, default_sensor_id, config$matching_padding_minutes %||% 30)
})

if (any(duplicated(sensor_rows$Date))) {
  dup_dates <- sensor_rows %>% count(Date) %>% filter(n > 1) %>% pull(Date)
  warning(sprintf("Duplicate sleep records detected for dates: %s", paste(format(dup_dates, "%Y-%m-%d"), collapse = ", ")))
  sensor_rows <- sensor_rows %>% arrange(Date) %>% distinct(Date, .keep_all = TRUE)
}

final_joined <- sensor_rows %>%
  left_join(lifestyle_df, by = "Date")
if (nrow(final_joined) == 0) stop("No joined data after merging lifestyle and sleep/nightly records.")

selected_metrics <- c("Sleep_Score", "RHR", "HRV", "Sleep_Duration")
missing_metrics <- setdiff(selected_metrics, names(final_joined))
if (length(missing_metrics) > 0) stop(sprintf("Required metrics missing: %s", paste(missing_metrics, collapse = ", ")))

base_predictors <- meta_cfg$meta$base_predictors %||% c("Avg_Temp", "Temp_SD", "Avg_Rel_Hum", "Rel_Hum_SD", "Avg_Abs_Hum", "Abs_Hum_SD")
factor_columns <- meta_cfg$meta$factor_columns %||% c(
  "Display_30min_off", "Display_1h_off", "Display_Off_Level", "Exercise_Level", "Traveling_Vacation",
  "Late_Meals", "Window_Open"
)
interaction_terms <- meta_cfg$meta$interaction_terms %||% c(
  "Avg_Temp:Display_30min_off", "Avg_Temp:Exercise_Level", "Display_1h_off:Exercise_Level"
)
meta_interaction_terms <- meta_cfg$meta$meta_interaction_terms %||% c(
  "Outcome:Avg_Temp", "Outcome:Display_Off_Level", "Outcome:Exercise_Level"
)

final_model_data <- final_joined %>%
  mutate(
    Display_30min_off = ifelse(is.na(Display_30min_off), FALSE, Display_30min_off),
    Display_1h_off = ifelse(is.na(Display_1h_off), FALSE, Display_1h_off),
    Traveling_Vacation = ifelse(is.na(Traveling_Vacation), FALSE, Traveling_Vacation),
    Late_Meals = ifelse(is.na(Late_Meals), FALSE, Late_Meals),
    Window_Open = ifelse(is.na(Window_Open), FALSE, Window_Open),
    Exercise_Level = factor(ifelse(is.na(Exercise_Level), "none", Exercise_Level), levels = c("none", "light", "moderate", "vigorous")),
    Display_Before_Bed = factor(ifelse(is.na(Display_Before_Bed), "none", Display_Before_Bed), levels = c("none", "30min", "1h")),
    Display_Off_Level = factor(ifelse(is.na(Display_Off_Level), "none", Display_Off_Level), levels = c("none", "30min", "1h"))
  )

valid_predictor_terms <- function(data, terms) {
  terms[sapply(terms, function(term) {
    if (!(term %in% names(data))) return(FALSE)
    col <- data[[term]]
    non_na <- col[!is.na(col)]
    if (length(non_na) == 0) return(FALSE)
    if (is.factor(col) || is.character(col) || is.logical(col)) {
      n_distinct(non_na) > 1
    } else if (is.numeric(col)) {
      length(unique(non_na)) > 1 && var(non_na, na.rm = TRUE) > 0
    } else {
      n_distinct(non_na) > 1
    }
  })]
}

valid_interaction_terms <- function(data, terms, allowed_terms) {
  terms[sapply(terms, function(term) {
    parts <- str_split(term, ":", simplify = TRUE)
    all(parts %in% allowed_terms)
  })]
}

fit_outcome_model <- function(outcome, data) {
  main_terms <- valid_predictor_terms(data, c(base_predictors, factor_columns))
  interaction_terms_filtered <- valid_interaction_terms(data, interaction_terms, main_terms)
  formula_terms <- c(main_terms, interaction_terms_filtered)
  if (length(formula_terms) == 0) {
    formula <- as.formula(paste0(outcome, " ~ 1"))
  } else {
    formula <- as.formula(paste0(outcome, " ~ ", paste(formula_terms, collapse = " + ")))
  }
  lm(formula, data = data)
}

models <- list()
for (metric in selected_metrics) {
  data_for_metric <- final_model_data %>% filter(!is.na(.data[[metric]]))
  if (nrow(data_for_metric) < 10) {
    message(sprintf("Skipping model for %s: insufficient rows (%d)", metric, nrow(data_for_metric)))
    next
  }
  models[[metric]] <- fit_outcome_model(metric, data_for_metric)
}

message("\n===== Outcome Models =====")
for (metric in names(models)) {
  model <- models[[metric]]
  message(sprintf("\n--- %s ---", metric))
  print(glance(model))
  print(tidy(model))
}

message("\n===== Meta-Model =====")
meta_predictors <- c(base_predictors, factor_columns)
meta_data <- final_model_data %>%
  select(Date, all_of(selected_metrics), all_of(meta_predictors)) %>%
  pivot_longer(cols = all_of(selected_metrics), names_to = "Outcome", values_to = "Value") %>%
  filter(!is.na(Value))
if (nrow(meta_data) < 20) {
  warning("Not enough data for meta-modeling.\n")
} else {
  valid_meta_preds <- valid_predictor_terms(meta_data, meta_predictors)
  valid_meta_interactions <- valid_interaction_terms(meta_data, meta_interaction_terms, c("Outcome", valid_meta_preds))
  meta_formula <- paste0(
    "Value ~ Outcome",
    if (length(valid_meta_preds) > 0) paste0(" + ", paste(valid_meta_preds, collapse = " + ")) else "",
    if (length(valid_meta_interactions) > 0) paste0(" + ", paste(valid_meta_interactions, collapse = " + ")) else ""
  )
  meta_model <- lm(as.formula(meta_formula), data = meta_data)
  print(glance(meta_model))
  print(tidy(meta_model))
}

message("\nSleepMetaModel.R completed.")
if (dry_run) message("Dry-run mode: no plots or output files were generated.")
