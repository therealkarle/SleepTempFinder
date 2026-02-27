# SleepTempFinder.R
# Primary analysis script: ingest Garmin sleep exports + room sensor CSVs,
# align on sleep periods, compute nightly averages, apply filters, and produce
# statistics & plots.
#
# Changes since original version:
#   * centralized date/time parsing orders via config
#   * added locale configuration for sensor imports
#   * moved plot colors/labels into config and derived vectors
#   * tightened scope of intermediate variables using local()
#   * removed obsolete dataframes and inline simple transforms
#   * unified sensor header detection/lookup helpers
#   * added dry-run flag to suppress plotting
#   * added utility helpers (parse_datetime_safe, night_date, map_sensor_to_nightly)
#   * updated config.yaml with new sections (parse_orders, locale, plot)
#
# --- 1. ENVIRONMENT SETUP ---
if (!require("rstudioapi")) install.packages("rstudioapi")
pkgs <- c("tidyverse", "lubridate", "yaml", "broom", "GGally", "gridExtra", "grid", "scales")
for (pkg in pkgs) { 
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE) 
}

if (interactive()) setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load configuration (primary + optional private override)
config <- read_yaml("config.yaml")
private_cfg_path <- "config.private.yaml"
if (file.exists(private_cfg_path)) {
  try({
    private_cfg <- read_yaml(private_cfg_path)
    # shallow merge: top-level keys in private_cfg replace those in config
    for (k in names(private_cfg)) config[[k]] <- private_cfg[[k]]
    cat("Private config loaded (RScript/config.private.yaml)\n")
  }, silent = TRUE)
} else {
  cat("No private config found (RScript/config.private.yaml) - using repo config.yaml\n")
}

# extract commonly used sub-configs for convenience
orders <- config$parse_orders
loc <- config$locale
plot_cfg <- config$plot

# command-line arguments support (e.g. dry run)
args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
if(dry_run) cat("*** dry-run mode enabled (plots suppressed) ***\n")

# helper that applies parsing orders by type and quiet=TRUE
parse_datetime_safe <- function(x, type = "garmin_datetime") {
  if (is.null(orders[[type]])) {
    warning("no parse orders for type: ", type)
    return(parse_date_time(x, quiet = TRUE))
  }
  parse_date_time(x, orders = orders[[type]], quiet = TRUE)
}

# locale object for sensor CSV imports
sensor_locale <- locale(decimal_mark = loc$decimal_mark %||% ",")

# --- 2. DATA CLEANING & LOADING ---
# helper for recursive discovery of CSV files under data directory
list_csv_files <- function(dir, recursive = FALSE) {
  if (!dir.exists(dir)) return(character(0))
  list.files(path = dir, pattern = "\\.csv$", recursive = recursive, full.names = TRUE)
}

# strip trailing numeric suffixes in parentheses, e.g. "Foo (1).csv" -> "Foo.csv"
canonical_basename <- function(path) {
  b <- basename(path)
  # remove parenthetical digits before extension
  b <- sub("\\s*\\(\\d+\\)(?=\\.[^.]+$)", "", b, perl = TRUE)
  b
}
# helper used when config lists explicit files that might be renamed copies
expand_explicit <- function(explicit_paths, discovered) {
  out <- character(0)
  for (p in explicit_paths) {
    if (p %in% discovered) {
      out <- c(out, p)
    } else {
      base <- basename(p)
      matches <- discovered[basename(discovered) == base]
      if (length(matches) > 0) {
        out <- c(out, matches)
      } else {
        out <- c(out, p)
      }
    }
  }
  unique(out)
}
# classify a CSV by header row using configured column mappings
is_sleep_csv <- function(path, mapping) {
  hdr <- tryCatch(names(read.csv(path, nrows = 1, stringsAsFactors = FALSE, check.names = FALSE)),
                  error = function(e) character(0))
  # primary check: configured date column
  if(mapping$garmin_date %in% hdr) return(TRUE)
  # fallback: presence of both bedtime and waketime columns
  if(mapping$garmin_bedtime %in% hdr && mapping$garmin_waketime %in% hdr) return(TRUE)
  FALSE
}

# helper to inspect a sensor CSV and return the matching temp_files entry (or NULL)
detect_sensor_config <- function(path) {
  base <- basename(path)
  # explicit paths first
  for (id in names(config$temp_files)) {
    if (base == basename(config$temp_files[[id]]$path)) return(config$temp_files[[id]])
  }
  hdr <- tryCatch(names(suppressWarnings(read_delim(path, delim = ",", n_max = 1, locale = sensor_locale, show_col_types = FALSE))),
                  error = function(e) character(0))
  for (id in names(config$temp_files)) {
    f <- config$temp_files[[id]]
    if (all(c(f$col_time, f$col_temp, f$col_hum) %in% hdr)) return(f)
  }
  NULL
}

is_sensor_csv <- function(path, temp_files) {
  !is.null(detect_sensor_config(path))
}

get_sensor_file_info <- function(path) {
  cfg <- detect_sensor_config(path)
  if (is.null(cfg)) {
    # fallback to first mapping if nothing matched
    config$temp_files[[1]]
  } else {
    cfg
  }
}

read_garmin_fixed <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines[1] <- gsub("^[^\t[:alnum:][:punct:][:space:]]+", "", lines[1])
  lines <- gsub("([+-]\\d+),(\\d+°)", "\\1.\\2", lines)
  lines <- gsub(",+$", "", lines)
  df <- read.csv(text = lines, sep = ",", header = TRUE, 
                 check.names = FALSE, stringsAsFactors = FALSE, 
                 na.strings = c(" ", "--", "NA", ""))
  return(as_tibble(df, .name_repair = "unique"))
}

clean_val_final <- function(x) {
  res <- map_dbl(as.character(x), function(val) {
    if (is.na(val) || val == "" || val == "--") return(NA_real_)
    if (str_detect(val, "h")) {
      h <- as.numeric(str_extract(val, "\\d+(?=h)"))
      m <- as.numeric(str_extract(val, "\\d+(?=min)"))
      return(ifelse(is.na(h), 0, h) + (ifelse(is.na(m), 0, m)/60))
    }
    num_str <- str_extract(val, "[-+]?[0-9]*\\.?[0-9]+")
    return(as.numeric(num_str))
  })
  return(res)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

trim_vector <- function(x) {
  x <- str_trim(as.character(x))
  x[x != "" & !is.na(x)]
}

# convert a timestamp to the corresponding "night" date by subtracting 12h
night_date <- function(ts) as.Date(ts - hours(12))

collapse_flags <- function(x) {
  vals <- sort(unique(trim_vector(x)))
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = ", ")
}

split_flags <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(character(0))
  sort(unique(trim_vector(str_split(x, "\\s*,\\s*")[[1]])))
}

read_ics_lines <- function(mode, url_value = NULL, file_value = NULL) {
  if (tolower(mode) == "url") {
    con <- base::url(url_value)
    on.exit(close(con), add = TRUE)
    return(readLines(con, warn = FALSE, encoding = "UTF-8"))
  }
  readLines(file_value, warn = FALSE, encoding = "UTF-8")
}

unfold_ics_lines <- function(lines) {
  out <- character(0)
  for (ln in lines) {
    if (length(out) > 0 && str_detect(ln, "^[ \\t]")) {
      out[length(out)] <- paste0(out[length(out)], str_sub(ln, 2))
    } else {
      out <- c(out, ln)
    }
  }
  out
}

extract_ics_prop <- function(lines, prop) {
  idx <- which(str_detect(lines, paste0("^", prop, "(;[^:]*)?:")))
  if (length(idx) == 0) return(list(value = NA_character_, params = ""))
  line <- lines[idx[1]]
  value <- str_replace(line, "^[^:]*:", "")
  params <- str_match(line, paste0("^", prop, "(;[^:]*)?:"))[, 2]
  list(value = value, params = params %||% "")
}

parse_ics_time <- function(value) {
  if (is.na(value) || value == "") return(NA)
  val <- str_trim(value)
  if (str_detect(val, "^\\d{8}$")) return(ymd(val, quiet = TRUE))
  if (str_detect(val, "^\\d{8}T\\d{6}Z$")) return(ymd_hms(val, tz = "UTC", quiet = TRUE))
  if (str_detect(val, "^\\d{8}T\\d{6}$")) return(ymd_hms(val, quiet = TRUE))
  if (str_detect(val, "^\\d{8}T\\d{4}$")) return(parse_date_time(val, orders = "Ymd HM", quiet = TRUE))
  parse_date_time(val, orders = c("Ymd HMS", "Ymd HM", "Y-m-d H:M:S", "Y-m-d H:M", "Y-m-d"), quiet = TRUE)
}

parse_sensor_flags <- function(text_value) {
  text_value <- text_value %||% ""
  sensor_match <- str_match(text_value, regex("sensor\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))
  flags_match <- str_match(text_value, regex("flags?\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))

  sensor_raw <- str_trim(sensor_match[, 2] %||% NA_character_)
  sensor_name <- sensor_raw
  if (!is.na(sensor_name) && str_detect(sensor_name, "/")) {
    sensor_name <- str_trim(str_split(sensor_name, "/", n = 2)[[1]][2])
  }

  flags <- character(0)
  if (!is.na(flags_match[, 2] %||% NA_character_)) {
    flags <- split_flags(flags_match[, 2])
  }

  list(sensor_raw = ifelse(is.na(sensor_raw) || sensor_raw == "", NA_character_, sensor_raw),
       sensor_name = ifelse(is.na(sensor_name) || sensor_name == "", NA_character_, sensor_name),
       flags = flags)
}

load_calendar_daily <- function(calendar_cfg, parser_cfg) {
  if (is.null(calendar_cfg) || !isTRUE(calendar_cfg$enabled)) {
    return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
  }

  mode <- tolower(calendar_cfg$mode %||% "url")
  url_value <- calendar_cfg$url %||% ""
  file_value <- calendar_cfg$file_path %||% ""

  if (mode == "url" && url_value == "") {
    cat("Calendar enabled but URL is empty. Calendar assignments skipped.\n")
    return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
  }
  if (mode == "file" && file_value == "") {
    cat("Calendar enabled but file_path is empty. Calendar assignments skipped.\n")
    return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
  }

  lines <- tryCatch(read_ics_lines(mode = mode, url_value = url_value, file_value = file_value),
                    error = function(e) {
                      cat(sprintf("Calendar load failed: %s\n", e$message))
                      character(0)
                    })
  if (length(lines) == 0) {
    return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
  }

  unfolded <- unfold_ics_lines(lines)
  begin_idx <- which(unfolded == "BEGIN:VEVENT")
  end_idx <- which(unfolded == "END:VEVENT")
  n_events <- min(length(begin_idx), length(end_idx))
  if (n_events == 0) {
    cat("Calendar loaded, but no VEVENT entries found.\n")
    return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
  }

  event_rows <- vector("list", n_events)
  for (i in seq_len(n_events)) {
    chunk <- unfolded[begin_idx[i]:end_idx[i]]
    dt_start <- extract_ics_prop(chunk, "DTSTART")
    dt_end <- extract_ics_prop(chunk, "DTEND")
    summary_p <- extract_ics_prop(chunk, "SUMMARY")
    description_p <- extract_ics_prop(chunk, "DESCRIPTION")

    start_time <- parse_ics_time(dt_start$value)
    end_time <- parse_ics_time(dt_end$value)
    if (is.na(start_time)) next

    start_date <- as.Date(start_time)
    end_date <- start_date
    if (!is.na(end_time)) {
      if (str_detect(dt_end$params %||% "", "VALUE=DATE")) {
        end_date <- as.Date(end_time) - days(1)
      } else {
        end_date <- as.Date(end_time)
      }
    }
    if (is.na(end_date) || end_date < start_date) end_date <- start_date

    fields_order <- toupper(unlist(parser_cfg$event_field_priority %||% c("SUMMARY", "DESCRIPTION")))
    field_text <- character(0)
    for (field_name in fields_order) {
      if (field_name == "SUMMARY" && !is.na(summary_p$value) && summary_p$value != "") field_text <- c(field_text, summary_p$value)
      if (field_name == "DESCRIPTION" && !is.na(description_p$value) && description_p$value != "") field_text <- c(field_text, description_p$value)
    }
    combined_text <- paste(unique(field_text), collapse = "; ")
    parsed <- parse_sensor_flags(combined_text)

    date_seq <- seq(start_date, end_date, by = "1 day")
    event_rows[[i]] <- tibble(Date = as.Date(date_seq),
                              Sensor_Raw = parsed$sensor_raw,
                              Sensor = parsed$sensor_name,
                              Flags_List = list(parsed$flags))
  }

  events_daily <- bind_rows(event_rows)
  if (nrow(events_daily) == 0) {
    cat("Calendar events found, but no Sensor/Flags metadata parsed.\n")
    return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
  }

  calendar_daily <- events_daily %>%
    group_by(Date) %>%
    summarise(
      sensor_values = list(unique(na.omit(Sensor))),
      flags_values = list(sort(unique(unlist(Flags_List)))),
      .groups = "drop"
    ) %>%
    mutate(
      Sensor = map_chr(sensor_values, ~ if (length(.x) == 1) .x[[1]] else NA_character_),
      Flags_List = map(flags_values, ~ trim_vector(.x)),
      Flags = map_chr(Flags_List, collapse_flags)
    )

  conflicting_sensor_days <- calendar_daily %>% filter(map_int(sensor_values, length) > 1) %>% nrow()
  if (conflicting_sensor_days > 0) {
    cat(sprintf("Calendar warning: %d day(s) had conflicting sensors and were set to NA.\n", conflicting_sensor_days))
  }

  calendar_daily <- calendar_daily %>% select(Date, Sensor, Flags, Flags_List)

  cat(sprintf("Calendar loaded: %d day assignments parsed.\n", nrow(calendar_daily)))
  calendar_daily
}

apply_calendar_three_day_rule <- function(calendar_daily, assignment_cfg) {
  if (nrow(calendar_daily) == 0) return(calendar_daily)
  if (is.null(assignment_cfg) || !isTRUE(assignment_cfg$require_prev_next_day)) return(calendar_daily)

  full_dates <- seq(min(calendar_daily$Date, na.rm = TRUE), max(calendar_daily$Date, na.rm = TRUE), by = "1 day")
  aligned <- tibble(Date = as.Date(full_dates)) %>%
    left_join(calendar_daily %>% select(Date, Sensor, Flags, Flags_List), by = "Date") %>%
    arrange(Date)

  sensor_vec <- aligned$Sensor
  sensor_prev <- lag(sensor_vec)
  sensor_next <- lead(sensor_vec)
  aligned$Sensor <- ifelse(!is.na(sensor_vec) & sensor_vec == sensor_prev & sensor_vec == sensor_next, sensor_vec, NA_character_)

  flags_raw <- map(aligned$Flags_List, ~ if (is.null(.x)) character(0) else trim_vector(.x))
  flags_keep <- vector("list", length(flags_raw))
  for (i in seq_along(flags_raw)) {
    prev_flags <- if (i > 1) flags_raw[[i - 1]] else character(0)
    next_flags <- if (i < length(flags_raw)) flags_raw[[i + 1]] else character(0)
    flags_keep[[i]] <- sort(unique(intersect(intersect(flags_raw[[i]], prev_flags), next_flags)))
  }

  aligned$Flags_List <- flags_keep
  aligned$Flags <- map_chr(aligned$Flags_List, collapse_flags)
  aligned
}

apply_analysis_subset_filter <- function(df, filter_cfg) {
  if (is.null(filter_cfg) || !isTRUE(filter_cfg$enabled) || nrow(df) == 0) return(df)

  sensor_include <- trim_vector(unlist(filter_cfg$sensor_include %||% character(0)))
  flags_include <- trim_vector(unlist(filter_cfg$flags_include %||% character(0)))
  flags_mode <- tolower(filter_cfg$flags_mode %||% "any")
  if (!flags_mode %in% c("any", "all")) flags_mode <- "any"

  out <- df
  if (length(sensor_include) > 0 && "Sensor" %in% names(out)) {
    out <- out %>% filter(!is.na(Sensor), Sensor %in% sensor_include)
  }

  if (length(flags_include) > 0 && "Flags" %in% names(out)) {
    out <- out %>%
      mutate(.flags_vec = map(Flags, split_flags)) %>%
      filter(map_lgl(.flags_vec, function(row_flags) {
        if (flags_mode == "all") all(flags_include %in% row_flags) else any(flags_include %in% row_flags)
      })) %>%
      select(-.flags_vec)
  }

  out
}

# determine which files under data_directory will be used
classification <- local({
  all_data_files <- list_csv_files(config$data_directory, recursive = isTRUE(config$scan_recursive))
  cat(sprintf("Found %d CSV file(s) under %s (recursive=%s)\n", length(all_data_files), config$data_directory, isTRUE(config$scan_recursive)))

  # classify discovered files
  sleep_candidates <- all_data_files[sapply(all_data_files, is_sleep_csv, mapping = config$column_names)]
  sensor_candidates <- all_data_files[sapply(all_data_files, is_sensor_csv, temp_files = config$temp_files)]
  unclassified_files <- setdiff(all_data_files, c(sleep_candidates, sensor_candidates))
  cat(sprintf("  sleep candidates: %d\n", length(sleep_candidates)))
  if(length(sleep_candidates) > 0) cat(paste0("    ", sleep_candidates, collapse="\n"), "\n")
  # show canonical names
  if(length(sleep_candidates) > 0) {
    cat("    canonical: ", paste(unique(canonical_basename(sleep_candidates)), collapse=", "), "\n")
  }
  cat(sprintf("  sensor candidates: %d\n", length(sensor_candidates)))
  if(length(sensor_candidates) > 0) cat(paste0("    ", sensor_candidates, collapse="\n"), "\n")
  if(length(sensor_candidates) > 0) {
    cat("    canonical: ", paste(unique(canonical_basename(sensor_candidates)), collapse=", "), "\n")
  }
  if (length(unclassified_files) > 0) {
    cat("Unclassified files (neither sleep nor sensor detected):\n", paste(unclassified_files, collapse = "\n"), "\n")
  }

  # expand explicit paths and merge with discovered names
  explicit_sleep_raw <- file.path(config$data_directory, config$sleep_data_sources)
  explicit_sensor_raw <- unlist(lapply(config$temp_files, function(x) file.path(config$data_directory, x$path)))
  explicit_sleep <- expand_explicit(explicit_sleep_raw, all_data_files)
  explicit_sensor <- expand_explicit(explicit_sensor_raw, all_data_files)

  # warn about missing explicit files
  missing <- setdiff(c(explicit_sleep, explicit_sensor), all_data_files)
  if (length(missing) > 0) {
    message("Warning: explicit files listed in config not found under data_directory:\n", paste(missing, collapse="\n"))
  }

  list(
    all_sleep_files = unique(c(explicit_sleep, sleep_candidates)),
    all_sensor_files = unique(c(explicit_sensor, sensor_candidates))
  )
})

all_sleep_files <- classification$all_sleep_files
all_sensor_files <- classification$all_sensor_files

# explicit lists from config are still respected but merged with discoveries
# (handled above in the classification block)

mapping <- config$column_names

# read sleep data, track source file and canonical name per row
sleep_df_raw <- map_df(all_sleep_files, function(f) {
  df <- read_garmin_fixed(f)
  hdr <- names(df)
  # determine which column to use for Date
  date_col <- mapping$garmin_date
  if(!(date_col %in% hdr)) {
    alt <- hdr[str_detect(hdr, regex("^Sleep Score", ignore_case = TRUE))]
    if(length(alt) == 1) {
      date_col <- alt[[1]]
      cat(sprintf("Warning: using alternate date column '%s' for file %s\n", date_col, f))
    }
  }
  df %>%
    mutate(Source_File = f,
           Source_Name = canonical_basename(f)) %>%
    mutate(Date = as.Date(!!sym(date_col)),
           bedtime = parse_datetime_safe(!!sym(mapping$garmin_bedtime), type = "garmin_time"),
           waketime = parse_datetime_safe(!!sym(mapping$garmin_waketime), type = "garmin_time")) %>%
    mutate(waketime = update(waketime, year = year(Date), month = month(Date), mday = day(Date)),
           bedtime = update(bedtime, year = year(Date), month = month(Date), mday = day(Date)),
           bedtime = if_else(bedtime > waketime, bedtime - days(1), bedtime)) %>%
    mutate(across(any_of(unlist(mapping[4:length(mapping)])), clean_val_final))
})

# helper: choose sensor file configuration based on path or header
get_sensor_file_info <- function(path) {
  base <- basename(path)
  # first try to match explicit path in config
  for (id in names(config$temp_files)) {
    if (base == basename(config$temp_files[[id]]$path)) return(config$temp_files[[id]])
  }
  # otherwise, try header matching
  hdr <- tryCatch(names(suppressWarnings(read_delim(path, delim = ",", n_max = 1, locale = sensor_locale, show_col_types = FALSE))),
                  error = function(e) character(0))
  for (id in names(config$temp_files)) {
    f <- config$temp_files[[id]]
    if (all(c(f$col_time, f$col_temp, f$col_hum) %in% hdr)) return(f)
  }
  # fallback to first mapping
  config$temp_files[[1]]
}

# read all discovered sensor CSVs, track source file and attempt column renaming
sensor_raw <- map_df(all_sensor_files, function(fp) {
  f_info <- get_sensor_file_info(fp)
  suppressWarnings(read_delim(fp, delim = ",", locale = sensor_locale, show_col_types = FALSE)) %>%
    rename(timestamp = !!f_info$col_time, room_temp = !!f_info$col_temp, rel_hum = !!f_info$col_hum, abs_hum = `Abs Humidity(g/m³)`) %>%
    mutate(timestamp = parse_datetime_safe(timestamp, type = "sensor_timestamp")) %>%
    mutate(Source_File = fp,
           Source_Name = canonical_basename(fp))
})

calendar_daily_raw <- load_calendar_daily(config$calendar_source, config$calendar_parser)
calendar_daily <- apply_calendar_three_day_rule(calendar_daily_raw, config$calendar_assignment)

# --- OUTLIER FILTERING (optional: configured in config.yaml) ---
apply_outlier_filter <- function(df, cols = c("room_temp","rel_hum","abs_hum"), method = "iqr", iqr_mult = 1.5, z_thresh = 3) {
  for(col in cols) {
    if(!col %in% names(df)) next
    vals <- df[[col]]
    if(method == "iqr") {
      Q1 <- quantile(vals, 0.25, na.rm=TRUE)
      Q3 <- quantile(vals, 0.75, na.rm=TRUE)
      IQRv <- Q3 - Q1
      lower <- Q1 - iqr_mult * IQRv
      upper <- Q3 + iqr_mult * IQRv
      df <- df %>% filter(is.na(.data[[col]]) | (.data[[col]] >= lower & .data[[col]] <= upper))
    } else if(method == "zscore") {
      m <- mean(vals, na.rm=TRUE)
      s <- sd(vals, na.rm=TRUE)
      if(is.na(s) || s == 0) next
      df <- df %>% filter(is.na(.data[[col]]) | (abs((.data[[col]] - m)/s) <= z_thresh))
    }
  }
  return(df)
}

# Track dates removed by outlier filtering (both sensor and nightly stages)
excluded_outlier_dates <- c()

# helper for translating sensor column names to nightly-average equivalents
map_sensor_to_nightly <- function(cols) {
  sapply(cols, function(c) {
    if(c == "room_temp") return("Avg_Temp")
    if(c == "rel_hum") return("Avg_Rel_Hum")
    if(c == "abs_hum") return("Avg_Abs_Hum")
    c
  }, USE.NAMES = FALSE)
}

if(!is.null(config$outlier_filter) && isTRUE(config$outlier_filter$enabled)) {
  local({
    cols_cfg <- if(!is.null(config$outlier_filter$columns)) unlist(config$outlier_filter$columns) else c("room_temp","rel_hum","abs_hum")
  method_cfg <- if(!is.null(config$outlier_filter$method)) config$outlier_filter$method else "iqr"
  iqr_cfg <- if(!is.null(config$outlier_filter$iqr_multiplier)) config$outlier_filter$iqr_multiplier else 1.5
  z_cfg <- if(!is.null(config$outlier_filter$z_threshold)) config$outlier_filter$z_threshold else 3
  stage_cfg <- if(!is.null(config$outlier_filter$apply_stage)) config$outlier_filter$apply_stage else "sensor"
  if(tolower(stage_cfg) == "sensor") {
    # wrap temporary calculations in local scope to avoid polluting global namespace
    local({
      cfg_cols_exist <- intersect(cols_cfg, names(sensor_raw))
      # Keep a copy of the raw sensor data before filtering so we can detect nights that lost nightly averages
      sensor_raw_before <- sensor_raw

      # Count per-night sensor readings and valid values before filtering (for configured cols)
      sensor_before <- sensor_raw_before %>%
        mutate(Date = as.Date(timestamp - hours(12))) %>%
        group_by(Date) %>%
        summarise(across(all_of(cfg_cols_exist), ~sum(!is.na(.x)), .names = "valid_{col}"), n = n(), .groups = 'drop')

      n_before <- nrow(sensor_raw_before)
      sensor_raw <- apply_outlier_filter(sensor_raw, cols_cfg, method_cfg, iqr_cfg, z_cfg)
      n_after <- nrow(sensor_raw)

      # Count valid values after filtering
      sensor_after <- sensor_raw %>%
        mutate(Date = as.Date(timestamp - hours(12))) %>%
        group_by(Date) %>%
        summarise(across(all_of(cfg_cols_exist), ~sum(!is.na(.x)), .names = "valid_{col}"), n = n(), .groups = 'drop')

      # Identify dates where there were valid values before, but none after (for any configured column)
      if(nrow(sensor_before) > 0) {
        valid_before_mat <- sensor_before %>% select(Date, starts_with("valid_"))
        valid_after_mat <- sensor_after %>% select(Date, starts_with("valid_"))

        valid_before_mat$has_valid_before <- apply(valid_before_mat %>% select(-Date), 1, function(r) any(r > 0))
        if(nrow(valid_after_mat) > 0) {
          valid_after_mat$has_valid_after <- apply(valid_after_mat %>% select(-Date), 1, function(r) any(r > 0))
        } else {
          valid_after_mat <- tibble(Date = as.Date(character(0)), has_valid_after = logical(0))
        }

        merged_val <- valid_before_mat %>% left_join(valid_after_mat, by = "Date") %>% mutate(has_valid_after = ifelse(is.na(has_valid_after), FALSE, has_valid_after))
        dates_lost_sensor <- merged_val %>% filter(has_valid_before == TRUE & has_valid_after == FALSE) %>% pull(Date)

        # Extra check via nightly averages: if nightly avg existed before but is NA after, treat as outlier-removed
        sensor_nightly_before <- sensor_raw_before %>%
          mutate(Date = night_date(timestamp)) %>%
          group_by(Date) %>%
          summarise(Avg_Temp = mean(room_temp, na.rm=TRUE), Avg_Rel_Hum = mean(rel_hum, na.rm=TRUE), Avg_Abs_Hum = mean(abs_hum, na.rm=TRUE), .groups = 'drop')

        sensor_nightly_after <- sensor_raw %>%
          mutate(Date = night_date(timestamp)) %>%
          group_by(Date) %>%
          summarise(Avg_Temp = mean(room_temp, na.rm=TRUE), Avg_Rel_Hum = mean(rel_hum, na.rm=TRUE), Avg_Abs_Hum = mean(abs_hum, na.rm=TRUE), .groups = 'drop')

        # Determine nights where any nightly average existed before filtering but is NA/NaN after filtering
        merged_nightly <- sensor_nightly_before %>% left_join(sensor_nightly_after, by = "Date", suffix = c("_before", "_after"))

        cols_to_check <- c("Avg_Temp", "Avg_Rel_Hum", "Avg_Abs_Hum")
        cols_present <- intersect(cols_to_check, names(merged_nightly))

        if(length(cols_present) > 0) {
          lost_mat <- sapply(cols_present, function(col) {
            before <- merged_nightly[[paste0(col, "_before")]]
            after <- merged_nightly[[paste0(col, "_after")]]
            (!is.na(before)) & (is.na(after) | is.nan(after))
          })
          # determine specific nights lost by average becoming NA/NaN
          if(is.matrix(lost_mat)) {
            lost_rows <- apply(lost_mat, 1, any)
          } else {
            lost_rows <- as.logical(lost_mat)
          }
          nights_lost_by_avg <- merged_nightly$Date[which(lost_rows)]
        } else {
          nights_lost_by_avg <- as.Date(character(0))
        }
      }
      # after per-night checks, combine with sensor-row losses
      dates_to_mark <- unique(c(dates_lost_sensor, nights_lost_by_avg))
      if(length(dates_to_mark) > 0) {
        removed_dates_fmt <- format(dates_to_mark, "%d.%m.%Y")
        excluded_outlier_dates <- unique(c(excluded_outlier_dates, removed_dates_fmt))
        cat(sprintf("Outlier filter (sensor) removed data for nights (all values NA after filtering): %s\n", paste(removed_dates_fmt, collapse = ", ")))
      }

      # update global variables after local processing
      sensor_raw <<- sensor_raw
      excluded_outlier_dates <<- excluded_outlier_dates
      cat(sprintf("Outlier filter enabled (sensor): removed %d sensor rows\n", n_before - n_after))
    })
  } else {
    cat(sprintf("Outlier filter enabled but set to apply at '%s' stage\n", stage_cfg))
  }
  })
}

# build a per-night sensor summary (after any outlier filtering)
sensor_summary <- sensor_raw %>%
  mutate(Date = night_date(timestamp),
         Source_Name = canonical_basename(Source_File)) %>%
  group_by(Date) %>%
  summarise(
    Sensor_Files = list(unique(Source_File)),
    Sensor_Names = list(unique(Source_Name)),
    N_Readings = n(),
    Valid_Temp = sum(!is.na(room_temp)),
    Valid_Rel_Hum = sum(!is.na(rel_hum)),
    Valid_Abs_Hum = sum(!is.na(abs_hum)),
    .groups = "drop"
  )


# --- 3. DATA PREP & AUDIT ---
# nightly_review_df has been constructed above; it contains a row per night
# with all input sources (sleep file path, sensor file(s)) plus flags and averages.
sleep_complete <- sleep_df_raw %>%
  rename(Sleep_Score = !!sym(mapping$garmin_sleep_score), HRV = !!sym(mapping$garmin_hrv), RHR = !!sym(mapping$garmin_rhr))

# drop rows with missing critical sleep metrics immediately

# nightly mapping: compute per-night sensor averages and join calendar & sensor summaries
temp_mapped <- sleep_complete %>% 
  filter(!is.na(Sleep_Score), !is.na(HRV), !is.na(RHR)) %>%
  rowwise() %>% 
  mutate(Avg_Temp = mean(sensor_raw$room_temp[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T),
         Avg_Rel_Hum = mean(sensor_raw$rel_hum[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T),
         Avg_Abs_Hum = mean(sensor_raw$abs_hum[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T)) %>%
  ungroup() %>%
  # attach calendar assignments
  left_join(calendar_daily %>% select(Date, Sensor, Flags, Flags_List), by = "Date") %>%
  # add per-night sensor file summary (could be multiple files)
  left_join(sensor_summary, by = "Date")

# --- OUTLIER FILTERING (nightly stage) ---
if(!is.null(config$outlier_filter) && isTRUE(config$outlier_filter$enabled)) {
  stage_cfg <- if(!is.null(config$outlier_filter$apply_stage)) config$outlier_filter$apply_stage else "sensor"
  if(tolower(stage_cfg) == "nightly") {
    cols_nightly <- if(!is.null(config$outlier_filter$columns)) unlist(config$outlier_filter$columns) else c("room_temp","rel_hum","abs_hum")
    # Map sensor column names to nightly average column names using helper
    cols_nightly_mapped <- map_sensor_to_nightly(cols_nightly)
    method_cfg <- if(!is.null(config$outlier_filter$method)) config$outlier_filter$method else "iqr"
    iqr_cfg <- if(!is.null(config$outlier_filter$iqr_multiplier)) config$outlier_filter$iqr_multiplier else 1.5
    z_cfg <- if(!is.null(config$outlier_filter$z_threshold)) config$outlier_filter$z_threshold else 3
    temp_mapped_before <- temp_mapped
    n_before <- nrow(temp_mapped_before)
    temp_mapped <- apply_outlier_filter(temp_mapped, cols = cols_nightly_mapped, method = method_cfg, iqr_mult = iqr_cfg, z_thresh = z_cfg)
    n_after <- nrow(temp_mapped)

    # Identify nights explicitly removed by nightly outlier filtering
    removed_dates_nightly <- setdiff(temp_mapped_before$Date, temp_mapped$Date)
    if(length(removed_dates_nightly) > 0) {
      removed_fmt <- format(removed_dates_nightly, "%d.%m.%Y")
      excluded_outlier_dates <- unique(c(excluded_outlier_dates, removed_fmt))
      cat(sprintf("Outlier filter (nightly) removed nights: %s\n", paste(removed_fmt, collapse = ", ")))
    }

    cat(sprintf("Outlier filter enabled (nightly): removed %d nights\n", n_before - n_after))
  }
}

# build a review data frame summarizing all inputs per night
# contains original sleep source file, sensor file(s), calendar sensor/flags, and final averages
nightly_review_df <- temp_mapped %>%
  mutate(
    Sleep_Source = Source_File,
    Sleep_Name = ifelse(str_detect(canonical_basename(Source_File), regex("schlaf", ignore_case = TRUE)),
                         "Sleep",
                         canonical_basename(Source_File)),
    Sensor_Sources = map_chr(Sensor_Files, ~ paste(.x, collapse = "; ")),
    Sensor_Names = map_chr(Sensor_Names, ~ paste(.x, collapse = "; "))
  )
# simple audit: how many distinct files and canonical names contribute
cat(sprintf("Review DF constructed: %d nights, %d unique sleep files (%d canonical), %d unique sensor file paths (%d canonical)\n", 
            nrow(nightly_review_df),
            n_distinct(nightly_review_df$Sleep_Source),
            n_distinct(nightly_review_df$Sleep_Name),
            n_distinct(unlist(nightly_review_df$Sensor_Files)),
            n_distinct(unlist(nightly_review_df$Sensor_Names))))

final_data_matched <- temp_mapped %>% 
  filter(!is.na(Avg_Temp), !is.nan(Avg_Temp), !is.na(Avg_Abs_Hum))

n_before_analysis_filter <- nrow(final_data_matched)
final_data_matched <- apply_analysis_subset_filter(final_data_matched, config$analysis_filter)
n_after_analysis_filter <- nrow(final_data_matched)

if(!is.null(config$analysis_filter) && isTRUE(config$analysis_filter$enabled)) {
  cat(sprintf("Analysis filter enabled: kept %d of %d nights\n", n_after_analysis_filter, n_before_analysis_filter))
}

if(nrow(final_data_matched) > 0) {
  full_dates <- seq(min(final_data_matched$Date, na.rm=T), max(final_data_matched$Date, na.rm=T), by="1 day")
  final_data_viz <- final_data_matched %>% complete(Date = full_dates)
} else {
  full_dates <- as.Date(character(0))
  final_data_viz <- final_data_matched
}

# --- EXCLUDE: Define reasons (Missing Sleep, Missing Room, Outliers) ---
excluded_sleep_dates <- sleep_complete %>% 
  filter(is.na(Sleep_Score) | is.na(HRV) | is.na(RHR)) %>% 
  pull(Date)

excluded_sensor_dates_all <- temp_mapped %>% 
  filter(is.na(Avg_Temp) | is.nan(Avg_Temp) | is.na(Avg_Abs_Hum)) %>% 
  pull(Date)

# Convert any collected outlier strings back to Date objects for set operations
excluded_outlier_dates_dates <- if(length(excluded_outlier_dates) > 0) as.Date(excluded_outlier_dates, "%d.%m.%Y") else as.Date(character(0))

# Nights that are missing room data but were NOT removed due to outlier-filtering
excluded_sensor_dates <- setdiff(excluded_sensor_dates_all, excluded_outlier_dates_dates)

# Unique total excluded nights across all reasons
unique_excluded_dates <- unique(c(excluded_sleep_dates, excluded_sensor_dates, excluded_outlier_dates_dates))

# Format for printing
excluded_sleep_fmt <- format(excluded_sleep_dates, "%d.%m.%Y")
excluded_sensor_fmt <- format(excluded_sensor_dates, "%d.%m.%Y")
excluded_outlier_fmt <- unique(excluded_outlier_dates)

calendar_days_raw_n <- nrow(calendar_daily_raw)
calendar_days_after_rule_n <- nrow(calendar_daily)
calendar_sensor_assigned_n <- sum(!is.na(temp_mapped$Sensor))
calendar_flags_assigned_n <- sum(!is.na(temp_mapped$Flags))

# --- OUTPUT: AUDIT ---
cat("\n===========================================================\n")
cat("                DATA QUALITY AUDIT\n")
cat("===========================================================\n")
cat(sprintf("Total nights detected:        %d\n", nrow(sleep_df_raw)))
cat(sprintf("Nights used for analysis:     %d\n", nrow(final_data_matched)))
cat(sprintf("Nights excluded total:        %d\n", length(unique_excluded_dates)))
cat("-----------------------------------------------------------\n")
cat(sprintf("Reason: Missing Sleep Data:   %d\n", length(excluded_sleep_fmt)))
if(length(excluded_sleep_fmt) > 0) cat(paste0("       [", paste(excluded_sleep_fmt, collapse = "], ["), "]\n"))
cat(sprintf("Reason: Missing Room Data:    %d\n", length(excluded_sensor_fmt)))
if(length(excluded_sensor_fmt) > 0) cat(paste0("       [", paste(excluded_sensor_fmt, collapse = "], ["), "]\n"))
cat(sprintf("Reason: Outlier Filtered:     %d\n", length(excluded_outlier_fmt)))
if(length(excluded_outlier_fmt) > 0) cat(paste0("       [", paste(excluded_outlier_fmt, collapse = "], ["), "]\n"))
cat("-----------------------------------------------------------\n")
cat(sprintf("Calendar days parsed:         %d\n", calendar_days_raw_n))
cat(sprintf("Calendar days after 3-day:    %d\n", calendar_days_after_rule_n))
cat(sprintf("Nights with Sensor assigned:  %d\n", calendar_sensor_assigned_n))
cat(sprintf("Nights with Flags assigned:   %d\n", calendar_flags_assigned_n))
if(!is.null(config$analysis_filter) && isTRUE(config$analysis_filter$enabled)) {
  cat(sprintf("Analysis filter kept:         %d / %d\n", n_after_analysis_filter, n_before_analysis_filter))
}
cat("===========================================================\n\n")

# --- OUTPUT: DESCRIPTIVE STATISTICS ---
sensor_nightly_raw <- sensor_raw %>%
  mutate(Date = night_date(timestamp)) %>% 
  group_by(Date) %>%
  summarise(Avg_Temp = mean(room_temp, na.rm=T), Avg_Rel_Hum = mean(rel_hum, na.rm=T), Avg_Abs_Hum = mean(abs_hum, na.rm=T), .groups = 'drop')

cat("                DESCRIPTIVE STATISTICS\n")
cat("===========================================================\n\n")
cat("TABLE 1: BIOMARKERS (Garmin Data)\n")
bio_vars <- list("Sleep_Score" = "Sleep_Score", "HRV" = "HRV", "RHR" = "RHR")
for(v_name in names(bio_vars)) {
  m_data <- final_data_matched[[bio_vars[[v_name]]]]; r_data <- sleep_complete[[bio_vars[[v_name]]]]
  cat(sprintf("%-15s (Used) | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n", v_name, mean(m_data, na.rm=T), sd(m_data, na.rm=T), min(m_data, na.rm=T), max(m_data, na.rm=T), sum(!is.na(m_data))))
  cat(sprintf("%-15s (Raw)  | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n\n", "", mean(r_data, na.rm=T), sd(r_data, na.rm=T), min(r_data, na.rm=T), max(r_data, na.rm=T), sum(!is.na(r_data))))
}
cat("\nTABLE 2: ROOM DATA (Nightly Averages)\n")
room_vars <- list("Room Temp" = "Avg_Temp", "Rel Humidity" = "Avg_Rel_Hum", "Abs Humidity" = "Avg_Abs_Hum")
for(v_name in names(room_vars)) {
  m_data <- final_data_matched[[room_vars[[v_name]]]]; r_data <- sensor_nightly_raw[[room_vars[[v_name]]]]
  cat(sprintf("%-15s (Used) | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n", v_name, mean(m_data, na.rm=T), sd(m_data, na.rm=T), min(m_data, na.rm=T), max(m_data, na.rm=T), sum(!is.na(m_data))))
  cat(sprintf("%-15s (Raw)  | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n\n", "", mean(r_data, na.rm=T), sd(r_data, na.rm=T), min(r_data, na.rm=T), max(r_data, na.rm=T), sum(!is.na(r_data))))
}

# --- 4. DASHBOARD DATAFRAME (TRANSPOSED) ---
# Build a transposed dashboard where rows are Metrics (incl. Sensor+Flags)
# and columns are Date strings. Filter out missing Date rows first.
dashboard_df <- final_data_viz %>%
  filter(!is.na(Date)) %>%
  select(Date, Sensor, Flags, Avg_Temp, Avg_Rel_Hum, Avg_Abs_Hum, Sleep_Score, HRV, RHR) %>%
  mutate(Date_Str = format(Date, "%d.%m.%Y")) %>%
  # ensure a common type for pivoting (character) to avoid type-combine errors
  mutate(across(-c(Date, Date_Str), ~ as.character(.x))) %>%
  pivot_longer(cols = -c(Date, Date_Str), names_to = "Metric", values_to = "Value") %>%
  select(-Date) %>%
  pivot_wider(names_from = Date_Str, values_from = Value, values_fn = list)

cat("\n>>> DASHBOARD DATAFRAME CREATED (Object: dashboard_df) — transponiert mit Sensor+Flags\n\n")


# --- 5. IMPACT ANALYSIS & OPTIMA ---
env_analysis_vars <- list("Room Temp" = list(col="Avg_Temp", unit="°C"), 
                          "Rel Humidity" = list(col="Avg_Rel_Hum", unit="%"),
                          "Abs Humidity" = list(col="Avg_Abs_Hum", unit="g/m³"))
optima_storage <- list()

cat("\n                     SLEEP ANALYSIS\n")
cat("===========================================================\n")
for(env_name in names(env_analysis_vars)) {
  e_col <- env_analysis_vars[[env_name]]$col
  e_unit <- env_analysis_vars[[env_name]]$unit
  cat(sprintf("\n>>> IMPACT OF %s:\n", toupper(env_name)))
  for(m in names(bio_vars)) {
    sub <- final_data_matched %>% filter(!is.na(.data[[e_col]]), !is.na(.data[[m]]))
    if(nrow(sub) < 5) next
    
    sub_model <- sub
    if(m == "RHR") sub_model[[m]] <- -sub_model[[m]] # Invert for peak
    
    fit_poly <- lm(as.formula(paste(m, "~ poly(", e_col, ", 2, raw=TRUE)")), data = sub_model)
    fit_lin <- lm(as.formula(paste(m, "~", e_col)), data = sub)
    
    slope <- coef(fit_lin)[2]
    b <- coef(fit_poly); opt <- -b[2] / (2 * b[3])
    is_peak <- b[3] < 0 && opt >= min(sub[[e_col]]) && opt <= max(sub[[e_col]])
    
    cat(sprintf("  [%s]\n", m))
    if(is_peak) {
      optima_storage[[paste0(env_name, "_", m)]] <- opt
      cat(sprintf("    - Optimal: %.1f %s\n", opt, e_unit))
    } else {
      trend_dir <- if(slope > 0) "increased" else "decreased"
      cat(sprintf("    - No clear optimum. Linear slope: %.2f per %s\n", slope, e_unit))
    }
    cat(sprintf("    - P-Value: %.4f | R-Squared: %.1f%%\n", summary(fit_lin)$coefficients[2,4], summary(fit_poly)$adj.r.squared * 100))
  }
}

cat("\n===========================================================\n")
cat("             DETAILED STATISTICAL EXPLANATION\n")
cat("===========================================================\n")
cat("1. P-VALUE: < 0.05 indicates a statistically significant relationship.\n")
cat("   (How likly is it that the result is pure chance)\n")
cat("2. R-SQUARED: % of sleep variance explained by this environment factor.\n")
cat("3. OPTIMUM: Calculated 'Sweet Spot' based on quadratic regression.\n")
cat("===========================================================\n")


# --- 6. INDIVIDUAL TIMELINE PLOTS ---
metric_list <- c("Avg_Temp", "Avg_Rel_Hum", "Avg_Abs_Hum", "Sleep_Score", "HRV", "RHR")
# derive labels/colors from configuration, with fallbacks
metric_labels <- unname(sapply(metric_list, function(m) {
  plot_cfg$metric_labels[[m]] %||% m
}))
metric_colors <- unname(sapply(metric_list, function(m) {
  plot_cfg$metric_colors[[m]] %||% "black"
}))

for(i in seq_along(metric_list)) {
  m <- metric_list[i]
  p <- ggplot(final_data_viz, aes(x = Date, y = .data[[m]])) +
    geom_line(color = metric_colors[i], linewidth = 1, na.rm = TRUE) +
    geom_point(color = metric_colors[i], size = 2, na.rm = TRUE) +
    scale_x_date(date_labels = "%d.%m.%Y", breaks = "2 days", minor_breaks = "1 day", expand = expansion(mult = c(0.01, 0.01))) +
    labs(title = metric_labels[i], x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor.x = element_line(color = "grey90"),
          plot.title = element_text(face = "bold", color = metric_colors[i]), plot.margin = margin(10, 10, 20, 10))
  if(!dry_run) print(p)
}



# --- 7. SCATTER PLOTS & COLORED MATRIX ---
# Individual Scatter Plots
for(env_name in names(env_analysis_vars)) {
  e_col <- env_analysis_vars[[env_name]]$col
  e_unit <- env_analysis_vars[[env_name]]$unit
  for(m in names(bio_vars)) {
    opt <- optima_storage[[paste0(env_name, "_", m)]]
    p <- ggplot(final_data_matched, aes(x = .data[[e_col]], y = .data[[m]])) +
      geom_point(alpha = 0.5, color = "darkgrey") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = metric_colors[match(m, metric_list)], linewidth = 1.2) +
      labs(title = paste(m, "vs", env_name), x = paste(env_name, e_unit), y = m) +
      theme_minimal()
    if(!is.null(opt)) {
      p <- p + geom_vline(xintercept = opt, linetype = "dashed") +
        annotate("text", x = opt, y = Inf, label = paste0(round(opt, 1), e_unit), vjust = 2, fontface = "bold")
    }
    if(!dry_run) print(p)
  }
}

# 3x3 Matrix Dashboard - COLORED & WITH OPTIMA
matrix_plots <- list()
for(m in names(bio_vars)) {
  m_color <- metric_colors[match(m, metric_list)]
  for(env_name in names(env_analysis_vars)) {
    e_col <- env_analysis_vars[[env_name]]$col
    e_unit <- env_analysis_vars[[env_name]]$unit
    opt <- optima_storage[[paste0(env_name, "_", m)]]
    
    p_mat <- ggplot(final_data_matched, aes(x = .data[[e_col]], y = .data[[m]])) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = m_color, fill = m_color, alpha = 0.1, linewidth = 1) +
      theme_minimal(base_size = 8) + 
      labs(x = e_unit, y = m, title = paste(m, "x", env_name)) +
      theme(plot.title = element_text(size = 7, face = "bold"))
    
    if(!is.null(opt)) {
      p_mat <- p_mat + geom_vline(xintercept = opt, linetype = "dashed", color = "black", alpha = 0.6)
    }
    
    matrix_plots[[length(matrix_plots) + 1]] <- p_mat
  }
}
grid.arrange(grobs = matrix_plots, ncol = 3, top = textGrob("Environmental Impact Matrix (with Optima)", gp=gpar(fontsize=12, font=2)))

