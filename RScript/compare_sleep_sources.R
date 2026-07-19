#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(yaml))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

get_script_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg_idx <- grep("^--file=", args)
  if (length(file_arg_idx) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", args[[file_arg_idx[[1]]]]))))
  }
  if (file.exists(file.path(getwd(), "config.yaml"))) {
    return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(getwd(), "RScript"), winslash = "/", mustWork = FALSE)
}

script_directory <- get_script_directory()
setwd(script_directory)

source(file.path(script_directory, "sleep_source_combined_helpers.R"), local = FALSE)

read_yaml_file <- function(path) {
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path)
}

config <- read_yaml_file("config.yaml")
private_cfg <- read_yaml_file("config.private.yaml")
for (k in names(private_cfg)) {
  config[[k]] <- private_cfg[[k]]
}

parse_orders <- config$parse_orders %||% list()
loc <- config$locale %||% list()
mapping <- config$column_names %||% list()
data_dir <- config$data_directory %||% "../data"
decimal_mark <- loc$decimal_mark %||% ","

sleep_source_cfg <- config$sleep_source %||% list()

# Null out HRV/HFV from CSV since the Garmin export values are unreliable.
analysis_metrics_enabled <- config$analysis_metrics$enabled %||% list()
hrv_enabled <- isTRUE(analysis_metrics_enabled$HRV)
csv_skip_fields <- if (hrv_enabled) {
  trimws(unlist(sleep_source_cfg$csv_skip_fields %||% "HRV"))
} else {
  character(0)
}
csv_skip_fields <- csv_skip_fields[csv_skip_fields != ""]

sleep_api_cfg <- sleep_source_cfg$api %||% config$api %||% list()
sleep_api_base_url <- trimws(as.character(sleep_api_cfg$base_url %||% "https://sleepscoreprivate.onrender.com"))
if (nzchar(sleep_api_base_url)) {
  sleep_api_base_url <- sub("/+$", "", sleep_api_base_url)
}
sleep_api_user_id <- trimws(as.character(sleep_api_cfg$user_id %||% ""))
sleep_api_user_email <- trimws(as.character(sleep_api_cfg$user_email %||% ""))
sleep_api_bearer_token <- trimws(as.character(sleep_api_cfg$bearer_token %||% Sys.getenv("API_INTERNAL_SECRET", unset = "")))

parse_cli_date <- function(args, flag, default_value) {
  matches <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(matches) == 0) return(default_value)
  val <- sub(paste0("^", flag, "="), "", matches[[1]])
  parsed <- as.Date(val)
  if (is.na(parsed)) {
    stop(sprintf("Invalid %s '%s'; expected YYYY-MM-DD", flag, val))
  }
  parsed
}

start_date <- parse_cli_date(commandArgs(trailingOnly = TRUE), "--start-date", as.Date("2026-04-05"))
end_date <- Sys.Date()

read_garmin_fixed <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) return(tibble())
  lines[1] <- gsub("^[^\\t[:alnum:][:punct:][:space:]]+", "", lines[1])
  lines <- gsub("([+-]\\d+),(\\d+°)", "\\1.\\2", lines)
  lines <- gsub(",+$", "", lines)
  df <- read.csv(
    text = lines,
    sep = ",",
    header = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c(" ", "--", "NA", "")
  )
  as_tibble(df)
}

find_first_existing <- function(hdr, candidates) {
  if (length(hdr) == 0 || length(candidates) == 0) return(NA_character_)
  for (candidate in candidates) {
    if (!is.null(candidate) && nzchar(candidate) && candidate %in% hdr) {
      return(candidate)
    }
  }
  NA_character_
}

coerce_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  orders <- unique(c(
    parse_orders$garmin_datetime %||% character(0),
    "Ymd",
    "Y-m-d",
    "d.m.Y",
    "d/m/Y"
  ))
  parsed <- parse_date_time(
    as.character(x),
    orders = orders,
    quiet = TRUE
  )
  as.Date(parsed)
}

parse_datetime_safe_local <- function(x, type = "garmin_datetime") {
  orders <- parse_orders[[type]]
  if (is.null(orders) || length(orders) == 0) {
    return(parse_date_time(x, quiet = TRUE))
  }
  parse_date_time(x, orders = orders, quiet = TRUE)
}

parse_duration_like <- function(x) {
  if (inherits(x, "difftime")) return(as.numeric(x, units = "hours"))
  if (inherits(x, "Duration")) return(as.numeric(x, units = "hours"))
  if (is.numeric(x)) return(as.numeric(x))
  txt <- trimws(as.character(x))
  txt[txt %in% c("", "NA")] <- NA_character_
  txt <- gsub(",", ".", txt, fixed = TRUE)
  out <- suppressWarnings(as.numeric(txt))
  bad <- is.na(out) & !is.na(txt)
  if (any(bad)) {
    hm_vals <- suppressWarnings(hm(txt[bad]))
    out[bad] <- as.numeric(hm_vals) / 3600
  }
  out
}

normalize_sleep_window <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df %>%
    mutate(
      waketime = update(waketime, year = year(Date), month = month(Date), mday = day(Date)),
      bedtime = update(bedtime, year = year(Date), month = month(Date), mday = day(Date))
    ) %>%
    mutate(
      .bedtime_orig = bedtime,
      .waketime_orig = waketime,
      .swap_flag = !is.na(.bedtime_orig) & !is.na(.waketime_orig) &
        hour(.bedtime_orig) <= 12 & hour(.waketime_orig) > 12 & .bedtime_orig < .waketime_orig
    ) %>%
    mutate(
      bedtime = if_else(.swap_flag, .waketime_orig, .bedtime_orig),
      waketime = if_else(.swap_flag, .bedtime_orig, .waketime_orig)
    ) %>%
    mutate(
      .missing_window = is.na(bedtime) & is.na(waketime),
      bedtime = if_else(.missing_window, as.POSIXct(Date) - hours(12), bedtime),
      waketime = if_else(.missing_window, as.POSIXct(Date) + hours(12), waketime)
    ) %>%
    mutate(bedtime = if_else(bedtime > waketime, bedtime - days(1), bedtime)) %>%
    select(-.bedtime_orig, -.waketime_orig, -.swap_flag, -.missing_window)
}

rename_first_match <- function(df, target_name, candidates) {
  if (is.null(target_name) || !nzchar(target_name) || is.null(df) || nrow(df) == 0) return(df)
  old_name <- find_first_existing(names(df), candidates)
  if (!is.na(old_name) && old_name != target_name) {
    names(df)[names(df) == old_name] <- target_name
  }
  df
}

ensure_columns <- function(df, cols) {
  if (is.null(df)) df <- tibble()
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df
}

normalize_sleep_csv_rows <- function(df, source_file, mapping) {
  if (is.null(df) || nrow(df) == 0) return(tibble())

  hdr <- names(df)
  date_col <- find_first_existing(hdr, c(mapping$garmin_date, mapping$garmin_date_alt, "Datum"))
  if (is.na(date_col)) {
    alt_matches <- hdr[grepl("^(Sleep Score|Datum)", hdr, ignore.case = TRUE)]
    if (length(alt_matches) == 1) {
      date_col <- alt_matches[[1]]
      warning(sprintf("Using alternate date column '%s' for file %s", date_col, source_file))
    }
  }
  bedtime_col <- find_first_existing(hdr, c(mapping$garmin_bedtime))
  waketime_col <- find_first_existing(hdr, c(mapping$garmin_waketime))
  score_col <- find_first_existing(hdr, c(mapping$garmin_sleep_score, "Sleep Score", "Sleep_Score", "score"))
  hrv_col <- find_first_existing(hdr, c(mapping$garmin_hrv, mapping$garmin_hrv_alt, "HRV", "HFV-Status", "Ø HFV über Nacht"))
  rhr_col <- find_first_existing(hdr, c(mapping$garmin_rhr, "RHR", "Ruheherzfrequenz"))
  duration_col <- find_first_existing(hdr, c(mapping$garmin_duration, "Sleep_Duration", "Dauer"))

  if (is.na(date_col)) {
    warning(sprintf("Skipping %s because no sleep date column was found.", source_file))
    return(tibble())
  }

  out <- tibble(
    Date = coerce_date(df[[date_col]]),
    bedtime = if (!is.na(bedtime_col)) parse_datetime_safe_local(df[[bedtime_col]], type = "garmin_time") else as.POSIXct(rep(NA, nrow(df))),
    waketime = if (!is.na(waketime_col)) parse_datetime_safe_local(df[[waketime_col]], type = "garmin_time") else as.POSIXct(rep(NA, nrow(df))),
    Sleep_Score = if (!is.na(score_col)) suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[score_col]]), fixed = TRUE))) else NA_real_,
    HRV = if (!is.na(hrv_col)) suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[hrv_col]]), fixed = TRUE))) else NA_real_,
    RHR = if (!is.na(rhr_col)) suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[rhr_col]]), fixed = TRUE))) else NA_real_,
    Sleep_Duration = if (!is.na(duration_col)) parse_duration_like(df[[duration_col]]) else NA_real_,
    Source_File = source_file,
    Source_Name = basename(source_file)
  ) %>%
    filter(!is.na(Date)) %>%
    normalize_sleep_window()

  ensure_columns(out, c("Date", "bedtime", "waketime", "Sleep_Score", "HRV", "RHR", "Sleep_Duration", "Source_File", "Source_Name"))
}

is_sleep_csv <- function(path, mapping) {
  hdr <- tryCatch(names(read.csv(path, nrows = 1, stringsAsFactors = FALSE, check.names = FALSE)), error = function(e) character(0))
  if (length(hdr) == 0) return(FALSE)
  if (!is.na(find_first_existing(hdr, c(mapping$garmin_date, mapping$garmin_date_alt)))) return(TRUE)
  if (!is.na(find_first_existing(hdr, c(mapping$garmin_bedtime))) && !is.na(find_first_existing(hdr, c(mapping$garmin_waketime)))) return(TRUE)
  FALSE
}

normalize_api_name <- function(x) {
  gsub("[^a-z0-9]+", "", tolower(as.character(x)))
}

find_first_api_column <- function(hdr, candidates) {
  if (length(hdr) == 0 || length(candidates) == 0) return(NA_character_)
  idx <- match(normalize_api_name(candidates), normalize_api_name(hdr))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NA_character_)
  hdr[[idx[[1]]]]
}

rename_api_column <- function(df, target_name, candidates) {
  if (is.null(target_name) || !nzchar(target_name)) return(df)
  old_name <- find_first_api_column(names(df), candidates)
  if (!is.na(old_name) && old_name != target_name) {
    names(df)[names(df) == old_name] <- target_name
  }
  df
}

extract_api_rows <- function(payload) {
  if (is.data.frame(payload)) {
    return(as_tibble(payload, .name_repair = "unique"))
  }
  if (is.list(payload)) {
    for (nm in c("entries", "data", "items", "rows", "sleep_entries")) {
      candidate <- payload[[nm]]
      if (is.data.frame(candidate)) {
        return(as_tibble(candidate, .name_repair = "unique"))
      }
      if (is.list(candidate) && length(candidate) > 0) {
        candidate_df <- tryCatch(bind_rows(candidate), error = function(e) NULL)
        if (!is.null(candidate_df) && nrow(candidate_df) > 0) {
          return(as_tibble(candidate_df, .name_repair = "unique"))
        }
      }
    }
    candidate_df <- tryCatch(bind_rows(payload), error = function(e) NULL)
    if (!is.null(candidate_df) && nrow(candidate_df) > 0) {
      return(as_tibble(candidate_df, .name_repair = "unique"))
    }
  }
  tibble()
}

normalize_sleep_api_rows <- function(df) {
  df <- as_tibble(df, .name_repair = "unique")
  df <- rename_api_column(df, "Date", c(
    "Date", "date", "sleep_date", "sleepDate", "view_date", "viewDate",
    "night_date", "nightDate", "day", "sleepDay"
  ))
  df <- rename_api_column(df, "bedtime", c(
    "bedtime", "bed_time", "sleep_start", "sleepStart", "start_time",
    "startTime", "start", "asleep_time", "sleep_begin", "sleepBegin",
    "sleep_start_time", "sleepStartTime"
  ))
  df <- rename_api_column(df, "waketime", c(
    "waketime", "wake_time", "wakeTime", "sleep_end", "sleepEnd",
    "end_time", "endTime", "end", "wake_up_time", "wakeUpTime",
    "sleep_end_time", "sleepEndTime"
  ))
  df <- rename_api_column(df, "Sleep_Score", c(
    "Sleep_Score", "sleep_score", "sleepScore", "score", "sleepscore"
  ))
  df <- rename_api_column(df, "HRV", c(
    "HRV", "hrv", "hrv_status", "hrvStatus", "overnight_hrv", "overnightHrv",
    "hrv_score", "avg_overnight_hrv", "avgOvernightHrv"
  ))
  df <- rename_api_column(df, "RHR", c(
    "RHR", "rhr", "resting_heart_rate", "restingHeartRate", "resting_hr"
  ))
  duration_col <- find_first_api_column(names(df), c(
    "Sleep_Duration", "sleep_duration", "sleepDuration", "duration", "sleep_length", "sleepLength",
    "total_sleep_duration_minutes", "totalSleepDurationMinutes", "sleep_duration_minutes", "sleepDurationMinutes"
  ))
  if (!is.na(duration_col)) {
    if (duration_col %in% c("total_sleep_duration_minutes", "totalSleepDurationMinutes", "sleep_duration_minutes", "sleepDurationMinutes")) {
      df$Sleep_Duration <- suppressWarnings(as.numeric(df[[duration_col]]) / 60)
      if (duration_col != "Sleep_Duration") {
        df[[duration_col]] <- NULL
      }
    } else if (duration_col != "Sleep_Duration") {
      df <- rename_api_column(df, "Sleep_Duration", c(
        "Sleep_Duration", "sleep_duration", "sleepDuration", "duration", "sleep_length", "sleepLength"
      ))
    }
  }

  df <- ensure_columns(df, c("Date", "bedtime", "waketime", "Sleep_Score", "HRV", "RHR", "Sleep_Duration"))

  df %>%
    mutate(
      Date = coerce_date(Date),
      bedtime = parse_datetime_safe_local(bedtime, type = "garmin_time"),
      waketime = parse_datetime_safe_local(waketime, type = "garmin_time"),
      Sleep_Score = suppressWarnings(as.numeric(gsub(",", ".", as.character(Sleep_Score), fixed = TRUE))),
      HRV = suppressWarnings(as.numeric(gsub(",", ".", as.character(HRV), fixed = TRUE))),
      RHR = suppressWarnings(as.numeric(gsub(",", ".", as.character(RHR), fixed = TRUE))),
      Sleep_Duration = if (is.numeric(Sleep_Duration)) as.numeric(Sleep_Duration) else parse_duration_like(Sleep_Duration)
    ) %>%
    filter(!is.na(Date)) %>%
    normalize_sleep_window()
}

read_sleep_api <- function(date_start, date_end) {
  if (!nzchar(sleep_api_bearer_token)) {
    stop("sleep_source.api.bearer_token is required in config.private.yaml (or set API_INTERNAL_SECRET).")
  }
  if (!nzchar(sleep_api_user_id) && !nzchar(sleep_api_user_email)) {
    stop("sleep_source.api.user_id or sleep_source.api.user_email is required.")
  }

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, 300))

  query_parts <- character(0)
  if (nzchar(sleep_api_user_id)) {
    query_parts <- c(query_parts, paste0("user_id=", URLencode(sleep_api_user_id, reserved = TRUE)))
  }
  if (nzchar(sleep_api_user_email)) {
    query_parts <- c(query_parts, paste0("user_email=", URLencode(sleep_api_user_email, reserved = TRUE)))
  }
  if (!is.null(date_start) && !is.na(date_start)) {
    query_parts <- c(query_parts, paste0("date_start=", URLencode(format(as.Date(date_start), "%Y-%m-%d"), reserved = TRUE)))
  }
  if (!is.null(date_end) && !is.na(date_end)) {
    query_parts <- c(query_parts, paste0("date_end=", URLencode(format(as.Date(date_end), "%Y-%m-%d"), reserved = TRUE)))
  }
  query_parts <- c(query_parts, "mode=entries")

  request_url <- paste0(
    sleep_api_base_url,
    "/api/users/sleep-export-data?",
    paste(query_parts, collapse = "&")
  )

  con <- url(
    request_url,
    open = "rb",
    headers = c(
      Authorization = paste0("Bearer ", sleep_api_bearer_token),
      Accept = "application/json"
    )
  )
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  raw_text <- tryCatch(
    readLines(con, warn = FALSE, encoding = "UTF-8"),
    error = function(e) stop("Failed to read sleep API response: ", conditionMessage(e))
  )
  payload <- tryCatch(
    jsonlite::fromJSON(paste(raw_text, collapse = "\n"), flatten = TRUE),
    error = function(e) stop("Failed to parse sleep API JSON: ", conditionMessage(e))
  )

  rows <- extract_api_rows(payload)
  if (nrow(rows) == 0) {
    stop("Sleep API returned no export rows.")
  }

  rows <- normalize_sleep_api_rows(rows)
  rows %>%
    mutate(
      Source_File = if (nzchar(sleep_api_user_id)) {
        paste0("api://user_id=", sleep_api_user_id)
      } else {
        paste0("api://user_email=", sleep_api_user_email)
      },
      Source_Name = "Sleep Score Private API"
    ) %>%
    select(Date, bedtime, waketime, Sleep_Score, HRV, RHR, Sleep_Duration, Source_File, Source_Name)
}

load_sleep_csv_rows <- function(data_dir, mapping, start_date) {
  all_files <- list.files(data_dir, recursive = TRUE, pattern = "\\.(csv|CSV)$", full.names = TRUE)
  sleep_files <- all_files[vapply(all_files, is_sleep_csv, logical(1), mapping = mapping)]
  if (length(sleep_files) == 0) {
    stop(sprintf("No sleep CSV files found under %s", data_dir))
  }

  rows <- bind_rows(lapply(sleep_files, function(path) {
    df <- tryCatch(read_garmin_fixed(path), error = function(e) {
      warning(sprintf("Skipping %s: %s", path, conditionMessage(e)))
      return(tibble())
    })
    if (nrow(df) == 0) return(tibble())
    normalize_sleep_csv_rows(df, path, mapping)
  }))

  if (nrow(rows) == 0) {
    stop("No sleep CSV rows were parsed.")
  }

  rows %>%
    filter(Date >= start_date) %>%
    prepare_sleep_source_rows("csv", skip_fields = csv_skip_fields) %>%
    select(Date, bedtime, waketime, Sleep_Score, HRV, RHR, Sleep_Duration, Source_File, Source_Name, Sleep_Source)
}

load_sleep_api_rows <- function(start_date, end_date) {
  api_ranges <- split_date_ranges(seq.Date(start_date, end_date, by = "day"))
  rows <- if (length(api_ranges) == 0) {
    tibble()
  } else {
    bind_rows(lapply(api_ranges, function(rng) read_sleep_api(rng$start, rng$end)))
  }

  if (nrow(rows) == 0) {
    stop("No API sleep rows were parsed.")
  }

  rows %>%
    filter(Date >= start_date) %>%
    prepare_sleep_source_rows("api") %>%
    select(Date, bedtime, waketime, Sleep_Score, HRV, RHR, Sleep_Duration, Source_File, Source_Name, Sleep_Source)
}

equal_pair <- function(lhs, rhs) {
  (is.na(lhs) & is.na(rhs)) | (!is.na(lhs) & !is.na(rhs) & lhs == rhs)
}

compare_metric <- function(df, field) {
  lhs <- df[[paste0(field, "_csv")]]
  rhs <- df[[paste0(field, "_api")]]
  ifelse(equal_pair(lhs, rhs), 1L, 0L)
}

csv_rows <- load_sleep_csv_rows(data_dir, mapping, start_date)
api_rows <- load_sleep_api_rows(start_date, end_date)

comparison <- full_join(
  csv_rows,
  api_rows,
  by = "Date",
  suffix = c("_csv", "_api")
) %>%
  arrange(Date)

compare_fields <- c("bedtime", "waketime", "Sleep_Score", "HRV", "RHR", "Sleep_Duration")
for (field in compare_fields) {
  comparison[[paste0(field, "_equal_1_0")]] <- compare_metric(comparison, field)
}
comparison$equal_1_0 <- ifelse(
  rowSums(comparison[paste0(compare_fields, "_equal_1_0")] == 1L, na.rm = TRUE) == length(compare_fields),
  1L,
  0L
)

ordered_columns <- c(
  "Date",
  unlist(lapply(compare_fields, function(field) c(paste0(field, "_csv"), paste0(field, "_api"), paste0(field, "_equal_1_0"))), use.names = FALSE),
  "Source_File_csv", "Source_File_api",
  "Source_Name_csv", "Source_Name_api",
  "Sleep_Source_csv", "Sleep_Source_api",
  "equal_1_0"
)

remaining_columns <- setdiff(names(comparison), ordered_columns)
comparison <- comparison[, c(intersect(ordered_columns, names(comparison)), remaining_columns)]

stopifnot("Date" %in% names(comparison))
stopifnot("equal_1_0" %in% names(comparison))
stopifnot(all(paste0(compare_fields, "_equal_1_0") %in% names(comparison)))
stopifnot(all(vapply(comparison[paste0(compare_fields, "_equal_1_0")], function(col) all(is.na(col) | col %in% c(0L, 1L)), logical(1))))
stopifnot(all(is.na(comparison$equal_1_0) | comparison$equal_1_0 %in% c(0L, 1L)))
stopifnot(all(is.na(comparison$Date) | comparison$Date >= start_date))

out_path <- file.path(script_directory, sprintf("sleep_source_compare_%s.csv", format(start_date, "%Y-%m-%d")))
write.csv(comparison, file = out_path, row.names = FALSE, na = "")

cat(sprintf("Wrote %s\n", out_path))
cat(sprintf("CSV rows: %d | API rows: %d | Joined rows: %d\n", nrow(csv_rows), nrow(api_rows), nrow(comparison)))
cat(sprintf("Equal rows: %d | Different rows: %d\n", sum(comparison$equal_1_0 == 1L, na.rm = TRUE), sum(comparison$equal_1_0 == 0L, na.rm = TRUE)))
