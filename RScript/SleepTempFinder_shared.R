# Shared helper functions for SleepTempFinder-derived scripts
# This file is intended to be sourced by independent analysis scripts
# such as SleepMetaModel.R without modifying the original SleepTempFinder.R.

if (!require("rstudioapi")) install.packages("rstudioapi")
if (is.null(getOption("repos")) || getOption("repos")[[1]] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}
pkgs <- c("tidyverse", "lubridate", "yaml", "broom")
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

get_script_path <- function() {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(normalizePath(p))
  }
  args1 <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args1)
  if (length(m) > 0) return(normalizePath(sub("^--file=", "", args1[m][1])))
  if (!is.null(sys.frame(1)$ofile)) return(normalizePath(sys.frame(1)$ofile))
  find_upward <- function(name) {
    d <- normalizePath(getwd(), mustWork = FALSE)
    repeat {
      candidate <- file.path(d, name)
      if (file.exists(candidate)) return(candidate)
      parent <- dirname(d)
      if (parent == d) break
      d <- parent
    }
    NULL
  }
  return(find_upward("SleepTempFinder_shared.R"))
}

script_path <- get_script_path()
if (!is.null(script_path)) {
  setwd(dirname(script_path))
} else {
  warning("Could not determine SleepTempFinder_shared.R location; working directory unchanged")
}
script_directory <- if (!is.null(script_path)) dirname(script_path) else normalizePath(getwd(), mustWork = FALSE)

# load configuration
config <- read_yaml(file.path(script_directory, "config.yaml"))
private_cfg_path <- file.path(script_directory, "config.private.yaml")
if (file.exists(private_cfg_path)) {
  try({
    private_cfg <- read_yaml(private_cfg_path)
    for (k in names(private_cfg)) config[[k]] <- private_cfg[[k]]
  }, silent = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

trim_vector <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x != "" & !is.na(x)]
}

is_absolute_path <- function(path_value) {
  grepl("^(?:[A-Za-z]:[\\/]|/)", path_value, perl = TRUE)
}
resolve_path <- function(path_value) {
  if (is.null(path_value) || path_value == "") return(NA_character_)
  if (is_absolute_path(path_value)) return(normalizePath(path_value, winslash = "/", mustWork = FALSE))
  normalizePath(file.path(script_directory, path_value), winslash = "/", mustWork = FALSE)
}

canonical_basename <- function(path) {
  b <- basename(path)
  b <- sub("\\s*\\(\\d+\\)(?=\\.[^\\.]+$)", "", b, perl = TRUE)
  b <- gsub("[_\\s]+", " ", b)
  b <- trimws(b)
  tolower(b)
}

parse_datetime_safe <- function(x, type = "garmin_datetime") {
  orders <- config$parse_orders
  if (is.null(orders[[type]])) {
    warning("no parse orders for type: ", type)
    return(parse_date_time(x, quiet = TRUE))
  }
  res <- parse_date_time(x, orders = orders[[type]], quiet = TRUE)
  res
}

clean_val_final <- function(x) {
  parse_duration_like <- function(val) {
    txt <- str_trim(as.character(val))
    txt <- str_replace_all(txt, ",", ".")
    if (txt == "") return(NA_real_)
    if (str_detect(txt, "^[-+]?[0-9]+:[0-5][0-9]$")) {
      parts <- str_split(txt, ":", simplify = TRUE)
      return(as.numeric(parts[1]) + as.numeric(parts[2]) / 60)
    }
    if (str_detect(txt, "^[-+]?[0-9]+\\s*h(?:\\s*[0-5]?[0-9]\\s*m?)?$$")) {
      parts <- str_match(txt, "^([-+]?[0-9]+)\\s*h(?:\\s*([0-5]?[0-9])\\s*m?)?$$")
      hrs <- as.numeric(parts[1,2])
      mins <- as.numeric(parts[1,3])
      if (is.na(mins)) mins <- 0
      return(hrs + mins / 60)
    }
    num_str <- str_extract(txt, "[-+]?[0-9]*\\.?[0-9]+")
    as.numeric(num_str)
  }
  res <- map_dbl(as.character(x), function(val) {
    if (is.na(val) || val == "" || val == "--") return(NA_real_)
    if (str_detect(val, ":") || str_detect(val, "h")) {
      parsed <- parse_duration_like(val)
      if (!is.na(parsed)) return(parsed)
    }
    num_str <- str_replace_all(as.character(val), ",", ".")
    num_str <- str_extract(num_str, "[-+]?[0-9]*\\.?[0-9]+")
    as.numeric(num_str)
  })
  res
}

is_single_day_sleep_csv <- function(path) {
  lines <- tryCatch(readLines(path, n = 20, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  if (length(lines) == 0) return(FALSE)
  lines <- trimws(lines)
  if (!any(str_detect(lines, regex("^Sleep Score", ignore_case = TRUE)))) return(FALSE)
  if (any(str_detect(lines, regex("^Datum\\s*,", ignore_case = TRUE)))) return(TRUE)
  if (any(str_detect(lines, regex("^Schlafdauer\\s*,", ignore_case = TRUE)))) return(TRUE)
  FALSE
}

read_garmin_single_day <- function(path, lines = NULL) {
  if (is.null(lines)) {
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  }
  if (length(lines) == 0) return(tibble())
  lines <- gsub("\\r", "", lines)
  lines <- gsub("([+-]?[0-9]+),(?=[0-9]+°)", "\\1.", lines, perl = TRUE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  lines <- lines[!str_detect(lines, regex("^(Sleep Score 1 Tag|Sleep Score-Faktoren|Daten für Schlafzeitleiste)\\s*,?$$", ignore_case = TRUE))]
  if (length(lines) == 0) return(tibble())
  kv <- suppressWarnings(read.csv(text = paste(lines, collapse = "\n"), header = FALSE, sep = ",",
                                   stringsAsFactors = FALSE, check.names = FALSE,
                                   na.strings = c(" ", "--", "NA", "")))
  if (ncol(kv) < 2) return(tibble())
  keys <- trimws(as.character(kv[[1]]))
  values <- as.character(kv[[2]])
  keep <- keys != "" & !is.na(keys)
  if (sum(keep) == 0) return(tibble())
  keys <- keys[keep]
  values <- values[keep]
  if (any(duplicated(keys))) keys <- make.unique(keys, sep = "_")
  out <- as_tibble(as.list(set_names(values, keys)))
  rename_map <- c(
    Datum = "Sleep Score 4 Wochen",
    Schlafdauer = "Dauer",
    "Sleep Score" = "Score",
    "Durchschnittlicher SpO₂" = "Pulsoximeter",
    "Ø Veränderung der Hauttemperatur" = "Veränderung der Hauttemperatur",
    "Ø HFV über Nacht" = "HFV-Status",
    "Ø Atemfrequenz" = "Atmung"
  )
  for (old_name in names(rename_map)) {
    if (old_name %in% names(out)) names(out)[names(out) == old_name] <- rename_map[[old_name]]
  }
  out %>% mutate(across(everything(), as.character))
}

read_garmin_fixed <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (is_single_day_sleep_csv(path)) {
    single <- read_garmin_single_day(path, lines)
    if (nrow(single) > 0) return(single)
  }
  lines[1] <- gsub("^[^\t[:alnum:][:punct:][:space:]]+", "", lines[1])
  lines <- gsub("([+-]\\d+),(\\d+°)", "\\1.\\2", lines)
  lines <- gsub(",+$", "", lines)
  df <- read.csv(text = lines, sep = ",", header = TRUE,
                 check.names = FALSE, stringsAsFactors = FALSE,
                 colClasses = "character",
                 na.strings = c(" ", "--", "NA", ""))
  as_tibble(df, .name_repair = "unique")
}

expand_explicit <- function(explicit_paths, discovered) {
  out <- character(0)
  for (p in explicit_paths) {
    if (p %in% discovered) {
      out <- c(out, p)
    } else {
      base <- canonical_basename(p)
      matches <- discovered[canonical_basename(discovered) == base]
      if (length(matches) > 0) {
        out <- c(out, matches)
      } else {
        out <- c(out, p)
      }
    }
  }
  unique(out)
}

list_csv_files <- function(dir, recursive = FALSE) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(path = dir, pattern = "\\.csv$", recursive = recursive, full.names = TRUE)
  normalizePath(files, winslash = "/", mustWork = FALSE)
}

resolve_alternate_column <- function(mapping, hdr, primary_key, alt_key = NULL) {
  if (!is.null(mapping[[primary_key]]) && mapping[[primary_key]] %in% hdr) return(mapping[[primary_key]])
  if (!is.null(alt_key) && !is.null(mapping[[alt_key]])) {
    alt_vals <- unlist(mapping[[alt_key]])
    alt_match <- alt_vals[alt_vals %in% hdr]
    if (length(alt_match) >= 1L) return(alt_match[[1]])
  }
  NULL
}

is_sleep_csv <- function(path, mapping) {
  if (is_single_day_sleep_csv(path)) return(TRUE)
  hdr <- tryCatch(names(read.csv(path, nrows = 1, stringsAsFactors = FALSE, check.names = FALSE)), error = function(e) character(0))
  if (!is.null(resolve_alternate_column(mapping, hdr, "garmin_date", "garmin_date_alt"))) return(TRUE)
  if (mapping$garmin_bedtime %in% hdr && mapping$garmin_waketime %in% hdr) return(TRUE)
  FALSE
}

is_sensor_csv <- function(path, sensor_files) {
  !is.null(detect_sensor_config(path))
}

detect_sensor_config <- function(path) {
  base <- canonical_basename(path)
  for (id in names(config$sensor_files)) {
    if (base == canonical_basename(config$sensor_files[[id]]$path)) return(config$sensor_files[[id]])
  }
  hdr <- tryCatch(names(suppressMessages(suppressWarnings(read_delim(path, delim = ",", n_max = 1, locale = locale(decimal_mark = config$locale$decimal_mark %||% ","), show_col_types = FALSE, name_repair = "unique")))), error = function(e) character(0))
  candidates <- names(config$sensor_files)[sapply(config$sensor_files, function(f) {
    all(c(f$col_time, f$col_temp, f$col_hum) %in% hdr)
  })]
  if (length(candidates) == 1L) return(config$sensor_files[[candidates]])
  if (length(candidates) > 1L) {
    warning(sprintf("Ambiguous sensor match for file '%s'; candidates: %s. Explicit path required.", base, paste(candidates, collapse = ", ")))
  }
  NULL
}

identify_sensor_id <- function(path) {
  base <- canonical_basename(path)
  for (id in names(config$sensor_files)) {
    if (base == canonical_basename(config$sensor_files[[id]]$path)) return(id)
  }
  cfg <- detect_sensor_config(path)
  if (!is.null(cfg)) {
    for (id in names(config$sensor_files)) {
      if (identical(cfg, config$sensor_files[[id]])) return(id)
    }
  }
  NA_character_
}

get_sensor_file_info <- function(path) {
  cfg <- detect_sensor_config(path)
  if (is.null(cfg)) {
    config$sensor_files[[1]]
  } else {
    cfg
  }
}

resolve_sensor_label <- function(label, cfg) {
  if (is.na(label) || label == "") return(NA_character_)
  lookup <- list()
  for (id in names(cfg$sensor_files)) {
    lookup[[tolower(id)]] <- id
    nicks <- cfg$sensor_files[[id]]$nickname %||% character(0)
    for (nick in as.character(nicks)) {
      if (nzchar(nick)) lookup[[tolower(nick)]] <- id
    }
  }
  key <- tolower(label)
  if (key %in% names(lookup)) return(lookup[[key]])
  matches <- unique(unlist(lapply(names(lookup), function(k) {
    if (str_detect(key, fixed(k))) return(lookup[[k]])
    NULL
  })))
  if (length(matches) == 1) return(matches[[1]])
  warning(sprintf("Calendar sensor '%s' not found in config", label))
  NA_character_
}

unfold_ics_lines <- function(lines) {
  out <- character(0)
  for (ln in lines) {
    if (length(out) > 0 && str_detect(ln, "^[ \\\t]")) {
      out[length(out)] <- paste0(out[length(out)], str_sub(ln, 2))
    } else {
      out <- c(out, ln)
    }
  }
  out
}

extract_ics_prop <- function(lines, prop) {
  idx <- which(str_detect(lines, paste0("^", prop, "(;[^:]+)?:")))
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
  if (str_detect(val, "^\\d{8}T\\d{6}Z$$")) return(ymd_hms(val, tz = "UTC", quiet = TRUE))
  if (str_detect(val, "^\\d{8}T\\d{6}$$")) return(ymd_hms(val, quiet = TRUE))
  if (str_detect(val, "^\\d{8}T\\d{4}$$")) return(parse_date_time(val, orders = "Ymd HM", quiet = TRUE))
  parse_date_time(val, orders = c("Ymd HMS", "Ymd HM", "Y-m-d H:M:S", "Y-m-d H:M", "Y-m-d"), quiet = TRUE)
}

unescape_ics <- function(val) {
  if (is.na(val)) return(val)
  out <- val
  out <- str_replace_all(out, "\\\
", "\n")
  out <- str_replace_all(out, "\\N", "\n")
  out <- str_replace_all(out, "\\,", ",")
  out <- str_replace_all(out, "\\;", ";")
  out <- str_replace_all(out, "\\\\", "\\")
  out
}

split_flags <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(character(0))
  parts <- str_split(x, "\\s*[,;]\\s*")[[1]]
  sort(unique(trim_vector(parts)))
}

parse_sensor_flags <- function(text_value) {
  text_value <- text_value %||% ""
  flags_raw_vec <- str_match_all(text_value, regex("(?:flags?|tags?)\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))[[1]][,2]
  text_no_flags <- str_replace_all(text_value, regex("(?:flags?|tags?)\\s*=\\s*[^;\\n]+", ignore_case = TRUE), "")
  sensor_match <- str_match(text_no_flags, regex("sensor\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))
  sensor_raw <- str_trim(sensor_match[, 2] %||% NA_character_)
  sensor_name <- sensor_raw
  if (!is.na(sensor_name) && str_detect(sensor_name, "/")) {
    sensor_name <- str_trim(str_split(sensor_name, "/", n = 2)[[1]][2])
  }
  flags <- character(0)
  if (length(flags_raw_vec) > 0) {
    flags <- sort(unique(unlist(lapply(flags_raw_vec, split_flags))))
  }
  list(sensor_raw = ifelse(is.na(sensor_raw) || sensor_raw == "", NA_character_, sensor_raw),
       sensor_name = ifelse(is.na(sensor_name) || sensor_name == "", NA_character_, sensor_name),
       flags = flags)
}

load_calendar_daily <- function(calendar_cfg, parser_cfg) {
  if (is.null(calendar_cfg) || !isTRUE(calendar_cfg$enabled)) return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
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
  lines <- tryCatch(read_ics_lines(mode = mode, url_value = url_value, file_value = file_value), error = function(e) { cat(sprintf("Calendar load failed: %s\n", e$message)); character(0) })
  if (length(lines) == 0) return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
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
    summary_p$value <- unescape_ics(summary_p$value)
    description_p$value <- unescape_ics(description_p$value)
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
    seq_start <- start_date
    if (!is.null(config$calendar_assignment$ignore_event_start) && isTRUE(config$calendar_assignment$ignore_event_start) && start_date < end_date) {
      seq_start <- start_date + 1
    }
    date_seq <- seq(seq_start, end_date, by = "1 day")
    event_rows[[i]] <- tibble(Date = as.Date(date_seq), Sensor_Raw = parsed$sensor_raw, Sensor = parsed$sensor_name, Flags_List = list(parsed$flags))
  }
  events_daily <- bind_rows(event_rows)
  if (nrow(events_daily) == 0) {
    cat("Calendar events found, but no Sensor/Flags metadata parsed.\n")
    return(tibble(Date = as.Date(character()), Sensor = character(), Flags = character()))
  }
  calendar_daily <- events_daily %>%
    group_by(Date) %>%
    summarise(sensor_values = list(unique(na.omit(Sensor))), flags_values = list(sort(unique(unlist(Flags_List)))), .groups = "drop") %>%
    mutate(Sensor = map_chr(sensor_values, ~ if (length(.x) == 1) .x[[1]] else NA_character_), Flags_List = map(flags_values, ~ trim_vector(.x)), Flags = map_chr(Flags_List, collapse_flags))
  conflicting_sensor_days <- calendar_daily %>% filter(map_int(sensor_values, length) > 1) %>% nrow()
  if (conflicting_sensor_days > 0) cat(sprintf("Calendar warning: %d day(s) had conflicting sensors and were set to NA.\n", conflicting_sensor_days))
  calendar_daily %>% select(Date, Sensor, Flags, Flags_List)
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

read_ics_lines <- function(mode, url_value = NULL, file_value = NULL) {
  if (tolower(mode) == "url") {
    con <- base::url(url_value)
    on.exit(close(con), add = TRUE)
    readLines(con, warn = FALSE, encoding = "UTF-8")
  } else {
    readLines(file_value, warn = FALSE, encoding = "UTF-8")
  }
}

wake_date <- function(ts) as.Date(ts + hours(12))

safe_bool <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x %in% c("YES", "Y", "TRUE", "1")
}

# Converts strings like "Displays 30min vor dem Schlafen aus" into a basic
# named list with logical values.
build_lifestyle_features <- function(lifestyle_df) {
  fields <- names(lifestyle_df)
  has_col <- function(name) any(name == fields)
  bool_col <- function(name) {
    if (has_col(name)) safe_bool(lifestyle_df[[name]]) else rep(FALSE, nrow(lifestyle_df))
  }
  lifestyle_df <- lifestyle_df %>% mutate(
    Display_30min_off = bool_col("Displays 30min vor dem Schlafen aus"),
    Display_1h_off = bool_col("Displays eine H vor dem Schlafen aus"),
    Traveling_Vacation = bool_col("Traveling/Vacation"),
    Late_Meals = bool_col("Late Meals"),
    Heavy_Meals = bool_col("Heavy Meals"),
    Window_Open = bool_col("Fenster offen beim Schlafen"),
    Sleep_Sounds = bool_col("Sleep Sounds"),
    Light_Exercise = bool_col("Light Exercise"),
    Moderate_Exercise = bool_col("Moderate Exercise"),
    Vigorous_Exercise = bool_col("Vigorous Exercise"),
    Light_Exercise_Before_Bed = bool_col("Light Exercise Before Bed"),
    Moderate_Exercise_Before_Bed = bool_col("Moderate Exercise Before Bed"),
    Vigorous_Exercise_Before_Bed = bool_col("Vigorous Exercise Before Bed")
  )
  lifestyle_df <- lifestyle_df %>% mutate(
    Exercise_Level = case_when(
      Vigorous_Exercise_Before_Bed ~ "vigorous",
      Moderate_Exercise_Before_Bed ~ "moderate",
      Light_Exercise_Before_Bed ~ "light",
      Vigorous_Exercise ~ "vigorous",
      Moderate_Exercise ~ "moderate",
      Light_Exercise ~ "light",
      TRUE ~ "none"
    ),
    Display_Before_Bed = case_when(
      Display_1h_off ~ "1h",
      Display_30min_off ~ "30min",
      TRUE ~ "none"
    )
  )
  lifestyle_df %>% mutate(
    Exercise_Level = factor(Exercise_Level, levels = c("none", "light", "moderate", "vigorous")),
    Display_Before_Bed = factor(Display_Before_Bed, levels = c("none", "30min", "1h"))
  )
}

find_lifestyle_csv <- function(lifestyle_path = NULL) {
  out_dir <- normalizePath(file.path(script_directory, "..", "LifestyleLoggingExtractor", "Out"), winslash = "/", mustWork = FALSE)
  files <- character(0)
  if (dir.exists(out_dir)) {
    files <- list.files(out_dir, pattern = "_LifestyleLogging\\.csv$", full.names = TRUE)
  }
  if (!is.null(lifestyle_path) && nzchar(lifestyle_path)) {
    candidate <- normalizePath(lifestyle_path, winslash = "/", mustWork = FALSE)
    if (file.exists(candidate)) return(candidate)
    warning(sprintf("Specified lifestyle path does not exist: %s", lifestyle_path))
  }
  if (length(files) == 1) return(files[[1]])
  if (length(files) > 1) {
    latest <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
    return(latest)
  }
  NA_character_
}

extract_lifestyle_from_raw <- function(raw_path) {
  python_exec <- Sys.which("python")
  if (python_exec == "") stop("Python not found in PATH. Please install Python or activate your environment.")
  extractor <- normalizePath(file.path(script_directory, "..", "LifestyleLoggingExtractor", "extract_lifestyle_logging.py"), winslash = "/", mustWork = TRUE)
  output_path <- file.path(tempdir(), "SleepMetaModel_lifestyle.csv")
  if (file.exists(output_path)) file.remove(output_path)
  args <- c(extractor, "--input", normalizePath(raw_path, winslash = "/", mustWork = FALSE), "--output", output_path)
  status <- system2(python_exec, args, stdout = TRUE, stderr = TRUE)
  if (!file.exists(output_path)) {
    stop(sprintf("Failed to extract lifestyle CSV from raw source. Python output:\n%s", paste(status, collapse = "\n")))
  }
  output_path
}

load_lifestyle_data <- function(lifestyle_path = NULL) {
  candidate <- find_lifestyle_csv(lifestyle_path)
  if (!is.na(candidate)) {
    return(read_csv(candidate, col_types = cols(.default = col_character())))
  }
  if (interactive()) {
    answer <- readline(prompt = "Lifestyle CSV not found. Enter path to Lifestyle CSV or raw Garmin data path: ")
    answer <- trimws(answer)
    if (answer == "") stop("Lifestyle data path required but not provided.")
    candidate <- normalizePath(answer, winslash = "/", mustWork = FALSE)
    if (!file.exists(candidate)) stop(sprintf("Path does not exist: %s", candidate))
  } else if (!is.null(lifestyle_path) && nzchar(lifestyle_path)) {
    candidate <- normalizePath(lifestyle_path, winslash = "/", mustWork = FALSE)
    if (!file.exists(candidate)) stop(sprintf("Path does not exist: %s", candidate))
  } else {
    candidate <- Sys.getenv("LIFESTYLE_CSV_PATH", unset = "")
    if (candidate == "") stop("Lifestyle CSV not found and no path provided. Set LIFESTYLE_CSV_PATH or run interactively.")
    candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    if (!file.exists(candidate)) stop(sprintf("Path does not exist: %s", candidate))
  }
  ext <- tolower(tools::file_ext(candidate))
  if (ext == "csv") {
    read_csv(candidate, col_types = cols(.default = col_character()))
  } else if (ext %in% c("json", "zip") || dir.exists(candidate)) {
    extracted <- extract_lifestyle_from_raw(candidate)
    read_csv(extracted, col_types = cols(.default = col_character()))
  } else {
    stop(sprintf("Unsupported lifestyle path type: %s", candidate))
  }
}

resolve_sleep_col <- function(mapping, hdr, key, alt_key = NULL) {
  col <- NULL
  if (!is.null(mapping[[key]]) && mapping[[key]] %in% hdr) col <- mapping[[key]]
  if (is.null(col) && !is.null(alt_key)) {
    alt_vals <- unlist(mapping[[alt_key]])
    alt_match <- alt_vals[alt_vals %in% hdr]
    if (length(alt_match) >= 1L) col <- alt_match[[1]]
  }
  col
}

select_best_sensor_by_file <- function(idx, sensor_raw, sensor_id = NA_character_) {
  if (length(idx) == 0) return(idx)
  if (!is.na(sensor_id)) {
    keep <- !is.na(sensor_raw$Sensor_ID[idx]) & sensor_raw$Sensor_ID[idx] == sensor_id
    idx <- idx[keep]
  }
  if (length(idx) == 0) return(idx)
  files <- sensor_raw$Source_File[idx]
  if (length(unique(files)) == 1L) return(idx)
  counts <- table(files)
  best_file <- names(counts)[which.max(counts)]
  idx[files == best_file]
}

compute_nightly_sensor_summary <- function(row, sensor_raw, default_sensor, padding_minutes) {
  sensor_selected <- row$Sensor
  if (is.na(sensor_selected) && is.na(row$Sensor_Raw) && !is.na(default_sensor)) {
    sensor_selected <- default_sensor
  }
  bed_pad <- row$bedtime - minutes(padding_minutes)
  wak_pad <- row$waketime + minutes(padding_minutes)
  idx <- which(sensor_raw$timestamp >= bed_pad & sensor_raw$timestamp <= wak_pad)
  idx <- select_best_sensor_by_file(idx, sensor_raw, sensor_selected)
  idx <- unlist(idx)
  row %>%
    mutate(
      Avg_Temp = mean(sensor_raw$room_temp[idx], na.rm = TRUE),
      Temp_SD = sd(sensor_raw$room_temp[idx], na.rm = TRUE),
      Avg_Rel_Hum = mean(sensor_raw$rel_hum[idx], na.rm = TRUE),
      Rel_Hum_SD = sd(sensor_raw$rel_hum[idx], na.rm = TRUE),
      Avg_Abs_Hum = mean(sensor_raw$abs_hum[idx], na.rm = TRUE),
      Abs_Hum_SD = sd(sensor_raw$abs_hum[idx], na.rm = TRUE),
      Raw_N_Readings = length(idx),
      Sensor_Files = list(unique(sensor_raw$Source_File[idx])),
      Sensor_Names = list(unique(sensor_raw$Source_Name[idx]))
    )
}
