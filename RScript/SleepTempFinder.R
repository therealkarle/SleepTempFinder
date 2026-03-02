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
#   * added utility helpers (parse_datetime_safe, wake_date/nigh_date alias, map_sensor_to_nightly)
#   * updated config.yaml with new sections (parse_orders, locale, plot)
#
# --- 1. ENVIRONMENT SETUP ---
if (!require("rstudioapi")) install.packages("rstudioapi")
pkgs <- c("tidyverse", "lubridate", "yaml", "broom", "GGally", "gridExtra", "grid", "scales")
for (pkg in pkgs) { 
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE) 
}

# always try to run from the script's directory so relative paths work.
# this works in both interactive (RStudio) and non-interactive invocations.
get_script_path <- function() {
  # interactive: rstudioapi yields the file being edited/run
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(normalizePath(p))
  }
  # non-interactive: look for --file= argument
  args1 <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args1)
  if (length(m) > 0) {
    return(normalizePath(sub("^--file=", "", args1[m][1])))
  }
  # as a last resort, look at the call stack (may work when sourced)
  if (!is.null(sys.frame(1)$ofile)) {
    return(normalizePath(sys.frame(1)$ofile))
  }
  # if we still don't know it, try upward search for the file name
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
  return(find_upward("SleepTempFinder.R"))
}

script_path <- get_script_path()
if (!is.null(script_path)) {
  setwd(dirname(script_path))
} else {
  warning("could not determine SleepTempFinder.R location; working directory unchanged")
}

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

# back-compat: older configs used 'temp_files'; rename to 'sensor_files'
if (is.null(config$sensor_files) && !is.null(config$temp_files)) {
  config$sensor_files <- config$temp_files
  cat("Config warning: 'temp_files' renamed to 'sensor_files' for clarity\n")
}

# extract commonly used sub-configs for convenience
orders <- config$parse_orders
loc <- config$locale
plot_cfg <- config$plot
# matching padding in minutes (expand bedtime..waketime by this amount each side)
matching_padding_minutes <- if (!is.null(config$matching_padding_minutes)) as.integer(config$matching_padding_minutes) else 0

# command-line arguments support (dry run, --filter)
#
# Optional flag: --dry-run  (suppress plots, useful for automated runs)
#
# Optional filter syntax (via --filter="...") allows restricting the
# dataset.  The string is semicolon-separated and can include at most one
# date selector plus zero or more sensor/flag clauses.  Date selectors
# recognize:
#   YYYY            entire year (e.g. 2025)
#   qN.YYYY         quarter (e.g. q1.2025)
#   MM.YYYY         month (e.g. 01.2025)
#   DD.MM.YYYY      a single date or, with a comma, a range
#                  (e.g. 02.02.2026,04.03.2026)
#
# Sensor or flag clauses look like "Sensors=Name" or "Flags=A|B".
# Multiple values may be separated by '|' (OR) or ',' (AND for flags;
# sensors are treated as OR).  Sensor names are resolved against
# configuration keys/nicknames.  The CLI filter is merged with
# config$analysis_filter (command-line values override config).
#
# Examples:
#   --filter="2025"                        # whole year 2025
#   --filter="q1.2025;Flags=Hochlitten"    # first quarter with flag
#   --filter="01.2025;Sensors=Wohnwagen"   # January with specific sensor
#   --filter="02.02.2026,04.03.2026"       # explicit date range
args <- commandArgs(trailingOnly = TRUE)

# when running interactively (e.g. in RStudio) we allow a helper to set
# arguments via an environment variable.  This makes it easy to define
# named presets in another script or via RStudio addins without editing
# the main file.
if (interactive()) {
  env_args <- Sys.getenv("_STF_ARGS_", "")
  if (nzchar(env_args)) {
    # simple split on whitespace; user helper should quote values if needed
    args <- strsplit(env_args, "\\s+")[[1]]
    cat("Interactive override: args=", paste(args, collapse=" "), "\n")
  }
}

# support a help message
if ("--help" %in% args || "-h" %in% args) {
  cat("Usage: Rscript SleepTempFinder.R [--dry-run] [--filter='...']\n",
      "Filter grammar: see script comments at top for examples.\n")
  quit(status = 0)
}

# simple dry-run flag
dry_run <- "--dry-run" %in% args
if(dry_run) cat("*** dry-run mode enabled (plots suppressed) ***\n")

# helper: parse a single date token into a start/end Date range
parse_date_token <- function(tok) {
  tok <- tolower(trimws(tok))
  # year e.g. 2025
  if (grepl("^\\d{4}$", tok)) {
    start <- as.Date(paste0(tok, "-01-01"))
    end <- as.Date(paste0(tok, "-12-31"))
    return(list(start = start, end = end))
  }
  # quarter e.g. q1.2025
  if (grepl("^q[1-4]\\.\\d{4}$", tok)) {
    parts <- strsplit(tok, "\\.")[[1]]
    q <- as.integer(sub("^q", "", parts[1]))
    yr <- as.integer(parts[2])
    mstart <- (q - 1) * 3 + 1
    start <- as.Date(sprintf("%04d-%02d-01", yr, mstart))
    mend <- mstart + 2
    end <- as.Date(sprintf("%04d-%02d-%02d", yr, mend,
                             lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", yr, mend)))))
    return(list(start = start, end = end))
  }
  # month e.g. 01.2025 or 1.2025
  if (grepl("^\\d{1,2}\\.\\d{4}$", tok)) {
    parts <- strsplit(tok, "\\.")[[1]]
    m <- as.integer(parts[1]); yr <- as.integer(parts[2])
    start <- as.Date(sprintf("%04d-%02d-01", yr, m))
    end <- as.Date(sprintf("%04d-%02d-%02d", yr, m,
                             lubridate::days_in_month(start)))
    return(list(start = start, end = end))
  }
  # explicit date or range dd.mm.yyyy[,dd.mm.yyyy]
  if (grepl("^\\d{2}\\.\\d{2}\\.\\d{4}", tok)) {
    parts <- strsplit(tok, ",")[[1]]
    ds <- lapply(parts, lubridate::dmy)
    if (length(ds) == 1) return(list(start = ds[[1]], end = ds[[1]]))
    if (length(ds) == 2) {
      return(list(start = min(ds[[1]], ds[[2]]), end = max(ds[[1]], ds[[2]])))
    }
  }
  NULL
}

# split a sequence of flags/sensors separated by pipe or comma
split_list_token <- function(val) {
  if (is.null(val) || val == "") return(character(0))
  parts <- unlist(strsplit(val, "\\||,", perl = TRUE))
  trimws(parts[parts != ""])
}

# parse the custom --filter argument; returns a list containing
#   enabled (bool), sensor_include, flags_include, flags_mode,
#   date_start, date_end (Date or NULL)
parse_filter_string <- function(arg) {
  cfg <- list(enabled = TRUE,
              sensor_include = character(0),
              flags_include = character(0),
              flags_mode = "any",
              date_start = NULL,
              date_end = NULL)
  tokens <- unlist(strsplit(arg, ";"))
  for (tok in tokens) {
    tok <- trimws(tok)
    if (tok == "") next
    if (grepl("^(?i)flags=", tok, perl = TRUE)) {
      body <- sub("^(?i)flags=", "", tok, perl = TRUE)
      if (grepl("\\|", body)) {
        cfg$flags_mode <- "any"
        cfg$flags_include <- split_list_token(body)
      } else if (grepl(",", body)) {
        cfg$flags_mode <- "all"
        cfg$flags_include <- split_list_token(body)
      } else {
        cfg$flags_include <- trimws(body)
      }
    } else if (grepl("^(?i)sensors=", tok, perl = TRUE)) {
      body <- sub("^(?i)sensors=", "", tok, perl = TRUE)
      cfg$sensor_include <- split_list_token(body)
    } else {
      # assume date token
      dr <- parse_date_token(tok)
      if (!is.null(dr)) {
        cfg$date_start <- dr$start
        cfg$date_end <- dr$end
      } else {
        warning("unrecognized filter token: ", tok)
      }
    }
  }
  cfg
}

# look for an explicit --filter= value
filter_arg <- NULL
for (a in args) {
  if (startsWith(a, "--filter=")) {
    filter_arg <- sub("^--filter=", "", a)
  }
}
cli_filter <- NULL
if (!is.null(filter_arg)) {
  cli_filter <- parse_filter_string(filter_arg)
  cat("CLI filter parsed: ", capture.output(str(cli_filter)), "\n")
  # merge into analysis_filter config so downstream code can apply it
  if (is.null(config$analysis_filter)) config$analysis_filter <- list(enabled = TRUE)
  config$analysis_filter$enabled <- TRUE
  # sensor_include will be resolved to canonical IDs later once the
  # helper function is defined (see below)
  if (length(cli_filter$sensor_include) > 0) {
    config$analysis_filter$sensor_include <- cli_filter$sensor_include
  }
  if (length(cli_filter$flags_include) > 0) {
    config$analysis_filter$flags_include <- cli_filter$flags_include
    config$analysis_filter$flags_mode <- cli_filter$flags_mode
  }
}

# helper that applies parsing orders by type and quiet=TRUE
parse_datetime_safe <- function(x, type = "garmin_datetime") {
  if (is.null(orders[[type]])) {
    warning("no parse orders for type: ", type)
    return(parse_date_time(x, quiet = TRUE))
  }
  res <- parse_date_time(x, orders = orders[[type]], quiet = TRUE)
  # warn when parsing yields NA values so we can diagnose missing timestamps
  if (length(res) > 0 && any(is.na(res))) {
    failed <- which(is.na(res))
    sample_vals <- unique(head(as.character(x[failed]), 5))
    warning(sprintf("parse_datetime_safe: %d values failed to parse for type '%s'. Examples: %s",
                    length(failed), type, paste(sample_vals, collapse = "; ")))
  }
  res
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

# helper to inspect a sensor CSV and return the matching sensor_files entry (or NULL)
detect_sensor_config <- function(path) {
  base <- basename(path)
  # explicit paths first
  for (id in names(config$sensor_files)) {
    if (base == basename(config$sensor_files[[id]]$path)) return(config$sensor_files[[id]])
  }
  hdr <- tryCatch(names(suppressWarnings(read_delim(path, delim = ",", n_max = 1, locale = sensor_locale, show_col_types = FALSE))),
                  error = function(e) character(0))
  for (id in names(config$sensor_files)) {
    f <- config$sensor_files[[id]]
    if (all(c(f$col_time, f$col_temp, f$col_hum) %in% hdr)) return(f)
  }
  NULL
}

is_sensor_csv <- function(path, sensor_files) {
  !is.null(detect_sensor_config(path))
}

get_sensor_file_info <- function(path) {
  cfg <- detect_sensor_config(path)
  if (is.null(cfg)) {
    # fallback to first mapping if nothing matched
    config$sensor_files[[1]]
  } else {
    cfg
  }
}

# return the canonical config key (sensor ID) for a given CSV path.  This
# mirrors the logic in detect_sensor_config but returns the name of the entry
# rather than the entry itself.  The ID is used later to restrict which
# rows are included when a calendar entry specifies a particular sensor.
identify_sensor_id <- function(path) {
  base <- basename(path)
  for (id in names(config$sensor_files)) {
    if (base == basename(config$sensor_files[[id]]$path)) return(id)
  }
  # try header-based match as a fallback
  cfg <- detect_sensor_config(path)
  if (!is.null(cfg)) {
    for (id in names(config$sensor_files)) {
      if (identical(cfg, config$sensor_files[[id]])) return(id)
    }
  }
  NA_character_
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

# convert a timestamp to the corresponding "wake" date by adding 12h
# (previous versions used a 12h subtraction to get the bedtime day; we keep
# the old name as an alias for compatibility but new code should call
# `wake_date`.)
wake_date <- function(ts) as.Date(ts + hours(12))

# legacy alias preserved for scripts that still reference night_date
night_date <- wake_date

collapse_flags <- function(x) {
  # incoming list/vector of flag strings; ensure uniqueness and sort
  # then join with comma+space for display.  The input may already contain
  # comma- or semicolon-separated flags which were split earlier by
  # `split_flags`, so we only need to collapse the cleaned values here.
  vals <- sort(unique(trim_vector(x)))
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = ", ")
}

split_flags <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(character(0))
  # split on commas or semicolons so the user can write either
  parts <- str_split(x, "\\s*[,;]\\s*")[[1]]
  sort(unique(trim_vector(parts)))
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

# ICS text values escape certain characters with a backslash (comma, semicolon,
# backslash and newline).  When we pull SUMMARY/DESCRIPTION from an event we
# should unescape so that a calendar entry such as
#   SUMMARY:Sensor\=LivingRoom, Flags\=quiet
# yields the literal `Sensor=LivingRoom, Flags=quiet` instead of including
# stray backslashes.  This also prevents the later parser from truncating at
# an escaped comma.
unescape_ics <- function(val) {
  if (is.na(val)) return(val)
  out <- val
  out <- str_replace_all(out, "\\\\n", "\n")
  out <- str_replace_all(out, "\\\\N", "\n")
  out <- str_replace_all(out, "\\\\,", ",")
  out <- str_replace_all(out, "\\\\;", ";")
  out <- str_replace_all(out, "\\\\\\\\", "\\")
  out
}

parse_sensor_flags <- function(text_value) {
  # text_value can come from either SUMMARY or DESCRIPTION (or both) of
  # an ics event.  We look for a `sensor=` assignment and an optional
  # `flag`/`flags=` assignment.  Flags may be declared after the sensor
  # (separated by a semicolon or comma) and multiple flags can be provided
  # as a comma‑separated list.  Examples:
  #   parse_sensor_flags("Sensor=foo; Flags=bar")
  #   parse_sensor_flags("sensor=foo; flags=a,b,c")
  #   parse_sensor_flags("flags=quiet; sensor=LivingRoom")
  text_value <- text_value %||% ""

  # there may be more than one flags= declaration (e.g. summary and
  # description both include them) so use match_all and combine results.
  flags_raw_vec <- str_match_all(text_value,
                                 regex("flags?\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))[[1]][,2]
  # to prevent sensor names that contain commas from being truncated we
  # strip any flag assignments from the text before looking for `sensor=`.
  text_no_flags <- str_replace_all(text_value,
                                   regex("flags?\\s*=\\s*[^;\\n]+", ignore_case = TRUE), "")

  # now capture sensor; allow commas in the value but stop at semicolon or
  # newline (multi-day events may add trailing semicolons during combine).
  sensor_match <- str_match(text_no_flags, regex("sensor\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))

  sensor_raw <- str_trim(sensor_match[, 2] %||% NA_character_)
  sensor_name <- sensor_raw
  if (!is.na(sensor_name) && str_detect(sensor_name, "/")) {
    sensor_name <- str_trim(str_split(sensor_name, "/", n = 2)[[1]][2])
  }

  flags <- character(0)
  if (length(flags_raw_vec) > 0) {
    # split each found value on commas and merge unique items
    flags <- sort(unique(unlist(lapply(flags_raw_vec, split_flags))))
  }

  list(sensor_raw = ifelse(is.na(sensor_raw) || sensor_raw == "", NA_character_, sensor_raw),
       sensor_name = ifelse(is.na(sensor_name) || sensor_name == "", NA_character_, sensor_name),
       flags = flags)
}

# resolve a calendar sensor label to a canonical config key.  The calendar
# value may be either the key itself or, if defined in the config, the
# sensor's `nickname`.  Returns NA and prints a warning if no match is found.
resolve_sensor_label <- function(label, cfg) {
  # attempt to map a calendar label to the canonical config key.  Accepts
  # either the key itself or any of the configured nicknames.  If no exact
  # match is found, we also try a simple substring match (case-insensitive)
  # so that values like "Wohnwagen" or "Wohnwagen Sensor" still resolve to
  # "WohnwagenSensor".
  if(is.na(label) || label == "") return(NA_character_)
  lookup <- list()
  for(id in names(cfg$sensor_files)) {
    lookup[[tolower(id)]] <- id
    nicks <- cfg$sensor_files[[id]]$nickname %||% character(0)
    for(nick in as.character(nicks)) {
      if(nzchar(nick)) lookup[[tolower(nick)]] <- id
    }
  }
  key <- tolower(label)
  if(key %in% names(lookup)) return(lookup[[key]])
  # substring fallback: find any lookup key that appears inside the label
  matches <- unique(unlist(lapply(names(lookup), function(k) {
    if(str_detect(key, fixed(k))) return(lookup[[k]])
    NULL
  })))
  if(length(matches) == 1) {
    return(matches[[1]])
  }
  cat(sprintf("Calendar warning: sensor '%s' not found in config\n", label))
  return(NA_character_)
}

# if CLI provided a sensor filter earlier, resolve any human-friendly names now
# that the helper is defined.
if (!is.null(cli_filter) && length(cli_filter$sensor_include) > 0) {
  cli_filter$sensor_include <- sapply(cli_filter$sensor_include,
                                     function(lbl) resolve_sensor_label(lbl, config),
                                     USE.NAMES = FALSE)
  cli_filter$sensor_include <- trimws(cli_filter$sensor_include)
  config$analysis_filter$sensor_include <- cli_filter$sensor_include
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
    # unescape any ICS backslash-escapes so commas/semicolons are treated as
    # literal text rather than delimiters in our parser
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
    # diagnostics: if the source text mentions "flag" but parser returned
    # an empty vector, log the raw text so the user can inspect the format
    if (str_detect(tolower(combined_text), "flags?") && length(parsed$flags) == 0) {
      cat(sprintf("Calendar parse warning: flags keyword found but none extracted in event text '%s'\n", combined_text))
    }

    # optionally drop the first date of a multi‑day event
    seq_start <- start_date
    if (!is.null(config$calendar_assignment$ignore_event_start) &&
        isTRUE(config$calendar_assignment$ignore_event_start) &&
        start_date < end_date) {
      seq_start <- start_date + 1
    }
    date_seq <- seq(seq_start, end_date, by = "1 day")
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
  sensor_candidates <- all_data_files[sapply(all_data_files, is_sensor_csv, sensor_files = config$sensor_files)]
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
  explicit_sensor_raw <- unlist(lapply(config$sensor_files, function(x) file.path(config$data_directory, x$path)))
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
      # align parsed times to the same calendar Date, then detect and correct
      # cases where columns were mis-parsed (e.g. duration '7h 40min' parsed as 07:40)
      mutate(waketime = update(waketime, year = year(Date), month = month(Date), mday = day(Date)),
        bedtime = update(bedtime, year = year(Date), month = month(Date), mday = day(Date))) %>%
      # if bedtime appears to be a morning time (<=12) while waketime is an evening time (>12)
      # and bedtime < waketime, it's likely the two were swapped during parsing; swap them back
      mutate(
        .bedtime_orig = bedtime,
        .waketime_orig = waketime,
        .swap_flag = (!is.na(.bedtime_orig) & !is.na(.waketime_orig) &
            (hour(.bedtime_orig) <= 12 & hour(.waketime_orig) > 12 & .bedtime_orig < .waketime_orig))
      ) %>%
      mutate(
        bedtime = if_else(.swap_flag, .waketime_orig, .bedtime_orig),
        waketime = if_else(.swap_flag, .bedtime_orig, .waketime_orig)
      ) %>%
      mutate(bedtime = if_else(bedtime > waketime, bedtime - days(1), bedtime)) %>%
      select(-.bedtime_orig, -.waketime_orig, -.swap_flag) %>%
      mutate(across(any_of(unlist(mapping[4:length(mapping)])), clean_val_final))
})

# helper: choose sensor file configuration based on path or header
# (this mirrors detect_sensor_config but is kept here for backwards
# compatibility with older versions of the script)
get_sensor_file_info <- function(path) {
  base <- basename(path)
  # first try to match explicit path in config
  for (id in names(config$sensor_files)) {
    if (base == basename(config$sensor_files[[id]]$path)) return(config$sensor_files[[id]])
  }
  # otherwise, try header matching
  hdr <- tryCatch(names(suppressWarnings(read_delim(path, delim = ",", n_max = 1, locale = sensor_locale, show_col_types = FALSE))),
                  error = function(e) character(0))
  for (id in names(config$sensor_files)) {
    f <- config$sensor_files[[id]]
    if (all(c(f$col_time, f$col_temp, f$col_hum) %in% hdr)) return(f)
  }
  # fallback to first mapping
  config$sensor_files[[1]]
}

# read all discovered sensor CSVs, track source file and attempt column renaming
sensor_raw <- map_df(all_sensor_files, function(fp) {
  f_info <- get_sensor_file_info(fp)
  suppressWarnings(read_delim(fp, delim = ",", locale = sensor_locale, show_col_types = FALSE)) %>%
    rename(timestamp = !!f_info$col_time, room_temp = !!f_info$col_temp, rel_hum = !!f_info$col_hum, abs_hum = `Abs Humidity(g/m³)`) %>%
    mutate(timestamp = parse_datetime_safe(timestamp, type = "sensor_timestamp")) %>%
    mutate(Source_File = fp,
           Source_Name = canonical_basename(fp),
           Sensor_ID = identify_sensor_id(fp))
})

# Diagnostic summary per sensor file: count rows, NA timestamps, first/last timestamp
if (exists("sensor_raw") && nrow(sensor_raw) > 0) {
  sensor_stats <- sensor_raw %>%
    group_by(Source_File) %>%
    summarise(
      N_Rows = n(),
      N_NA_Timestamp = sum(is.na(timestamp)),
      First_TS = if(all(is.na(timestamp))) as.POSIXct(NA) else min(timestamp, na.rm = TRUE),
      Last_TS = if(all(is.na(timestamp))) as.POSIXct(NA) else max(timestamp, na.rm = TRUE),
      .groups = 'drop'
    )

  cat("Sensor files read:\n")
  for(i in seq_len(nrow(sensor_stats))) {
    row <- sensor_stats[i,]
    cat(sprintf(" - %s: rows=%d, NA_timestamps=%d, first=%s, last=%s\n",
                as.character(row$Source_File), as.integer(row$N_Rows), as.integer(row$N_NA_Timestamp),
                ifelse(is.na(row$First_TS), "NA", format(row$First_TS, "%Y-%m-%d %H:%M")),
                ifelse(is.na(row$Last_TS), "NA", format(row$Last_TS, "%Y-%m-%d %H:%M"))))
  }
}

# save a nightly summary of the raw sensor data *before* any filtering
# (used later to illustrate the effect of the sensor-stage filter)
sensor_nightly_prefilter <- sensor_raw %>%
  mutate(Date = wake_date(timestamp)) %>%
  group_by(Date) %>%
  summarise(
    Avg_Temp = mean(room_temp, na.rm = TRUE),
    Avg_Rel_Hum = mean(rel_hum, na.rm = TRUE),
    Avg_Abs_Hum = mean(abs_hum, na.rm = TRUE),
    .groups = 'drop'
  )

calendar_daily_raw <- load_calendar_daily(config$calendar_source, config$calendar_parser)
# when using wake‑date semantics the three‑day smoothing rule is usually
# unnecessary; it can be disabled via calendar_assignment.require_prev_next_day
# (default in config.yaml is now FALSE).
calendar_daily <- apply_calendar_three_day_rule(calendar_daily_raw, config$calendar_assignment)

# retain the raw value parsed from the calendar so that we can
# distinguish between a genuinely missing assignment and a label that failed
# to resolve against the config.
if (nrow(calendar_daily) > 0) {
  calendar_daily$Sensor_Raw <- calendar_daily$Sensor
  calendar_daily$Sensor <- sapply(calendar_daily$Sensor_Raw,
                                  resolve_sensor_label,
                                  cfg = config,
                                  USE.NAMES = FALSE)
}

# if a default sensor is configured, fill only those rows that had no raw
# label at all.  Do not override entries where the user supplied a value that
# could not be resolved (we want those to remain NA so the script keeps both
# sensors and the audit will show the mismatch).
if (!is.null(config$calendar_default_sensor) && nzchar(config$calendar_default_sensor)) {
  default_s <- config$calendar_default_sensor
  n_na_before <- sum(is.na(calendar_daily$Sensor) & is.na(calendar_daily$Sensor_Raw))
  calendar_daily$Sensor[is.na(calendar_daily$Sensor) & is.na(calendar_daily$Sensor_Raw)] <- default_s
  if (n_na_before > 0) {
    cat(sprintf("Calendar default sensor applied: '%s' to %d day(s)\n", 
                default_s, n_na_before))
  }
}

# warn the user if any days still lack a valid sensor mapping; include the
# original raw text for debugging
if (nrow(calendar_daily) > 0) {
  problem <- calendar_daily %>% filter(is.na(Sensor) & !is.na(Sensor_Raw))
  if (nrow(problem) > 0) {
    cat(sprintf("Calendar warning: %d day(s) have unrecognized sensor labels:\n",
                nrow(problem)))
    for(i in seq_len(nrow(problem))) {
      row <- problem[i,]
      cat(sprintf("  %s -> '%s'\n", format(row$Date), row$Sensor_Raw))
    }
  }
}



# build a per-night sensor summary (after any filtering)
sensor_summary <- sensor_raw %>%
  mutate(Date = wake_date(timestamp),
         Source_Name = canonical_basename(Source_File)) %>%
  group_by(Date) %>%
  summarise(
    Sensor_Files = list(unique(Source_File)),
    Sensor_Names = list(unique(Source_Name)),
    Sensor_IDs = list(unique(na.omit(Sensor_ID))),
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
# when a calendar entry specifies a sensor, only rows from that
# sensor should contribute to the nightly average; otherwise all sensors are
# considered.  To accomplish this we join the calendar information before
# computing the averages and then conditionally filter `sensor_raw` inside the
# rowwise mutate.

temp_mapped <- sleep_complete %>% 
  filter(!is.na(Sleep_Score), !is.na(HRV), !is.na(RHR)) %>%
  # calendar assignment may provide a default sensor value already; also
  # carry the raw label so it can be referenced later when building the
  # review dataframe.
  left_join(calendar_daily %>% select(Date, Sensor, Sensor_Raw, Flags, Flags_List), by = "Date") %>%
  rowwise() %>% 
  mutate(
    # apply configurable padding when matching sensor rows
    .bed_pad = bedtime - minutes(matching_padding_minutes),
    .wak_pad = waketime + minutes(matching_padding_minutes),
    .idx_used = list({
      idx <- which(sensor_raw$timestamp >= .bed_pad & sensor_raw$timestamp <= .wak_pad)
      if(!is.na(Sensor)) {
        idx <- idx[sensor_raw$Sensor_ID[idx] == Sensor]
      }
      idx
    }),
    # restrict to the selected sensor if one is assigned
    Avg_Temp = mean(sensor_raw$room_temp[unlist(.idx_used)], na.rm = TRUE),
    Avg_Rel_Hum = mean(sensor_raw$rel_hum[unlist(.idx_used)], na.rm = TRUE),
    Avg_Abs_Hum = mean(sensor_raw$abs_hum[unlist(.idx_used)], na.rm = TRUE),
    Raw_N_Readings = length(unlist(.idx_used)),
    Sensor_Files = list(unique(sensor_raw$Source_File[unlist(.idx_used)])),
    Sensor_Names = list(unique(sensor_raw$Source_Name[unlist(.idx_used)]))
  ) %>%
  ungroup() %>%
  select(-.bed_pad, -.wak_pad, -.idx_used)

# detect and collapse any duplicated nights before further processing.  It is
# common for Garmin exports to include multiple files that happen to have the
# same calendar Date; earlier versions of the script left these duplicates
# intact until visualization, causing audit counts (and downstream analyses)
# to report twice the number of unique nights.  Keep the first appearance and
# warn so the user can inspect if unexpected.
dup_dates <- temp_mapped %>% count(Date) %>% filter(n > 1) %>% pull(Date)
if (length(dup_dates) > 0) {
  cat(sprintf("Warning: %d duplicate sleep records detected for dates: %s\n",
              length(dup_dates), paste(format(dup_dates, "%Y-%m-%d"), collapse = ", ")))
  temp_mapped <- temp_mapped %>% arrange(Date) %>% distinct(Date, .keep_all = TRUE)
}



# build a review data frame summarizing all inputs per night
# contains original sleep source file, sensor file(s), calendar sensor/flags, and final averages
nightly_review_df <- temp_mapped %>%
  mutate(
    Sleep_Source = Source_File,
    Sleep_Name = ifelse(str_detect(canonical_basename(Source_File), regex("schlaf", ignore_case = TRUE)),
                         "Sleep",
                         canonical_basename(Source_File)),
    # when a calendar sensor is specified we only show the files that belong
    # to that sensor; otherwise list whatever was available
    Sensor_Sources = map2_chr(Sensor_Files, Sensor, function(files, sensor) {
      if(!is.na(sensor)) {
        sel <- files[sapply(files, identify_sensor_id) == sensor]
        if(length(sel) > 0) return(paste(sel, collapse = "; "))
      }
      paste(files, collapse = "; ")
    }),
    # the name we show should reflect what the calendar told us; if that
    # value failed to resolve we also include the raw text so the user can
    # spot the problem.
    Sensor_Names = case_when(
      !is.na(Sensor) ~ Sensor,
      !is.na(Sensor_Raw) ~ paste0("(raw: ", Sensor_Raw, ")"),
      TRUE ~ map_chr(Sensor_Names, ~ paste(.x, collapse = "; "))
    )
  )
# simple audit: how many distinct files and canonical names contribute
cat(sprintf("Review DF constructed: %d nights, %d unique sleep files (%d canonical), %d unique sensor file paths (%d canonical)\n", 
            nrow(nightly_review_df),
            n_distinct(nightly_review_df$Sleep_Source),
            n_distinct(nightly_review_df$Sleep_Name),
            n_distinct(unlist(nightly_review_df$Sensor_Files)),
            n_distinct(unlist(nightly_review_df$Sensor_Names))))

final_data_matched <- temp_mapped %>% 
  filter(
    (!is.na(Avg_Temp) & !is.nan(Avg_Temp)) |
      (!is.na(Avg_Rel_Hum) & !is.nan(Avg_Rel_Hum)) |
      (!is.na(Avg_Abs_Hum) & !is.nan(Avg_Abs_Hum))
  )

n_before_analysis_filter <- nrow(final_data_matched)
# if CLI supplied a date range, trim the results accordingly
if (!is.null(cli_filter) && !is.null(cli_filter$date_start)) {
  cat(sprintf("Applying date filter %s -> %s\n", cli_filter$date_start, cli_filter$date_end))
  final_data_matched <- final_data_matched %>%
    filter(Date >= cli_filter$date_start & Date <= cli_filter$date_end)
}

final_data_matched <- apply_analysis_subset_filter(final_data_matched, config$analysis_filter)
n_after_analysis_filter <- nrow(final_data_matched)

if(!is.null(config$analysis_filter) && isTRUE(config$analysis_filter$enabled)) {
  cat(sprintf("Analysis filter enabled: kept %d of %d nights\n", n_after_analysis_filter, n_before_analysis_filter))
}

if(nrow(final_data_matched) > 0) {
  full_dates <- seq(min(final_data_matched$Date, na.rm=T), max(final_data_matched$Date, na.rm=T), by="1 day")
  # complete() fills in any missing dates.  The script now de-duplicates
  # sleep records earlier, so duplicates should not normally reach this point,
  # but we still guard against them here to avoid the dashboard showing a date
  # twice.  Keep the first appearance if multiple rows slip through.
  final_data_viz <- final_data_matched %>%
    complete(Date = full_dates) %>%
    distinct(Date, .keep_all = TRUE)
} else {
  full_dates <- as.Date(character(0))
  final_data_viz <- final_data_matched
}

# --- EXCLUDE: Define reasons (Missing Sleep, Missing Room) ---
excluded_sleep_dates <- sleep_complete %>% 
  filter(is.na(Sleep_Score) | is.na(HRV) | is.na(RHR)) %>% 
  pull(Date)

excluded_sensor_dates_all <- temp_mapped %>% 
  filter(
    (is.na(Avg_Temp) | is.nan(Avg_Temp)) &
      (is.na(Avg_Rel_Hum) | is.nan(Avg_Rel_Hum)) &
      (is.na(Avg_Abs_Hum) | is.nan(Avg_Abs_Hum))
  ) %>% 
  pull(Date)

# no outlier filtering, so treat all sensor-missing nights equally
excluded_outlier_dates_dates <- as.Date(character(0))

# Nights that are missing room data (no filtering has been applied)
excluded_sensor_dates <- excluded_sensor_dates_all

# Unique total excluded nights across all reasons
unique_excluded_dates <- unique(c(excluded_sleep_dates, excluded_sensor_dates))

# Format for printing
excluded_sleep_fmt <- format(excluded_sleep_dates, "%d.%m.%Y")
excluded_sensor_fmt <- format(excluded_sensor_dates, "%d.%m.%Y")

calendar_days_raw_n <- nrow(calendar_daily_raw)
calendar_days_after_rule_n <- nrow(calendar_daily)
calendar_sensor_assigned_n <- sum(!is.na(temp_mapped$Sensor))
calendar_flags_assigned_n <- sum(!is.na(temp_mapped$Flags))

# --- OUTPUT: AUDIT ---
cat("\n===========================================================\n")
cat("                DATA QUALITY AUDIT\n")
cat("===========================================================\n")
cat(sprintf("Total nights detected:        %d\n", nrow(sleep_df_raw)))
cat(sprintf("Nights used for analysis:     %d (unique dates %d)\n",
            nrow(final_data_matched), n_distinct(final_data_matched$Date)))
cat(sprintf("Nights excluded total:        %d\n", length(unique_excluded_dates)))
cat("-----------------------------------------------------------\n")
cat(sprintf("Reason: Missing Sleep Data:   %d\n", length(excluded_sleep_fmt)))
if(length(excluded_sleep_fmt) > 0) cat(paste0("       [", paste(excluded_sleep_fmt, collapse = "], ["), "]\n"))
cat(sprintf("Reason: Missing Room Data:    %d\n", length(excluded_sensor_fmt)))
if(length(excluded_sensor_fmt) > 0) cat(paste0("       [", paste(excluded_sensor_fmt, collapse = "], ["), "]\n"))
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
  mutate(Date = wake_date(timestamp)) %>% 
  group_by(Date) %>%
  summarise(Avg_Temp = mean(room_temp, na.rm=T), Avg_Rel_Hum = mean(rel_hum, na.rm=T), Avg_Abs_Hum = mean(abs_hum, na.rm=T), .groups = 'drop')

# compute counts of nights that have any data at all before/after filter; these
# are *not* the same as the per-variable counts shown later.  A night can be
# present only for humidity, for example, which increments the "used" total
# but not the temperature-specific n value below.
n_any_raw_prefilter <- sensor_nightly_prefilter %>%
  filter(!is.na(Avg_Temp) | !is.na(Avg_Rel_Hum) | !is.na(Avg_Abs_Hum)) %>%
  nrow()
n_any_raw_after <- sensor_nightly_raw %>%
  filter(!is.na(Avg_Temp) | !is.na(Avg_Rel_Hum) | !is.na(Avg_Abs_Hum)) %>%
  nrow()

cat("                DESCRIPTIVE STATISTICS\n")
cat("===========================================================\n\n")
# show some overall counts to avoid confusion between variable-specific n's
cat(sprintf("Nights used for analysis (any metric): %d\n", nrow(final_data_matched)))
cat(sprintf("Nights with any raw sensor data before filter: %d\n", n_any_raw_prefilter))
cat(sprintf("Nights with any raw sensor data after filter: %d\n\n", n_any_raw_after))
cat("TABLE 1: BIOMARKERS (Garmin Data)\n")
bio_vars <- list("Sleep_Score" = "Sleep_Score", "HRV" = "HRV", "RHR" = "RHR")
for(v_name in names(bio_vars)) {
  m_data <- final_data_matched[[bio_vars[[v_name]]]]; r_data <- sleep_complete[[bio_vars[[v_name]]]]
  cat(sprintf("%-15s (Used) | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n", v_name, mean(m_data, na.rm=T), sd(m_data, na.rm=T), min(m_data, na.rm=T), max(m_data, na.rm=T), sum(!is.na(m_data))))
  cat(sprintf("%-15s (Raw)  | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n\n", "", mean(r_data, na.rm=T), sd(r_data, na.rm=T), min(r_data, na.rm=T), max(r_data, na.rm=T), sum(!is.na(r_data))))
}
cat("\nTABLE 2: ROOM DATA (Nightly Averages)\n")
cat("  (n values below are count of non‑missing nights for that variable)\n")
room_vars <- list("Room Temp" = "Avg_Temp", "Rel Humidity" = "Avg_Rel_Hum", "Abs Humidity" = "Avg_Abs_Hum")
for(v_name in names(room_vars)) {
  m_data <- final_data_matched[[room_vars[[v_name]]]]
  r_data <- sensor_nightly_raw[[room_vars[[v_name]]]]
  pre_data <- sensor_nightly_prefilter[[room_vars[[v_name]]]]
  cat(sprintf("%-15s (Used) | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n", v_name, mean(m_data, na.rm=T), sd(m_data, na.rm=T), min(m_data, na.rm=T), max(m_data, na.rm=T), sum(!is.na(m_data))))
  cat(sprintf("%-15s (Raw after filter) | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n", "", mean(r_data, na.rm=T), sd(r_data, na.rm=T), min(r_data, na.rm=T), max(r_data, na.rm=T), n_any_raw_after))
  cat(sprintf("%-15s (Raw before filter)| Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n\n", "", mean(pre_data, na.rm=T), sd(pre_data, na.rm=T), min(pre_data, na.rm=T), max(pre_data, na.rm=T), n_any_raw_prefilter))
}

# --- 4. DASHBOARD DATAFRAME ---
# Build a dashboard table keeping original structure. Filter out missing Date rows,
# and add a human-readable date string for display.  Also include the sensor
# file(s) that contributed to each night so the dashboard can show which source
# the merged room data came from.
dashboard_df <- final_data_viz %>%
  filter(!is.na(Date)) %>%
  # Sensor_Files is a list-column; convert to semicolon-separated string for
  # ease of display.  Use Sensor_Names if you prefer canonical names instead.
  mutate(Sensor_File = map_chr(Sensor_Files, ~ paste(.x, collapse = "; ")) ) %>%
  select(Date, Sensor, Flags, Sensor_File, Avg_Temp, Avg_Rel_Hum, Avg_Abs_Hum, Sleep_Score, HRV, RHR) %>%
  mutate(Date_Str = format(Date, "%d.%m.%Y"))

cat("\n>>> DASHBOARD DATAFRAME CREATED (Object: dashboard_df)\n\n")


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

