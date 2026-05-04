# SleepTempFinder.R
# Primary analysis script: ingest Garmin sleep exports + room sensor CSVs,
# align on sleep periods, compute nightly averages, apply filters, and produce
# statistics & plots.
#
## Changes since original version:
##   * centralized date/time parsing orders via config
##   * added locale configuration for sensor imports
##   * moved plot colors/labels into config and derived vectors
##   * tightened scope of intermediate variables using local()
##   * removed obsolete dataframes and inline simple transforms
##   * unified sensor header detection/lookup helpers
##   * added dry-run flag to suppress plotting
##   * added utility helpers (parse_datetime_safe, wake_date/nigh_date alias, map_sensor_to_nightly)
##   * updated config.yaml with new sections (parse_orders, locale, plot)
#
# --- 1. ENVIRONMENT SETUP ---
if (!require("rstudioapi")) install.packages("rstudioapi")
pkgs <- c("tidyverse", "lubridate", "yaml", "broom", "GGally", "gridExtra", "grid", "scales")
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# explicitly load packages so static linters can resolve tidyverse/lubridate functions
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))

utils::globalVariables(
  c("Date", "Sensor", "Flags", "Flags_List", "sensor_values", "flags_values", ".flags_vec")
)

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
script_directory <- if (!is.null(script_path)) dirname(script_path) else normalizePath(getwd(), mustWork = FALSE)

# Load flag expression parser for complex boolean flag expressions
source(file.path(script_directory, "flag_expression_parser.R"), local = FALSE)

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

# determine the default sensor from sensor_files.default = true, falling back
# to calendar_default_sensor for older configs.
get_default_sensor_id <- function(cfg) {
  if (is.null(cfg$sensor_files) || length(cfg$sensor_files) == 0) return(NA_character_)
  default_ids <- names(cfg$sensor_files)[sapply(cfg$sensor_files, function(x) isTRUE(x$default))]
  if (length(default_ids) > 1L) {
    cat(sprintf("Config warning: multiple sensor_files entries have default: true (%s); using first.\n",
                paste(default_ids, collapse = ", ")))
  }
  if (length(default_ids) >= 1L) {
    return(default_ids[[1]])
  }
  if (!is.null(cfg$calendar_default_sensor) && nzchar(cfg$calendar_default_sensor)) {
    return(cfg$calendar_default_sensor)
  }
  NA_character_
}

default_sensor_id <- get_default_sensor_id(config)

# extract commonly used sub-configs for convenience
orders <- config$parse_orders
loc <- config$locale
plot_cfg <- config$plot
if (is.null(plot_cfg)) plot_cfg <- list()
# matching padding in minutes (expand bedtime..waketime by this amount each side)
matching_padding_minutes <- if (!is.null(config$matching_padding_minutes)) as.integer(config$matching_padding_minutes) else 0

# summary interval for reported value ranges; default is 90% if config value missing or invalid
summary_interval <- if (!is.null(config$summary_interval)) as.numeric(config$summary_interval) else NA_real_
if (is.na(summary_interval) || summary_interval <= 0 || summary_interval >= 1) {
  summary_interval <- 0.90
}
summary_percent <- format(round(summary_interval * 100), trim = TRUE, scientific = FALSE)
summary_interval_label <- sprintf("%s%% Value Interval", summary_percent)
summary_interval_lower <- (1 - summary_interval) / 2
summary_interval_upper <- 1 - summary_interval_lower

plot_export_cfg <- plot_cfg$export
if (is.null(plot_export_cfg)) plot_export_cfg <- list()
plot_export_enabled <- if (is.null(plot_export_cfg$enabled)) TRUE else isTRUE(plot_export_cfg$enabled)
plot_export_dir_cfg <- plot_export_cfg$output_dir
if (is.null(plot_export_dir_cfg) || !nzchar(plot_export_dir_cfg)) plot_export_dir_cfg <- "../PlotOutput"

is_absolute_path <- function(path_value) {
  grepl("^(?:[A-Za-z]:[\\\\/]|/)", path_value, perl = TRUE)
}

resolve_plot_output_dir <- function(path_value) {
  if (is.null(path_value) || !nzchar(path_value)) {
    path_value <- "../PlotOutput"
  }
  if (is_absolute_path(path_value)) {
    return(normalizePath(path_value, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(script_directory, path_value), winslash = "/", mustWork = FALSE)
}

plot_output_dir <- resolve_plot_output_dir(plot_export_dir_cfg)

slugify_plot_name <- function(...) {
  parts <- vapply(list(...), function(value) as.character(value)[1], character(1))
  plot_name <- paste(parts[nzchar(parts)], collapse = "_")
  plot_name <- iconv(plot_name, to = "ASCII//TRANSLIT")
  if (is.na(plot_name) || !nzchar(plot_name)) plot_name <- "plot"
  plot_name <- gsub("[^A-Za-z0-9]+", "_", plot_name)
  plot_name <- gsub("_+", "_", plot_name)
  plot_name <- gsub("^_|_$", "", plot_name)
  tolower(plot_name)
}

save_plot_image <- function(plot_object, file_stub, width = 10, height = 6, dpi = 300) {
  if (dry_run || !plot_export_enabled) return(invisible(NULL))
  dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)
  file_path <- file.path(plot_output_dir, paste0(file_stub, ".png"))
  tryCatch({
    ggplot2::ggsave(filename = file_path, plot = plot_object, width = width, height = height, dpi = dpi, bg = "white")
    invisible(file_path)
  }, error = function(e) {
    warning("Failed to save plot '", file_stub, "': ", conditionMessage(e), call. = FALSE)
    invisible(NULL)
  })
}

auto_open_browser_viewer <- FALSE
# Plot output mode:
# - "rstudio": use the normal graphics device and show plots in RStudio
# - "browser": start httpgd for the VS Code/browser plot viewer
# - "both": run the exact same plotting logic first for RStudio, then
#           start httpgd and run the same plotting logic again for browser
plot_output_mode <- plot_cfg$output_mode
if (is.null(plot_output_mode) || !nzchar(plot_output_mode)) {
  plot_output_mode <- "rstudio"
}
plot_output_mode <- tolower(plot_output_mode)
if (!plot_output_mode %in% c("rstudio", "browser", "both")) {
  warning(sprintf("Unknown plot.output_mode '%s'; falling back to 'rstudio'.", plot_output_mode))
  plot_output_mode <- "rstudio"
}

# determine run modes sequence: 'both' -> c('rstudio','browser')
run_modes <- if (plot_output_mode == "both") c("rstudio", "browser") else c(plot_output_mode)

# httpgd-related options will be set if we will run browser mode at any point
auto_open_browser_viewer <- FALSE
if (!is.null(plot_cfg$auto_open_browser_viewer)) {
  auto_open_browser_viewer <- isTRUE(plot_cfg$auto_open_browser_viewer)
}
if ("browser" %in% run_modes) {
  if (!requireNamespace("httpgd", quietly = TRUE)) {
    install.packages("httpgd")
  }
}

# placeholder for browser viewer URL (populated when httpgd is started)
browser_viewer_url <- NULL

verbose <- isTRUE(config$verbose)
log_verbose <- function(...) {
  if (isTRUE(verbose)) cat(..., sep = "")
}

# ensure httpgd-based plotting is off by default so the first 'rstudio' pass
# renders to the RStudio device even if previous runs enabled httpgd
options(r.plot.useHttpgd = FALSE, vsc.plot.useHttpgd = FALSE, vsc.httpgd = FALSE)

# command-line arguments support (dry run, --verbose, --filter)
#
# Optional flag: --dry-run  (suppress plots, useful for automated runs)
# Optional flag: --verbose  (enable extended debug output)
#
# Optional filter syntax (via --filter="...") allows restricting the
# dataset.  The string is semicolon-separated and can include at most one
# date selector plus zero or more sensor/flag clauses or arbitrary logical
# expressions on numeric columns (e.g. "SleepScore>80" or "18<temp<22").
# Date selectors recognize:
#   YYYY            entire year (e.g. 2025)
#   qN.YYYY         quarter (e.g. q1.2025)
#   MM.YYYY or YYYY.MM  month (e.g. 01.2025 or 2025.01)
#   DD.MM.YYYY      a single date or, with a comma, a range
#                  (e.g. 02.02.2026,04.03.2026)
#
# Sensor or tag clauses look like "Sensors=Name" or "Tags=A|B".
# Multiple values may be separated by '|' (OR) or ',' (AND for tags;
# sensors are treated as OR).  Sensor names are resolved against
# configuration keys/nicknames.  The CLI filter is merged with
# config$analysis_filter (command-line values override config).
#
# Examples:
#   --filter="2025"                        # whole year 2025
#   --filter="q1.2025;Tags=Hochlitten"    # first quarter with tag
#   --filter="01.2025;Sensors=Wohnwagen"   # January with specific sensor
#   --filter="2025.02,2025.04"             # February through April 2025
#   --filter="02.02.2026,04.03.2026"       # explicit date range
#   --filter="temp>18"                     # average temp over 18°C
#   --filter="SleepScore>80"               # biomarker filter
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
    Sys.unsetenv("_STF_ARGS_")
    log_verbose("Interactive override: args=", paste(args, collapse=" "), "\n")
  }
}

# command-line verbosity switch
if ("--verbose" %in% args) {
  verbose <- TRUE
  args <- args[args != "--verbose"]
}

# support a help message
if ("--help" %in% args || "-h" %in% args) {
  cat("Usage: Rscript SleepTempFinder.R [--dry-run] [--verbose] [--filter='...']\n",
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
  # month e.g. 01.2025, 1.2025, 2025.01, or 2025.1
  if (grepl("^(\\d{1,2}\\.\\d{4}|\\d{4}\\.\\d{1,2})(,\\d{1,2}\\.\\d{4}|,\\d{4}\\.\\d{1,2})?$", tok)) {
    parts <- strsplit(tok, ",")[[1]]
    ranges <- lapply(parts, function(part) {
      subparts <- strsplit(part, "\\.")[[1]]
      if (nchar(subparts[1]) == 4) {
        yr <- as.integer(subparts[1]); m <- as.integer(subparts[2])
      } else {
        m <- as.integer(subparts[1]); yr <- as.integer(subparts[2])
      }
      start <- as.Date(sprintf("%04d-%02d-01", yr, m))
      end <- as.Date(sprintf("%04d-%02d-%02d", yr, m,
                               lubridate::days_in_month(start)))
      list(start = start, end = end)
    })
    if (length(ranges) == 1) {
      return(list(start = ranges[[1]]$start, end = ranges[[1]]$end))
    }
    if (length(ranges) == 2) {
      return(list(start = min(ranges[[1]]$start, ranges[[2]]$start), end = max(ranges[[1]]$end, ranges[[2]]$end)))
    }
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
#   enabled (bool), sensor_include, tags_include, tags_mode, tags_ast,
#   date_start, date_end (Date or NULL)
# 
# NEW: Supports TagsExpr= for complex boolean expressions with &, |, !, *
# Example: TagsExpr=(Urlaub, Wohnmobil) & !HomeOffice
parse_filter_string <- function(arg) {
  # expanded filter grammar now supports arbitrary logical expressions
  # referring to columns present in the final dataset.  Unquoted expressions
  # (e.g. "temp>18" or "SleepScore>80") are stored and later evaluated
  # by `apply_analysis_subset_filter()` after sensor/tags filtering.
  cfg <- list(enabled = TRUE,
              sensor_include = character(0),
              tags_include = character(0),
              tags_mode = "any",
              tags_ast = NULL,
              flags_include = character(0),
              flags_mode = "any",
              flags_ast = NULL,
              date_start = NULL,
              date_end = NULL,
              expr = character(0))
  tokens <- unlist(strsplit(arg, ";"))
  for (tok in tokens) {
    tok <- trimws(tok)
    if (tok == "") next
    if (grepl("^(?i)(flagsexpr|tagsexpr)=", tok, perl = TRUE)) {
      # NEW: Complex boolean expression for tags/flags
      body <- sub("^(?i)(flagsexpr|tagsexpr)=", "", tok, perl = TRUE)
      tryCatch({
        cfg$tags_ast <- parse_flag_expression(body)
        cfg$flags_ast <- cfg$tags_ast
      }, error = function(e) {
        warning(sprintf("Failed to parse TagsExpr/FlagsExpr: %s\nError: %s", body, e$message))
      })
    } else if (grepl("^(?i)(flags|tags)=", tok, perl = TRUE)) {
      body <- sub("^(?i)(flags|tags)=", "", tok, perl = TRUE)
      if (grepl("\\|", body)) {
        cfg$tags_mode <- "any"
        cfg$flags_mode <- "any"
        cfg$tags_include <- split_list_token(body)
        cfg$flags_include <- cfg$tags_include
      } else if (grepl(",", body)) {
        cfg$tags_mode <- "all"
        cfg$flags_mode <- "all"
        cfg$tags_include <- split_list_token(body)
        cfg$flags_include <- cfg$tags_include
      } else {
        cfg$tags_include <- trimws(body)
        cfg$flags_include <- cfg$tags_include
      }
    } else if (grepl("^(?i)sensors=", tok, perl = TRUE)) {
      body <- sub("^(?i)sensors=", "", tok, perl = TRUE)
      cfg$sensor_include <- split_list_token(body)
    } else {
      # if it parses as a date range, treat accordingly, otherwise
      # consider the token a logical expression for later evaluation.
      dr <- parse_date_token(tok)
      if (!is.null(dr)) {
        cfg$date_start <- dr$start
        cfg$date_end <- dr$end
      } else {
        # treat as a metric/biomarker filter expression
        cfg$expr <- c(cfg$expr, tok)
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
  log_verbose("CLI filter parsed: ", paste(capture.output(str(cli_filter)), collapse = "\n"), "\n")
  # merge into analysis_filter config so downstream code can apply it
  if (is.null(config$analysis_filter)) config$analysis_filter <- list(enabled = TRUE)
  config$analysis_filter$enabled <- TRUE
  # sensor_include will be resolved to canonical IDs later once the
  # helper function is defined (see below)
  if (length(cli_filter$sensor_include) > 0) {
    config$analysis_filter$sensor_include <- cli_filter$sensor_include
  }
  if (length(cli_filter$tags_include) > 0) {
    config$analysis_filter$tags_include <- cli_filter$tags_include
    config$analysis_filter$tags_mode <- cli_filter$tags_mode
  }
  if (length(cli_filter$flags_include) > 0 && length(cli_filter$tags_include) == 0) {
    config$analysis_filter$tags_include <- cli_filter$flags_include
    config$analysis_filter$tags_mode <- cli_filter$flags_mode
  }
  if (!is.null(cli_filter$tags_ast)) {
    config$analysis_filter$tags_ast <- cli_filter$tags_ast
  } else if (!is.null(cli_filter$flags_ast)) {
    config$analysis_filter$tags_ast <- cli_filter$flags_ast
  }
  if (length(cli_filter$expr) > 0) {
    # expressions override any existing config expressions
    config$analysis_filter$expr <- cli_filter$expr
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

# strip trailing numeric suffixes in parentheses, normalize separators,
# and collapse whitespace so similarly named sensor files can match.
canonical_basename <- function(path) {
  b <- basename(path)
  # remove parenthetical digits before extension (e.g. Foo (1).csv -> Foo.csv)
  b <- sub("\\s*\\(\\d+\\)(?=\\.[^.]+$)", "", b, perl = TRUE)
  # normalize underscores and multiple whitespace to single spaces
  b <- gsub("[_\\s]+", " ", b)
  b <- trimws(b)
  b <- tolower(b)
  b
}

is_single_day_sleep_csv <- function(path) {
  lines <- tryCatch(readLines(path, n = 20, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (length(lines) == 0) return(FALSE)
  lines <- trimws(lines)
  if (!any(str_detect(lines, regex("^Sleep Score", ignore_case = TRUE)))) return(FALSE)
  if (any(str_detect(lines, regex("^Datum\\s*,", ignore_case = TRUE)))) return(TRUE)
  if (any(str_detect(lines, regex("^Schlafdauer\\s*,", ignore_case = TRUE)))) return(TRUE)
  FALSE
}

# helper used when config lists explicit files that might be renamed copies
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
resolve_alternate_column <- function(mapping, hdr, primary_key, alt_key = NULL) {
  if (!is.null(mapping[[primary_key]]) && mapping[[primary_key]] %in% hdr) {
    return(mapping[[primary_key]])
  }
  if (!is.null(alt_key) && !is.null(mapping[[alt_key]])) {
    alt_vals <- unlist(mapping[[alt_key]])
    alt_match <- alt_vals[alt_vals %in% hdr]
    if (length(alt_match) >= 1L) return(alt_match[[1]])
  }
  NULL
}

# classify a CSV by header row using configured column mappings
is_sleep_csv <- function(path, mapping) {
  if (is_single_day_sleep_csv(path)) return(TRUE)

  hdr <- tryCatch(names(read.csv(path, nrows = 1, stringsAsFactors = FALSE, check.names = FALSE)),
                  error = function(e) character(0))
  # primary check: configured date column or alternate date column
  if (!is.null(resolve_alternate_column(mapping, hdr, "garmin_date", "garmin_date_alt"))) return(TRUE)
  # fallback: presence of both bedtime and waketime columns
  if(mapping$garmin_bedtime %in% hdr && mapping$garmin_waketime %in% hdr) return(TRUE)
  FALSE
}

# helper to inspect a sensor CSV and return the matching sensor_files entry (or NULL)
detect_sensor_config <- function(path) {
  base <- canonical_basename(path)
  # explicit paths first
  for (id in names(config$sensor_files)) {
    if (base == canonical_basename(config$sensor_files[[id]]$path)) return(config$sensor_files[[id]])
  }
  hdr <- tryCatch(names(suppressMessages(suppressWarnings(read_delim(path, delim = ",", n_max = 1, locale = sensor_locale, show_col_types = FALSE, name_repair = "unique")))),
                  error = function(e) character(0))
  candidates <- names(config$sensor_files)[sapply(config$sensor_files, function(f) {
    all(c(f$col_time, f$col_temp, f$col_hum) %in% hdr)
  })]
  if (length(candidates) == 1L) {
    return(config$sensor_files[[candidates]])
  }
  if (length(candidates) > 1L) {
    cat(sprintf("Config warning: ambiguous sensor match for file '%s' based on headers; candidates: %s. Explicit path required.\n",
                base, paste(candidates, collapse = ", ") ) )
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
  base <- canonical_basename(path)
  for (id in names(config$sensor_files)) {
    if (base == canonical_basename(config$sensor_files[[id]]$path)) return(id)
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

read_garmin_single_day <- function(path, lines = NULL) {
  if (is.null(lines)) {
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  }
  if (length(lines) == 0) return(tibble())
  lines <- gsub("\r", "", lines)
  # fix comma decimals in values such as -0,1° before splitting on comma
  lines <- gsub("([+-]?[0-9]+),(?=[0-9]+°)", "\\1.", lines, perl = TRUE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  lines <- lines[!str_detect(lines, regex("^(Sleep Score 1 Tag|Sleep Score-Faktoren|Daten für Schlafzeitleiste)\\s*,?$", ignore_case = TRUE))]
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
  if (any(duplicated(keys))) {
    keys <- make.unique(keys, sep = "_")
  }
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
    if (old_name %in% names(out)) {
      names(out)[names(out) == old_name] <- rename_map[[old_name]]
    }
  }
  out <- out %>% mutate(across(everything(), as.character))
  out
}

read_garmin_fixed <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (is_single_day_sleep_csv(path)) {
    single <- read_garmin_single_day(path, lines)
    if (nrow(single) > 0) {
      return(single)
    }
  }
  lines[1] <- gsub("^[^\t[:alnum:][:punct:][:space:]]+", "", lines[1])
  lines <- gsub("([+-]\\d+),(\\d+°)", "\\1.\\2", lines)
  lines <- gsub(",+$", "", lines)
  df <- read.csv(text = lines, sep = ",", header = TRUE, 
                 check.names = FALSE, stringsAsFactors = FALSE,
                 colClasses = "character",
                 na.strings = c(" ", "--", "NA", ""))
  return(as_tibble(df, .name_repair = "unique"))
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

    if (str_detect(txt, "^[-+]?[0-9]+\\s*h(?:\\s*[0-5]?[0-9]\\s*m?)?$") ) {
      parts <- str_match(txt, "^([-+]?[0-9]+)\\s*h(?:\\s*([0-5]?[0-9])\\s*m?)?$")
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
    return(as.numeric(num_str))
  })
  return(res)
}

format_hours_minutes <- function(hours) {
  hours_num <- as.numeric(hours)
  total_minutes <- round(hours_num * 60)
  hh <- total_minutes %/% 60
  mm <- total_minutes %% 60
  out <- sprintf("%02d:%02d", hh, mm)
  out[is.na(hours_num)] <- NA_character_
  out
}

outlier_metrics <- c("Avg_Temp", "Avg_Rel_Hum", "Avg_Abs_Hum", "Sleep_Score", "HRV", "RHR", "Sleep_Duration")

parse_outlier_threshold_value <- function(val, metric = NULL) {
  if (is.null(val) || length(val) == 0 || is.na(val) || val == "") return(NA_real_)
  if (is.numeric(val)) return(as.numeric(val))
  txt <- str_trim(as.character(val))
  txt <- str_replace_all(txt, ",", ".")
  if (txt == "") return(NA_real_)

  if (str_detect(txt, "^[-+]?[0-9]+:[0-5][0-9]$")) {
    parts <- str_split(txt, ":", simplify = TRUE)
    return(as.numeric(parts[1]) + as.numeric(parts[2]) / 60)
  }

  if (str_detect(txt, "^[-+]?[0-9]+\\s*h(?:\\s*[0-5]?[0-9]\\s*m?)?$") ) {
    parts <- str_match(txt, "^([-+]?[0-9]+)\\s*h(?:\\s*([0-5]?[0-9])\\s*m?)?$")
    hrs <- as.numeric(parts[1,2])
    mins <- as.numeric(parts[1,3])
    if (is.na(mins)) mins <- 0
    return(hrs + mins / 60)
  }

  num_str <- str_extract(txt, "[-+]?[0-9]*\\.?[0-9]+")
  as.numeric(num_str)
}

is_outlier_value <- function(value, min_val, max_val) {
  if (is.na(value)) return(FALSE)
  if (!is.na(min_val) && value < min_val) return(TRUE)
  if (!is.na(max_val) && value > max_val) return(TRUE)
  FALSE
}

normalize_outlier_filter <- function(cfg) {
  if (is.null(cfg)) return(list(mode = "false"))
  if (!is.null(cfg$normalized) && isTRUE(cfg$normalized)) return(cfg)
  mode <- cfg$mode %||% cfg$enabled %||% "false"
  if (is.logical(mode)) {
    if (!isTRUE(mode)) return(list(mode = "false"))
    mode <- "manual"
  }
  mode <- tolower(as.character(mode))
  if (mode %in% c("false", "off", "none", "no")) return(list(mode = "false"))
  if (!mode %in% c("manual", "value_interval")) {
    warning(sprintf("Unknown outlier_filter.mode '%s'; disabling outlier filtering.", mode))
    return(list(mode = "false"))
  }

  columns <- trim_vector(cfg$columns %||% character(0))
  if (length(columns) == 0) {
    columns <- outlier_metrics
  }

  if (mode == "manual") {
    manual_cfg <- cfg$manual$metrics %||% cfg$manual %||% list()
    bounds <- list()
    for (metric in names(manual_cfg)) {
      metric_cfg <- manual_cfg[[metric]]
      if (!is.list(metric_cfg)) next
      min_val <- parse_outlier_threshold_value(metric_cfg$min, metric)
      max_val <- parse_outlier_threshold_value(metric_cfg$max, metric)
      if (!is.na(min_val) || !is.na(max_val)) {
        bounds[[metric]] <- list(min = min_val, max = max_val)
      }
    }
    if (length(columns) == 0) columns <- names(bounds)
    return(list(mode = "manual", columns = columns, manual_bounds = bounds, normalized = TRUE))
  }

  # value_interval
  vi_cfg <- cfg$value_interval %||% cfg
  interval <- as.numeric(vi_cfg$interval %||% 0.9)
  if (is.na(interval) || interval <= 0 || interval >= 1) interval <- 0.9
  symmetric <- if (is.null(vi_cfg$symmetric)) TRUE else isTRUE(vi_cfg$symmetric)
  use_individual <- if (is.null(vi_cfg$use_individual)) FALSE else isTRUE(vi_cfg$use_individual)
  global_min <- parse_outlier_threshold_value(vi_cfg$min)
  global_max <- parse_outlier_threshold_value(vi_cfg$max)
  metric_overrides <- vi_cfg$metrics %||% list()

  if (length(columns) == 0) {
    if (use_individual && length(metric_overrides) > 0) {
      columns <- trim_vector(names(metric_overrides))
    } else {
      columns <- outlier_metrics
    }
  }

  list(mode = "value_interval",
       columns = columns,
       symmetric = symmetric,
       interval = interval,
       use_individual = use_individual,
       global_min = global_min,
       global_max = global_max,
       metric_overrides = metric_overrides,
       normalized = TRUE)
}

resolve_value_interval_probs <- function(metric_cfg, global_cfg) {
  metric_cfg <- metric_cfg %||% list()
  lower <- metric_cfg$min %||% metric_cfg$lower
  upper <- metric_cfg$max %||% metric_cfg$upper
  sym_width <- if (!is.null(metric_cfg$sym)) as.numeric(metric_cfg$sym) else NA_real_
  interval <- if (!is.null(metric_cfg$interval)) as.numeric(metric_cfg$interval) else global_cfg$interval
  if (length(interval) == 0 || is.na(interval) || interval <= 0 || interval >= 1) interval <- global_cfg$interval

  if (isTRUE(global_cfg$symmetric)) {
    if (!is.na(sym_width) && sym_width > 0 && sym_width < 1) {
      lower <- (1 - sym_width) / 2
      upper <- 1 - lower
    } else {
      lower <- (1 - interval) / 2
      upper <- 1 - lower
    }
  } else {
    if (!is.na(sym_width) && sym_width > 0 && sym_width < 1 && (is.null(lower) || is.null(upper))) {
      lower <- (1 - sym_width) / 2
      upper <- 1 - lower
    }
    if (is.null(lower) && !is.null(upper)) lower <- 1 - as.numeric(upper)
    if (is.null(upper) && !is.null(lower)) upper <- 1 - as.numeric(lower)
    lower <- as.numeric(lower)
    upper <- as.numeric(upper)
    if (length(lower) == 0) lower <- NA_real_
    if (length(upper) == 0) upper <- NA_real_

    if (is.na(lower) || is.na(upper) || lower >= upper) {
      if (!is.na(global_cfg$global_min) && !is.na(global_cfg$global_max)) {
        lower <- global_cfg$global_min
        upper <- global_cfg$global_max
      } else {
        lower <- global_cfg$lower
        upper <- global_cfg$upper
      }
    }
  }

  lower <- max(0, min(1, lower))
  upper <- max(0, min(1, upper))
  list(lower = lower, upper = upper)
}

compute_outlier_bounds <- function(df, outlier_cfg, metrics) {
  cfg <- normalize_outlier_filter(outlier_cfg)
  if (cfg$mode == "false" || nrow(df) == 0) return(list())

  columns <- intersect(cfg$columns, names(df))
  if (length(columns) == 0) columns <- intersect(metrics, names(df))
  if (length(columns) == 0) return(list())

  bounds <- list()
  if (cfg$mode == "manual") {
    for (metric in columns) {
      if (!is.null(cfg$manual_bounds[[metric]])) {
        bounds[[metric]] <- cfg$manual_bounds[[metric]]
      }
    }
    return(bounds)
  }

  global_cfg <- list(lower = cfg$global_min,
                     upper = cfg$global_max,
                     symmetric = cfg$symmetric,
                     interval = cfg$interval,
                     global_min = cfg$global_min,
                     global_max = cfg$global_max,
                     use_individual = cfg$use_individual)
  for (metric in columns) {
    if (!metric %in% names(df) || !is.numeric(df[[metric]])) next
    metric_cfg <- if (isTRUE(cfg$use_individual)) cfg$metric_overrides[[metric]] %||% list() else list()
    probs <- resolve_value_interval_probs(metric_cfg, global_cfg)
    quantiles <- quantile(df[[metric]], probs = c(probs$lower, probs$upper), na.rm = TRUE, names = FALSE, type = 7)
    bounds[[metric]] <- list(min = as.numeric(quantiles[1]), max = as.numeric(quantiles[2]))
  }
  bounds
}

get_outlier_columns_from_row <- function(row, bounds) {
  outlier_columns <- character(0)
  for (metric in names(bounds)) {
    if (!metric %in% names(row)) next
    value <- row[[metric]]
    if (is_outlier_value(value, bounds[[metric]]$min, bounds[[metric]]$max)) {
      outlier_columns <- c(outlier_columns, metric)
    }
  }
  outlier_columns
}

apply_outlier_filter <- function(df, outlier_cfg) {
  cfg <- normalize_outlier_filter(outlier_cfg)
  if (cfg$mode == "false" || nrow(df) == 0) {
    out <- df %>% mutate(Outlier_Columns = list(character(0)), Outlier_Reason = NA_character_)
    return(list(data = out, excluded = as.Date(character(0)), bounds = list()))
  }

  bounds <- compute_outlier_bounds(df, cfg, outlier_metrics)
  if (length(bounds) == 0) {
    out <- df %>% mutate(Outlier_Columns = list(character(0)), Outlier_Reason = NA_character_)
    return(list(data = out, excluded = as.Date(character(0)), bounds = bounds))
  }

  out <- df %>%
    mutate(
      Outlier_Columns = pmap(select(., all_of(names(bounds))), function(...) {
        get_outlier_columns_from_row(set_names(list(...), names(bounds)), bounds)
      }),
      Outlier_Reason = map_chr(Outlier_Columns, function(cols) {
        if (length(cols) == 0) return(NA_character_)
        paste(cols, collapse = ", ")
      })
    )

  excluded <- out %>% filter(map_int(Outlier_Columns, length) > 0) %>% pull(Date)
  filtered <- out %>% filter(map_int(Outlier_Columns, length) == 0)
  list(data = filtered,
       all = out,
       excluded = unique(as.Date(excluded)),
       bounds = bounds)
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
  # `flag`/`flags=` or `tag`/`tags=` assignment.  Flags/Tags may be declared
  # before or after the sensor assignment (separated by a semicolon or comma)
  # and multiple values can be provided as a comma‑separated list. Examples:
  #   parse_sensor_flags("Sensor=foo; Flags=bar")
  #   parse_sensor_flags("sensor=foo; tags=a,b,c")
  #   parse_sensor_flags("Tags=quiet,urlaub; sensor=LivingRoom")
  text_value <- text_value %||% ""

  # there may be more than one flags/tags declaration (e.g. summary and
  # description both include them) so use match_all and combine results.
  flags_raw_vec <- str_match_all(text_value,
                                 regex("(?:flags?|tags?)\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))[[1]][,2]
  # to prevent sensor names that contain commas from being truncated we
  # strip any flag/tag assignments from the text before looking for `sensor=`.
  text_no_flags <- str_replace_all(text_value,
                                   regex("(?:flags?|tags?)\\s*=\\s*[^;\\n]+", ignore_case = TRUE), "")

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

  cat(sprintf("Calendar loaded: %d days parsed.\n\n", nrow(calendar_daily)))
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

# helper: normalize simple filter expressions before evaluation
#  - expand chained comparisons (e.g. `18<temp<22` -> `(18<temp & temp<22)`)
#  - map common shorthand names to actual column names
normalize_expr <- function(expr_str, df) {
  # mapping of simple names to actual column names present in df
  syn <- list(
    temp = "Avg_Temp",
    room_temp = "Avg_Temp",
    avg_temp = "Avg_Temp",
    sleepscore = "Sleep_Score",
    sleep_score = "Sleep_Score",
    hrv = "HRV",
    rhr = "RHR",
    relhum = "Avg_Rel_Hum",
    absum = "Avg_Abs_Hum", # typo safe
    abshum = "Avg_Abs_Hum",
    rel_hum = "Avg_Rel_Hum",
    abs_hum = "Avg_Abs_Hum"
  )
  # replace synonyms (word boundaries, case-insensitive)
  for (alias in names(syn)) {
    pat <- paste0("\\b", alias, "\\b")
    expr_str <- gsub(pat, syn[[alias]], expr_str, ignore.case = TRUE, perl = TRUE)
  }

  # chained comparison expansion
  expand_chains <- function(e) {
    repeat {
      m <- regexpr("(\\b[[:alnum:]_.]+\\b)\\s*([<>])\\s*(\\b[[:alnum:]_.]+\\b)\\s*([<>])\\s*(\\b[[:alnum:]_.]+\\b)",
                   e, perl = TRUE)
      if (m == -1) break
      groups <- regmatches(e, regexec("(\\b[[:alnum:]_.]+\\b)\\s*([<>])\\s*(\\b[[:alnum:]_.]+\\b)\\s*([<>])\\s*(\\b[[:alnum:]_.]+\\b)",
                                 e, perl = TRUE))[[1]]
      a <- groups[2]; op1 <- groups[3]; b <- groups[4]; op2 <- groups[5]; c <- groups[6]
      replacement <- paste0("(", a, op1, b, " & ", b, op2, c, ")")
      e <- sub("(\\b[[:alnum:]_.]+\\b)\\s*([<>])\\s*(\\b[[:alnum:]_.]+\\b)\\s*([<>])\\s*(\\b[[:alnum:]_.]+\\b)",
               replacement, e, perl = TRUE)
    }
    e
  }

  expr_str <- expand_chains(expr_str)
  expr_str
}

apply_analysis_subset_filter <- function(df, filter_cfg) {
  if (is.null(filter_cfg) || !isTRUE(filter_cfg$enabled) || nrow(df) == 0) return(df)

  sensor_include <- trim_vector(unlist(filter_cfg$sensor_include %||% character(0)))
  tags_include <- trim_vector(unlist(filter_cfg$tags_include %||% filter_cfg$flags_include %||% character(0)))
  tags_mode <- tolower(filter_cfg$tags_mode %||% filter_cfg$flags_mode %||% "any")
  if (!tags_mode %in% c("any", "all")) tags_mode <- "any"
  tags_ast <- filter_cfg$tags_ast %||% filter_cfg$flags_ast

  out <- df
  if (length(sensor_include) > 0 && "Sensor" %in% names(out)) {
    out <- out %>% filter(!is.na(Sensor), Sensor %in% sensor_include)
  }

  # NEW: If tags_ast is provided (complex boolean expression), use it
  if (!is.null(tags_ast) && "Flags" %in% names(out)) {
    out <- out %>%
      mutate(.flags_vec = map(Flags, split_flags)) %>%
      filter(map_lgl(.flags_vec, function(row_flags) {
        evaluate_flag_ast(tags_ast, row_flags)
      })) %>%
      select(-.flags_vec)
  } else if (length(tags_include) > 0 && "Flags" %in% names(out)) {
    # OLD: Simple tags filter (backward compatibility)
    out <- out %>%
      mutate(.flags_vec = map(Flags, split_flags)) %>%
      filter(map_lgl(.flags_vec, function(row_flags) {
        if (tags_mode == "all") all(tags_include %in% row_flags) else any(tags_include %in% row_flags)
      })) %>%
      select(-.flags_vec)
  }

  # numeric/column expressions
  exprs <- trim_vector(unlist(filter_cfg$expr %||% character(0)))
  if (length(exprs) > 0) {
    # IMPROVED: Before evaluating as column expressions, separate flag comparisons
    flag_exprs <- character(0)
    col_exprs <- character(0)
    
    for (expr in exprs) {
      # Check if this looks like a tags/flags comparison: tags or flags op value
      if (grepl("(?:flags|tags)\\s*(==|!=|%in%)", expr, perl = TRUE)) {
        log_verbose("[apply_analysis_subset_filter] Detected tag comparison in expr: ", expr, "\n")
        flag_exprs <- c(flag_exprs, expr)
      } else {
        col_exprs <- c(col_exprs, expr)
      }
    }
    
    # Process flag expressions by converting them to flags_ast and evaluating
    if (length(flag_exprs) > 0) {
      log_verbose("[apply_analysis_subset_filter] Processing ", length(flag_exprs), " flag expressions\n")
      
      for (expr in flag_exprs) {
        expr <- trimws(expr)
        
        # Use better regex patterns with space handling
        converted_expr <- NULL
        
        # Pattern 1: flags != 'value' or tags != 'value'
        if (grepl("(?:flags|tags)\\s*!=\\s*", expr, perl = TRUE)) {
          m <- regexec("(?:flags|tags)\\s*!=\\s*['\"]([^'\"]+)['\"]", expr, perl = TRUE)
          if (m[[1]][1] > 0) {
            value <- regmatches(expr, m)[[1]][2]
            if (!is.na(value)) {
              converted_expr <- paste0("!", value)
            }
          }
        }
        # Pattern 2: flags == 'value' or tags == 'value'
        else if (grepl("(?:flags|tags)\\s*==\\s*", expr, perl = TRUE)) {
          m <- regexec("(?:flags|tags)\\s*==\\s*['\"]([^'\"]+)['\"]", expr, perl = TRUE)
          if (m[[1]][1] > 0) {
            value <- regmatches(expr, m)[[1]][2]
            if (!is.na(value)) {
              converted_expr <- value
            }
          }
        }
        # Pattern 3: flags %in% c(...) or tags %in% c(...)
        else if (grepl("(?:flags|tags)\\s*%in%\\s*c\\(", expr, perl = TRUE)) {
          m <- regexec("(?:flags|tags)\\s*%in%\\s*c\\((.+?)\\)", expr, perl = TRUE)
          if (m[[1]][1] > 0) {
            values_str <- regmatches(expr, m)[[1]][2]
            if (!is.na(values_str)) {
              values <- unlist(strsplit(values_str, ",", fixed = TRUE))
              values <- trimws(values)
              values <- gsub("^['\"]|['\"]$", "", values)
              values <- values[nzchar(values)]
              if (length(values) > 0) {
                converted_expr <- paste(values, collapse = ",")
              }
            }
          }
        }
        
        # If we successfully converted, parse and evaluate as flag AST
        if (!is.null(converted_expr)) {
          log_verbose("[apply_analysis_subset_filter] Converted: ", expr, " -> ", converted_expr, "\n")
          tryCatch({
            flag_ast <- parse_flag_expression(converted_expr)
            out <- out %>%
              mutate(.flags_vec = map(Flags, split_flags)) %>%
              filter(map_lgl(.flags_vec, function(row_flags) {
                evaluate_flag_ast(flag_ast, row_flags)
              })) %>%
              select(-.flags_vec)
          }, error = function(e) {
            cat("[ERROR] Failed to parse flag expression:", converted_expr, "\n")
            cat("[ERROR]", e$message, "\n")
          })
        }
      }
    }
    
    # Process remaining column expressions
    if (length(col_exprs) > 0) {
      combined <- paste(sapply(col_exprs, normalize_expr, df = out), collapse = " & ")
      out <- out %>% filter(!!rlang::parse_expr(combined))
    }
  }

  out
}

# determine which files under data_directory will be used
classification <- local({
  all_data_files <- list_csv_files(config$data_directory, recursive = isTRUE(config$scan_recursive))
  cat(sprintf("Found %d CSV file(s) under %s (recursive=%s)\n", length(all_data_files), config$data_directory, isTRUE(config$scan_recursive)))
  cat("\n")

  # classify discovered files
  sleep_candidates <- all_data_files[sapply(all_data_files, is_sleep_csv, mapping = config$column_names)]
  sensor_candidates <- all_data_files[sapply(all_data_files, is_sensor_csv, sensor_files = config$sensor_files)]
  unclassified_files <- setdiff(all_data_files, c(sleep_candidates, sensor_candidates))
  cat(sprintf("Sleep candidates: %d\n", length(sleep_candidates)))
  if (isTRUE(verbose) && length(sleep_candidates) > 0) cat(paste0("    ", sleep_candidates, collapse="\n"), "\n")
  # show canonical names
  if (isTRUE(verbose) && length(sleep_candidates) > 0) {
    cat("    canonical: ", paste(unique(canonical_basename(sleep_candidates)), collapse=", "), "\n")
  }
  cat(sprintf("Sensor candidates: %d\n", length(sensor_candidates)))
  if (isTRUE(verbose) && length(sensor_candidates) > 0) cat(paste0("    ", sensor_candidates, collapse="\n"), "\n")
  if (isTRUE(verbose) && length(sensor_candidates) > 0) {
    cat("    canonical: ", paste(unique(canonical_basename(sensor_candidates)), collapse=", "), "\n")
  }
  if (length(unclassified_files) > 0) {
    if (isTRUE(verbose)) {
      cat("Unclassified files (neither sleep nor sensor detected):\n", paste(unclassified_files, collapse = "\n"), "\n")
    } else {
      cat(sprintf("Unclassified files detected: %d (use --verbose for details)\n", length(unclassified_files)))
      cat("\n")
    }
  }
  cat("\n")

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
    if ("Datum" %in% hdr) {
      date_col <- "Datum"
      cat(sprintf("Warning: using alternate date column '%s' for file %s\n", date_col, f))
      cat("\n")
    } else {
      alt <- hdr[str_detect(hdr, regex("^(Sleep Score|Datum)", ignore_case = TRUE))]
      if(length(alt) == 1) {
        date_col <- alt[[1]]
        cat(sprintf("Warning: using alternate date column '%s' for file %s\n", date_col, f))
      }
    }
  }
    df %>%
      mutate(Source_File = f,
        Source_Name = canonical_basename(f)) %>%
      mutate(Date = as.Date(!!sym(date_col)),
        bedtime = if (mapping$garmin_bedtime %in% hdr) parse_datetime_safe(!!sym(mapping$garmin_bedtime), type = "garmin_time") else as.POSIXct(NA),
        waketime = if (mapping$garmin_waketime %in% hdr) parse_datetime_safe(!!sym(mapping$garmin_waketime), type = "garmin_time") else as.POSIXct(NA)) %>%
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
      mutate(
        .missing_window = is.na(bedtime) & is.na(waketime),
        bedtime = if_else(.missing_window, as.POSIXct(Date) - hours(12), bedtime),
        waketime = if_else(.missing_window, as.POSIXct(Date) + hours(12), waketime)
      ) %>%
      mutate(bedtime = if_else(bedtime > waketime, bedtime - days(1), bedtime)) %>%
      select(-.bedtime_orig, -.waketime_orig, -.swap_flag, -.missing_window) %>%
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
  hdr <- tryCatch(names(suppressMessages(suppressWarnings(read_delim(path, delim = ",", n_max = 1, locale = sensor_locale, show_col_types = FALSE, name_repair = "unique")))),
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
  suppressMessages(suppressWarnings(read_delim(fp, delim = ",", locale = sensor_locale, show_col_types = FALSE, name_repair = "unique"))) %>%
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
  total_rows <- sum(sensor_stats$N_Rows)
  total_na <- sum(sensor_stats$N_NA_Timestamp)

  cat(sprintf("Sensor files read: %d files, total rows=%d, total NA timestamps=%d\n",
              nrow(sensor_stats), total_rows, total_na))
  if (isTRUE(verbose)) {
    for(i in seq_len(nrow(sensor_stats))) {
      row <- sensor_stats[i,]
      cat(sprintf(" - %s: rows=%d, NA_timestamps=%d, first=%s, last=%s\n",
                  as.character(row$Source_File), as.integer(row$N_Rows), as.integer(row$N_NA_Timestamp),
                  ifelse(is.na(row$First_TS), "NA", format(row$First_TS, "%Y-%m-%d %H:%M")),
                  ifelse(is.na(row$Last_TS), "NA", format(row$Last_TS, "%Y-%m-%d %H:%M"))))
    }
  } else {
    cat("Use --verbose for per-file sensor read details.\n\n")
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

# if a default sensor is configured, use it for dates where the calendar
# did not specify any sensor at all. Do not override entries where the user
# supplied an unrecognized sensor label.
if (!is.na(default_sensor_id)) {
  default_s <- default_sensor_id
  n_na_before <- sum(is.na(calendar_daily$Sensor) & is.na(calendar_daily$Sensor_Raw))
  calendar_daily$Sensor[is.na(calendar_daily$Sensor) & is.na(calendar_daily$Sensor_Raw)] <- default_s
  if (n_na_before > 0) {
    #cat(sprintf("Calendar default sensor applied: '%s' to %d day(s)\n",default_s, n_na_before))
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

# drop rows with missing critical sleep metrics immediately

# nightly mapping: compute per-night sensor averages and join calendar & sensor summaries
# when a calendar entry specifies a sensor, only rows from that
# sensor should contribute to the nightly average; if no calendar sensor
# was assigned, the configured default sensor is used.  To accomplish this we
# join the calendar information before computing the averages and then
# conditionally filter `sensor_raw` inside the rowwise mutate.

calendar_default_sensor <- default_sensor_id

# if multiple source files match the same night, pick the one with the most
# valid sensor rows so that we use only one sensor file per night.
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

temp_mapped <- sleep_complete %>% 
  filter(!is.na(Sleep_Score), !is.na(HRV), !is.na(RHR)) %>%
  # calendar assignment may provide a default sensor value already; also
  # carry the raw label so it can be referenced later when building the
  # review dataframe.
  left_join(calendar_daily %>% select(Date, Sensor, Sensor_Raw, Flags, Flags_List), by = "Date") %>%
  mutate(
    Sensor = ifelse(is.na(Sensor) & is.na(Sensor_Raw) & !is.na(calendar_default_sensor),
                    calendar_default_sensor,
                    Sensor)
  ) %>%
  rowwise() %>% 
  mutate(
    # apply configurable padding when matching sensor rows
    .bed_pad = bedtime - minutes(matching_padding_minutes),
    .wak_pad = waketime + minutes(matching_padding_minutes),
    .idx_used = list({
      idx <- which(sensor_raw$timestamp >= .bed_pad & sensor_raw$timestamp <= .wak_pad)
      idx <- select_best_sensor_by_file(idx, sensor_raw, Sensor)
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
  cat("\n")
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
    Actual_Sensor = map_chr(Sensor_Files, function(files) {
      if (length(files) == 0) return(NA_character_)
      ids <- unique(na.omit(sapply(files, identify_sensor_id)))
      if (length(ids) == 0) return(NA_character_)
      ids[1]
    }),
    # the name we show should reflect the sensor that was actually used.
    # If the calendar assignment exists, it should agree with the actual file.
    Sensor_Names = case_when(
      !is.na(Actual_Sensor) ~ Actual_Sensor,
      !is.na(Sensor) ~ Sensor,
      !is.na(Sensor_Raw) ~ paste0("(raw: ", Sensor_Raw, ")"),
      TRUE ~ map_chr(Sensor_Names, ~ paste(.x, collapse = "; "))
    )
  )
# simple audit: how many distinct files and canonical names contribute
cat(sprintf("Review DF constructed: %d nights, %d unique sleep files (%d canonical), %d unique sensor file paths (%d canonical)\n\n\n", 
            nrow(nightly_review_df),
            n_distinct(nightly_review_df$Sleep_Source),
            n_distinct(nightly_review_df$Sleep_Name),
            n_distinct(unlist(nightly_review_df$Sensor_Files)),
            n_distinct(unlist(nightly_review_df$Sensor_Names))))

mismatch_df <- nightly_review_df %>% filter(!is.na(Sensor) & !is.na(Actual_Sensor) & Sensor != Actual_Sensor)
if (nrow(mismatch_df) > 0) {
  cat(sprintf("Warning: %d nights with calendar sensor != actual sensor used:\n", nrow(mismatch_df)))
  for (i in seq_len(nrow(mismatch_df))) {
    row <- mismatch_df[i, ]
    cat(sprintf("  %s: calendar=%s actual=%s files=%s\n",
                format(row$Date), row$Sensor, row$Actual_Sensor,
                paste(unlist(row$Sensor_Files), collapse = "; ")))
  }
}

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

outlier_result <- apply_outlier_filter(final_data_matched, config$outlier_filter)
excluded_outlier_dates_dates <- outlier_result$excluded
final_data_outlier_meta <- outlier_result$all
final_data_matched <- outlier_result$data

n_before_analysis_filter_post_outlier <- nrow(final_data_matched)
final_data_matched <- apply_analysis_subset_filter(final_data_matched, config$analysis_filter)
n_after_analysis_filter <- nrow(final_data_matched)

if(!is.null(config$analysis_filter) && isTRUE(config$analysis_filter$enabled)) {
  cat(sprintf("Analysis filter enabled: kept %d of %d nights\n", n_after_analysis_filter, n_before_analysis_filter_post_outlier))
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

n_unique_sleep_dates <- n_distinct(sleep_complete$Date)
n_excluded_sleep <- length(excluded_sleep_dates)
n_excluded_sensor <- length(excluded_sensor_dates)
n_excluded_outlier <- length(excluded_outlier_dates_dates)
n_filtered_out <- n_before_analysis_filter - n_after_analysis_filter

cat("\n=== NIGHTLY DATA SUMMARY ===\n")
cat(sprintf("Total unique sleep dates discovered: %d\n", n_unique_sleep_dates))
cat(sprintf("Excluded due to missing sleep data: %d\n", n_excluded_sleep))
cat(sprintf("Excluded due to missing sensor data: %d\n", n_excluded_sensor))
cat(sprintf("Excluded due to outlier filtering: %d\n", n_excluded_outlier))
cat(sprintf("Nights considered for analysis before filters: %d\n", n_before_analysis_filter))
cat(sprintf("Nights kept after filter application: %d\n", n_after_analysis_filter))
if (n_filtered_out > 0) {
  cat(sprintf("Nights removed by date/analysis filters: %d\n", n_filtered_out))
}
cat("\n")

if (n_after_analysis_filter > 0) {
  format_ci_value <- function(x) {
    if (is.na(x)) return(NA_character_)
    s <- format(round(x, 2), nsmall = 2, trim = TRUE)
    sub("\\.?0+$", "", s)
  }

  summarize_metric <- function(df, col) {
    vals <- df[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) {
      out <- tibble(
        metric = col,
        mean = NA_character_,
        median = NA_character_,
        std_dev = NA_character_
      )
      out[[summary_interval_label]] <- NA_character_
      return(out)
    }
    n <- length(vals)
    mean_val <- mean(vals)
    sd_val <- if (n > 1) sd(vals) else NA_real_
    q_bounds <- quantile(vals, c(summary_interval_lower, summary_interval_upper), na.rm = TRUE, type = 7)
    format_val <- if (col == "Sleep_Duration") format_hours_minutes else format_ci_value
    out <- tibble(
      metric = col,
      mean = format_val(mean_val),
      median = format_val(median(vals)),
      std_dev = ifelse(is.na(sd_val), NA_character_, format_val(sd_val))
    )
    out[[summary_interval_label]] <- sprintf("[%s;%s]",
      format_val(q_bounds[[1]]),
      format_val(q_bounds[[2]]))
    out
  }
  stat_cols <- c("Avg_Temp", "Avg_Rel_Hum", "Avg_Abs_Hum", "Sleep_Score", "HRV", "RHR", "Sleep_Duration")
  stats_df <- map_dfr(stat_cols, summarize_metric, df = final_data_matched)
  cat("\nStatistics for used nights:\n\n")
  print(stats_df)
  cat("\n\n")
} else {
  cat("\nNo nights remain after filtering; summary statistics unavailable.\n\n\n\n")
}

# --- Plot helper functions (extracted so the same logic can be called twice) ---
# Define biomarker variables (sleep quality indicators)
bio_vars <- c("Sleep_Score", "HRV", "RHR")

metric_list <- c("Avg_Temp", "Avg_Rel_Hum", "Avg_Abs_Hum", "Sleep_Score", "HRV", "RHR")
# derive labels/colors from configuration, with fallbacks
metric_labels <- unname(sapply(metric_list, function(m) {
  plot_cfg$metric_labels[[m]] %||% m
}))
metric_colors <- unname(sapply(metric_list, function(m) {
  plot_cfg$metric_colors[[m]] %||% "black"
}))

plot_individual_timelines <- function(data_viz, metric_list, metric_colors, metric_labels, dry_run) {
  for(i in seq_along(metric_list)) {
    m <- metric_list[i]
    tryCatch({
      p <- ggplot(data_viz, aes(x = Date, y = .data[[m]])) +
        geom_line(color = metric_colors[i], linewidth = 1, na.rm = TRUE) +
        geom_point(color = metric_colors[i], size = 2, na.rm = TRUE) +
        scale_x_date(date_labels = "%d.%m.%Y", breaks = "2 days", minor_breaks = "1 day", expand = expansion(mult = c(0.01, 0.01))) +
        labs(title = metric_labels[i], x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor.x = element_line(color = "grey90"),
              plot.title = element_text(face = "bold", color = metric_colors[i]), plot.margin = margin(10, 10, 20, 10))
      save_plot_image(p, slugify_plot_name("timeline", sprintf("%02d", i), m), width = 8.5, height = 5.5)
      if(!dry_run) print(p)
    }, error = function(e) {
      warning(sprintf("Failed to create timeline plot for %s: %s\n", m, conditionMessage(e)))
    })
  }
}

plot_scatter_and_matrix <- function(data_matched, env_analysis_vars, metric_list, metric_colors, optima_storage, bio_vars, dry_run) {
  # Individual Scatter Plots
  for(env_name in names(env_analysis_vars)) {
    e_col <- env_analysis_vars[[env_name]]$col
    e_unit <- env_analysis_vars[[env_name]]$unit
    for(m in bio_vars) {
      opt <- optima_storage[[paste0(env_name, "_", m)]]
      tryCatch({
        p <- ggplot(data_matched, aes(x = .data[[e_col]], y = .data[[m]])) +
          geom_point(alpha = 0.5, color = "darkgrey") +
          geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = metric_colors[match(m, metric_list)], linewidth = 1.2) +
          labs(title = paste(m, "vs", env_name), x = paste(env_name, e_unit), y = m) +
          theme_minimal()
        if(!is.null(opt)) {
          p <- p + geom_vline(xintercept = opt, linetype = "dashed") +
            annotate("text", x = opt, y = Inf, label = paste0(round(opt, 1), e_unit), vjust = 2, fontface = "bold")
        }
        save_plot_image(p, slugify_plot_name("scatter", env_name, m), width = 7.5, height = 5.5)
        if(!dry_run) print(p)
      }, error = function(e) {
        warning(sprintf("Failed to create scatter plot for %s vs %s: %s\n", m, env_name, conditionMessage(e)))
      })
    }
  }

  # 3x3 Matrix Dashboard - COLORED & WITH OPTIMA
  matrix_plots <- list()
  for(m in bio_vars) {
    m_color <- metric_colors[match(m, metric_list)]
    for(env_name in names(env_analysis_vars)) {
      e_col <- env_analysis_vars[[env_name]]$col
      e_unit <- env_analysis_vars[[env_name]]$unit
      opt <- optima_storage[[paste0(env_name, "_", m)]]

      tryCatch({
        p_mat <- ggplot(data_matched, aes(x = .data[[e_col]], y = .data[[m]])) +
          geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = m_color, fill = m_color, alpha = 0.1, linewidth = 1) +
          theme_minimal(base_size = 8) + 
          labs(x = e_unit, y = m, title = paste(m, "x", env_name)) +
          theme(plot.title = element_text(size = 7, face = "bold"))

        if(!is.null(opt)) {
          p_mat <- p_mat + geom_vline(xintercept = opt, linetype = "dashed", color = "black", alpha = 0.6)
        }

        # Only add if plot object is valid
        if(!is.null(p_mat) && inherits(p_mat, "ggplot")) {
          matrix_plots[[length(matrix_plots) + 1]] <- p_mat
        }
      }, error = function(e) {
        warning(sprintf("Failed to create matrix plot for %s x %s: %s\n", m, env_name, conditionMessage(e)))
      })
    }
  }
  
  # Only render matrix dashboard if we have valid plots
  if(length(matrix_plots) > 0) {
    tryCatch({
      matrix_dashboard <- gridExtra::arrangeGrob(grobs = matrix_plots, ncol = 3, top = textGrob("Environmental Impact Matrix (with Optima)", gp = gpar(fontsize = 12, font = 2)))
      save_plot_image(matrix_dashboard, slugify_plot_name("impact", "matrix"), width = 12, height = 9)
      if(!dry_run) {
        tryCatch({
          grid::grid.newpage()
          grid::grid.draw(matrix_dashboard)
        }, error = function(e) {
          warning(sprintf("Failed to render matrix dashboard to screen: %s\n", conditionMessage(e)))
          # Attempt to save without rendering
          cat("Matrix dashboard saved to file but could not be rendered on screen.\n")
        })
      }
    }, error = function(e) {
      warning(sprintf("Failed to arrange matrix plots: %s\n", conditionMessage(e)))
    })
  } else {
    warning("No valid matrix plots were created; skipping matrix dashboard.\n")
  }
}

dashboard_df <- final_data_viz %>%
  filter(!is.na(Date)) %>%
  select(-any_of(c("Outlier_Columns", "Outlier_Reason"))) %>%
  # Attach any outlier metadata for the day, if available.
  left_join(select(final_data_outlier_meta, Date, Outlier_Reason), by = "Date") %>%
  # Sensor_Files is a list-column; convert to semicolon-separated string for
  # ease of display.  Use Sensor_Names if you prefer canonical names instead.
  mutate(
    Sensor_File = map_chr(Sensor_Files, ~ paste(.x, collapse = "; ")),
    Actual_Sensor = map_chr(Sensor_Files, function(files) {
      if (is.null(files) || length(files) == 0) return(NA_character_)
      ids <- unique(na.omit(sapply(files, identify_sensor_id)))
      if (length(ids) == 0) return(NA_character_)
      ids[1]
    }),
    Sensor = ifelse(!is.na(Actual_Sensor), Actual_Sensor, Sensor)
  ) %>%
  mutate(Sleep_Duration = format_hours_minutes(Sleep_Duration)) %>%
  select(Date, Sensor, Flags, Sensor_File, Avg_Temp, Avg_Rel_Hum, Avg_Abs_Hum, Sleep_Score, HRV, RHR, Sleep_Duration, Outlier_Reason) %>%
  mutate(Date_Str = format(Date, "%d.%m.%Y"))




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
  for(m in bio_vars) {
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


for (mode in run_modes) {
  if (mode == "browser") {
    options(r.plot.useHttpgd = TRUE, vsc.plot.useHttpgd = TRUE, vsc.httpgd = TRUE)
    tryCatch({ invisible(capture.output(httpgd::hgd())) }, error = function(e) warning("Failed to start httpgd: ", conditionMessage(e), "\n"))
    browser_viewer_url <- tryCatch(httpgd::hgd_url(which = grDevices::dev.cur()), error = function(e) NULL)
    if (auto_open_browser_viewer && !is.null(browser_viewer_url)) {
      tryCatch({ utils::browseURL(browser_viewer_url) }, error = function(e) warning("Failed to open browser viewer: ", conditionMessage(e), "\n"))
    }
  } else {
    options(r.plot.useHttpgd = FALSE, vsc.plot.useHttpgd = FALSE, vsc.httpgd = FALSE)
  }
  
  if(!dry_run) plot_individual_timelines(final_data_viz, metric_list, metric_colors, metric_labels, dry_run)
  if(!dry_run) plot_scatter_and_matrix(final_data_matched, env_analysis_vars, metric_list, metric_colors, optima_storage, bio_vars, dry_run)

  if (mode == "browser" && !is.null(browser_viewer_url)) {
    cat("\nBrowser viewer URL:\n")
    cat(sprintf("%s\n", browser_viewer_url))
  }
}

