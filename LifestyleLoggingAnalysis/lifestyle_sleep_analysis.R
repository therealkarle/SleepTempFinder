#!/usr/bin/env Rscript

# Analyse Garmin LifestyleLogging against Garmin sleep metrics.

if (!requireNamespace("yaml", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install the R packages yaml and jsonlite.")
}

DEFAULT_METRICS <- list(
  Sleep_Score = c("Score", "Sleep Score", "sleepScore", "overallSleepScore"),
  Sleep_Duration = c("Dauer", "Sleep Duration", "sleepDuration", "totalSleepTime"),
  HRV = c("HFV-Status", "HRV", "avgOvernightHrv", "averageOvernightHrv"),
  RHR = c("Ruheherzfrequenz", "Resting Heart Rate", "restingHeartRate", "restingHr")
)
TRUE_VALUES <- c("true", "yes", "y", "1", "done", "completed", "complete", "ja", "gemacht")
FALSE_VALUES <- c("false", "no", "n", "0", "not done", "not_done", "nicht gemacht", "nein")
`%||%` <- function(x, y) if (is.null(x)) y else x
progress <- function(...) {
  cat(..., "\n", sep = "")
  flush.console()
}

norm <- function(x) {
  x <- tolower(trimws(ifelse(is.na(x), "", as.character(x))))
  gsub(" +", " ", gsub("_", " ", x))
}

parse_date <- function(x) {
  if (is.list(x) && length(x) >= 3) return(as.Date(sprintf("%04d-%02d-%02d", as.integer(x[[1]]), as.integer(x[[2]]), as.integer(x[[3]]))))
  if (length(x) == 0 || is.null(x) || is.na(x)[1]) return(as.Date(NA))
  text <- substr(as.character(x)[1], 1, 10)
  for (format in c("%Y-%m-%d", "%d.%m.%Y", "%m/%d/%Y")) {
    parsed <- as.Date(text, format = format)
    if (!is.na(parsed)) return(parsed)
  }
  as.Date(NA)
}

parse_number <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x)[1]) return(NA_real_)
  text <- trimws(as.character(x)[1])
  if (!nzchar(text) || text %in% c("--", "-", "n/a", "NA")) return(NA_real_)
  text <- gsub("[^0-9,.-]", "", text)
  if (!nzchar(text)) return(NA_real_)
  if (grepl(",", text, fixed = TRUE) && grepl("\\.", text)) text <- sub(",", ".", gsub("\\.", "", text), fixed = TRUE) else text <- sub(",", ".", text, fixed = TRUE)
  suppressWarnings(as.numeric(text))
}

duration_to_hours <- function(x) {
  number <- parse_number(x); text <- as.character(ifelse(length(x) == 0 || is.null(x), "", x))[1]
  if (!is.na(number) && !grepl("[hHmMs]", text)) return(number)
  parts <- regmatches(text, regexec("(?:(\\d+)\\s*h)?\\s*(?:(\\d+)\\s*min?)?", text, perl = TRUE))[[1]]
  if (length(parts) >= 3 && (nzchar(parts[2]) || nzchar(parts[3]))) return((as.numeric(ifelse(nzchar(parts[2]), parts[2], 0)) * 60 + as.numeric(ifelse(nzchar(parts[3]), parts[3], 0))) / 60)
  NA_real_
}

read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)

find_daily_logs <- function(value) {
  if (is.list(value) && !is.null(value$dailyLogList) && is.list(value$dailyLogList)) return(value$dailyLogList)
  if (is.list(value)) for (item in value) { found <- find_daily_logs(item); if (!is.null(found)) return(found) }
  NULL
}

normalize_status <- function(value) {
  if (is.logical(value) && length(value)) return(value[1])
  text <- norm(value)
  if (text %in% TRUE_VALUES) return(TRUE)
  if (text %in% FALSE_VALUES) return(FALSE)
  NA
}

lifestyle_rows <- function(payload) {
  logs <- find_daily_logs(payload); if (is.null(logs) && is.list(payload)) logs <- payload
  rows <- list()
  for (entry in logs %||% list()) {
    if (!is.list(entry)) next
    day <- parse_date(entry$calendarDate %||% entry$date %||% entry$logDate)
    name <- trimws(as.character(entry$behaviourName %||% entry$behaviorName %||% entry$name %||% entry$label %||% ""))[1]
    status <- normalize_status(entry$status %||% entry$value)
    if (is.na(day) || !nzchar(name) || is.na(status)) next
    key <- as.character(day); if (is.null(rows[[key]])) rows[[key]] <- list()
    rows[[key]][[name]] <- isTRUE(rows[[key]][[name]] %||% FALSE) || isTRUE(status)
  }
  rows
}

materialize_source <- function(source) {
  source <- normalizePath(source, mustWork = TRUE)
  if (dir.exists(source)) {
    progress("[Lifestyle] Using folder: ", source)
    return(list(root = source, direct_json = NULL, cleanup = FALSE))
  }
  if (grepl("\\.zip$", source, ignore.case = TRUE)) {
    progress("[Lifestyle] Extracting ZIP: ", source)
    root <- tempfile("garmin_export_"); dir.create(root); utils::unzip(source, exdir = root)
    return(list(root = root, direct_json = NULL, cleanup = TRUE))
  }
  if (grepl("LifestyleLogging\\.json$", source, ignore.case = TRUE)) {
    progress("[Lifestyle] Using JSON: ", source)
    return(list(root = NULL, direct_json = source, cleanup = FALSE))
  }
  stop("Input must be a Garmin folder, ZIP archive, or LifestyleLogging.json: ", source)
}

source_files <- function(materialized, pattern) if (!is.null(materialized$root)) list.files(materialized$root, pattern = pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE) else character()

read_csv_flexible <- function(path) {
  first <- readLines(path, n = 1, encoding = "UTF-8", warn = FALSE)
  commas <- if (length(first)) lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))) else 0
  semicolons <- if (length(first)) lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))) else 0
  separator <- if (semicolons > commas) ";" else ","
  tryCatch(utils::read.csv(path, sep = separator, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
}

json_named_value <- function(value, aliases) {
  if (!is.list(value)) return(NULL)
  wanted <- norm(aliases)
  for (name in names(value)) {
    if (norm(name) %in% wanted && !is.list(value[[name]])) return(value[[name]])
  }
  for (child in value) {
    found <- json_named_value(child, aliases)
    if (!is.null(found)) return(found)
  }
  NULL
}

sleep_json_value <- function(entry, metric, aliases) {
  value <- json_named_value(entry, aliases)
  if (!is.null(value)) {
    return(if (metric == "Sleep_Duration") duration_to_hours(value) else parse_number(value))
  }
  if (metric == "Sleep_Score") {
    value <- json_named_value(entry$sleepScores %||% list(), c("overallScore", "overall sleep score"))
    return(parse_number(value))
  }
  if (metric == "Sleep_Duration") {
    stages <- c(entry$deepSleepSeconds, entry$lightSleepSeconds, entry$remSleepSeconds)
    if (length(stages) == 3 && all(!vapply(stages, is.null, logical(1)))) {
      seconds <- suppressWarnings(sum(as.numeric(unlist(stages)), na.rm = TRUE))
      if (is.finite(seconds) && seconds > 0) return(seconds / 3600)
    }
  }
  NA_real_
}

sleep_rows_json <- function(materialized, specs) {
  json_files <- source_files(materialized, "_sleepData\\.json$")
  if (!length(json_files)) return(list())
  progress("[Lifestyle] Reading ", length(json_files), " sleep JSON file(s)...")
  result <- list()
  for (path in json_files) {
    progress("[Lifestyle] Reading sleep file: ", path)
    payload <- tryCatch(read_json(path), error = function(e) NULL)
    if (!is.list(payload)) next
    for (entry in payload) {
      if (!is.list(entry)) next
      day <- parse_date(entry$calendarDate %||% entry$date)
      if (is.na(day)) next
      key <- as.character(day); if (is.null(result[[key]])) result[[key]] <- list()
      for (metric in names(specs)) {
        value <- sleep_json_value(entry, metric, specs[[metric]])
        if (!is.na(value) && is.null(result[[key]][[metric]])) result[[key]][[metric]] <- value
      }
    }
  }
  result
}

metric_specs <- function(config) {
  configured <- config$sleep_metrics %||% names(DEFAULT_METRICS)
  if (is.list(configured) && !is.null(names(configured))) return(lapply(configured, function(x) as.character(unlist(x))))
  names <- as.character(unlist(configured)); setNames(lapply(names, function(name) DEFAULT_METRICS[[name]] %||% name), names)
}

sleep_rows <- function(materialized, specs) {
  result <- sleep_rows_json(materialized, specs)
  csv_files <- source_files(materialized, "\\.csv$")
  if (!length(result)) progress("[Lifestyle] Reading ", length(csv_files), " sleep CSV file(s)...")
  for (path in csv_files) {
    if (length(result) && grepl("INREACH", path, ignore.case = TRUE)) next
    progress("[Lifestyle] Reading sleep file: ", path)
    data <- read_csv_flexible(path); if (is.null(data) || !nrow(data)) next
    date_candidates <- names(data)[norm(names(data)) %in% c("date", "datum", "sleep score 4 wochen", "sleep date", "calendar date")]
    if (!length(date_candidates)) next
    date_col <- date_candidates[1]
    matched <- lapply(specs, function(aliases) { columns <- names(data)[norm(names(data)) %in% norm(aliases)]; if (length(columns)) columns[1] else NA_character_ })
    if (!any(!is.na(unlist(matched)))) next
    for (i in seq_len(nrow(data))) {
      day <- parse_date(data[[date_col]][i]); if (is.na(day)) next
      key <- as.character(day); if (is.null(result[[key]])) result[[key]] <- list()
      for (metric in names(specs)) {
        column <- matched[[metric]]; if (is.na(column)) next
        value <- if (metric == "Sleep_Duration") duration_to_hours(data[[column]][i]) else parse_number(data[[column]][i])
        if (!is.na(value) && is.null(result[[key]][[metric]])) result[[key]][[metric]] <- value
      }
    }
  }
  if (!length(result)) stop("No compatible Garmin sleep JSON or CSV found. Provide a Garmin export folder/ZIP or sleep_input.")
  progress("[Lifestyle] Sleep dates loaded: ", length(result))
  result
}

group_stats <- function(values, interval) {
  if (!length(values)) return(list(n = 0, mean = NULL, median = NULL, sd = NULL, interval_low = NULL, interval_high = NULL))
  low <- (1 - interval) / 2
  list(n = length(values), mean = mean(values), median = median(values), sd = if (length(values) > 1) stats::sd(values) else NULL, interval_low = as.numeric(stats::quantile(values, low, names = FALSE)), interval_high = as.numeric(stats::quantile(values, 1 - low, names = FALSE)))
}

analyse <- function(config, lifestyle_materialized, sleep_materialized) {
  progress("[Lifestyle] Starting analysis...")
  start <- parse_date(config$start_date); end <- parse_date(config$end_date)
  if (is.na(start) || is.na(end) || end < start) stop("Config requires valid start_date and end_date with end_date >= start_date")
  interval <- as.numeric(config$value_interval %||% 0.80); confidence <- as.numeric(config$confidence_level %||% 0.95); alpha <- as.numeric(config$significance_level %||% 0.05)
  if (!(interval > 0 && interval <= 1 && confidence > 0 && confidence < 1 && alpha > 0 && alpha < 1)) stop("Invalid interval, confidence_level, or significance_level")
  lifestyle_files <- if (!is.null(lifestyle_materialized$direct_json)) lifestyle_materialized$direct_json else source_files(lifestyle_materialized, "LifestyleLogging\\.json$")
  if (!length(lifestyle_files)) stop("No LifestyleLogging.json found in Garmin export")
  progress("[Lifestyle] Reading ", length(lifestyle_files), " LifestyleLogging JSON file(s)...")
  lifestyle <- list()
  for (path in lifestyle_files) {
    progress("[Lifestyle] Reading lifestyle file: ", path)
    rows <- lifestyle_rows(read_json(path))
    for (key in names(rows)) {
    if (is.null(lifestyle[[key]])) lifestyle[[key]] <- list()
    entries <- rows[[key]]
    for (activity in names(entries)) lifestyle[[key]][[activity]] <- isTRUE(lifestyle[[key]][[activity]]) || isTRUE(entries[[activity]])
    }
  }
  specs <- metric_specs(config); sleep <- sleep_rows(sleep_materialized, specs)
  excluded <- norm(unlist(config$excluded_activities %||% list())); configured <- as.character(unlist(config$activities %||% list()))
  found <- unique(unlist(lapply(lifestyle, names))); activities <- unique(c(configured, found)); activities <- activities[!norm(activities) %in% excluded]
  progress("[Lifestyle] Activities to analyse: ", length(activities), "; metrics: ", length(specs))
  missing_default <- isTRUE(config$missing_activity_is_no %||% TRUE); overrides <- config$missing_activity_is_no_by_activity %||% list()
  days <- seq.Date(start, end, by = "day"); results <- list(); index <- 1
  sorted_activities <- sort(activities)
  for (activity_index in seq_along(sorted_activities)) {
    activity <- sorted_activities[activity_index]
    progress("[Lifestyle] Activity ", activity_index, "/", length(sorted_activities), ": ", activity)
    for (metric in names(specs)) {
    override_name <- names(overrides)[norm(names(overrides)) == norm(activity)][1]
    missing_no <- if (length(override_name) && !is.na(override_name)) isTRUE(overrides[[override_name]]) else missing_default
    done_values <- not_done_values <- numeric()
    for (day in days) {
      value <- sleep[[as.character(day)]][[metric]] %||% NA_real_; if (is.null(value) || is.na(value)) next
      status <- lifestyle[[as.character(day)]][[activity]] %||% NA
      if (is.na(status)) { if (!missing_no) next; status <- FALSE }
      if (isTRUE(status)) done_values <- c(done_values, value) else not_done_values <- c(not_done_values, value)
    }
    done <- group_stats(done_values, interval); not_done <- group_stats(not_done_values, interval)
    row <- c(list(activity = activity, metric = metric, missing_activity_is_no = missing_no), setNames(done, paste0("done_", names(done))), setNames(not_done, paste0("not_done_", names(not_done))))
    delta <- p_value <- ci_low <- ci_high <- NULL
    if (length(done_values) >= 2 && length(not_done_values) >= 2) { delta <- mean(done_values) - mean(not_done_values); test <- stats::t.test(done_values, not_done_values, var.equal = FALSE); p_value <- unname(test$p.value); ci_low <- unname(test$conf.int[1]); ci_high <- unname(test$conf.int[2]) }
    significant <- !is.null(p_value) && p_value < alpha && !is.null(delta) && delta != 0
    classification <- if (significant && delta > 0) "significant_positive" else if (significant && delta < 0) "significant_negative" else "not_significant"
    results[[index]] <- c(row, list(delta = delta, delta_ci_low = ci_low, delta_ci_high = ci_high, p_value = p_value, significant = significant, classification = classification)); index <- index + 1
    }
  }
  progress("[Lifestyle] Statistical analysis finished: ", length(results), " activity/metric combinations")
  list(metadata = list(start_date = as.character(start), end_date = as.character(end), value_interval = interval, confidence_level = confidence, significance_level = alpha, method = "Welch two-sample t-test", delta_definition = "mean(done) - mean(not_done)"), results = results)
}

write_outputs <- function(result, output_dir, config) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  progress("[Lifestyle] Writing results to: ", normalizePath(output_dir, mustWork = FALSE))
  for (classification in c("significant_positive", "significant_negative", "not_significant")) {
    selected <- Filter(function(x) identical(x$classification, classification), result$results)
    if (length(selected)) {
      frame <- do.call(rbind, lapply(selected, function(x) as.data.frame(lapply(x, function(v) if (is.null(v)) NA else v), stringsAsFactors = FALSE)))
      deltas <- as.numeric(frame$delta); frame <- frame[order(is.na(deltas), if (classification == "significant_negative") deltas else -deltas, na.last = TRUE), , drop = FALSE]
    } else frame <- data.frame(activity = character(), metric = character())
    utils::write.csv(frame, file.path(output_dir, paste0(classification, ".csv")), row.names = FALSE, na = "")
  }
  result$config <- config; jsonlite::write_json(result, file.path(output_dir, "lifestyle_sleep_analysis.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
}

script_directory <- function() {
  files <- vapply(sys.frames(), function(frame) if (!is.null(frame$ofile)) frame$ofile else "", character(1))
  files <- files[nzchar(files)]
  if (length(files)) dirname(normalizePath(tail(files, 1))) else getwd()
}

find_config <- function(config_path = NULL) {
  if (!is.null(config_path) && nzchar(config_path)) return(config_path)
  candidates <- unique(c(
    file.path(script_directory(), "GarminLifestyleAnalysisConfig.yaml"),
    file.path(getwd(), "GarminLifestyleAnalysisConfig.yaml"),
    file.path(getwd(), "LifestyleLoggingAnalysis", "GarminLifestyleAnalysisConfig.yaml")
  ))
  found <- candidates[file.exists(candidates)]
  if (length(found)) return(found[1])
  stop("No GarminLifestyleAnalysisConfig.yaml found. Searched:\n", paste(candidates, collapse = "\n"))
}

run_lifestyle_analysis <- function(config_path = NULL, input_override = NULL, sleep_input_override = NULL) {
  config_path <- find_config(config_path)
  progress("[Lifestyle] Config: ", normalizePath(config_path, mustWork = TRUE))
  config <- yaml::read_yaml(config_path)
  input_path <- input_override %||% config$input_path
  sleep_input <- sleep_input_override %||% config$sleep_input
  if (is.null(input_path) || !nzchar(input_path)) stop("Set input_path in the config or provide input_override")
  if (is.null(sleep_input) || !nzchar(sleep_input)) sleep_input <- input_path
  lifestyle_source <- materialize_source(input_path); sleep_source <- materialize_source(sleep_input)
  on.exit({
    if (lifestyle_source$cleanup) unlink(lifestyle_source$root, recursive = TRUE)
    if (sleep_source$cleanup && !identical(sleep_source$root, lifestyle_source$root)) unlink(sleep_source$root, recursive = TRUE)
  }, add = TRUE)
  result <- analyse(config, lifestyle_source, sleep_source)
  write_outputs(result, config$output_dir %||% "LifestyleLoggingAnalysis/Out", config)
  progress("[Lifestyle] Analysed ", length(result$results), " activity/metric combinations")
  invisible(result)
}

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name) { i <- match(name, args); if (is.na(i) || i == length(args)) NULL else args[i + 1] }

# With no command-line arguments, source() and plain Rscript both use automatic
# config discovery. Explicit arguments keep the CLI behaviour unchanged.
progress("[Lifestyle] Script loaded; preparing to run...")
if (!length(args)) {
  run_lifestyle_analysis()
} else {
  config_arg <- get_arg("--config") %||% get_arg("-c")
  run_lifestyle_analysis(config_arg, get_arg("--input"), get_arg("--sleep-input"))
}
