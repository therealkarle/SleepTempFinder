`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

normalize_sleep_source_mode <- function(mode) {
  mode <- tolower(trimws(as.character(mode %||% "csv")))
  if (!mode %in% c("csv", "api", "combined")) {
    stop(sprintf("Invalid sleep_source.mode '%s'; expected 'csv', 'api', or 'combined'.", mode))
  }
  mode
}

normalize_sleep_source_priority <- function(priority) {
  priority <- tolower(trimws(as.character(priority %||% "csv")))
  if (!priority %in% c("csv", "api")) {
    stop(sprintf("Invalid sleep_source.priority '%s'; expected 'csv' or 'api'.", priority))
  }
  priority
}

row_non_na_count <- function(df, cols) {
  if (is.null(df) || nrow(df) == 0 || length(cols) == 0) {
    return(integer(0))
  }
  rowSums(!is.na(as.matrix(df[, cols, drop = FALSE])))
}

harmonize_sleep_source_types <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  keep_cols <- names(df)[vapply(df, function(col) inherits(col, c("Date", "POSIXt")), logical(1))]
  keep_cols <- unique(c(keep_cols, intersect(c("Sleep_Source", "Source_File", "Source_Name"), names(df))))
  convert_cols <- setdiff(names(df), keep_cols)
  if (length(convert_cols) > 0) {
    df[convert_cols] <- lapply(df[convert_cols], as.character)
  }
  df
}

prepare_sleep_source_rows <- function(df, source_label) {
  if (is.null(df) || nrow(df) == 0) {
    if (is.null(df)) {
      return(tibble::tibble())
    }
    return(dplyr::mutate(df, Sleep_Source = source_label))
  }
  out <- df |>
    dplyr::mutate(Sleep_Source = source_label) |>
    dplyr::filter(!is.na(Date))

  quality_cols <- intersect(c("Sleep_Score", "HRV", "RHR", "Sleep_Duration"), names(out))
  if (length(quality_cols) > 0) {
    out$.sleep_quality <- row_non_na_count(out, quality_cols)
    out <- out |>
      dplyr::arrange(Date, dplyr::desc(.sleep_quality)) |>
      dplyr::distinct(Date, .keep_all = TRUE) |>
      dplyr::select(-.sleep_quality)
  } else {
    out <- out |>
      dplyr::arrange(Date) |>
      dplyr::distinct(Date, .keep_all = TRUE)
  }
  out
}

merge_sleep_source_rows <- function(csv_df, api_df, priority = "csv") {
  priority <- normalize_sleep_source_priority(priority)
  csv_df <- harmonize_sleep_source_types(prepare_sleep_source_rows(csv_df, "csv"))
  api_df <- harmonize_sleep_source_types(prepare_sleep_source_rows(api_df, "api"))

  if (nrow(csv_df) == 0) return(api_df)
  if (nrow(api_df) == 0) return(csv_df)

  if (priority == "csv") {
    combined <- dplyr::bind_rows(csv_df, api_df)
  } else {
    combined <- dplyr::bind_rows(api_df, csv_df)
  }

  combined |>
    dplyr::arrange(Date) |>
    dplyr::distinct(Date, .keep_all = TRUE)
}

split_date_ranges <- function(dates) {
  dates <- sort(unique(as.Date(dates)))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(list())
  }

  breaks <- c(1L, which(diff(dates) != 1) + 1L)
  ends <- c(breaks[-1] - 1L, length(dates))

  Map(function(start_idx, end_idx) {
    list(
      start = dates[[start_idx]],
      end = dates[[end_idx]]
    )
  }, breaks, ends)
}

sleep_source_query_dates <- function(temp_dates, csv_dates = character(0), priority = "csv") {
  temp_dates <- sort(unique(as.Date(temp_dates)))
  temp_dates <- temp_dates[!is.na(temp_dates)]
  if (length(temp_dates) == 0) {
    return(as.Date(character(0)))
  }

  priority <- normalize_sleep_source_priority(priority)
  if (priority == "csv") {
    csv_dates <- sort(unique(as.Date(csv_dates)))
    csv_dates <- csv_dates[!is.na(csv_dates)]
    temp_dates[!(temp_dates %in% csv_dates)]
  } else {
    temp_dates
  }
}
