#!/usr/bin/env Rscript
library(readr)
library(yaml)
library(lubridate)
library(dplyr)

cfg <- read_yaml('config.yaml')
data_dir <- cfg$data_directory %||% '../data'
parse_orders <- cfg$parse_orders
sensor_orders <- parse_orders$sensor_timestamp %||% c('d/m/Y H:M')
garmin_time_orders <- parse_orders$garmin_time %||% c('H:M')
decimal_mark <- cfg$locale$decimal_mark %||% ','

all_files <- list.files(data_dir, recursive = TRUE, pattern = '\\.(csv|CSV)$', full.names = TRUE)

read_garmin_fixed <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines[1] <- gsub("^[^\\t[:alnum:][:punct:][:space:]]+", "", lines[1])
  lines <- gsub("([+-]\\d+),(\\d+°)", "\\1.\\2", lines)
  lines <- gsub(",+$", "", lines)
  df <- read.csv(text = lines, sep = ",", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c(" ", "--", "NA", ""))
  as_tibble(df)
}

# identify sensor files from config.sensor_files (by basename)
sensor_defs <- cfg$sensor_files %||% list()
sensor_paths <- character(0)
for (id in names(sensor_defs)) {
  p <- sensor_defs[[id]]$path %||% ''
  if (p == '') next
  matches <- all_files[basename(all_files) == basename(p)]
  if (length(matches) > 0) sensor_paths <- c(sensor_paths, matches[1])
}
sensor_paths <- unique(sensor_paths)

if (length(sensor_paths) == 0) stop('No sensor files found based on config.sensor_files')

sensor_locale <- locale(decimal_mark = decimal_mark)
sensor_df <- bind_rows(lapply(sensor_paths, function(fp) {
  df <- suppressWarnings(read_delim(fp, delim = ',', locale = sensor_locale, show_col_types = FALSE))
  ts_col <- names(df)[1]
  ts <- parse_date_time(df[[ts_col]], orders = sensor_orders, quiet = TRUE)
  temp_col <- names(df)[grepl('Temperature', names(df), ignore.case = TRUE)][1]
  tibble(Source_File = fp, timestamp = ts, room_temp = if (!is.null(temp_col)) as.numeric(gsub(',', '.', as.character(df[[temp_col]]))) else NA_real_)
}))

sensor_file_range <- sensor_df %>% group_by(Source_File) %>% summarise(First_TS = min(timestamp, na.rm = TRUE), Last_TS = max(timestamp, na.rm = TRUE), N = n(), .groups='drop')

# find sleep files by header containing garmin_date column name
mapping <- cfg$column_names
sleep_files <- c()
for (f in all_files) {
  hdr <- tryCatch(names(read.csv(f, nrows = 1, stringsAsFactors = FALSE, check.names = FALSE)), error = function(e) character(0))
  if (mapping$garmin_date %in% hdr) sleep_files <- c(sleep_files, f)
}
sleep_files <- unique(sleep_files)
if (length(sleep_files) == 0) stop('No sleep files found')

out_rows <- list()
for (sf in sleep_files) {
  sdt <- read_garmin_fixed(sf)
  date_col <- mapping$garmin_date
  bt_col <- mapping$garmin_bedtime
  wt_col <- mapping$garmin_waketime
  if (!(date_col %in% names(sdt))) next
  for (i in seq_len(nrow(sdt))) {
    row <- sdt[i,]
    date_val <- as.character(row[[date_col]])
    if (is.na(date_val) || date_val == '') next
    # attempt to coerce date
    sleep_date <- as.Date(date_val)
    if (is.na(sleep_date)) {
      # try other formats
      sleep_date <- parse_date_time(date_val, orders = c('Y-m-d','d.%m.%Y','d/m/Y'), quiet = TRUE)
      sleep_date <- as.Date(sleep_date)
    }
    bed_s <- as.character(row[[bt_col]])
    wake_s <- as.character(row[[wt_col]])
    bed_dt <- tryCatch(as.POSIXct(paste(sleep_date, bed_s), format='%Y-%m-%d %H:%M'), error=function(e) NA)
    wake_dt <- tryCatch(as.POSIXct(paste(sleep_date, wake_s), format='%Y-%m-%d %H:%M'), error=function(e) NA)
    if (is.na(bed_dt)) bed_dt <- parse_date_time(paste(sleep_date, bed_s), orders = garmin_time_orders, quiet = TRUE)
    if (is.na(wake_dt)) wake_dt <- parse_date_time(paste(sleep_date, wake_s), orders = garmin_time_orders, quiet = TRUE)
    # align to same calendar date then correct if swapped
    bed_dt <- update(bed_dt, year = year(sleep_date), month = month(sleep_date), mday = day(sleep_date))
    wake_dt <- update(wake_dt, year = year(sleep_date), month = month(sleep_date), mday = day(sleep_date))
    if (!is.na(bed_dt) && !is.na(wake_dt) && (hour(bed_dt) <= 12 & hour(wake_dt) > 12 & bed_dt < wake_dt)) {
      tmp <- bed_dt; bed_dt <- wake_dt; wake_dt <- tmp
    }
    if (!is.na(bed_dt) && !is.na(wake_dt) && bed_dt > wake_dt) bed_dt <- bed_dt - days(1)

    # count sensor hits with padding (if configured)
    pad_minutes <- if (!is.null(cfg$matching_padding_minutes)) as.integer(cfg$matching_padding_minutes) else 0
    n_hits <- 0
    avg_temp <- NA_real_
    if (!is.na(bed_dt) && !is.na(wake_dt)) {
      bed_p <- bed_dt - minutes(pad_minutes)
      wake_p <- wake_dt + minutes(pad_minutes)
      hits <- sensor_df %>% filter(!is.na(timestamp) & timestamp >= bed_p & timestamp <= wake_p)
      n_hits <- nrow(hits)
      if (n_hits > 0) avg_temp <- mean(hits$room_temp, na.rm = TRUE)
    }

    out_rows[[length(out_rows) + 1]] <- tibble(
      Date = as.Date(sleep_date),
      Sleep_Source = sf,
      Bedtime = ifelse(is.na(bed_dt), NA_character_, format(bed_dt, '%Y-%m-%d %H:%M')),
      Waketime = ifelse(is.na(wake_dt), NA_character_, format(wake_dt, '%Y-%m-%d %H:%M')),
      Raw_N_Readings = n_hits,
      Avg_Temp = ifelse(is.na(avg_temp), NA_real_, avg_temp)
    )
  }
}

out_df <- bind_rows(out_rows)
out_df <- out_df %>% left_join(sensor_file_range %>% summarise(First_TS = min(First_TS, na.rm=TRUE), Last_TS = max(Last_TS, na.rm=TRUE)), by = character())
out_path <- if (basename(getwd()) == 'RScript') 'nightly_diagnostics.csv' else file.path('RScript', 'nightly_diagnostics.csv')
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write.csv(out_df, file = out_path, row.names = FALSE)
cat('Wrote', out_path, 'with', nrow(out_df), 'rows\n')
