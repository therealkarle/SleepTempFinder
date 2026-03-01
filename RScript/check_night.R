#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
date_arg <- if (length(args) >= 1) args[1] else '2026-01-06'
library(readr)
library(lubridate)
library(dplyr)

folder <- format(as.Date(date_arg), '%Y.%m.%d')
data_dir <- file.path('..', 'data', folder)
if (!dir.exists(data_dir)) stop('data folder not found: ', data_dir)

csvs <- list.files(data_dir, pattern='\\.csv$', full.names=TRUE)
sensor_file <- csvs[grepl('Thermometer|Wohnwagen|temp|Thermo', csvs, ignore.case=TRUE)][1]
if (is.na(sensor_file) || is.null(sensor_file)) sensor_file <- csvs[1]
cat('Using sensor file:', sensor_file, '\n')

sensor <- read_delim(sensor_file, delim=',', locale=locale(decimal_mark=','), show_col_types=FALSE)
ts_col <- names(sensor)[1]
orders <- c('d/m/Y H:M','d.%m.%Y %H:%M','Y-%m-%d %H:%M','d-%m-%Y %H:%M','d/%m/%Y %H:%M')
sensor$timestamp <- parse_date_time(sensor[[ts_col]], orders=orders, quiet=TRUE)
cat('Sensor rows:', nrow(sensor), 'NA timestamps:', sum(is.na(sensor$timestamp)), 'min/max:', format(min(sensor$timestamp, na.rm=TRUE),'%Y-%m-%d %H:%M'), format(max(sensor$timestamp, na.rm=TRUE),'%Y-%m-%d %H:%M'), '\n')

sleep_file <- list.files(data_dir, pattern='Schlaf', full.names=TRUE)[1]
cat('Using sleep file:', sleep_file, '\n')
sleep <- read.csv(sleep_file, fileEncoding='UTF-8', stringsAsFactors=FALSE, check.names=FALSE)
date_col <- names(sleep)[1]
row <- sleep[ sleep[[date_col]] == date_arg, ]
if (nrow(row) == 0) { cat('No sleep row for', date_arg, '\n'); q(status=0) }

bed_str <- row[['Schlafenszeit']][1]
wake_str <- row[['Aufstehzeit']][1]
sleep_date <- as.Date(row[[date_col]][1])

bed_dt <- as.POSIXct(paste(sleep_date, bed_str), format='%Y-%m-%d %H:%M')
wake_dt <- as.POSIXct(paste(sleep_date, wake_str), format='%Y-%m-%d %H:%M')
if (is.na(bed_dt)) bed_dt <- parse_date_time(paste(sleep_date, bed_str), orders=c('Y-m-d H:M','Y-m-d H:M:S','d.m.Y H:M'), quiet=TRUE)
if (is.na(wake_dt)) wake_dt <- parse_date_time(paste(sleep_date, wake_str), orders=c('Y-m-d H:M','Y-m-d H:M:S','d.m.Y H:M'), quiet=TRUE)
if (bed_dt > wake_dt) wake_dt <- wake_dt + days(1)
cat('Bedtime =', format(bed_dt, '%Y-%m-%d %H:%M'), ' Waketime =', format(wake_dt, '%Y-%m-%d %H:%M'), '\n')

hits <- sensor %>% filter(!is.na(timestamp) & timestamp >= bed_dt & timestamp <= wake_dt)
cat('Matching sensor rows:', nrow(hits), '\n')
if (nrow(hits) > 0) print(hits)
