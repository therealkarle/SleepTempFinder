#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(magrittr))
source(file.path("RScript", "sleep_source_combined_helpers.R"), local = FALSE)

stopifnot(identical(normalize_sleep_source_mode(" combined "), "combined"))
stopifnot(identical(normalize_sleep_source_priority(" api "), "api"))

ranges <- split_date_ranges(as.Date(c("2026-07-01", "2026-07-02", "2026-07-04", "2026-07-05")))
stopifnot(length(ranges) == 2L)
stopifnot(identical(ranges[[1]]$start, as.Date("2026-07-01")))
stopifnot(identical(ranges[[1]]$end, as.Date("2026-07-02")))
stopifnot(identical(ranges[[2]]$start, as.Date("2026-07-04")))
stopifnot(identical(ranges[[2]]$end, as.Date("2026-07-05")))

temp_days <- as.Date(c("2026-07-01", "2026-07-02", "2026-07-03", "2026-07-04"))
csv_days <- as.Date(c("2026-07-02"))
query_days <- sleep_source_query_dates(temp_days, csv_days, "csv")
stopifnot(identical(query_days, as.Date(c("2026-07-01", "2026-07-03", "2026-07-04"))))

csv_df <- data.frame(
  Date = as.Date(c("2026-07-01", "2026-07-02")),
  Sleep_Score = c(81, 82),
  HRV = c(55, 56),
  RHR = c(49, 48),
  Sleep_Duration = c(7.5, 7.0),
  Source_File = c("csv-a", "csv-b"),
  Source_Name = c("csv-a", "csv-b"),
  stringsAsFactors = FALSE
)

api_df <- data.frame(
  Date = as.Date(c("2026-07-01", "2026-07-03")),
  Sleep_Score = c(70, 74),
  HRV = c(40, 44),
  RHR = c(60, 58),
  Sleep_Duration = c(6.5, 6.0),
  Source_File = c("api-a", "api-c"),
  Source_Name = c("api-a", "api-c"),
  stringsAsFactors = FALSE
)

merged_csv_first <- merge_sleep_source_rows(csv_df, api_df, "csv")
stopifnot(nrow(merged_csv_first) == 3L)
stopifnot(merged_csv_first$Sleep_Source[match(as.Date("2026-07-01"), merged_csv_first$Date)] == "csv")
stopifnot(merged_csv_first$Sleep_Source[match(as.Date("2026-07-03"), merged_csv_first$Date)] == "api")

merged_api_first <- merge_sleep_source_rows(csv_df, api_df, "api")
stopifnot(merged_api_first$Sleep_Source[match(as.Date("2026-07-01"), merged_api_first$Date)] == "api")

cat("sleep_source combined checks OK\n")
