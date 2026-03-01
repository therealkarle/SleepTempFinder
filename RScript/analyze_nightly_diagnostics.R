#!/usr/bin/env Rscript
library(readr)
library(dplyr)
library(lubridate)

fn <- 'RScript/nightly_diagnostics.csv'
if (!file.exists(fn)) stop('file not found: ', fn)
df <- read_csv(fn, show_col_types = FALSE)

parse_dt <- function(x) {
  x <- as.character(x)
  res1 <- parse_date_time(x, orders = c('Y-m-d H:M','Y-m-d H:M:S'), quiet = TRUE)
  as.POSIXct(res1)
}

df <- df %>% mutate(
  BedtimePOS = parse_dt(Bedtime),
  WaketimePOS = parse_dt(Waketime),
  FirstPOS = parse_dt(First_TS),
  LastPOS = parse_dt(Last_TS)
)

zeros <- df %>% filter(Raw_N_Readings == 0 | is.na(Avg_Temp))

classify_row <- function(b,w,f,l) {
  if (is.na(b) || is.na(w)) return('missing bed/wake')
  if (!is.na(l) && l < b) return('sensor ends before bedtime')
  if (!is.na(f) && f > w) return('sensor starts after waketime')
  return('no overlap despite sensor range (parsing mismatch)')
}

zeros <- zeros %>% rowwise() %>% mutate(Reason = classify_row(BedtimePOS, WaketimePOS, FirstPOS, LastPOS)) %>% ungroup()

cat('Zero-hit nights by reason:\n')
print(table(zeros$Reason))
cat('\nDetails (Date, Bedtime, Waketime, Raw_N_Readings, Avg_Temp, First_TS, Last_TS, Reason):\n')
print(select(zeros, Date, Bedtime, Waketime, Raw_N_Readings, Avg_Temp, First_TS, Last_TS, Reason))

write_csv(zeros, 'RScript/nightly_diagnostics_zero_hits.csv')
cat('\nWrote RScript/nightly_diagnostics_zero_hits.csv with', nrow(zeros), 'rows\n')
