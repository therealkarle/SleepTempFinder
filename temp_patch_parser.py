from pathlib import Path
path = Path(r'C:\Users\flori\OneDrive\Documents\AppDesign\SleepTempFinder\RScript\SleepTempFinder.R')
text = path.read_text(encoding='utf-8').replace('\r\n','\n')
old = '''  # month e.g. 01.2025, 1.2025, 2025.01, or 2025.1
  if (grepl("^\\d{1,2}\\.\\d{4}$", tok)) {
    parts <- strsplit(tok, "\\.")[[1]]
    m <- as.integer(parts[1]); yr <- as.integer(parts[2])
    start <- as.Date(sprintf("%04d-%02d-01", yr, m))
    end <- as.Date(sprintf("%04d-%02d-%02d", yr, m,
                             lubridate::days_in_month(start)))
    return(list(start = start, end = end))
  }
'''
new = '''  # month e.g. 01.2025, 1.2025, 2025.01, or 2025.1
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
'''
if old not in text:
    raise SystemExit('Old block not found')
path.write_text(text.replace(old, new), encoding='utf-8')
print('patched')
