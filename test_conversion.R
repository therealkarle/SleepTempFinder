# Test flag conversion
source("RScript/studio_commands.R")

cat("\n=== Testing convert_flag_comparison ===\n")

# Test cases
tests <- list(
  "flags != 'Hochlitten'" = "!Hochlitten",
  "flags == 'Hochlitten'" = "Hochlitten",
  "flags != \"Hochlitten\"" = "!Hochlitten",
  "flags %in% c('A', 'B')" = "A,B",
  "flags %in% c(\"A\", \"B\")" = "A,B"
)

for (input in names(tests)) {
  expected <- tests[[input]]
  result <- convert_flag_comparison(input)
  status <- if (!is.null(result) && result == expected) "✓" else "❌"
  cat(sprintf("%s Input: %s\n", status, input))
  cat(sprintf("  Expected: %s\n", expected))
  cat(sprintf("  Got:      %s\n", result))
}

cat("\n=== Testing grepl pattern ===\n")
test_strings <- c(
  "flags != 'Hochlitten'",
  "flags == 'Hochlitten'",
  "flags %in% c('A', 'B')",
  "temp > 18"
)

for (str in test_strings) {
  matches <- grepl("flags\\s*(!==|==|!=|%in%|!=)", str, perl = TRUE)
  cat(sprintf("%s: %s\n", str, if (matches) "MATCHES (will be converted)" else "no match"))
}

cat("\n=== Done ===\n")
