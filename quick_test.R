# Quick sanity check for the parser
source("RScript/flag_expression_parser.R", local = FALSE)

cat("\n=== QUICK PARSER TEST ===\n")

# Test 1: Simple flag
cat("\nTest 1: Simple 'A'\n")
result <- evaluate_flag_expression("A", c("A"))
cat("Expected: TRUE, Got:", result, "\n")

# Test 2: OR
cat("\nTest 2: 'A, B' with flags c('A')\n")
result <- evaluate_flag_expression("A, B", c("A"))
cat("Expected: TRUE, Got:", result, "\n")

# Test 3: AND
cat("\nTest 3: 'A & B' with flags c('A', 'B')\n")
result <- evaluate_flag_expression("A & B", c("A", "B"))
cat("Expected: TRUE, Got:", result, "\n")

# Test 4: NOT
cat("\nTest 4: '!A' with flags c('B')\n")
result <- evaluate_flag_expression("!A", c("B"))
cat("Expected: TRUE, Got:", result, "\n")

# Test 5: XOR
cat("\nTest 5: 'A * B' with flags c('A')\n")
result <- evaluate_flag_expression("A * B", c("A"))
cat("Expected: TRUE, Got:", result, "\n")

# Test 6: Complex
cat("\nTest 6: '(Urlaub, Wohnmobil) & !HomeOffice' with flags c('Urlaub')\n")
result <- evaluate_flag_expression("(Urlaub, Wohnmobil) & !HomeOffice", c("Urlaub"))
cat("Expected: TRUE, Got:", result, "\n")

# Test 7: Parser error handling
cat("\nTest 7: Malformed expression '(A &' - should error\n")
tryCatch({
  parse_flag_expression("(A &")
  cat("ERROR: Should have thrown an error!\n")
}, error = function(e) {
  cat("Correctly caught error:", e$message, "\n")
})

cat("\n=== DONE ===\n")
