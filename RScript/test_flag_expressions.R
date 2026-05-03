# Test Suite for Flag Expression Parser
# 
# Run this file in RStudio to validate the flag expression parser implementation.
# Example: source('RScript/test_flag_expressions.R')

# Load the parser if not already loaded
if (!exists("parse_flag_expression")) {
  source("flag_expression_parser.R", local = FALSE)
}

# ============================================================================
# Test Helper Functions
# ============================================================================

test_tokenizer <- function(expr, expected_count = NULL) {
  cat("\n[TOKENIZER] Testing:", expr, "\n")
  tryCatch({
    tokens <- tokenize_flag_expression(expr)
    cat("  Tokens:", paste(sapply(tokens, function(t) sprintf("%s(%s)", t$type, t$value)), collapse = ", "), "\n")
    if (!is.null(expected_count) && length(tokens) != expected_count) {
      cat("  ❌ FAIL: Expected", expected_count, "tokens but got", length(tokens), "\n")
      return(FALSE)
    }
    cat("  ✓ OK\n")
    return(TRUE)
  }, error = function(e) {
    cat("  ❌ ERROR:", e$message, "\n")
    return(FALSE)
  })
}

test_parser <- function(expr) {
  cat("\n[PARSER] Testing:", expr, "\n")
  tryCatch({
    ast <- parse_flag_expression(expr)
    cat("  AST type:", ast$type, "\n")
    print_ast(ast, indent = 4)
    cat("  ✓ OK\n")
    return(TRUE)
  }, error = function(e) {
    cat("  ❌ ERROR:", e$message, "\n")
    return(FALSE)
  })
}

print_ast <- function(ast, indent = 0) {
  prefix <- strrep(" ", indent)
  if (ast$type == "flag") {
    cat(prefix, "FLAG:", ast$name, "\n", sep = "")
  } else if (ast$type == "not") {
    cat(prefix, "NOT\n", sep = "")
    print_ast(ast$arg, indent + 2)
  } else if (ast$type %in% c("or", "and", "xor")) {
    cat(prefix, toupper(ast$type), "\n", sep = "")
    cat(prefix, "  LEFT:\n", sep = "")
    print_ast(ast$left, indent + 4)
    cat(prefix, "  RIGHT:\n", sep = "")
    print_ast(ast$right, indent + 4)
  }
}

test_evaluator <- function(expr, row_flags, expected) {
  cat("\n[EVALUATOR] Testing:", expr, "with flags =", paste(row_flags, collapse = ", "), "\n")
  tryCatch({
    ast <- parse_flag_expression(expr)
    result <- evaluate_flag_ast(ast, row_flags)
    status <- if (result == expected) "✓ OK" else "❌ FAIL"
    cat("  Expected:", expected, "Got:", result, status, "\n")
    return(result == expected)
  }, error = function(e) {
    cat("  ❌ ERROR:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================================
# TOKENIZER TESTS
# ============================================================================

cat("\n================== TOKENIZER TESTS ==================\n")

test_tokenizer("A")
test_tokenizer("A & B", expected_count = 4)  # A, &, B, EOF
test_tokenizer("A , B", expected_count = 4)  # A, ,, B, EOF
test_tokenizer("(A)")
test_tokenizer("!A")
test_tokenizer("A * B")
test_tokenizer("(A & B), !C")

# ============================================================================
# PARSER TESTS
# ============================================================================

cat("\n================== PARSER TESTS ==================\n")

test_parser("A")
test_parser("A & B")
test_parser("A, B")
test_parser("A * B")
test_parser("!A")
test_parser("(A)")
test_parser("(A & B)")
test_parser("A & B, C")  # A & (B , C) due to precedence
test_parser("(A, B) & C")
test_parser("!A & B")  # (!A) & B
test_parser("A * B & C")  # (A * B) & C
test_parser("(Urlaub, Wohnmobil) & !HomeOffice")
test_parser("(A & B) * (C, D)")

# ============================================================================
# EVALUATOR TESTS: Simple Flags
# ============================================================================

cat("\n================== EVALUATOR TESTS: Simple Flags ==================\n")

test_evaluator("A", c("A"), TRUE)
test_evaluator("A", c("B"), FALSE)
test_evaluator("A", character(0), FALSE)

# ============================================================================
# EVALUATOR TESTS: OR (,)
# ============================================================================

cat("\n================== EVALUATOR TESTS: OR (,) ==================\n")

test_evaluator("A, B", c("A"), TRUE)
test_evaluator("A, B", c("B"), TRUE)
test_evaluator("A, B", c("A", "B"), TRUE)
test_evaluator("A, B", c("C"), FALSE)

# ============================================================================
# EVALUATOR TESTS: AND (&)
# ============================================================================

cat("\n================== EVALUATOR TESTS: AND (&) ==================\n")

test_evaluator("A & B", c("A", "B"), TRUE)
test_evaluator("A & B", c("A"), FALSE)
test_evaluator("A & B", c("B"), FALSE)
test_evaluator("A & B", c("C"), FALSE)
test_evaluator("A & B & C", c("A", "B", "C"), TRUE)

# ============================================================================
# EVALUATOR TESTS: NOT (!)
# ============================================================================

cat("\n================== EVALUATOR TESTS: NOT (!) ==================\n")

test_evaluator("!A", c("B"), TRUE)
test_evaluator("!A", c("A"), FALSE)
test_evaluator("!A", character(0), TRUE)
test_evaluator("!!A", c("A"), TRUE)  # Double negation

# ============================================================================
# EVALUATOR TESTS: XOR (*)
# ============================================================================

cat("\n================== EVALUATOR TESTS: XOR (*) ==================\n")

test_evaluator("A * B", c("A"), TRUE)  # Only A
test_evaluator("A * B", c("B"), TRUE)  # Only B
test_evaluator("A * B", c("A", "B"), FALSE)  # Both
test_evaluator("A * B", character(0), FALSE)  # Neither
test_evaluator("A * B * C", c("A"), TRUE)  # Only A
test_evaluator("A * B * C", c("A", "B"), FALSE)  # Two active
test_evaluator("A * B * C", c("A", "B", "C"), FALSE)  # All active

# ============================================================================
# EVALUATOR TESTS: Operator Precedence
# ============================================================================

cat("\n================== EVALUATOR TESTS: Operator Precedence ==================\n")

# Precedence: ! > * > & > ,
# A & B , C should be parsed as (A & B) , C

# NOT binds tightest
test_evaluator("!A & B", c("B"), TRUE)  # (!A) & B; A absent, B present → true

# XOR tighter than AND
test_evaluator("A * B & C", c("A", "C"), TRUE)  # (A * B) & C; A present (not B), C present → true
test_evaluator("A * B & C", c("A", "B", "C"), FALSE)  # (A * B) & C; A and B both present → (A*B) false

# AND tighter than OR
test_evaluator("A & B, C", c("C"), TRUE)  # (A & B) , C; C present → true
test_evaluator("A & B, C", c("A", "B"), TRUE)  # (A & B) , C; both A and B present → true

# ============================================================================
# EVALUATOR TESTS: Parentheses
# ============================================================================

cat("\n================== EVALUATOR TESTS: Parentheses ==================\n")

test_evaluator("(A, B) & C", c("A", "C"), TRUE)  # (A OR B) AND C
test_evaluator("(A, B) & C", c("B", "C"), TRUE)  # (A OR B) AND C
test_evaluator("(A, B) & C", c("C"), FALSE)  # (A OR B) AND C; C present but neither A nor B

test_evaluator("(A & B), C", c("A", "B"), TRUE)  # (A AND B) OR C
test_evaluator("(A & B), C", c("C"), TRUE)  # (A AND B) OR C

test_evaluator("!(A & B)", c("A"), TRUE)  # NOT(A AND B); A present → NOT false → true
test_evaluator("!(A & B)", c("A", "B"), FALSE)  # NOT(A AND B); both present → NOT true → false

# ============================================================================
# EVALUATOR TESTS: De Morgan's Laws
# ============================================================================

cat("\n================== EVALUATOR TESTS: De Morgan's Laws ==================\n")

# !(A & B) should be equivalent to (!A , !B)
test_evaluator("!(A & B)", c("C"), TRUE)
test_evaluator("!A , !B", c("C"), TRUE)

test_evaluator("!(A & B)", c("A"), TRUE)
test_evaluator("!A , !B", c("A"), TRUE)

test_evaluator("!(A & B)", c("A", "B"), FALSE)
test_evaluator("!A , !B", c("A", "B"), FALSE)

# !(A , B) should be equivalent to (!A & !B)
test_evaluator("!(A , B)", c("C"), TRUE)
test_evaluator("!A & !B", c("C"), TRUE)

test_evaluator("!(A , B)", c("A"), FALSE)
test_evaluator("!A & !B", c("A"), FALSE)

# ============================================================================
# EVALUATOR TESTS: Complex Real-World Examples
# ============================================================================

cat("\n================== EVALUATOR TESTS: Complex Examples ==================\n")

# Example from requirements: (Urlaub, Wohnmobil) & !HomeOffice
test_evaluator("(Urlaub, Wohnmobil) & !HomeOffice", 
               c("Urlaub"), TRUE)  # Urlaub set, no HomeOffice
test_evaluator("(Urlaub, Wohnmobil) & !HomeOffice", 
               c("Wohnmobil"), TRUE)  # Wohnmobil set, no HomeOffice
test_evaluator("(Urlaub, Wohnmobil) & !HomeOffice", 
               c("Urlaub", "HomeOffice"), FALSE)  # Urlaub set but HomeOffice also set
test_evaluator("(Urlaub, Wohnmobil) & !HomeOffice", 
               c("HomeOffice"), FALSE)  # Neither Urlaub nor Wohnmobil set

# Nested: ((A & B), C) * !D
test_evaluator("((A & B), C) * !D", c("A", "B"), TRUE)  # Left true (A & B), right true (!D) → XOR true
test_evaluator("((A & B), C) * !D", c("D"), FALSE)  # Both false → XOR false
test_evaluator("((A & B), C) * !D", c("D", "A", "B"), FALSE)  # Both true → XOR false

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n\n================== TEST SUMMARY ==================\n")
cat("All tests completed. Check output above for any ❌ FAIL or ❌ ERROR markers.\n")
