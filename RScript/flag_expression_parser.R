# Flag Expression Parser
# Parses boolean flag expressions with support for:
#   - OR (,)
#   - AND (&)
#   - XOR (*)
#   - NOT (!)
#   - Parentheses for grouping
#
# Operator Precedence (highest to lowest):
#   ! (NOT)
#   * (XOR)
#   & (AND)
#   , (OR)
#
# Usage:
#   ast <- parse_flag_expression("(Urlaub, Wohnmobil) & !HomeOffice")
#   result <- evaluate_flag_ast(ast, c("Urlaub", "HomeOffice"))  # FALSE (both present, but need NOT HomeOffice)

# ============================================================================
# Token types for lexer
# ============================================================================

TOKEN_FLAG <- "FLAG"
TOKEN_LPAREN <- "LPAREN"
TOKEN_RPAREN <- "RPAREN"
TOKEN_AND <- "AND"
TOKEN_OR <- "OR"
TOKEN_XOR <- "XOR"
TOKEN_NOT <- "NOT"
TOKEN_EOF <- "EOF"

# ============================================================================
# Lexer: String -> Token list
# ============================================================================

#' Tokenize a flag expression string
#'
#' @param expr Character string like "(Urlaub, Wohnmobil) & !HomeOffice"
#'
#' @return List of tokens with structure: list(type = "...", value = "...", pos = 123)
#'
#' @examples
#' tokenize_flag_expression("A & !B")
#'
#' @export
tokenize_flag_expression <- function(expr) {
  if (!is.character(expr) || length(expr) != 1) {
    stop("expr must be a single character string")
  }
  
  tokens <- list()
  pos <- 1
  expr_len <- nchar(expr)
  
  while (pos <= expr_len) {
    # Skip whitespace
    if (grepl("^\\s", substr(expr, pos, expr_len))) {
      pos <- pos + 1
      next
    }
    
    # Single-character tokens
    char <- substr(expr, pos, pos)
    
    if (char == "(") {
      tokens[[length(tokens) + 1]] <- list(type = TOKEN_LPAREN, value = "(", pos = pos)
      pos <- pos + 1
      next
    }
    if (char == ")") {
      tokens[[length(tokens) + 1]] <- list(type = TOKEN_RPAREN, value = ")", pos = pos)
      pos <- pos + 1
      next
    }
    if (char == "!") {
      tokens[[length(tokens) + 1]] <- list(type = TOKEN_NOT, value = "!", pos = pos)
      pos <- pos + 1
      next
    }
    if (char == "&") {
      tokens[[length(tokens) + 1]] <- list(type = TOKEN_AND, value = "&", pos = pos)
      pos <- pos + 1
      next
    }
    if (char == ",") {
      tokens[[length(tokens) + 1]] <- list(type = TOKEN_OR, value = ",", pos = pos)
      pos <- pos + 1
      next
    }
    if (char == "*") {
      tokens[[length(tokens) + 1]] <- list(type = TOKEN_XOR, value = "*", pos = pos)
      pos <- pos + 1
      next
    }
    
    # Flag names: alphanumeric + underscore
    if (grepl("[a-zA-Z0-9_]", char)) {
      start <- pos
      while (pos <= expr_len && grepl("[a-zA-Z0-9_]", substr(expr, pos, pos))) {
        pos <- pos + 1
      }
      flag_name <- substr(expr, start, pos - 1)
      tokens[[length(tokens) + 1]] <- list(type = TOKEN_FLAG, value = flag_name, pos = start)
      next
    }
    
    # Unknown character
    stop(sprintf(
      "Unexpected character '%s' at position %d in expression: %s",
      char, pos, expr
    ))
  }
  
  # Add EOF token
  tokens[[length(tokens) + 1]] <- list(type = TOKEN_EOF, value = "", pos = expr_len + 1)
  
  return(tokens)
}

# ============================================================================
# Parser: Token list -> AST
# ============================================================================

#' Parse a flag expression into an Abstract Syntax Tree (AST)
#'
#' @param expr Character string like "(Urlaub, Wohnmobil) & !HomeOffice"
#'
#' @return AST structure: list(type = "...", left = ..., right = ...) or
#'         list(type = "flag", name = "FlagName") or
#'         list(type = "not", arg = ...)
#'
#' @examples
#' ast <- parse_flag_expression("A & !B")
#' str(ast)
#'
#' @export
parse_flag_expression <- function(expr) {
  if (!is.character(expr) || length(expr) != 1) {
    stop("expr must be a single character string")
  }
  
  tokens <- tokenize_flag_expression(expr)
  parser <- list(tokens = tokens, pos = 1)
  
  # Start parsing from lowest precedence (OR)
  ast <- parse_or_expr(parser)
  
  # Verify we consumed all tokens
  if (parser$pos <= length(parser$tokens)) {
    current <- parser$tokens[[parser$pos]]
    if (current$type != TOKEN_EOF) {
      stop(sprintf(
        "Unexpected token '%s' at position %d",
        current$value, current$pos
      ))
    }
  }
  
  return(ast)
}

#' Helper: get current token
#' @keywords internal
current_token <- function(parser) {
  if (parser$pos > length(parser$tokens)) {
    return(list(type = TOKEN_EOF, value = "", pos = -1))
  }
  return(parser$tokens[[parser$pos]])
}

#' Helper: advance to next token
#' @keywords internal
advance_token <- function(parser) {
  parser$pos <- parser$pos + 1
  return(parser)
}

#' Helper: expect and consume a specific token type
#' @keywords internal
expect_token <- function(parser, expected_type) {
  token <- current_token(parser)
  if (token$type != expected_type) {
    stop(sprintf(
      "Expected token %s but got %s at position %d",
      expected_type, token$type, token$pos
    ))
  }
  return(advance_token(parser))
}

# OR expression (lowest precedence: ,)
# or_expr := and_expr ( "," and_expr )*
#' @keywords internal
parse_or_expr <- function(parser) {
  left <- parse_and_expr(parser)
  
  while (current_token(parser)$type == TOKEN_OR) {
    parser <- advance_token(parser)
    right <- parse_and_expr(parser)
    left <- list(type = "or", left = left, right = right)
  }
  
  return(left)
}

# AND expression (mid precedence: &)
# and_expr := xor_expr ( "&" xor_expr )*
#' @keywords internal
parse_and_expr <- function(parser) {
  left <- parse_xor_expr(parser)
  
  while (current_token(parser)$type == TOKEN_AND) {
    parser <- advance_token(parser)
    right <- parse_xor_expr(parser)
    left <- list(type = "and", left = left, right = right)
  }
  
  return(left)
}

# XOR expression (higher precedence: *)
# xor_expr := not_expr ( "*" not_expr )*
#' @keywords internal
parse_xor_expr <- function(parser) {
  left <- parse_not_expr(parser)
  
  while (current_token(parser)$type == TOKEN_XOR) {
    parser <- advance_token(parser)
    right <- parse_not_expr(parser)
    left <- list(type = "xor", left = left, right = right)
  }
  
  return(left)
}

# NOT expression (highest precedence: !)
# not_expr := ( "!" )* primary
#' @keywords internal
parse_not_expr <- function(parser) {
  if (current_token(parser)$type == TOKEN_NOT) {
    parser <- advance_token(parser)
    arg <- parse_not_expr(parser)  # Right-associative: ! ! A
    return(list(type = "not", arg = arg))
  }
  
  return(parse_primary(parser))
}

# Primary expression: flag name or parenthesized expression
# primary := FLAG | "(" or_expr ")"
#' @keywords internal
parse_primary <- function(parser) {
  token <- current_token(parser)
  
  if (token$type == TOKEN_FLAG) {
    parser <- advance_token(parser)
    return(list(type = "flag", name = token$value))
  }
  
  if (token$type == TOKEN_LPAREN) {
    parser <- advance_token(parser)
    ast <- parse_or_expr(parser)
    parser <- expect_token(parser, TOKEN_RPAREN)
    return(ast)
  }
  
  stop(sprintf(
    "Expected FLAG or '(' but got %s at position %d",
    token$type, token$pos
  ))
}

# ============================================================================
# Evaluator: AST + flags -> boolean
# ============================================================================

#' Evaluate a flag expression AST against a set of flags
#'
#' @param ast Abstract Syntax Tree from parse_flag_expression()
#' @param row_flags Character vector of flags present in current row
#'                   Example: c("Urlaub", "HomeOffice")
#'
#' @return Logical: TRUE if expression is satisfied, FALSE otherwise
#'
#' @examples
#' ast <- parse_flag_expression("(Urlaub, Wohnmobil) & !HomeOffice")
#' evaluate_flag_ast(ast, c("Urlaub"))  # TRUE
#' evaluate_flag_ast(ast, c("Urlaub", "HomeOffice"))  # FALSE
#'
#' @export
evaluate_flag_ast <- function(ast, row_flags) {
  if (!is.list(ast)) {
    stop("ast must be a list from parse_flag_expression()")
  }
  
  if (!is.vector(row_flags)) {
    stop("row_flags must be a vector")
  }
  
  # Normalize row_flags: remove NA, convert to character, unique
  row_flags <- as.character(na.omit(row_flags))
  row_flags <- unique(row_flags)
  
  return(.evaluate_ast_recursive(ast, row_flags))
}

#' Internal recursive evaluator
#' @keywords internal
.evaluate_ast_recursive <- function(ast, row_flags) {
  if (!is.list(ast) || length(ast) == 0 || !("type" %in% names(ast))) {
    stop("Invalid AST structure")
  }
  
  type <- ast$type
  
  if (type == "flag") {
    # Simple flag: check if present in row_flags
    return(ast$name %in% row_flags)
  }
  
  if (type == "not") {
    # Negation: flip the result
    return(!.evaluate_ast_recursive(ast$arg, row_flags))
  }
  
  if (type == "or") {
    # OR: left OR right
    left_result <- .evaluate_ast_recursive(ast$left, row_flags)
    right_result <- .evaluate_ast_recursive(ast$right, row_flags)
    return(left_result || right_result)
  }
  
  if (type == "and") {
    # AND: left AND right
    left_result <- .evaluate_ast_recursive(ast$left, row_flags)
    right_result <- .evaluate_ast_recursive(ast$right, row_flags)
    return(left_result && right_result)
  }
  
  if (type == "xor") {
    # XOR: exactly one must be true
    # For binary: (A && !B) || (!A && B)
    # For chained (A * B * C): convert to multi-way XOR
    # Multi-way XOR = "exactly one operand is true"
    left_result <- .evaluate_ast_recursive(ast$left, row_flags)
    right_result <- .evaluate_ast_recursive(ast$right, row_flags)
    return(xor(left_result, right_result))
  }
  
  stop(sprintf("Unknown AST node type: %s", type))
}

# ============================================================================
# Convenience function: evaluate expression string directly
# ============================================================================

#' Evaluate a flag expression string against a set of flags
#'
#' Combines parsing and evaluation in one step.
#'
#' @param expr Character string like "(Urlaub, Wohnmobil) & !HomeOffice"
#' @param row_flags Character vector of flags
#'
#' @return Logical: TRUE if expression is satisfied
#'
#' @examples
#' evaluate_flag_expression("A, B", c("A"))  # TRUE
#' evaluate_flag_expression("A & B", c("A"))  # FALSE
#'
#' @export
evaluate_flag_expression <- function(expr, row_flags) {
  if (!is.character(expr) || length(expr) != 1) {
    stop("expr must be a single character string")
  }
  
  ast <- parse_flag_expression(expr)
  return(evaluate_flag_ast(ast, row_flags))
}
