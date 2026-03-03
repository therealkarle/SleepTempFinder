# studio_commands.R
#
# Utility helpers for interactive use (RStudio).  Source this file in the
# console or add it to your project .Rprofile to make commands available.
# When sourced the script prints a brief summary so you know what helpers
# were defined.

# store the directory containing this helper script; `sys.frame(1)$ofile`
# should point at the file when it is sourced.  This lets run_analysis
# locate `SleepTempFinder.R` relative to the helper even if the working
# directory is completely unrelated (a common RStudio situation).
.helper_dir <- {
  path <- NULL
  if (!is.null(sys.frame(1)$ofile) && nzchar(sys.frame(1)$ofile)) {
    path <- normalizePath(sys.frame(1)$ofile, mustWork = FALSE)
  }
  if (is.null(path) || path == "") {
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      doc <- rstudioapi::getActiveDocumentContext()
      if (!is.null(doc$path) && nzchar(doc$path)) {
        path <- normalizePath(doc$path, mustWork = FALSE)
      }
    }
  }
  if (is.null(path) || path == "") NA_character_ else dirname(path)
}

cat("Command Syntax: run_analysis(date=NULL, flags=NULL, sensors=NULL, dry_run=FALSE, filter=NULL)\n\n",
    "  date/flags/sensors may be given positionally or by name; date is\n",
    "  optional and when first it can be supplied without naming.\n\n",
    "  `filter` accepts a raw string (e.g. '2025;Flags=Hochlitten' or 'temp>18').\n",
    "  and overrides other arguments.  \n\n\n",
    "  The filter grammar now supports logical expressions on any numeric column (e.g. SleepScore>80, 18<temp<22).\n",
    "  Unquoted comparison expressions can also be supplied \n",
    "  – the first argument is treated as a filter\n",
    "    if it looks like a logical test (run_analysis(temp>18)).\n\n",
    "Example: run_analysis('2025'), run_analysis(flags='Hochlitten'), run_analysis(filter='temp>18'),\n",
    "         run_analysis('01.2025', sensors='Wohnwagen'), run_analysis(filter='SleepScore>80')\n")
# The primary helper is `run_analysis()`, which sets an environment variable
# that the main script reads when in interactive mode.  (Recent updates also
# ensure any duplicated sleep records for the same calendar date are collapsed
# before analysis; audit output will report the number of unique dates.)
#
# Example usage in RStudio console:
#   source('RScript/studio_commands.R')
#
#   # original style: supply complete filter string
#   run_analysis('2025')                       # run entire 2025 dataset (date arg)
#   run_analysis('q1.2025;Flags=Hochlitten')   # first quarter + flag
#   run_analysis('Sensors=Wohnwagen')         # Wohnwagen sensor only
#
#   # named-argument style (shorthand date allowed because it is first)
#   run_analysis('2025')                       # equivalent to date='2025'
#   run_analysis(flags = 'Hochlitten')         # specify only flags
#   run_analysis(sensors = c('Wohnwagen','FlorianZimmerSensor'))
#   run_analysis('01.2025', flags = 'Quiet,Vacation')  # date + flags
#   run_analysis(date = '01.2025', sensors = 'Wohnwagen')  # explicit name
#   run_analysis(filter = 'Flags=Hochlitten|Urlaub')  # raw string overrides others
#   run_analysis(filter = 'temp>18')              # keep nights with average temp > 18°C
#   run_analysis(filter = 'SleepScore>80')         # biomarker filter example
#
# You can also define simple one‑line wrappers below for frequently used
# filters (pre‑configured commands).

run_analysis <- function(date = NULL, flags = NULL, sensors = NULL, dry_run = FALSE, filter = NULL) {
  # signature arranged so the first positional argument is `date`.
  # `filter` is now last and should only be used when you want to pass a
  # raw filter expression; otherwise the named args form the preferred API.
  #
  # support unquoted expressions in `date` or `filter` by deparsing the
  # promise.  this allows `run_analysis(temp>18)` (or equivalently
  # `run_analysis(filter=temp>18)`) to work without needing manual quotes.
  date_expr <- substitute(date)
  filter_expr <- substitute(filter)
  if (is.null(filter) && is.call(date_expr)) {
    # treat a bare call passed as first arg as if it were the filter string
    filter <- deparse(date_expr)
    date <- NULL
  }
  if (!is.null(filter) && !is.character(filter)) {
    # convert non-character filter values (e.g. expressions) to string
    filter <- deparse(filter_expr)
  }

  # Users may call `run_analysis("2025")` and the value will populate
  # `date` automatically.
  #
  # When `filter` is provided it bypasses the other arguments and,
  # if non-NULL, is used verbatim.
  if (is.null(filter)) {
    parts <- character(0)
    if (!is.null(date)) {
      parts <- c(parts, date)
    }
    if (!is.null(flags)) {
      if (length(flags) > 1) flags <- paste(flags, collapse = ",")
      parts <- c(parts, paste0("Flags=", flags))
    }
    if (!is.null(sensors)) {
      if (length(sensors) > 1) sensors <- paste(sensors, collapse = ",")
      parts <- c(parts, paste0("Sensors=", sensors))
    }
    if (length(parts) > 0) {
      filter <- paste(parts, collapse = ";")
    }
  }
  # compose CLI-style args and export to environment variable
  args <- character(0)
  if (dry_run) args <- c(args, "--dry-run")
  if (!is.null(filter)) {
    # remove whitespace so the split-on-whitespace in the main script
    # doesn't break when the filter contains spaces (e.g. "temp > 18").
    filter <- gsub("\\s+", "", filter)
    args <- c(args, paste0("--filter=", filter))
  }
  prev_args <- Sys.getenv("_STF_ARGS_", unset = NA_character_)
  on.exit({
    if (is.na(prev_args)) {
      Sys.unsetenv("_STF_ARGS_")
    } else {
      Sys.setenv("_STF_ARGS_" = prev_args)
    }
  }, add = TRUE)
  Sys.setenv("_STF_ARGS_" = paste(args, collapse = " "))
  # load & execute the main script; it will pick up the env var above
  # we first locate the file (searching upward if needed) then switch into
  # its directory so relative paths inside SleepTempFinder.R resolve correctly.
  # try helper location first (may be NA if we couldn't detect it)
  script_path <- NA_character_
  if (!is.na(.helper_dir)) {
    candidate <- file.path(.helper_dir, "SleepTempFinder.R")
    if (file.exists(candidate)) {
      script_path <- candidate
    }
  }
  # if still not found and we're in RStudio, try project root
  if (is.na(script_path) &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("getActiveProject")) {
    proj <- rstudioapi::getActiveProject()
    if (!is.null(proj) && nzchar(proj)) {
      candidate <- file.path(proj, "RScript", "SleepTempFinder.R")
      if (file.exists(candidate)) script_path <- candidate
    }
  }
  # otherwise fall back to traditional lookup relative to cwd
  if (is.na(script_path)) {
    script_path <- "RScript/SleepTempFinder.R"
    if (!file.exists(script_path)) {
      # search upward from the current working directory until we find the file
      find_upward <- function(name) {
        d <- normalizePath(getwd(), mustWork = FALSE)
        repeat {
          candidate <- file.path(d, name)
          if (file.exists(candidate)) return(candidate)
          parent <- dirname(d)
          if (parent == d) break
          d <- parent
        }
        NULL
      }
      alt <- find_upward("RScript/SleepTempFinder.R")
      if (!is.null(alt)) script_path <- alt
    }
  }
  if (!file.exists(script_path)) {
    stop("cannot locate SleepTempFinder.R; please make sure you're in the project or that the 'RScript' folder exists")
  }
  # change into script directory for the duration of the call
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(dirname(script_path))
  source(script_path)
}

# convenience presets (modify or add as desired):

# helper: if you're editing a file in RStudio, call this to make the
# working directory the folder containing that file.  Useful before
# running `run_analysis()` so the relative paths resolve automatically.
setwd_to_active <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    doc <- rstudioapi::getActiveDocumentContext()
    wd <- dirname(doc$path)
    if (nzchar(wd)) {
      setwd(wd)
      message("working directory set to ", wd)
      return(invisible(wd))
    }
  }
  stop("rstudioapi not available or no active document")
}

run_clear_filter <- function() {
  Sys.unsetenv("_STF_ARGS_")
  message("Temporary analysis filter cleared (_STF_ARGS_ removed).")
  invisible(TRUE)
}


run_winter2026 <- function() run_analysis('02.2026')
run_wohnwagen <- function() run_analysis('Sensors=Wohnwagen')
run_hochlitten <- function() run_analysis('Flags=Hochlitten')

# The above functions are simple wrappers; you may create your own presets
# or call run_analysis() directly.  If you prefer RStudio Addins, point them
# at any of these functions by adding an addins.dcf entry (see RStudio docs).
