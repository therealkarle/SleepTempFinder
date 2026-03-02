# studio_commands.R
#
# Utility helpers for interactive use (RStudio).  Source this file in the
# console or add it to your project .Rprofile to make commands available.
# When sourced the script prints a brief summary so you know what helpers
# were defined.

cat("Command Syntax: run_analysis(date=NULL, flags=NULL, sensors=NULL, dry_run=FALSE, filter=NULL)\n\n",
    "  date/flags/sensors may be given positionally or by name; date is\n",
    "  optional and when first it can be supplied without naming.\n\n",
    "  `filter` accepts a raw string (e.g. '2025;Flags=Hochlitten') and\n",
    "  overrides other arguments.\n\n\n",
    "Example: run_analysis('2025'), run_analysis(flags='Hochlitten'),\n",
    "         run_analysis('01.2025', sensors='Wohnwagen')\n")
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
#
# You can also define simple one‑line wrappers below for frequently used
# filters (pre‑configured commands).

run_analysis <- function(date = NULL, flags = NULL, sensors = NULL, dry_run = FALSE, filter = NULL) {
  # signature arranged so the first positional argument is `date`.
  # `filter` is now last and should only be used when you want to pass a
  # raw filter expression; otherwise the named args form the preferred API.
  #
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
  # ensure we can find it regardless of current working directory
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
  if (!file.exists(script_path)) {
    stop("cannot locate SleepTempFinder.R; please make sure you're in the project or that the 'RScript' folder exists")
  }
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


run_winter2026 <- function() run_analysis('02.2026')
run_wohnwagen <- function() run_analysis('Sensors=Wohnwagen')
run_hochlitten <- function() run_analysis('Flags=Hochlitten')

# The above functions are simple wrappers; you may create your own presets
# or call run_analysis() directly.  If you prefer RStudio Addins, point them
# at any of these functions by adding an addins.dcf entry (see RStudio docs).
