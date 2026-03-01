library(stringr)

parse_sensor_flags <- function(text_value) {
  text_value <- text_value %||% ""
  sensor_match <- str_match(text_value, regex("sensor\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))
  flags_match <- str_match(text_value, regex("flags?\\s*=\\s*([^;\\n]+)", ignore_case = TRUE))

  sensor_raw <- str_trim(sensor_match[, 2] %||% NA_character_)
  sensor_name <- sensor_raw
  if (!is.na(sensor_name) && str_detect(sensor_name, "/")) {
    sensor_name <- str_trim(str_split(sensor_name, "/", n = 2)[[1]][2])
  }

  flags <- character(0)
  if (!is.na(flags_match[, 2] %||% NA_character_)) {
    flags <- split_flags(flags_match[, 2])
  }

  list(sensor_raw = ifelse(is.na(sensor_raw) || sensor_raw == "", NA_character_, sensor_raw),
       sensor_name = ifelse(is.na(sensor_name) || sensor_name == "", NA_character_, sensor_name),
       flags = flags)
}

trim_vector <- function(x) { x <- str_trim(as.character(x)); x[x != "" & !is.na(x)] }

split_flags <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(character(0))
  sort(unique(trim_vector(str_split(x, "\\s*,\\s*")[[1]])))
}

cats <- c("Sensor=foo; Flags=bar",
           "Sensor=foo;flags=baz,qux",
           "sensor=foo; flags=bar; other",
           "Sensor=foo; something; Flags=bar",
           "Sensor=foo;Flags=bar;sensor=other;Flags=baz",
           "sensor=foo ; flags = a, b, c")
for (s in cats) {
  print(paste(s, '->', toString(parse_sensor_flags(s))))
}
