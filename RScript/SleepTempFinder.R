# --- 1. ENVIRONMENT SETUP ---
if (!require("rstudioapi")) install.packages("rstudioapi")
pkgs <- c("tidyverse", "lubridate", "yaml", "broom", "GGally", "gridExtra", "grid", "scales")
for (pkg in pkgs) { 
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE) 
}

if (interactive()) setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
config <- read_yaml("config.yaml")

# --- 2. DATA CLEANING & LOADING ---
read_garmin_fixed <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines[1] <- gsub("^[^\t[:alnum:][:punct:][:space:]]+", "", lines[1])
  lines <- gsub("([+-]\\d+),(\\d+°)", "\\1.\\2", lines)
  lines <- gsub(",+$", "", lines)
  df <- read.csv(text = lines, sep = ",", header = TRUE, 
                 check.names = FALSE, stringsAsFactors = FALSE, 
                 na.strings = c(" ", "--", "NA", ""))
  return(as_tibble(df, .name_repair = "unique"))
}

clean_val_final <- function(x) {
  res <- map_dbl(as.character(x), function(val) {
    if (is.na(val) || val == "" || val == "--") return(NA_real_)
    if (str_detect(val, "h")) {
      h <- as.numeric(str_extract(val, "\\d+(?=h)"))
      m <- as.numeric(str_extract(val, "\\d+(?=min)"))
      return(ifelse(is.na(h), 0, h) + (ifelse(is.na(m), 0, m)/60))
    }
    num_str <- str_extract(val, "[-+]?[0-9]*\\.?[0-9]+")
    return(as.numeric(num_str))
  })
  return(res)
}

mapping <- config$column_names
sleep_df_raw <- map_df(config$sleep_data_sources, function(f) {
  read_garmin_fixed(file.path(config$data_directory, f)) %>%
    mutate(Date = as.Date(!!sym(mapping$garmin_date)),
           bedtime = parse_date_time(!!sym(mapping$garmin_bedtime), orders = c("H:M", "HM")),
           waketime = parse_date_time(!!sym(mapping$garmin_waketime), orders = c("H:M", "HM"))) %>%
    mutate(waketime = update(waketime, year = year(Date), month = month(Date), mday = day(Date)),
           bedtime = update(bedtime, year = year(Date), month = month(Date), mday = day(Date)),
           bedtime = if_else(bedtime > waketime, bedtime - days(1), bedtime)) %>%
    mutate(across(any_of(unlist(mapping[4:length(mapping)])), clean_val_final))
})

sensor_raw <- map_df(config$usage_timeline, function(x) {
  f_info <- config$temp_files[[x$file_id]]
  read_delim(file.path(config$data_directory, f_info$path), delim = ",", locale = locale(decimal_mark = ","), show_col_types = FALSE) %>%
    rename(timestamp = !!f_info$col_time, room_temp = !!f_info$col_temp, rel_hum = !!f_info$col_hum, abs_hum = `Abs Humidity(g/m³)`) %>% 
    mutate(timestamp = parse_date_time(timestamp, orders = c("d/m/Y H:M", "dmY HM", "Ymd HMS")))
})

# --- OUTLIER FILTERING (optional: configured in config.yaml) ---
apply_outlier_filter <- function(df, cols = c("room_temp","rel_hum","abs_hum"), method = "iqr", iqr_mult = 1.5, z_thresh = 3) {
  for(col in cols) {
    if(!col %in% names(df)) next
    vals <- df[[col]]
    if(method == "iqr") {
      Q1 <- quantile(vals, 0.25, na.rm=TRUE)
      Q3 <- quantile(vals, 0.75, na.rm=TRUE)
      IQRv <- Q3 - Q1
      lower <- Q1 - iqr_mult * IQRv
      upper <- Q3 + iqr_mult * IQRv
      df <- df %>% filter(is.na(.data[[col]]) | (.data[[col]] >= lower & .data[[col]] <= upper))
    } else if(method == "zscore") {
      m <- mean(vals, na.rm=TRUE)
      s <- sd(vals, na.rm=TRUE)
      if(is.na(s) || s == 0) next
      df <- df %>% filter(is.na(.data[[col]]) | (abs((.data[[col]] - m)/s) <= z_thresh))
    }
  }
  return(df)
}

# Track dates removed by outlier filtering (both sensor and nightly stages)
excluded_outlier_dates <- c()

if(!is.null(config$outlier_filter) && isTRUE(config$outlier_filter$enabled)) {
  cols_cfg <- if(!is.null(config$outlier_filter$columns)) unlist(config$outlier_filter$columns) else c("room_temp","rel_hum","abs_hum")
  method_cfg <- if(!is.null(config$outlier_filter$method)) config$outlier_filter$method else "iqr"
  iqr_cfg <- if(!is.null(config$outlier_filter$iqr_multiplier)) config$outlier_filter$iqr_multiplier else 1.5
  z_cfg <- if(!is.null(config$outlier_filter$z_threshold)) config$outlier_filter$z_threshold else 3
  stage_cfg <- if(!is.null(config$outlier_filter$apply_stage)) config$outlier_filter$apply_stage else "sensor"
  if(tolower(stage_cfg) == "sensor") {
    cfg_cols_exist <- intersect(cols_cfg, names(sensor_raw))
    # Keep a copy of the raw sensor data before filtering so we can detect nights that lost nightly averages
    sensor_raw_before <- sensor_raw

    # Count per-night sensor readings and valid values before filtering (for configured cols)
    sensor_before <- sensor_raw_before %>%
      mutate(Date = as.Date(timestamp - hours(12))) %>%
      group_by(Date) %>%
      summarise(across(all_of(cfg_cols_exist), ~sum(!is.na(.x)), .names = "valid_{col}"), n = n(), .groups = 'drop')

    n_before <- nrow(sensor_raw_before)
    sensor_raw <- apply_outlier_filter(sensor_raw, cols_cfg, method_cfg, iqr_cfg, z_cfg)
    n_after <- nrow(sensor_raw)

    # Count valid values after filtering
    sensor_after <- sensor_raw %>%
      mutate(Date = as.Date(timestamp - hours(12))) %>%
      group_by(Date) %>%
      summarise(across(all_of(cfg_cols_exist), ~sum(!is.na(.x)), .names = "valid_{col}"), n = n(), .groups = 'drop')

    # Identify dates where there were valid values before, but none after (for any configured column)
    if(nrow(sensor_before) > 0) {
      valid_before_mat <- sensor_before %>% select(Date, starts_with("valid_"))
      valid_after_mat <- sensor_after %>% select(Date, starts_with("valid_"))

      valid_before_mat$has_valid_before <- apply(valid_before_mat %>% select(-Date), 1, function(r) any(r > 0))
      if(nrow(valid_after_mat) > 0) {
        valid_after_mat$has_valid_after <- apply(valid_after_mat %>% select(-Date), 1, function(r) any(r > 0))
      } else {
        valid_after_mat <- tibble(Date = as.Date(character(0)), has_valid_after = logical(0))
      }

      merged_val <- valid_before_mat %>% left_join(valid_after_mat, by = "Date") %>% mutate(has_valid_after = ifelse(is.na(has_valid_after), FALSE, has_valid_after))
      dates_lost_sensor <- merged_val %>% filter(has_valid_before == TRUE & has_valid_after == FALSE) %>% pull(Date)

      # Extra check via nightly averages: if nightly avg existed before but is NA after, treat as outlier-removed
      sensor_nightly_before <- sensor_raw_before %>%
        mutate(Date = as.Date(timestamp - hours(12))) %>%
        group_by(Date) %>%
        summarise(Avg_Temp = mean(room_temp, na.rm=TRUE), Avg_Rel_Hum = mean(rel_hum, na.rm=TRUE), Avg_Abs_Hum = mean(abs_hum, na.rm=TRUE), .groups = 'drop')

      sensor_nightly_after <- sensor_raw %>%
        mutate(Date = as.Date(timestamp - hours(12))) %>%
        group_by(Date) %>%
        summarise(Avg_Temp = mean(room_temp, na.rm=TRUE), Avg_Rel_Hum = mean(rel_hum, na.rm=TRUE), Avg_Abs_Hum = mean(abs_hum, na.rm=TRUE), .groups = 'drop')

      # Determine nights where any nightly average existed before filtering but is NA/NaN after filtering
      merged_nightly <- sensor_nightly_before %>% left_join(sensor_nightly_after, by = "Date", suffix = c("_before", "_after"))

      cols_to_check <- c("Avg_Temp", "Avg_Rel_Hum", "Avg_Abs_Hum")
      cols_present <- intersect(cols_to_check, names(merged_nightly))

      if(length(cols_present) > 0) {
        lost_mat <- sapply(cols_present, function(col) {
          before <- merged_nightly[[paste0(col, "_before")]]
          after <- merged_nightly[[paste0(col, "_after")]]
          (!is.na(before)) & (is.na(after) | is.nan(after))
        })
        if(is.matrix(lost_mat)) {
          lost_rows <- apply(lost_mat, 1, any)
        } else {
          lost_rows <- as.logical(lost_mat)
        }
        nights_lost_by_avg <- merged_nightly$Date[which(lost_rows)]
      } else {
        nights_lost_by_avg <- as.Date(character(0))
      }

      dates_to_mark <- unique(c(dates_lost_sensor, nights_lost_by_avg))

      if(length(dates_to_mark) > 0) {
        removed_dates_fmt <- format(dates_to_mark, "%d.%m.%Y")
        excluded_outlier_dates <- unique(c(excluded_outlier_dates, removed_dates_fmt))
        cat(sprintf("Outlier filter (sensor) removed data for nights (all values NA after filtering): %s\n", paste(removed_dates_fmt, collapse = ", ")))
      }
    }

    cat(sprintf("Outlier filter enabled (sensor): removed %d sensor rows\n", n_before - n_after))
  } else {
    cat(sprintf("Outlier filter enabled but set to apply at '%s' stage\n", stage_cfg))
  }
}

# --- 3. DATA PREP & AUDIT ---
sleep_complete <- sleep_df_raw %>%
  rename(Sleep_Score = !!sym(mapping$garmin_sleep_score), HRV = !!sym(mapping$garmin_hrv), RHR = !!sym(mapping$garmin_rhr))

sleep_filtered <- sleep_complete %>% 
  filter(!is.na(Sleep_Score), !is.na(HRV), !is.na(RHR))

temp_mapped <- sleep_filtered %>% 
  rowwise() %>% 
  mutate(Avg_Temp = mean(sensor_raw$room_temp[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T),
         Avg_Rel_Hum = mean(sensor_raw$rel_hum[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T),
         Avg_Abs_Hum = mean(sensor_raw$abs_hum[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T)) %>%
  ungroup()

# --- OUTLIER FILTERING (nightly stage) ---
if(!is.null(config$outlier_filter) && isTRUE(config$outlier_filter$enabled)) {
  stage_cfg <- if(!is.null(config$outlier_filter$apply_stage)) config$outlier_filter$apply_stage else "sensor"
  if(tolower(stage_cfg) == "nightly") {
    cols_nightly <- if(!is.null(config$outlier_filter$columns)) unlist(config$outlier_filter$columns) else c("room_temp","rel_hum","abs_hum")
    # Map sensor column names to nightly average column names
    cols_nightly_mapped <- sapply(cols_nightly, function(c) {
      if(c == "room_temp") return("Avg_Temp")
      if(c == "rel_hum") return("Avg_Rel_Hum")
      if(c == "abs_hum") return("Avg_Abs_Hum")
      return(c)
    })
    method_cfg <- if(!is.null(config$outlier_filter$method)) config$outlier_filter$method else "iqr"
    iqr_cfg <- if(!is.null(config$outlier_filter$iqr_multiplier)) config$outlier_filter$iqr_multiplier else 1.5
    z_cfg <- if(!is.null(config$outlier_filter$z_threshold)) config$outlier_filter$z_threshold else 3
    temp_mapped_before <- temp_mapped
    n_before <- nrow(temp_mapped_before)
    temp_mapped <- apply_outlier_filter(temp_mapped, cols = cols_nightly_mapped, method = method_cfg, iqr_mult = iqr_cfg, z_thresh = z_cfg)
    n_after <- nrow(temp_mapped)

    # Identify nights explicitly removed by nightly outlier filtering
    removed_dates_nightly <- setdiff(temp_mapped_before$Date, temp_mapped$Date)
    if(length(removed_dates_nightly) > 0) {
      removed_fmt <- format(removed_dates_nightly, "%d.%m.%Y")
      excluded_outlier_dates <- unique(c(excluded_outlier_dates, removed_fmt))
      cat(sprintf("Outlier filter (nightly) removed nights: %s\n", paste(removed_fmt, collapse = ", ")))
    }

    cat(sprintf("Outlier filter enabled (nightly): removed %d nights\n", n_before - n_after))
  }
}

final_data_matched <- temp_mapped %>% 
  filter(!is.na(Avg_Temp), !is.nan(Avg_Temp), !is.na(Avg_Abs_Hum))

full_dates <- seq(min(temp_mapped$Date, na.rm=T), max(temp_mapped$Date, na.rm=T), by="1 day")
final_data_viz <- temp_mapped %>% complete(Date = full_dates)

# --- EXCLUDE: Define reasons (Missing Sleep, Missing Room, Outliers) ---
excluded_sleep_dates <- sleep_complete %>% 
  filter(is.na(Sleep_Score) | is.na(HRV) | is.na(RHR)) %>% 
  pull(Date)

excluded_sensor_dates_all <- temp_mapped %>% 
  filter(is.na(Avg_Temp) | is.nan(Avg_Temp) | is.na(Avg_Abs_Hum)) %>% 
  pull(Date)

# Convert any collected outlier strings back to Date objects for set operations
excluded_outlier_dates_dates <- if(length(excluded_outlier_dates) > 0) as.Date(excluded_outlier_dates, "%d.%m.%Y") else as.Date(character(0))

# Nights that are missing room data but were NOT removed due to outlier-filtering
excluded_sensor_dates <- setdiff(excluded_sensor_dates_all, excluded_outlier_dates_dates)

# Unique total excluded nights across all reasons
unique_excluded_dates <- unique(c(excluded_sleep_dates, excluded_sensor_dates, excluded_outlier_dates_dates))

# Format for printing
excluded_sleep_fmt <- format(excluded_sleep_dates, "%d.%m.%Y")
excluded_sensor_fmt <- format(excluded_sensor_dates, "%d.%m.%Y")
excluded_outlier_fmt <- unique(excluded_outlier_dates)

# --- OUTPUT: AUDIT ---
cat("\n===========================================================\n")
cat("                DATA QUALITY AUDIT\n")
cat("===========================================================\n")
cat(sprintf("Total nights detected:        %d\n", nrow(sleep_df_raw)))
cat(sprintf("Nights used for analysis:     %d\n", nrow(final_data_matched)))
cat(sprintf("Nights excluded total:        %d\n", length(unique_excluded_dates)))
cat("-----------------------------------------------------------\n")
cat(sprintf("Reason: Missing Sleep Data:   %d\n", length(excluded_sleep_fmt)))
if(length(excluded_sleep_fmt) > 0) cat(paste0("       [", paste(excluded_sleep_fmt, collapse = "], ["), "]\n"))
cat(sprintf("Reason: Missing Room Data:    %d\n", length(excluded_sensor_fmt)))
if(length(excluded_sensor_fmt) > 0) cat(paste0("       [", paste(excluded_sensor_fmt, collapse = "], ["), "]\n"))
cat(sprintf("Reason: Outlier Filtered:     %d\n", length(excluded_outlier_fmt)))
if(length(excluded_outlier_fmt) > 0) cat(paste0("       [", paste(excluded_outlier_fmt, collapse = "], ["), "]\n"))
cat("===========================================================\n\n")

# --- OUTPUT: DESCRIPTIVE STATISTICS ---
sensor_nightly_raw <- sensor_raw %>%
  mutate(Date = as.Date(timestamp - hours(12))) %>% 
  group_by(Date) %>%
  summarise(Avg_Temp = mean(room_temp, na.rm=T), Avg_Rel_Hum = mean(rel_hum, na.rm=T), Avg_Abs_Hum = mean(abs_hum, na.rm=T), .groups = 'drop')

cat("                DESCRIPTIVE STATISTICS\n")
cat("===========================================================\n\n")
cat("TABLE 1: BIOMARKERS (Garmin Data)\n")
bio_vars <- list("Sleep_Score" = "Sleep_Score", "HRV" = "HRV", "RHR" = "RHR")
for(v_name in names(bio_vars)) {
  m_data <- final_data_matched[[bio_vars[[v_name]]]]; r_data <- sleep_complete[[bio_vars[[v_name]]]]
  cat(sprintf("%-15s (Used) | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n", v_name, mean(m_data, na.rm=T), sd(m_data, na.rm=T), min(m_data, na.rm=T), max(m_data, na.rm=T), sum(!is.na(m_data))))
  cat(sprintf("%-15s (Raw)  | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n\n", "", mean(r_data, na.rm=T), sd(r_data, na.rm=T), min(r_data, na.rm=T), max(r_data, na.rm=T), sum(!is.na(r_data))))
}
cat("\nTABLE 2: ROOM DATA (Nightly Averages)\n")
room_vars <- list("Room Temp" = "Avg_Temp", "Rel Humidity" = "Avg_Rel_Hum", "Abs Humidity" = "Avg_Abs_Hum")
for(v_name in names(room_vars)) {
  m_data <- final_data_matched[[room_vars[[v_name]]]]; r_data <- sensor_nightly_raw[[room_vars[[v_name]]]]
  cat(sprintf("%-15s (Used) | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n", v_name, mean(m_data, na.rm=T), sd(m_data, na.rm=T), min(m_data, na.rm=T), max(m_data, na.rm=T), sum(!is.na(m_data))))
  cat(sprintf("%-15s (Raw)  | Mean: %6.2f | SD: %6.2f | Min: %6.2f | Max: %6.2f | n: %d\n\n", "", mean(r_data, na.rm=T), sd(r_data, na.rm=T), min(r_data, na.rm=T), max(r_data, na.rm=T), sum(!is.na(r_data))))
}

# --- 4. DASHBOARD DATAFRAME (TRANSPOSED) ---
dashboard_df <- final_data_viz %>%
  select(Date, Avg_Temp, Avg_Rel_Hum, Avg_Abs_Hum, Sleep_Score, HRV, RHR) %>%
  pivot_longer(cols = -Date, names_to = "Metric", values_to = "Value") %>%
  mutate(Date_Str = format(Date, "%d.%m.%Y")) %>%
  select(-Date) %>%
  pivot_wider(names_from = Date_Str, values_from = Value)

cat("\n>>> DASHBOARD DATAFRAME CREATED (Object: dashboard_df)\n\n")
#print(dashboard_df)


# --- 5. IMPACT ANALYSIS & OPTIMA ---
env_analysis_vars <- list("Room Temp" = list(col="Avg_Temp", unit="°C"), 
                          "Rel Humidity" = list(col="Avg_Rel_Hum", unit="%"),
                          "Abs Humidity" = list(col="Avg_Abs_Hum", unit="g/m³"))
optima_storage <- list()

cat("\n                     SLEEP ANALYSIS\n")
cat("===========================================================\n")
for(env_name in names(env_analysis_vars)) {
  e_col <- env_analysis_vars[[env_name]]$col
  e_unit <- env_analysis_vars[[env_name]]$unit
  cat(sprintf("\n>>> IMPACT OF %s:\n", toupper(env_name)))
  for(m in names(bio_vars)) {
    sub <- final_data_matched %>% filter(!is.na(.data[[e_col]]), !is.na(.data[[m]]))
    if(nrow(sub) < 5) next
    
    sub_model <- sub
    if(m == "RHR") sub_model[[m]] <- -sub_model[[m]] # Invert for peak
    
    fit_poly <- lm(as.formula(paste(m, "~ poly(", e_col, ", 2, raw=TRUE)")), data = sub_model)
    fit_lin <- lm(as.formula(paste(m, "~", e_col)), data = sub)
    
    slope <- coef(fit_lin)[2]
    b <- coef(fit_poly); opt <- -b[2] / (2 * b[3])
    is_peak <- b[3] < 0 && opt >= min(sub[[e_col]]) && opt <= max(sub[[e_col]])
    
    cat(sprintf("  [%s]\n", m))
    if(is_peak) {
      optima_storage[[paste0(env_name, "_", m)]] <- opt
      cat(sprintf("    - Optimal: %.1f %s\n", opt, e_unit))
    } else {
      trend_dir <- if(slope > 0) "increased" else "decreased"
      cat(sprintf("    - No clear optimum. Linear slope: %.2f per %s\n", slope, e_unit))
    }
    cat(sprintf("    - P-Value: %.4f | R-Squared: %.1f%%\n", summary(fit_lin)$coefficients[2,4], summary(fit_poly)$adj.r.squared * 100))
  }
}

cat("\n===========================================================\n")
cat("             DETAILED STATISTICAL EXPLANATION\n")
cat("===========================================================\n")
cat("1. P-VALUE: < 0.05 indicates a statistically significant relationship.\n")
cat("   (How likly is it that the result is pure chance)\n")
cat("2. R-SQUARED: % of sleep variance explained by this environment factor.\n")
cat("3. OPTIMUM: Calculated 'Sweet Spot' based on quadratic regression.\n")
cat("===========================================================\n")


# --- 6. INDIVIDUAL TIMELINE PLOTS ---
metric_list <- c("Avg_Temp", "Avg_Rel_Hum", "Avg_Abs_Hum", "Sleep_Score", "HRV", "RHR")
metric_labels <- c("Room Temp (°C)", "Rel. Humidity (%)", "Abs. Humidity (g/m³)", "Sleep Score (pts)", "HRV (ms)", "RHR (bpm)")
metric_colors <- c("#e67e22", "#3498db", "#9b59b6", "#27ae60", "#2980b9", "#c0392b")

for(i in seq_along(metric_list)) {
  m <- metric_list[i]
  p <- ggplot(final_data_viz, aes(x = Date, y = .data[[m]])) +
    geom_line(color = metric_colors[i], linewidth = 1, na.rm = TRUE) +
    geom_point(color = metric_colors[i], size = 2, na.rm = TRUE) +
    scale_x_date(date_labels = "%d.%m.%Y", breaks = "2 days", minor_breaks = "1 day", expand = expansion(mult = c(0.01, 0.01))) +
    labs(title = metric_labels[i], x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor.x = element_line(color = "grey90"),
          plot.title = element_text(face = "bold", color = metric_colors[i]), plot.margin = margin(10, 10, 20, 10))
  print(p)
}



# --- 7. SCATTER PLOTS & COLORED MATRIX ---
# Individual Scatter Plots
for(env_name in names(env_analysis_vars)) {
  e_col <- env_analysis_vars[[env_name]]$col
  e_unit <- env_analysis_vars[[env_name]]$unit
  for(m in names(bio_vars)) {
    opt <- optima_storage[[paste0(env_name, "_", m)]]
    p <- ggplot(final_data_matched, aes(x = .data[[e_col]], y = .data[[m]])) +
      geom_point(alpha = 0.5, color = "darkgrey") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = metric_colors[match(m, metric_list)], linewidth = 1.2) +
      labs(title = paste(m, "vs", env_name), x = paste(env_name, e_unit), y = m) +
      theme_minimal()
    if(!is.null(opt)) {
      p <- p + geom_vline(xintercept = opt, linetype = "dashed") +
        annotate("text", x = opt, y = Inf, label = paste0(round(opt, 1), e_unit), vjust = 2, fontface = "bold")
    }
    print(p)
  }
}

# 3x3 Matrix Dashboard - COLORED & WITH OPTIMA
matrix_plots <- list()
for(m in names(bio_vars)) {
  m_color <- metric_colors[match(m, metric_list)]
  for(env_name in names(env_analysis_vars)) {
    e_col <- env_analysis_vars[[env_name]]$col
    e_unit <- env_analysis_vars[[env_name]]$unit
    opt <- optima_storage[[paste0(env_name, "_", m)]]
    
    p_mat <- ggplot(final_data_matched, aes(x = .data[[e_col]], y = .data[[m]])) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = m_color, fill = m_color, alpha = 0.1, linewidth = 1) +
      theme_minimal(base_size = 8) + 
      labs(x = e_unit, y = m, title = paste(m, "x", env_name)) +
      theme(plot.title = element_text(size = 7, face = "bold"))
    
    if(!is.null(opt)) {
      p_mat <- p_mat + geom_vline(xintercept = opt, linetype = "dashed", color = "black", alpha = 0.6)
    }
    
    matrix_plots[[length(matrix_plots) + 1]] <- p_mat
  }
}
grid.arrange(grobs = matrix_plots, ncol = 3, top = textGrob("Environmental Impact Matrix (with Optima)", gp=gpar(fontsize=12, font=2)))

