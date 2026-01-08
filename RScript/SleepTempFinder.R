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

final_data_matched <- temp_mapped %>% 
  filter(!is.na(Avg_Temp), !is.nan(Avg_Temp), !is.na(Avg_Abs_Hum))

full_dates <- seq(min(temp_mapped$Date, na.rm=T), max(temp_mapped$Date, na.rm=T), by="1 day")
final_data_viz <- temp_mapped %>% complete(Date = full_dates)

excluded_sleep_dates <- sleep_complete %>% 
  filter(is.na(Sleep_Score) | is.na(HRV) | is.na(RHR)) %>% 
  pull(Date) %>% format("%d.%m.%Y")

excluded_sensor_dates <- temp_mapped %>% 
  filter(is.na(Avg_Temp) | is.nan(Avg_Temp) | is.na(Avg_Abs_Hum)) %>% 
  pull(Date) %>% format("%d.%m.%Y")

# --- OUTPUT: AUDIT ---
cat("\n===========================================================\n")
cat("                DATA QUALITY AUDIT\n")
cat("===========================================================\n")
cat(sprintf("Total nights detected:        %d\n", nrow(sleep_df_raw)))
cat(sprintf("Nights used for analysis:     %d\n", nrow(final_data_matched)))
cat(sprintf("Nights excluded total:        %d\n", length(excluded_sleep_dates) + length(excluded_sensor_dates)))
cat("-----------------------------------------------------------\n")
cat(sprintf("Reason: Missing Sleep Data:   %d\n", length(excluded_sleep_dates)))
if(length(excluded_sleep_dates) > 0) cat(paste0("       [", paste(excluded_sleep_dates, collapse = "], ["), "]\n"))
cat(sprintf("Reason: Missing Room Data:    %d\n", length(excluded_sensor_dates)))
if(length(excluded_sensor_dates) > 0) cat(paste0("       [", paste(excluded_sensor_dates, collapse = "], ["), "]\n"))
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

