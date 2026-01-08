# --- 1. ENVIRONMENT SETUP ---
if (!require("rstudioapi")) install.packages("rstudioapi")
pkgs <- c("tidyverse", "lubridate", "yaml", "broom", "GGally", "gridExtra", "grid")
for (pkg in pkgs) { 
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE) 
}

if (interactive()) setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
config <- read_yaml("configShiny.yaml")

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

# Load raw Garmin data
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

# Load Sensor data
sensor_raw <- map_df(config$usage_timeline, function(x) {
  f_info <- config$temp_files[[x$file_id]]
  read_delim(file.path(config$data_directory, f_info$path), delim = ",", locale = locale(decimal_mark = ","), show_col_types = FALSE) %>%
    rename(timestamp = !!f_info$col_time, 
           room_temp = !!f_info$col_temp, 
           rel_hum = !!f_info$col_hum, 
           abs_hum = `Abs Humidity(g/m³)`) %>% 
    mutate(timestamp = parse_date_time(timestamp, orders = c("d/m/Y H:M", "dmY HM", "Ymd HMS")))
})

# --- 3. DATA QUALITY AUDIT ---
total_nights <- nrow(sleep_df_raw)

# Filter for missing Sleep Metrics first
sleep_complete <- sleep_df_raw %>%
  rename(Sleep_Score = !!sym(mapping$garmin_sleep_score), 
         HRV = !!sym(mapping$garmin_hrv), 
         RHR = !!sym(mapping$garmin_rhr)) %>%
  filter(!is.na(Sleep_Score), !is.na(HRV), !is.na(RHR))

lost_sleep_data <- total_nights - nrow(sleep_complete)

# Map Sensor data and filter
final_data <- sleep_complete %>% 
  rowwise() %>% 
  mutate(Avg_Temp = mean(sensor_raw$room_temp[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T),
         Avg_Rel_Hum = mean(sensor_raw$rel_hum[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T),
         Avg_Abs_Hum = mean(sensor_raw$abs_hum[sensor_raw$timestamp >= bedtime & sensor_raw$timestamp <= waketime], na.rm=T)) %>% 
  ungroup() %>%
  filter(!is.na(Avg_Temp), !is.nan(Avg_Temp), !is.na(Avg_Abs_Hum))

used_nights <- nrow(final_data)
lost_sensor_data <- nrow(sleep_complete) - used_nights

# Audit Output
cat("\n===========================================================\n")
cat("                DATA QUALITY AUDIT\n")
cat("===========================================================\n")
cat(sprintf("Total nights detected:            %d\n", total_nights))
cat(sprintf("Nights used for analysis:         %d\n", used_nights))
cat(sprintf("Nights excluded total:            %d\n", total_nights - used_nights))
cat("-----------------------------------------------------------\n")
cat(sprintf("Reason: Corrupt/Missing Sleep Data:      %d\n", lost_sleep_data))
cat(sprintf("Reason: Missing/Incomplete Room Data:     %d\n", lost_sensor_data))
cat("===========================================================\n\n")

# --- 4. STATISTICAL ANALYSIS ---
env_vars <- list("Room Temp" = list(col="Avg_Temp", unit="°C"), 
                 "Rel Humidity" = list(col="Avg_Rel_Hum", unit="%"),
                 "Abs Humidity" = list(col="Avg_Abs_Hum", unit="g/m³"))
metrics <- c("Sleep_Score", "HRV", "RHR")
optima_storage <- list()

cat("                     SLEEP ANALYSIS\n")
cat("===========================================================\n")

for(env_name in names(env_vars)) {
  e_col <- env_vars[[env_name]]$col
  cat(sprintf("\n>>> IMPACT OF %s:\n", toupper(env_name)))
  for(m in metrics) {
    sub <- final_data %>% filter(!is.na(.data[[e_col]]), !is.na(.data[[m]]))
    if(nrow(sub) < 5) next
    
    sub_model <- sub
    if(m == "RHR") sub_model[[m]] <- -sub_model[[m]]
    
    fit_poly <- lm(as.formula(paste(m, "~ poly(", e_col, ", 2, raw=TRUE)")), data = sub_model)
    fit_lin <- lm(as.formula(paste(m, "~", e_col)), data = sub)
    
    b <- coef(fit_poly); opt <- -b[2] / (2 * b[3])
    is_peak <- b[3] < 0 && opt >= min(sub[[e_col]]) && opt <= max(sub[[e_col]])
    
    cat(sprintf("  [%s]\n", m))
    if(is_peak) {
      optima_storage[[paste0(env_name, "_", m)]] <- opt
      cat(sprintf("    - Optimal: %.1f %s\n", opt, env_vars[[env_name]]$unit))
    }
    cat(sprintf("    - P-Value: %.4f | R-Squared: %.1f%%\n", summary(fit_lin)$coefficients[2,4], summary(fit_poly)$adj.r.squared * 100))
  }
}

cat("\n--- STATISTICAL GLOSSARY ---\n")
cat("1. P-VALUE (Significance):\n")
cat("   Measures the probability that the observed pattern is random. P < 0.05 means\n")
cat("   there is less than a 5% chance the environment does NOT affect your sleep.\n\n")
cat("2. R-SQUARED (Effect Size):\n")
cat("   Tells you how much of your sleep quality 'swings' are explained by this factor.\n")
cat("   If R2 = 30%, then 30% of your sleep variance is due to this environmental factor.\n")
cat("===========================================================\n")

# --- 5. VISUALIZATIONS ---
metric_labels <- c("Sleep_Score" = "Sleep Score (↑ better)", "HRV" = "HRV (↑ better)", "RHR" = "RHR (↓ better)")
metric_colors <- c("Sleep_Score" = "#27ae60", "HRV" = "#2980b9", "RHR" = "#c0392b")

# LEVEL 1: Individual Plots
for(env_name in names(env_vars)) {
  e_col <- env_vars[[env_name]]$col
  for(m in metrics) {
    opt <- optima_storage[[paste0(env_name, "_", m)]]
    p <- ggplot(final_data, aes(x = .data[[e_col]], y = .data[[m]])) +
      geom_point(alpha = 0.4, color = "grey") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = metric_colors[m], linewidth = 1.2, se = TRUE, fill = metric_colors[m], alpha = 0.1) +
      labs(title = paste(m, "vs", env_name), x = paste(env_name, env_vars[[env_name]]$unit), y = m) +
      theme_minimal()
    if(!is.null(opt)) p <- p + geom_vline(xintercept = opt, linetype = "dashed", color = "black", linewidth = 0.8)
    print(p)
  }
}

# LEVEL 2: 3x3 Matrix Dashboard
matrix_plots <- list()
for(m in metrics) {
  for(env_name in names(env_vars)) {
    e_col <- env_vars[[env_name]]$col
    opt <- optima_storage[[paste0(env_name, "_", m)]]
    
    # Invert RHR for display so "UP" is always "GOOD"
    plot_data <- final_data
    if(m == "RHR") plot_data[[m]] <- -plot_data[[m]]
    
    p_mat <- ggplot(plot_data, aes(x = .data[[e_col]], y = .data[[m]])) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = metric_colors[m], linewidth = 1.2, se = TRUE, fill = metric_colors[m], alpha = 0.2) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), axis.title = element_text(size = 8), axis.text = element_text(size = 7)) +
      labs(x = env_name, y = if(m == "RHR") "RHR (inv)" else m)
    
    if(!is.null(opt)) p_mat <- p_mat + geom_vline(xintercept = opt, linetype = "dashed", color = "black", linewidth = 0.6)
    matrix_plots[[length(matrix_plots) + 1]] <- p_mat
  }
}


grid.arrange(grobs = matrix_plots, ncol = 3, top = textGrob("Sleep Environment Matrix: Trend, Variance & Optima", gp=gpar(fontsize=14, font=2)))