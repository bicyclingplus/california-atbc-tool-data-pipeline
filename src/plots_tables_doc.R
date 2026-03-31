library(tidyverse)
library(ggplot2)
library(patchwork)
library(targets)
library(scales)

# ============================================================================
# Functions
# ============================================================================

# Function to read and write data to directory of store (Box, etc.)
data_io <- function(sub_path) {
  # Get the store path (e.g., "C:/Users/You/Box/Project/_targets")
  store_path <- targets::tar_config_get("store")
  
  # Go up one level to get the Project Root on Box
  box_root <- dirname(store_path)
  
  # Join with your requested file sub-path
  file.path(box_root, sub_path)
}

# ATRC Brand Colors 
atrc <- list(
  navy      = "#183F66",
  navy_lt   = "#AEBCC9",
  orange    = "#D17224",
  orange_lt = "#EFCEB2",
  green     = "#30704A",
  green_lt  = "#B7CDC0",
  sky_blue  = "#8AD5FF",
  yellow    = "#FFED83"
)

# Custom minimal theme applied to all plots
theme_atrc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", color = atrc$navy),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", color = "gray30"),
      legend.position = "bottom"
    )
}

# Volume Scatter Plot (Log1p Scale & Greyscale)
plot_count_scatter <- function(data, obs_col, pred_col) {
  log_breaks <- c(0, 10, 100, 1000, 10000, 50000, 100000)
  
  ggplot(data, aes(x = .data[[obs_col]], y = .data[[pred_col]])) +
    # Grayscale dots with high transparency for density
    geom_point(alpha = 0.25, color = "gray20", size = 1.5) +
    # Solid reference line
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(trans = "log1p", breaks = log_breaks, labels = scales::comma) +
    scale_y_continuous(trans = "log1p", breaks = log_breaks, labels = scales::comma) +
    coord_fixed() + # Enforces perfect square 1:1 aspect ratio inside the plot
    labs(
      x = "Observed Counts (Log Scale)",
      y = "Predicted Counts (Log Scale)"
    ) +
    theme_atrc() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Prep safety data for plotting
prep_val_data <- function(models, prepped_data, facility_label) {
  
  # Hurdle (Probability)
  val_hurdle <- prepped_data$freq_data %>%
    na.omit() %>%
    mutate(
      obs_has_crash = factor(has_crash),
      pred_prob = predict(models$model_hurdle, newdata = ., type = "response"),
      facility = facility_label
    )
  
  # Count (Frequency)
  val_count <- prepped_data$freq_data %>%
    filter(has_crash == 1) %>%
    na.omit() %>%
    mutate(
      obs_count = factor(crash_count),
      pred_count = predict(models$model_count, newdata = ., type = "response"),
      facility = facility_label
    )
  
  # Severity (Classification)
  val_sev <- prepped_data$sev_data %>%
    na.omit() %>%
    mutate(
      obs_severity = severity_ord,
      pred_severity = predict(models$model_severity, newdata = ., type = "class"),
      facility = facility_label
    )
  
  return(list(hurdle = val_hurdle, count = val_count, sev = val_sev))
}

# Main Data Generator: Takes massive models and outputs lightweight plotting data
generate_safety_plot_data <- function(node_models, node_data, link_models, link_data) {
  
  # 1. Extract raw predictions
  nodes_val <- prep_val_data(node_models, node_data, "Nodes")
  links_val <- prep_val_data(link_models, link_data, "Links")
  
  # 2. Bind into combined datasets
  df_hurdle <- bind_rows(nodes_val$hurdle, links_val$hurdle)
  df_count  <- bind_rows(nodes_val$count,  links_val$count)
  df_sev    <- bind_rows(nodes_val$sev,    links_val$sev)
  
  # 3. Pre-calculate the Severity Accuracy for the Bar Chart
  df_sev_acc <- df_sev %>%
    group_by(facility, obs_severity) %>%
    summarize(
      total = n(),
      correct = sum(obs_severity == pred_severity),
      accuracy = correct / total,
      .groups = "drop"
    )
  
  # Return just the dataframes needed for the plots
  return(list(
    hurdle  = df_hurdle,
    count   = df_count,
    sev_acc = df_sev_acc
  ))
}

#' Integrated 3-Panel Safety Plot (Nodes vs. Links)
#' Returns a single vertically stacked patchwork plot
plot_combined_safety <- function(plot_data, mode_name = "Bicycle") {
  
  # Map specific ATRC brand colors to facility types
  fac_colors <- c("Nodes" = atrc$navy, "Links" = atrc$orange)
  
  # Plot 1: Hurdle (Probability)
  p_hurdle <- ggplot(plot_data$hurdle, aes(x = obs_has_crash, y = pred_prob, fill = facility)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.3, color = "gray20") +
    scale_fill_manual(values = fac_colors) +
    labs(
      subtitle = "A. Crash Occurrence Probability",
      x = "Observed Crash Occurrence (0 = No, 1 = Yes)",
      y = "Predicted Prob."
    ) +
    theme_atrc() +
    theme(axis.title.x = element_blank()) # Hides x-axis title to save vertical space
  
  # Plot 2: Count (Frequency)
  p_count <- ggplot(plot_data$count, aes(x = obs_count, y = pred_count, fill = facility)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.3, color = "gray20") +
    scale_fill_manual(values = fac_colors) +
    labs(
      subtitle = "B. Expected Crash Frequency (Given ≥ 1 Crash)",
      x = "Observed Crash Count",
      y = "Expected Crashes"
    ) +
    theme_atrc()
  
  # Plot 3: Severity Accuracy (Bar Chart)
  p_sev <- ggplot(plot_data$sev_acc, aes(x = obs_severity, y = accuracy, fill = facility)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "gray20") +
    scale_fill_manual(values = fac_colors) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      subtitle = "C. Severity Prediction Accuracy",
      x = "Observed Severity Level",
      y = "% Correctly Predicted"
    ) +
    theme_atrc()
  
  # Combine via patchwork and format the overall layout
  combined_plot <- (p_hurdle / p_count / p_sev) + 
    plot_layout(guides = "collect") + 
    plot_annotation(
      title = paste(mode_name, "Safety Model Performance: Nodes vs. Links"),
      theme = theme(
        # Uses the ATRC navy for the master title
        plot.title = element_text(face = "bold", size = 16, color = atrc$navy, hjust = 0.5),
        legend.position = "bottom"
      )
    )
  
  return(combined_plot)
}

# ============================================================================
# GENERATE AND ASSEMBLE PLOTS
# ============================================================================

# --- Load Random Forest Validation Data ---
# (Assuming these targets output a list with a $predictions dataframe)
rf_bike_final <- tar_read(model_bike_final)
rf_ped_final  <- tar_read(model_ped_final)

# --- Generate GLM Validation Data ---
# Load the fitted HGLM models and the raw training data
hglm_bike_final <- tar_read(hglm_model_bike)
hglm_ped_final  <- tar_read(hglm_model_ped)

# Raw Training Data (contains the actual observed counts)
bike_train <- tar_read(bike_train)
ped_train  <- tar_read(ped_train)

# --- Prepare Random Forest Validation Data ---
# Reverse the log1p() transformation using expm1()
bike_full_val <- bike_train %>%
  na.omit() %>% # Ensure rows match the training environment exactly
  mutate(
    obs_aadb  = as.integer(round(pmax(aadb, 0), 0)), # Match your training count logic
    pred_aadb = expm1(rf_bike_final$predictions)       # Reverse log1p back to raw counts
  )

ped_full_val <- ped_train %>%
  na.omit() %>% 
  mutate(
    obs_aadp  = as.integer(round(pmax(aadp, 0), 0)),
    pred_aadp = expm1(rf_ped_final$predictions) 
  )

# --- Prepare GLM Validation Data ---
# Use predict function
bike_glm_val <- bike_train %>%
  na.omit() %>%
  mutate(
    obs_aadb  = as.integer(round(pmax(aadb, 0), 0)),
    pred_aadb = predict(hglm_bike_final, newdata = ., type = "response",
                        re.form = NA)
  )

ped_glm_val <- ped_train %>%
  na.omit() %>%
  mutate(
    obs_aadp  = as.integer(round(pmax(aadp, 0), 0)),
    pred_aadp = predict(hglm_ped_final , newdata = ., type = "response",
                        re.form = NA)
  )

# ---- Generate Volume Plots ----
p_bike_rf  <- plot_count_scatter(bike_full_val, "obs_aadb", "pred_aadb")
p_ped_rf   <- plot_count_scatter(ped_full_val,  "obs_aadp", "pred_aadp")
p_bike_glm <- plot_count_scatter(bike_glm_val,  "obs_aadb", "pred_aadb")
p_ped_glm  <- plot_count_scatter(ped_glm_val,   "obs_aadp", "pred_aadp")

# ---- Generate Safety Plots ----
bike_plot_data <- generate_safety_plot_data(
  node_models = tar_read(bike_node_models), 
  node_data   = tar_read(prepped_bike_nodes), 
  link_models = tar_read(bike_link_models), 
  link_data   = tar_read(prepped_bike_links)
)
p_bike_safety <- plot_combined_safety(
  plot_data = bike_plot_data,
  mode_name = "Bicycle"
)

ped_plot_data <- generate_safety_plot_data(
  node_models = tar_read(ped_node_models), 
  node_data   = tar_read(prepped_ped_nodes), 
  link_models = tar_read(ped_link_models), 
  link_data   = tar_read(prepped_ped_links)
)
p_ped_safety <- plot_combined_safety(
  plot_data = ped_plot_data,
  mode_name = "Pedestrian"
)
# ---- Export Plots ----
# Output directory (Ensure this exists, or use your data_io function)
out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

# Export Scatter Plots (Strict squares: 6x6 inches fits well within 6.5x9 limit)
ggsave(data.io("results/figures/bike_rf.png"),  p_bike_rf,  width = 6, height = 6, units = "in", dpi = 300)
ggsave(data.io("results/figures/ped_rf.png"),   p_ped_rf,   width = 6, height = 6, units = "in", dpi = 300)
ggsave(data.io("results/figures/bike_glm.png"), p_bike_glm, width = 6, height = 6, units = "in", dpi = 300)
ggsave(data.io("results/figures/ped_glm.png"),  p_ped_glm,  width = 6, height = 6, units = "in", dpi = 300)

# Export Safety Plots (Rectangles: 6.5x4.5 inches)
ggsave(data.io("results/figures/crash_prob.png"), p_crash_prob, width = 6.5, height = 4.5, units = "in", dpi = 300)
ggsave(data.io("results/figures/crash_freq.png"), p_crash_freq, width = 6.5, height = 4.5, units = "in", dpi = 300)
ggsave(data.io("results/figures/severity.png"),   p_severity,   width = 6.5, height = 4.5, units = "in", dpi = 300)

# Example of saving with data_io pathing:
# ggsave(data_io("data_processed/volume_validation_plots.png"), volume_layout, width = 12, height = 10, dpi = 300)