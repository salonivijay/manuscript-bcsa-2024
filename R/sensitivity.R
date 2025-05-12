# description -------------------------------------------------------------

# this file contains code for the sensitivity analysis figures 
# in the main text of the manuscript.(Figure 4 and 6) 

# read data-smoothing.R file ----------------------------------------------

source(here::here("R/data-smoothing.R"))

theme_sens <- theme(axis.text.y   = element_text(size=10,
                                                 color = "black"),
               axis.text.x   = element_text(size=10, 
                                            angle = 45, 
                                            hjust = 1,
                                            color = "black"),
               axis.title.y  = element_text(size=13),
               axis.title.x  = element_text(size=13),
               legend.title = element_text(size = 13),# Legend title
               legend.text = element_text(size = 10),
               #panel.background = element_rect(fill='transparent'), #transparent panel bg.
               #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg.
               #axis.line = element_line(colour = "black"),
               panel.border = element_rect(colour = "black", 
                                           fill=NA, 
                                           size=0.5),
               axis.line = element_line(),
               axis.line.x = element_blank(),
               axis.line.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               plot.margin = margin(0,0,0,0),
               legend.position = "bottom",
               legend.margin = margin(t = -2, 
                                      r = 0, 
                                      b = 0, 
                                      l = 0, 
                                      unit = "pt"),
               strip.text = element_text(size = 13))

func_sensitivity <- function(df, threshold1, threshold2) {
  df %>%
    mutate(aae_range = case_when(
      aae_blue_ir <= threshold1 ~ "ff",
      aae_blue_ir > threshold1 & aae_blue_ir <= threshold2 ~ "Mixed",
      aae_blue_ir > threshold2 ~ "bb"
    )) %>%
    group_by(settlement_id, aae_range) %>%
    summarise(
      observation_count = n(),
      mean_ebc = mean(ir_bcc),
      sd_ebc = sd(ir_bcc),
      median_ebc = median(ir_bcc)
    ) %>%
    ungroup() %>%
    group_by(settlement_id) %>%
    mutate(
      total_obs = sum(observation_count),
      pct_obs = round(observation_count / total_obs * 100, 1),
      aae_range = fct_relevel(aae_range, c("bb", "Mixed", "ff"))
    ) %>%
    arrange(aae_range, .by_group = TRUE) %>%
    mutate(
      wt_av_ebc = mean_ebc * pct_obs / 100,
      sum_ebc = sum(wt_av_ebc),
      wt_percent = wt_av_ebc / sum_ebc * 100
    )
}

df_mm_sens <- df_mm %>% 
  filter(exp_type == "mobile_monitoring",
         type_of_road == "non_main_road",
         time_of_day != "Morning")

df_sm_sens <- df_sm_hourly %>% 
  filter(exp_type == "stationary_monitoring")

# Function to calculate sensitivity over a range of thresholds
test_sensitivity <- function(df, threshold1_range, threshold2_range) {
  threshold_combinations <- expand.grid(threshold1 = threshold1_range, 
                                        threshold2 = threshold2_range, 
                                        stringsAsFactors = FALSE)
  
  results <- threshold_combinations %>%
    rowwise() %>%
    mutate(sensitivity_result = list(func_sensitivity(df, threshold1, threshold2))) %>%
    unnest(cols = c(sensitivity_result))
  
  return(results)
}

# Function to analyze sensitivity results
analyze_sensitivity <- function(results) {
  sensitivity_summary <- results %>%
    group_by(threshold1, threshold2, settlement_id) %>%
    summarise(
      mean_wt_percent_ff = mean(wt_percent[aae_range == "ff"], na.rm = TRUE),
      mean_wt_percent_Mixed = mean(wt_percent[aae_range == "Mixed"], na.rm = TRUE),
      mean_wt_percent_bb = mean(wt_percent[aae_range == "bb"], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(sensitivity_summary)
}



threshold1_range <- seq(1.21, 1.37, by = 0.02)
threshold2_range <- seq(1.55, 1.71, by = 0.02)

sensitivity_results <- test_sensitivity(df_mm_sens, threshold1_range, threshold2_range)
sensitivity_summary <- analyze_sensitivity(sensitivity_results)


# Reshape the data for plotting
sensitivity_long <- sensitivity_summary %>%
  pivot_longer(cols = starts_with("mean_wt_percent_"),
               names_to = "category",
               values_to = "mean_wt_percent")

## Function to calculate difference from closest reference point
calculate_difference <- function(data, ref_threshold1, ref_threshold2) {
  # Find the closest point to the reference
  closest_point <- data %>%
    mutate(dist = sqrt((threshold1 - ref_threshold1)^2 + (threshold2 - ref_threshold2)^2)) %>%
    arrange(dist) %>%
    slice(1)
  
  ref_value <- closest_point$mean_wt_percent
  
  data %>%
    mutate(difference = mean_wt_percent - ref_value)
}

# Calculate differences from reference point (1.29, 1.63)
fig04_mm_sensitivity <- sensitivity_long %>%
  group_by(settlement_id, category) %>%
  group_modify(~calculate_difference(.x, 1.29, 1.63)) %>%
  ungroup()

category_labels <- c(
  "mean_wt_percent_bb" = "bb",
  "mean_wt_percent_ff" = "ff",
  "mean_wt_percent_Mixed" = "Mixed"
  # Add more as needed
)


# Create heat maps
p_sensitivity_mm <- ggplot(fig04_mm_sensitivity, aes(x = threshold1, y = threshold2, fill = difference)) +
  geom_tile() +
  #geom_text(aes(label = round(difference, 0)), color = "black", size = 3) +
  facet_grid(settlement_id ~ category, labeller = labeller(category = category_labels)) +
  scale_fill_gradient2(low = "#4575B4", mid = "#FFFFBF", high = "#D73027", midpoint = 0) +
  scale_x_continuous(breaks = seq(1.21, 1.37, by = 0.04),
                     limits = c(1.20, 1.38)) +
  scale_y_continuous(breaks = seq(1.55, 1.71, by = 0.04),
                     limits = c(1.54, 1.72)) +
  labs(x = expression(atop("", paste("AAE"[ff]))),
       y = expression(atop("", paste("AAE"[bb]))), 
       fill = "Difference in percent contribution to eBC from Reference") +
  theme_minimal() +
  theme +
  theme(panel.border = element_rect(color = "gray", 
                                    size = 0.5, 
                                    linetype = "solid", 
                                    fill = alpha("black", 0)),
        legend.position = "bottom",
        legend.margin = margin(t = -2, r = 0, b = 0, l = 0, unit = "pt"),
        axis.text.x = element_text(angle = 45, hjust = 1)) + # Add thick border)
  geom_vline(xintercept = 1.29, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 1.63, linetype = "dashed", color = "black")

sensitivity_results_sm <- test_sensitivity(df_sm_sens, threshold1_range, threshold2_range)
sensitivity_summary_sm <- analyze_sensitivity(sensitivity_results_sm)

# Reshape the data for plotting
sensitivity_long_sm <- sensitivity_summary_sm %>%
  pivot_longer(cols = starts_with("mean_wt_percent_"),
               names_to = "category",
               values_to = "mean_wt_percent")

## Function to calculate difference from closest reference point
calculate_difference <- function(data, ref_threshold1, ref_threshold2) {
  # Find the closest point to the reference
  closest_point <- data %>%
    mutate(dist = sqrt((threshold1 - ref_threshold1)^2 + (threshold2 - ref_threshold2)^2)) %>%
    arrange(dist) %>%
    slice(1)
  
  ref_value <- closest_point$mean_wt_percent
  
  data %>%
    mutate(difference = mean_wt_percent - ref_value)
}

# Calculate differences from reference point (1.29, 1.63)
fig06_sm_sensitivity <- sensitivity_long_sm %>%
  group_by(settlement_id, category) %>%
  group_modify(~calculate_difference(.x, 1.29, 1.63)) %>%
  ungroup()


# write csv files for data behind the figures -----------------------------

fig04_mm_sensitivity %>% 
  write_csv(here::here("data/processed-data/fig04_mm_sensitivity.csv"))

fig06_sm_sensitivity %>% 
  write_csv(here::here("data/processed-data/fig06_sm_sensitivity.csv"))

# Create heat maps
p_sensitivity_sm <- ggplot(fig06_sm_sensitivity, 
                           aes(x = threshold1, 
                               y = threshold2, 
                               fill = difference)) +
  geom_tile() +
  #geom_text(aes(label = round(difference, 0)), color = "black", size = 3) +
  facet_grid(settlement_id ~ category, 
             labeller = labeller(category = category_labels)) +
  scale_fill_gradient2(low = "#4575B4", mid = "#FFFFBF", high = "#D73027", midpoint = 0) +
  scale_x_continuous(breaks = seq(1.21, 1.37, 
                                  by = 0.04),
                     limits = c(1.20, 1.38)) +
  scale_y_continuous(breaks = seq(1.55, 1.71, 
                                  by = 0.04),
                     limits = c(1.54, 1.72)) +
  labs(x = expression(atop("", paste("AAE"[ff]))),
       y = expression(atop("", paste("AAE"[bb]))), 
       fill = "Difference in percent contribution to eBC from Reference") +
  theme_minimal() +
  theme +
  theme(panel.border = element_rect(color = "gray", 
                                    size = 0.5, 
                                    linetype = "solid", 
                                    fill = alpha("black", 0)),
                                    legend.position = "bottom",
                                    legend.margin = margin(t = -2, r = 0, b = 0, l = 0, unit = "pt"),
                                    axis.text.x = element_text(angle = 45, hjust = 1)) + # Add thick border)
  geom_vline(xintercept = 1.29, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 1.63, linetype = "dashed", color = "black")


ggsave("figures/p_sensitivity_mm.jpeg", 
       plot = p_sensitivity_mm,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 16,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

ggsave("figures/p_sensitivity_sm.jpeg", 
       plot = p_sensitivity_sm,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 10,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

# ###

check_mm <- sensitivity_summary_sm %>%
  filter(threshold1 == "1.37" &
         threshold2 == "1.55") 
  
  
