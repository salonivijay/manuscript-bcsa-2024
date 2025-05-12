# description -------------------------------------------------------------

# this file contains code for the figures in the main text of the 
# manuscript. The code for map (Figure 1), and sensitivity analysis 
# (Figure 4 and 6) are in separate .R files.

# load the smooth data ----------------------------------------------------

source(here::here("R/data-smoothing.R"))

# r packages --------------------------------------------------------------

library(patchwork)

# define a common plot theme ----------------------------------------------

theme <- theme(
  axis.text.y   = element_text(size=7,
                               color = "black"),
  axis.text.x   = element_text(size=7,
                               color = "black"),
  axis.title.y  = element_text(size=7),
  axis.title.x  = element_text(size=7),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 7),
  panel.border = element_rect(colour = "black", fill=NA, size=0.5),
  axis.line = element_line(),
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.margin = margin(0,0,0,0),
  strip.text = element_text(size = 7)  # This line sets the size of facet labels
)

# data preparation for figures ----------------------------------------------

df_mm_highway_overview <- df_mm |> 
  filter(!time_of_day %in% "Morning",
         type_of_road == "main_road") |> 
  mutate(settlement_id = "Highways") |> 
  mutate(type_of_settlement = "Highways")

df_monitoring <- df_mm |> 
  filter(!time_of_day %in% "Morning",
         type_of_road == "non_main_road") |> 
  bind_rows(df_mm_highway_overview) |>
  bind_rows(df_sm_hourly) |> 
  mutate(ir_bcc = ir_bcc/1000) |>
  filter(ir_bcc > 0) |> 
  drop_na(ir_bcc, aae_blue_ir)

df_aae_overview <- df_monitoring |> 
  mutate(aae_range = case_when(
    aae_blue_ir <= 1.29 ~ "ff",
    aae_blue_ir > 1.29 & aae_blue_ir <= 1.63 ~ "Mixed",
    aae_blue_ir > 1.63 ~ "bb"
  )) %>% 
  group_by(settlement_id, aae_range, exp_type, day_type, type_of_settlement) |>
  summarise(observation_count = n(),
            mean_ebc = mean(ir_bcc),
            sd_ebc = sd(ir_bcc),
            median_ebc = median(ir_bcc))   |> 
  ungroup() |>
  group_by(settlement_id, day_type, exp_type, type_of_settlement) |>
  mutate(total_obs = sum(observation_count),
         pct_obs = round(observation_count / total_obs * 100, 1)) |>
  ungroup() 

df_summary_mobile <- df_aae_overview %>% 
  filter(exp_type == "mobile_monitoring")

df_aae_range <- df_monitoring |>
  mutate(aae_range = case_when(
    aae_blue_ir <= 1.29 ~ "ff",
    aae_blue_ir > 1.29 & aae_blue_ir <= 1.63 ~ "Mixed",
    aae_blue_ir > 1.63 ~ "bb"
  ))

df_aae_range_sm <- df_aae_range |>
  filter(exp_type == "stationary_monitoring")

df_sm_hourly %>% 
  group_by(settlement_id) %>% 
  summarise(mean_ir = mean(ir_bcc),
            median_ir = median(ir_bcc),
            sd_ir = sd(ir_bcc))

# diurnal pattern 

confidence_intervals <- df_sm_hourly %>%
  mutate(ir_bcc = ir_bcc/1000) |> 
  group_by(hour, settlement_id) |> 
  summarize(lower_ci = t.test(aae_blue_ir)$conf.int[1],
            upper_ci = t.test(aae_blue_ir)$conf.int[2],
            mean = mean(aae_blue_ir),
            lower_ci_ir = t.test(ir_bcc)$conf.int[1],
            upper_ci_ir = t.test(ir_bcc)$conf.int[2],
            mean_ir = mean(ir_bcc)) 

# Figure 2: Notched box plots from mobile monitoring data -----------------

settlements_vec <- c("Ndirande", "Chirimba", "Kachere", "Bangwe",
                     "Sunnyside", "Naperi", "Nyambadwe", "Namiwawa", "Highways")

settlement_types_order <- df_monitoring |>
  filter(exp_type == "mobile_monitoring") |>
  mutate(settlement_id = factor(settlement_id, 
                                levels = settlements_vec)) |>
  arrange(settlement_id) |>
  pull(type_of_settlement) |>
  unique()

#transformation function
scaleFUN0 <- function(x) sprintf("%.0f", x)
scaleFUN1 <- function(x) sprintf("%.1f", x)
scaleFUN2 <- function(x) sprintf("%.2f", x)

# Define a color-blind friendly palette
cb_fill_palette <- c(
  "#56B4E9",  # Sky Blue
  "#F0E442",  # Yellow
  "#CC79A7"   # Reddish Purple
)

fig02_mm_boxplot <- df_monitoring |>
  filter(exp_type == "mobile_monitoring") |>
  mutate(
    settlement_id = factor(settlement_id, levels = settlements_vec),
    type_of_settlement = factor(type_of_settlement, 
                                levels = settlement_types_order)
  ) 

df_mm_summary <- df_monitoring %>% 
  filter(exp_type == "mobile_monitoring") %>% 
  group_by(settlement_id) %>% 
  summarise(mean_ir = mean(ir_bcc),
            median_ir = median(ir_bcc))
  

# fig02_mm_boxplot %>% 
# write_csv(here::here("data/processed-data/fig02_mm_boxplot.csv"))

# Boxplot Code
p_boxplot_mm <- fig02_mm_boxplot |>
  ggplot(aes(x = settlement_id, 
             y = ir_bcc, 
             fill = type_of_settlement)) +
  geom_boxplot(position = position_dodge(width = 1),
               outlier.shape = NA,
               notch = TRUE,
               width = 0.5) +
  stat_summary(fun.y=mean, geom="point", 
               shape=18, 
               size=2, 
               position = position_dodge(width = 1)) +
  scale_y_continuous(labels=scaleFUN2, 
                     breaks = seq(0, 
                                  25, 
                                  by = 5)) +
  coord_cartesian(ylim=c(0, 25)) +
  scale_fill_manual(values = cb_fill_palette) +  # Apply the color-blind palette
  labs(y = expression("eBC concentration (µg m"^-3*")"),
       x = "Location",
       fill = "Type of location") +
  theme +
  theme(axis.text.x = 
          element_text(angle = 45, 
                       hjust=1))


p_boxplot_mm



# Figure 3: Source apportionment mobile monitoring ------------------------
# create functions for source apportionment figures

# function for figures using clustering method

p_func_sa_cluster <- function(data) {
  
  df_cluster <- data %>% 
    group_by(settlement_id, day_type) %>%
    mutate(aae_range = fct_relevel(aae_range,
                                   c("ff",
                                     "Mixed",
                                     "bb"))) %>%
    arrange(aae_range, .by_group = TRUE) %>%
    mutate(right = cumsum(pct_obs),
           left = lag(right, 1, default = 0),
           mid = (left+right)/2,
           wt_av_ebc = mean_ebc*pct_obs/100,
           sum_ebc = sum(wt_av_ebc),
           wt_percent = wt_av_ebc/sum_ebc*100) 
  
  # Create the first plot (p1)
  p <- ggplot(df_cluster,
              aes(xmin = left, 
                  xmax = right, 
                  ymax = mean_ebc, 
                  ymin = 0, 
                  fill = aae_range)) +
    geom_rect(color = "black",
              size = 0.3) +
    scale_fill_manual(values = c("ff" = "#00BFC4",
                                 "Mixed" = "#A3A500", 
                                 "bb" = "#D55E00")) +
    labs(x = "Percentage of observations (%)", 
         y = expression("eBC concentration (µg m"^-3*")"), 
         fill = "Source") +
    scale_y_continuous(position = "right") +
    theme(
      axis.title.y.right = element_text(margin = margin(0, 0, 0, 1)),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      strip.background.y = element_blank(), 
      strip.text.y.left = element_text(angle = 0, 
                                       hjust = 0.5)) +
    theme +
    facet_grid(settlement_id ~ day_type, switch = "y")
  
  return(list(plot = p, data = df_cluster))
}

# function for figures of weighted average using clustering method

p_func_wt_avg <- function(df) {
  data <- df |>
    group_by(settlement_id, aae_range) |>
    summarise(observation_count = n(),
              mean_ebc = mean(ir_bcc),
              sd_ebc = sd(ir_bcc),
              median_ebc = median(ir_bcc))   |> 
    ungroup() |>
    group_by(settlement_id) |>
    mutate(total_obs = sum(observation_count),
           pct_obs = round(observation_count / total_obs * 100, 1),
           aae_range = fct_relevel(aae_range, c("bb", "Mixed", "ff"))) |>
    arrange(aae_range, .by_group = TRUE) %>%
    mutate(wt_av_ebc = mean_ebc*pct_obs/100,
           sum_ebc = sum(wt_av_ebc),
           wt_percent = wt_av_ebc/sum_ebc*100) 
  
  p <- data %>% 
    ggplot(aes(x = wt_percent, 
               y = fct_rev(settlement_id), 
               fill = aae_range)) +
    geom_bar(stat = "identity", 
             width = 0.5, 
             color = "black", 
             size = 0.3) +
    geom_text(aes(label = scaleFUN0(wt_percent)), 
              size = 7/.pt, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("bb" = "#D55E00", 
                                 "Mixed" = "#A3A500", 
                                 "ff" = "#00BFC4")) +
    labs(x = "Percentage contribution to eBC (%)",
         fill = "Source") +
    theme_bw() +  
    theme +
    theme(panel.grid.major.y = element_blank(),
          #axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
          axis.title.y = element_blank(),
          legend.key.size = unit(0.8, "lines")) +
    guides(fill = guide_legend(reverse = TRUE))
  
  return(list(plot = p, data = data))
}

result_sa_cluster_formal <- p_func_sa_cluster(df_aae_overview %>% 
                              filter(type_of_settlement == "Formal",
                                     !settlement_id %in% "Highways",
                                     exp_type == "mobile_monitoring")) 

# Figure 3 (a): clustering formal settlements

p_mm_sa_cluster_formal <- result_sa_cluster_formal$plot + 
  theme(legend.position = "none")

fig03_a_cluster_formal <- result_sa_cluster_formal$data

# fig03_a_cluster_formal %>% 
#   write_csv(here::here("data/processed-data/fig03_a_cluster_formal.csv"))

# Figure 3 (b): clustering informal settlements

result_sa_cluster_informal <- p_func_sa_cluster(df_aae_overview |> 
                                                filter(type_of_settlement == "Informal",
                                                       !settlement_id %in% "Highways",
                                                       exp_type == "mobile_monitoring")) 

p_mm_sa_cluster_informal <- result_sa_cluster_informal$plot + 
  theme(legend.position = "none")

fig03_b_cluster_informal <- result_sa_cluster_informal$data

# fig03_b_cluster_informal %>% 
#   write_csv(here::here("data/processed-data/fig03_b_cluster_informal.csv"))

# Figure 3 (c): weighted average formal settlements

result_wt_avg_mm_formal <- p_func_wt_avg(df_aae_range %>% 
                                      filter(exp_type == "mobile_monitoring",
                                             type_of_settlement == "Formal",
                                             !settlement_id %in% "Highways"))

p_wt_avg_mm_formal <- result_wt_avg_mm_formal$plot + 
  theme(legend.position = "none")

fig03_c_wt_avg_formal <- result_wt_avg_mm_formal$data

# fig03_c_wt_avg_formal %>% 
#   write_csv(here::here("data/processed-data/fig03_c_wt_avg_formal.csv"))

# Figure 3 (d): weighted average informal settlements

result_wt_avg_mm_informal <- p_func_wt_avg(df_aae_range %>% 
                                        filter(exp_type == "mobile_monitoring",
                                               type_of_settlement == "Informal",
                                               !settlement_id %in% "Highways")) 

p_wt_avg_mm_informal <- result_wt_avg_mm_informal$plot + 
  theme(legend.position = "none")

fig03_d_wt_avg_informal <- result_wt_avg_mm_informal$data

# fig03_d_wt_avg_informal %>% 
#   write_csv(here::here("data/processed-data/fig03_d_wt_avg_informal.csv"))

# Figure 3 (a) + Figure 3 (b) 

p_mm_sa <- p_mm_sa_cluster_formal + p_mm_sa_cluster_informal +
  plot_layout(guides = "collect")

# Figure 3 (c) + Figure 3 (d) 

p_wt_avg_mm <- p_wt_avg_mm_formal  + p_wt_avg_mm_informal +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Figure 4: Sensitivity analysis mobile monitoring ------------------------

# see sensitivity.R file

# Figure 5: Source apportionment stationary monitoring --------------------

# Figure 5 (a): clustering stationary monitoring

result_sm_sa_cluster <- p_func_sa_cluster(df_aae_overview %>% 
                                       filter(exp_type == "stationary_monitoring")) 

p_sm_sa_cluster <- result_sm_sa_cluster$plot + 
  theme(legend.position = "none")

fig05_a_sm_cluster <- result_sm_sa_cluster$data

# fig05_a_sm_cluster %>% 
#   write_csv(here::here("data/processed-data/fig05_a_sm_cluster.csv"))

# Figure 5 (b): weighted average stationary monitoring

result_wt_avg_sm <- p_func_wt_avg(df_aae_range_sm) 

p_wt_avg_sm <- result_wt_avg_sm$plot +
  theme(legend.position = "bottom")

fig05_b_sm_wt_avg <- result_wt_avg_sm$data

# fig05_b_sm_wt_avg %>% 
#   write_csv(here::here("data/processed-data/fig05_b_sm_wt_avg.csv"))

# Figure 7: Diurnal pattern of eBC ----------------------------------------

# Define colorblind-friendly colors
color1 <- "#E69F00"  # Orange
color2 <- "#56B4E9"  # Light blue

# confidence_intervals %>% 
#   write_csv(here::here("data/processed-data/fig07_diurnal_pattern.csv"))

p_diurnal <- confidence_intervals |>
  ggplot(aes(x = hour)) +
  geom_line(aes(y = mean_ir), color = color1) +
  geom_ribbon(aes(ymin = lower_ci_ir,
                  ymax = upper_ci_ir),
              fill = color1,
              alpha = 0.3) +
  geom_line(aes(y = (mean*8-6)), color = color2) +
  geom_ribbon(aes(ymin = (lower_ci*8-6), ymax = (upper_ci*8-6)),
              fill = color2, alpha = 0.3) +
  scale_y_continuous(
    name = expression("eBC concentration (µg m"^-3*")"),
    sec.axis = sec_axis(~(.+6)/8, 
                        name="AAE values (470/880 nm)",
                        breaks = seq(1, 2, by = 0.5)),
    breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  labs(x = "Hour") +
  facet_wrap(~settlement_id) +
  theme(legend.position = "none",
        axis.line.y.left = element_line(color = color1),
        axis.line.y.right = element_line(color = color2),
        axis.title.y.left = element_text(color = color1),
        axis.title.y.right = element_text(color = color2),
        axis.text.y.left = element_text(color = color1),
        axis.text.y.right = element_text(color = color2),
        axis.ticks.y.left = element_line(color = color1),
        axis.ticks.y.right = element_line(color = color2)) +
  theme


# Save figures ------------------------------------------------------------

# Save Figure 7
ggsave("figures/p_boxplot_mm.jpeg",
       plot = p_boxplot_mm,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 8,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

ggsave(
  filename = "figures/p_mm_sa.jpeg",
  plot = p_mm_sa,                        # The ggplot object
  width = 12.7,                            # Width in cm for single-column (3.5 inches)
  height = 7,                           # Height in cm (can be adjusted as needed)
  dpi = 300,
  units = "cm", # Units for width and height
)

ggsave("figures/p_wt_avg_mm.jpeg",
       plot = p_wt_avg_mm,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 5,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

ggsave("figures/p_sm_sa_cluster.jpeg",
       plot = p_sm_sa_cluster,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 6,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height


ggsave("figures/p_wt_avg_sm.jpeg",
       plot = p_wt_avg_sm,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 5,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height


# Save Figure 7
ggsave("figures/p_diurnal.jpeg",
       plot = p_diurnal,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 5,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

# statistical difference --------------------------------------------------

## check normality by Shapiro test

library(tidyverse)
library(broom)
library(ggpubr)

# 1. Test normality
normality_results <- fig02_mm_boxplot %>% 
  group_by(settlement_id) %>% 
  summarise(
    shapiro = list(shapiro.test(ir_bcc)),
    tidied = map(shapiro, tidy)
  ) %>% 
  unnest(tidied)

# 2. Print results
normality_results %>% select(settlement_id, statistic, p.value)

# 3. Visualize
ggplot(fig02_mm_boxplot, aes(sample = ir_bcc)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~settlement_id)

# # Perform pairwise Wilcoxon rank-sum tests

library(rstatix)
library(FSA)

# evaluate differences among sites
kruskal.test(ir_bcc ~ settlement_id, data = fig02_mm_boxplot)
# chi-squared = 835.52, df = 8, p-value < 2.2e-16

# wilcox rank sum test
check_stats <- list(
  sample_sizes = table(fig02_mm_boxplot$settlement_id),
  wilcoxon = wilcox_test(ir_bcc ~ settlement_id, data=fig02_mm_boxplot),
  effect_size = rstatix::wilcox_effsize(ir_bcc ~ settlement_id, data=fig02_mm_boxplot),
  visual_summary = ggplot(fig02_mm_boxplot, aes(x=settlement_id, y=ir_bcc)) + geom_boxplot()
)

# Convert list components to a tidy dataframe
df_stats <- bind_cols(
  
  
  # Extract wilcoxon test results
  check_stats$wilcoxon,
  
  # Extract effect size
  check_stats$effect_size %>% select(effsize, magnitude)
) %>% 
  mutate(effsize = round(effsize, 2))

# df_stats %>% 
#   write_csv(here::here("data/processed-data/tabA1_wilcox_test.csv"))

# Filter to keep only (date, hour) groups with at least 2 settlements
filtered_data <- df_sm_hourly %>%
  group_by(date, hour) %>%
  filter(n_distinct(settlement_id) >= 2) %>%
  ungroup()

# Run Wilcoxon test for each (date, hour)
pairwise_results <- filtered_data %>%
  group_by(date, hour) %>%
  wilcox_test(
    formula = ir_bcc ~ settlement_id,
    p.adjust.method = "BH"
  )

# View the results
pairwise_results

# # Define the function to calculate confidence intervals for the median
# calculate_notch <- function(data) {
#   median_value <- median(data)
#   iqr <- IQR(data)  # Interquartile Range
#   n <- length(data) # Sample size
#   
#   # Confidence Interval calculation for notched box plots
#   lower_bound <- median_value - 1.58 * iqr / sqrt(n)
#   upper_bound <- median_value + 1.58 * iqr / sqrt(n)
#   
#   return(c(lower_bound, upper_bound))
# }
# 
# 
# # Calculate notches for both datasets
# notch1 <- calculate_notch(irbcc_nyambadwe$ir_bcc)
# notch2 <- calculate_notch(irbcc_ndirande$ir_bcc)
# 
# # Print results
# cat("Dataset 1 Notch CI:", notch1, "\n")
# cat("Dataset 2 Notch CI:", notch2, "\n")
# 
# # Assess statistical significance based on overlap of notches
# if (notch1[2] < notch2[1] || notch2[2] < notch1[1]) {
#   cat("The medians are statistically significantly different.\n")
# } else {
#   cat("The medians are not statistically significantly different.\n")
# }


