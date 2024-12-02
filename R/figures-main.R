# description -------------------------------------------------------------

source(here::here("R/data-smoothing.R"))

# r packages --------------------------------------------------------------

library(patchwork)

# define a common plot theme ----------------------------------------------


theme <- theme(axis.text.y   = element_text(size=6),
               axis.text.x   = element_text(size=6),
               axis.title.y  = element_text(size=7),
               axis.title.x  = element_text(size=7),
               legend.title = element_text(size = 7),# Legend title
               legend.text = element_text(size = 6),
               #panel.background = element_rect(fill='transparent'), #transparent panel bg.
               #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg.
               #axis.line = element_line(colour = "black"),
               panel.border = element_rect(colour = "black", fill=NA, size=0.5),
               axis.line = element_line(),
               axis.line.x = element_blank(),
               axis.line.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               plot.margin = margin(0,0,0,0))

# data preparation for figures ----------------------------------------------

df_mm_highway_overview <- df_mm |> 
  filter(!time_of_day %in% "Morning",
         type_of_road == "main_road") |> 
  mutate(settlement_id = "Highways") |> 
  mutate(type_of_settlement = "Highways")

df_pm_internal <- df_pm |> 
  mutate(day_type = "weekday internal")

df_monitoring <- df_mm |> 
  filter(!time_of_day %in% "Morning",
         type_of_road == "non_main_road") |> 
  bind_rows(df_pm_internal) |> 
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

df_aae_range <- df_monitoring |>
  mutate(aae_range = case_when(
    aae_blue_ir <= 1.29 ~ "ff",
    aae_blue_ir > 1.29 & aae_blue_ir <= 1.63 ~ "Mixed",
    aae_blue_ir > 1.63 ~ "bb"
  ))

df_aae_range_sm <- df_aae_range |>
  filter(exp_type == "stationary_monitoring")

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

settlements_vec <- c("Ndirande", "Chirimba", "Kacheri", "Bangwe",
                     "Sunnyside", "Naperi", "Nyambadwe", "Namiwawa", "Highways")

settlement_types_order <- df_monitoring |>
  filter(exp_type == "mobile_monitoring") |>
  mutate(settlement_id = factor(settlement_id, levels = settlements_vec)) |>
  arrange(settlement_id) |>
  pull(type_of_settlement) |>
  unique()

#transformation function
scaleFUN0 <- function(x) sprintf("%.0f", x)
scaleFUN1 <- function(x) sprintf("%.1f", x)
scaleFUN2 <- function(x) sprintf("%.2f", x)

p_boxplot_mm <- df_monitoring |>
  filter(exp_type == "mobile_monitoring") |>
  mutate(
    settlement_id = factor(settlement_id, levels = settlements_vec),
    type_of_settlement = factor(type_of_settlement, levels = settlement_types_order)
  ) |>
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
  labs(y = expression("eBC concentration ("*µ*"g/m"^3*")"),
       x = "Location",
       fill = "Type of location") +
  theme +
  theme(axis.text.x = 
          element_text(angle = 45, 
                       hjust=1))


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
              size = 0.5) +
    scale_fill_manual(values = c("ff" = "#00BFC4", 
                                 "Mixed" = "#00BA38", 
                                 "bb" = "#F8766D")) +
    labs(x = "Percentage of observations (%)", 
         y = expression("Mean eBC concentration ("*µ*"g/m"^3*")"), 
         fill = "Source") +
    theme_bw() +
    scale_y_continuous(position = "right") +
    theme(
      axis.title.y.right = element_text(margin = margin(0, 0, 0, 1)),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      strip.background.y = element_blank(), 
      strip.text.y.left = element_text(angle = 0, 
                                       hjust = 0.5, 
                                       size = 7),
      strip.text.x = element_text(size = 7) # Remove boxes around facet titles
    ) +
    theme +
    facet_grid(settlement_id ~ day_type, switch = "y")
  
  return(p)
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
    geom_bar(stat = "identity", width = 0.5, color = "black") +
    geom_text(aes(label = scaleFUN0(wt_percent)), 
              size = 11/.pt, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("bb" = "#F8766D", "Mixed" = "#00BA38", "ff" = "#00BFC4")) +
    labs(x = "Percentage of wt. average eBC concentration (%)",
         fill = "Source") +
    theme_bw(base_size = 10) +  # Set base font size to 12 +
    # ggtitle("Location") +
    # theme(plot.title = element_text(hjust = -0.8, vjust = -5)) +
    theme(panel.grid.major.y = element_blank(),
          #axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
          axis.title.y = element_blank()) +
    theme +
    guides(fill = guide_legend(reverse = TRUE))
  
  return(p)
}


# Figure 3: Source apportionment mobile monitoring ------------------------

# Figure 3 (a): clustering formal settlements

p_mm_sa_cluster_formal <- p_func_sa_cluster(df_aae_overview |> 
                                              filter(type_of_settlement == "Formal",
                                                     !settlement_id %in% "Highways",
                                                     exp_type == "mobile_monitoring")) + 
  theme(legend.position = "none")

# Figure 3 (b): clustering informal settlements

p_mm_sa_cluster_informal <- p_func_sa_cluster(df_aae_overview |> 
                                                filter(type_of_settlement == "Informal",
                                                       !settlement_id %in% "Highways",
                                                       exp_type == "mobile_monitoring")) + 
  theme(legend.position = "none")

# Figure 3 (c): weighted average formal settlements

p_wt_avg_mm_formal <- p_func_wt_avg(df_aae_range %>% 
                                      filter(exp_type == "mobile_monitoring",
                                             type_of_settlement == "Formal",
                                             !settlement_id %in% "Highways")) + 
  theme(legend.position = "none")

# Figure 3 (d): weighted average informal settlements

p_wt_avg_mm_informal <- p_func_wt_avg(df_aae_range %>% 
                                        filter(exp_type == "mobile_monitoring",
                                               type_of_settlement == "Informal",
                                               !settlement_id %in% "Highways")) + 
  theme(legend.position = "none")

# Figure 3 (a) + Figure 3 (b) 

p_mm_sa <- p_mm_sa_cluster_formal + p_mm_sa_cluster_informal +
  plot_layout(guides = "collect")

# Figure 3 (c) + Figure 3 (d) 

p_wt_avg_mm <- p_wt_avg_mm_formal  + p_wt_avg_mm_informal +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


# Figure 4: Sensitivity analysis mobile monitoring ------------------------



# Figure 5: Source apportionment stationary monitoring --------------------

# Figure 5 (a): clustering stationary monitoring

p_sm_sa_cluster <- p_func_sa_cluster(df_aae_overview %>% 
                                       filter(exp_type == "stationary_monitoring")) +
  theme(legend.position = "none")

# Figure 5 (b): weighted average stationary monitoring

p_wt_avg_sm <- p_func_wt_avg(df_aae_range_sm) +
  theme(legend.position = "bottom")

# Figure 7: Diurnal pattern of eBC ----------------------------------------

p_diurnal <- confidence_intervals |>
  ggplot(aes(x = hour)) +
  geom_line(aes(y = mean_ir), color = "#003366") +
  geom_ribbon(aes(ymin = lower_ci_ir,
                  ymax = upper_ci_ir),
              fill = "#003366",
              alpha = 0.3) +
  geom_line(aes(y = mean*3.5), color = "red") +
  geom_ribbon(aes(ymin = lower_ci*3.5, ymax = upper_ci*3.5),
              fill = "red", alpha = 0.3) +
  scale_y_continuous(
    name = expression("eBC concentration ("*µ*"g/m"^3*")"),
    sec.axis = sec_axis(~./3.500, 
                        name="AAE values (470/880 nm)",
                        breaks = seq(0, 3, by = 0.5)),
    breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  facet_wrap(~settlement_id) +
  theme(legend.position = "none",
        axis.line.y.left = element_line(color = "#003366"),
        axis.line.y.right = element_line(color = "red"),
        axis.title.y.left = element_text(color = "#003366"),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.left = element_text(color = "#003366"),
        axis.text.y.right = element_text(color = "red"),
        axis.ticks.y.left = element_line(color = "#003366"),
        axis.ticks.y.right = element_line(color = "red")) +
  theme


# Save figures ------------------------------------------------------------

ggsave(
  filename = "p_mm_sa.tiff", 
  plot = p_mm_sa,                        # The ggplot object
  width = 12.7,                            # Width in cm for single-column (3.5 inches)
  height = 6,                           # Height in cm (can be adjusted as needed)
  dpi = 300,
  units = "cm", # Units for width and height
)

ggsave("p_wt_avg_mm.tiff", 
       plot = p_wt_avg_mm,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 14,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

ggsave("p_sm_sa_cluster.tiff", 
       plot = p_sm_sa_cluster,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 14,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height


ggsave("p_wt_avg_sm.tiff", 
       plot = p_wt_avg_sm,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 14,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

