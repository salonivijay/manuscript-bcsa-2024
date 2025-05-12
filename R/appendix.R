# meteorology -------------------------------------------------------------
library(circular)
library(openair)
library(dplyr)
library(tidyverse)
library(ggpmisc)

source(here::here("R/data-smoothing.R"))
source(here::here("R/tables.R"))

# define a common plot theme ----------------------------------------------

theme <- theme(axis.text.y   = element_text(size=10),
               axis.text.x   = element_text(size=10),
               axis.title.y  = element_text(size=10),
               axis.title.x  = element_text(size=10),
               legend.title = element_text(size = 10),# Legend title
               legend.text = element_text(size = 10),
               #panel.background = element_rect(fill='transparent'), #transparent panel bg.
               #plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg.
               #axis.line = element_line(colour = "black"),
               panel.border = element_rect(colour = "black", fill=NA, size=0.5),
               axis.line = element_line(),
               axis.line.x = element_blank(),
               axis.line.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               plot.margin = margin(0,0,0,0))


# meteorology -------------------------------------------------------------

load(here::here("data/raw-data/df_met.rda"))

# df_met is the file taken from TAHMO. The data during particular experiment 
# type was filtered from the met data from TAHMO. We cannot provide
# the raw data as it is not for distribution as per TAHMO data policy. 
# But the data can be directly requested from their website:
# https://tahmo.org/climate-data/

df_met <- df_met %>% 
filter(!id %in% c(1:8, 17:24,65:72)) 

# summary table of meteorology during different experiments
df_avg_met <- df_met |> 
  select(exp_type, 
         `temperature (degrees Celsius)`, 
         windspeed, 
         `precipitation (mm)`, 
         `relativehumidity (-)`, 
         winddirection) |> 
  drop_na() |> 
  group_by(exp_type) |> 
  summarize(count = n(),
            mean_temp = mean(`temperature (degrees Celsius)`),
            sd_temp = sd(`temperature (degrees Celsius)`),
            mean_windspeed = mean(windspeed),
            median_windspeed = median(windspeed),
            sd_windspeed = sd(windspeed),
            mean_rain = mean(`precipitation (mm)`),
            sd_rain = sd(`precipitation (mm)`),
            mean_humidity = mean(`relativehumidity (-)`),
            sd_humidity = sd(`relativehumidity (-)`), 
            mean_wd = mean(circular(winddirection, units = "degrees")))

df_avg_wind <- df_met |> 
  select(exp_type, windspeed, winddirection) |> 
  drop_na() |> 
  group_by(exp_type) |> 
  summarize(
            mean_windspeed = mean(windspeed),
            sd_windspeed = sd(windspeed),
            max_windspeed = max(windspeed),
            min_windspeed = min(windspeed),
            mean_wd = mean(circular(winddirection, units = "degrees")))


# Figure S1: Box-plot of wind speed during mobile monitoring times --------

p_app_boxplot_windspeed_mobile <- df_met %>% 
  filter(exp_type == "mobile_monitoring",
         !id %in% c(1:8)) %>% 
  #remove values during sensor collocation (remove 420 monitor - random)
  filter(!id %in% c(17:24,65:72)) %>% 
  ggplot() +
  geom_boxplot(aes(y = windspeed, x = exp_type)) +
  labs(y = expression("Wind speed (m s"^{-1}*")"),
       x = "Mobile monitoring") +
  theme +
  theme(axis.text.x = element_blank()) 

p_app_boxplot_windspeed_mobile

wind_df_met <- df_met %>% 
  filter(exp_type == "mobile_monitoring",
         !id %in% c(1:8)) %>% 
#remove values during sensor collocation (remove 420 monitor - random)
filter(!id %in% c(17:24,65:72))  

wind_df_met %>% 
  summarise(max_windspeed = max(windspeed))

wind_df_mm <- df_mm_road_type %>%
  filter(exp_type == "mobile_monitoring", !id %in% 1:8) %>%
  mutate(date_time = floor_date(date_time, "5 minutes")) %>%
  group_by(id, session_id, date_time) %>%
  summarise(ir_bcc = mean(ir_bcc, na.rm = TRUE), .groups = "drop")

library(data.table)

# Convert both to data.table format
dt_met_mm <- as.data.table(wind_df_met)
dt_mm_mm <- as.data.table(wind_df_mm)


# Set keys for rolling join
setkey(dt_mm_mm, id, session_id, date_time)
setkey(dt_met_mm, id, session_id, date_time)

# Perform rolling join: nearest time, match on id and session_id
joined_df <- dt_mm_mm[dt_met_mm, roll = "nearest"]

joined_df %>% 
ggplot(aes(x = windspeed, y = ir_bcc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ id, scales = "free") 

cor.test(joined_df$windspeed, joined_df$ir_bcc, method = "spearman")

p_app_windrose_mobile <- windRose(wind_df_met, 
         ws = "windspeed", 
         wd = "winddirection") 

ggsave("figures/appendix/fig-S01.jpeg", 
       plot = p_app_boxplot_windspeed_mobile,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 8.9,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

ggsave("figures/appendix/fig-S01-2.jpeg", 
       plot = p_app_windrose_mobile,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 8.9,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

# Figure S2: Wind rose plot during stationary monitoring period -----------

df_met_stationary <- df_met |> 
  filter(exp_type == "stationary_monitoring")

p_app_wr_stationary <- windRose(df_met_stationary, 
                          ws = "windspeed", 
                          wd = "winddirection") 

wd_sm <- as_tibble((p_app_wr_stationary[["data"]][["freqs"]]), rownames = "degrees") |> 
  filter(value == max(value)) |> 
  pull(degrees)

p_app_wr_stationary

# Figure S3: eBC v/s wind speed during stationary monitoring --------

df_windspeed_stationary <- df_met_stationary %>% 
  left_join(df_sm, by = c("id", 
                          "exp_type", 
                          "date_start",
                          "start_time",
                          "end_time",
                          "date_end",
                          "session_id", 
                          "serial_number", 
                          "date_time"))

library(viridis)

p_app_ebc_windspeed_stationary <- df_windspeed_stationary %>% 
  drop_na(windspeed, ir_bcc) %>% 
  ggplot(aes(x = windspeed, y = ir_bcc/1000)) +
  geom_point(aes(color = settlement_id), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = expression("Wind speed (m/s)"),
       y = expression("Mean eBC concentration ("*µ*"g/m"^3*")"),
       color = "Settlement") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  facet_wrap(~settlement_id, scales = "free") +
  theme_minimal() +
  theme +
  scale_color_viridis(discrete = TRUE) # Add this line for color-blind friendly colors

ggsave("figures/appendix/fig-S03.jpeg", 
       plot = p_app_ebc_windspeed_stationary,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 6,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

# Figure S4: Diurnal pattern weekday and weekend --------------------------

confidence_intervals_day_type <- df_sm_hourly %>%
  mutate(ir_bcc = ir_bcc/1000) |> 
  group_by(hour, settlement_id, day_type) |> 
  summarize(lower_ci = t.test(aae_blue_ir)$conf.int[1],
            upper_ci = t.test(aae_blue_ir)$conf.int[2],
            mean = mean(aae_blue_ir),
            lower_ci_ir = t.test(ir_bcc)$conf.int[1],
            upper_ci_ir = t.test(ir_bcc)$conf.int[2],
            mean_ir = mean(ir_bcc)) 

# Define colorblind-friendly colors
color1 <- "#E69F00"  # Orange
color2 <- "#56B4E9"  # Light blue

p_app_diurnal_day_type <- confidence_intervals_day_type |>
  ggplot(aes(x = hour)) +
  geom_line(aes(y = mean_ir), color = color1) +
  geom_ribbon(aes(ymin = lower_ci_ir,
                  ymax = upper_ci_ir),
              fill = color1,
              alpha = 0.3) +
  geom_line(aes(y = mean*3.5), color = color2) +
  geom_ribbon(aes(ymin = lower_ci*3.5, ymax = upper_ci*3.5),
              fill = color2, alpha = 0.3) +
  scale_y_continuous(
    name = expression("eBC concentration ("*µ*"g/m"^3*")"),
    sec.axis = sec_axis(~./3.500, 
                        name="AAE values (470/880 nm)",
                        breaks = seq(0, 3, by = 0.5)),
    breaks = seq(0, 12, by = 2)) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  facet_wrap(day_type~settlement_id) +
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

ggsave("figures/appendix/fig-S04.jpeg", 
       plot = p_app_diurnal_day_type,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 8.9,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

p_app_coll <- df_sc |> 
  pivot_wider(names_from = serial_number,
              values_from = c(ir_bcc, blue_bcc)) |> 
  drop_na() |>
  filter(main_exp == "post_stationary_3",
         `ir_bcc_MA200-0416` > 0,
         `ir_bcc_MA200-0420` > 0,) |> 
  mutate(`ir_bcc_MA200-0416` = `ir_bcc_MA200-0416`/1000,
         `ir_bcc_MA200-0420` = `ir_bcc_MA200-0420`/1000) %>% 
  ggplot(aes(x = `ir_bcc_MA200-0416`, y = `ir_bcc_MA200-0420`)) +
  geom_point(size = 0.5, alpha = 0.6) +
  geom_smooth(method = "lm", color = "#0072B2") +
  labs(x = expression("Mean eBC concentration ("*µ*"g/m"^3*") - 0416"),
       y = expression("Mean eBC concentration ("*µ*"g/m"^3*") - 0420"),
       color = "Settlement") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#D55E00") +
  annotate("text", x = 14, y = 15, label = "y = x", hjust = 1.1, vjust = 1.1, color = "#D55E00") +
  theme +
  scale_color_viridis_d()



ggsave("figures/appendix/fig-S05.jpeg", 
       plot = p_app_coll,  # your plot object
       width = 12.7,                            # Width in cm for single-column (3.5 inches)
       height = 8.9,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height

# Figure S6: Collocation of hourly data from MA200-0416 and MA200-0420 -------

p_app_coll_hourly <- df_sc |> 
  pivot_wider(names_from = serial_number,
              values_from = c(ir_bcc, blue_bcc)) |> 
  drop_na() |>
  filter(main_exp == "post_stationary_3",
         `ir_bcc_MA200-0416` > 0,
         `ir_bcc_MA200-0420` > 0,) |> 
  mutate(`ir_bcc_MA200-0416` = `ir_bcc_MA200-0416`/1000,
         `ir_bcc_MA200-0420` = `ir_bcc_MA200-0420`/1000) %>% mutate(hour = hour(time)) |>
  group_by(hour) |> 
  summarise(mean_0416 = mean(`ir_bcc_MA200-0416`),
            mean_0420 = mean(`ir_bcc_MA200-0420`)) %>% 
  ggplot(aes(x = mean_0416, y = mean_0420)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = expression("Mean eBC concentration ("*µ*"g/m"^3*") - 0416"),
       y = expression("Mean eBC concentration ("*µ*"g/m"^3*") - 0420"),
       color = "Settlement") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  annotate("text", x = 15, y = 16, label = "y = x", hjust = 1.1, vjust = 1.1, color = "red") +
  theme

ggsave("figures/appendix/fig-S06.jpeg", 
       plot = p_app_coll_hourly,  # your plot object
       width = 8.9,                            # Width in cm for single-column (3.5 inches)
       height = 8.9,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
       units = "cm") # Units for width and height


