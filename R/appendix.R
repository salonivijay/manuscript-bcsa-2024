# meteorology -------------------------------------------------------------
library(circular)
library(openair)
library(dplyr)
library(tidyverse)
library(ggpmisc)

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


# meteorology -------------------------------------------------------------

load(here::here("data/df_met.rda"))

# df_met is the file taken from TAHMO. The data during particular experiment 
# type was filtered from the met data from TAHMO. We cannot provide
# the raw data as it is not for distribution as per TAHMO data policy. 
# But the data can be directly requested from their website:
# https://tahmo.org/climate-data/

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
  summarize(mean_temp = mean(`temperature (degrees Celsius)`),
            sd_temp = sd(`temperature (degrees Celsius)`),
            mean_windspeed = mean(windspeed),
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
  filter(exp_type == "mobile_monitoring") %>% 
  ggplot() +
  geom_boxplot(aes(y = windspeed, x = exp_type)) +
  labs(y = expression("Wind speed (m/s)"),
       x = "Mobile monitoring") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  theme


# Figure S2: Wind rose plot during stationary monitoring period -----------

df_met_stationary <- df_met |> 
  filter(exp_type == "stationary_monitoring")

p_app_wr_stationary <- windRose(df_met_stationary, 
                          ws = "windspeed", 
                          wd = "winddirection") 

wd_sm <- as_tibble((wr_stationary[["data"]][["freqs"]]), rownames = "degrees") |> 
  filter(value == max(value)) |> 
  pull(degrees)


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
  theme


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

p_app_diurnal_day_type <- confidence_intervals_day_type |>
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
  facet_wrap(day_type~settlement_id) +
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


# Figure S5: Collocation of MA200-0416 and MA200-0420 ---------------------

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
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = expression("Mean eBC concentration ("*µ*"g/m"^3*") - 0416"),
       y = expression("Mean eBC concentration ("*µ*"g/m"^3*") - 0420"),
       color = "Settlement") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               label.x = "right", label.y = "top") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  annotate("text", x = 16, y = 17, label = "y = x", hjust = 1.1, vjust = 1.1, color = "red") +
  theme

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




