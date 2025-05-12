# description -------------------------------------------------------------

# this file contains code for the tables in the main text.

# load the smooth data ----------------------------------------------------

source(here::here("R/data-smoothing.R"))

library(geosphere)
library(broom)

# read files

# read files --------------------------------------------------------------

table_bc_literature <- read_csv(here::here("data/raw-data/bc-lit-rev.csv"), 
                                locale = readr::locale(encoding = "latin1"),
                                col_types = "cccc")

# Table 1: Distance travelled during mobile monitoring --------------------

# distance covered during mm ---------------------------------------

df_mm_pm <- df_mm 

coords_df_mm <- list()
df_filter_mm <- list()
df_distance_mm <- list()

for (i in unique(df_mm_pm$id)) {
  
  coords_df_mm[[i]] <- df_mm_pm |>
    filter(id == i) |>
    select(id, lat, long) |>
    rename(latitude = "lat",
           longitude = "long")
  
  df_filter_mm[[i]] <- df_mm_pm |>
    filter(id == i)
  
  df_distance_mm[[i]] <- df_filter_mm[[i]] |>
    mutate(distance = c(0, distVincentySphere(coords_df_mm[[i]][-nrow(coords_df_mm[[i]]),], coords_df_mm[[i]][-1,])))
  
}

df_dist_raw <- df_distance_mm |>
  bind_rows() |>
  select(id, distance, lat, long, settlement_id, exp_type, type_of_road) |>
  filter(!id %in% c(1:8)) |>
  group_by(id, exp_type, settlement_id, type_of_road) |>
  summarise(sum = sum(distance))

df_dist_mm_pm <- df_dist_raw |>
  group_by(exp_type, settlement_id, type_of_road) |>
  summarise(mean = mean(sum/1000),
            sd = sd(sum/1000)) 

df_dist_mm_pm$exp_type <- recode(df_dist_mm_pm$exp_type, 	
                                 mobile_monitoring = "Mobile monitoring",
                                 personal_monitoring = "Mobile personal monitoring")

tab01_dist_mm <- df_dist_mm_pm |> 
  rename("Monitoring type" = exp_type,
         "Location" = settlement_id,
         "Average distance [km]" = mean,
         "SD of distance [km]" = sd) |> 
  mutate_if(is.numeric,
            round,
            digits = 2) 

tab01_dist_mm %>% 
  write_csv(here::here("data/processed-data/tab01_dist_mm.csv"))

# Table 2: Literature eBC values ------------------------------------------


table_bc_literature[is.na(table_bc_literature)] <- ""  

table_bc_literature %>% 
  write_csv(here::here("data/processed-data/tab02_bc_literature.csv"))


# Table 3: sensor collocation ---------------------------------------------

df_sc_aae_1 <- df_collocation |>
  filter(id %in% c(124,125)) |> 
  mutate(time = case_when(
    serial_number == "MA200-0420" ~ time - 1,
    TRUE ~ time
  )) |> 
  mutate("Monitoring frequency (s)" = 30)

df_sc_aae_2 <- df_collocation |>
  filter(id %in% c(126,127)) |> 
  mutate("Monitoring frequency (s)" = 30)

df_sc_pre_sm <- df_collocation |>
  filter(main_exp == "pre_stationary") |> 
  mutate(time = case_when(
    serial_number == "MA200-0420" ~ time + 2,
    TRUE ~ time
  )) |> 
  mutate("Monitoring frequency (s)" = 300)

df_sc_post_sm <- df_collocation |>
  filter(id %in% c(158:163)) |> 
  mutate(time = case_when(
    id == 160 ~ time - 1,
    TRUE ~ time
  )) |>
  mutate("Monitoring frequency (s)" = 300)

df_sc_mm_1 <- df_collocation |> 
  filter(id %in% c(17:20, 25:28)) |> 
  mutate(time = case_when(
    serial_number == "MA200-0416" ~ time - 4,
    TRUE ~ time
  )) |> 
  mutate("Monitoring frequency (s)" = 30)

df_sc_mm_2 <- df_collocation |> 
  filter(id %in% c(21:24, 29:32, 57:72)) |> 
  mutate("Monitoring frequency (s)" = 30)

df_sc <- df_sc_aae_1 |> 
  bind_rows(df_sc_aae_2) |> 
  bind_rows(df_sc_pre_sm) |> 
  bind_rows(df_sc_post_sm) |> 
  bind_rows(df_sc_mm_1) |> 
  bind_rows(df_sc_mm_2) |> 
  select(date, date_start, time, ir_bcc, blue_bcc, main_exp, serial_number, `Monitoring frequency (s)`)|>
  # 6405 observations. -ve values are removed as they are considered noise
  filter(! ir_bcc < 0) 
# 6391 observations after -ve values were removed
# mutate(hour = hour(time)) |>
# group_by(hour, time, date, main_exp, serial_number) |>
# summarise(ir_bcc = mean(ir_bcc),
#           blue_bcc = mean(blue_bcc))

df_sc_wide <- df_sc |> 
  pivot_wider(names_from = serial_number,
              values_from = c(ir_bcc, blue_bcc)) |> 
  # (3228 observations) due to filter tape moving forward, data goes missing for some time,
  # hence, those missing data rows are removed 
  drop_na() 
# 3166 observations remain

lmfit_ir <- df_sc_wide |> 
  group_by(main_exp, date_start) |> 
  do(glance(lm(`ir_bcc_MA200-0416` ~ `ir_bcc_MA200-0420`, data = .))) |> 
  select(date_start, main_exp, r.squared) 

# post stationary collocation 1 and 2 were removed due to problems with the monitoring
# setup. eg., the micro-cyclone dust cap was not positioned downward 

table_sc_r2 <- df_sc_wide |> 
  filter(!main_exp %in% c("post_stationary_1", "post_stationary_2")) |> 
  group_by(main_exp, `Monitoring frequency (s)`) |> 
  summarise(count = n()) |> 
  left_join(lmfit_ir, by = "main_exp") |> 
  mutate_if(is.numeric,
            round,
            digits = 2)

table_sc_hourly_r2 <- df_sc_wide |> 
  filter(main_exp %in% c("post_stationary_3")) |>
  mutate(hour = hour(time)) |>
  group_by(hour) |> 
  summarise(`ir_bcc_MA200-0416` = mean(`ir_bcc_MA200-0416`),
            `ir_bcc_MA200-0420` = mean(`ir_bcc_MA200-0420`)) |>
  do(glance(lm(`ir_bcc_MA200-0416` ~ `ir_bcc_MA200-0420`, data = .))) |> 
  mutate_if(is.numeric,
            round,
            digits = 2) 

table_sc_r2_final <- table_sc_r2 |> 
  rename("Phase" = main_exp,
         "Date" = date_start,
         "No. of samples" = count,
         "R2" = r.squared)

table_sc_r2_final$Phase <- recode(table_sc_r2_final$Phase, 	
                                  mobile_monitoring_1 = 'Mobile monitoring',
                                  mobile_monitoring_2  = 'Mobile monitoring',
                                  aae_1 = 'AAE experiments',
                                  aae_2  = 'AAE experiments',
                                  pre_stationary = 'Pre stationary monitoring',
                                  post_stationary_3 = 'Post stationary monitoring')

# table_sc_r2_final %>% 
#   write_csv(here::here("data/processed-data/tab03_sc_r2.csv"))