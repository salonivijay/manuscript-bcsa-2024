# description -------------------------------------------------------------


# r packages --------------------------------------------------------------

library(bcsa)
library(dplyr)
library(tidyverse)

parameters <- read_csv(here::here("data/MA200-parameters.csv"))

## all monitoring data used in the manuscript

df_coll <- df_collocation |> 
  mutate(exp_type = "sensor_collocation")

df_main <- df_coll %>% 
  drop_na(ir_bcc, blue_bcc, uv_bcc)

df_main <- df_aae |>
  bind_rows(df_mm_road_type) |>
  bind_rows(df_sm) |>
  bind_rows(df_coll) |>
  drop_na(ir_bcc, blue_bcc, uv_bcc)

# data smoothing ----------------------------------------------------------

## count number of negative values in each experiment in raw data

# df_negative_raw <- df_main |>
#   group_by(id, exp_type) |> 
#   summarise(neg_irbcc = sum(ir_bcc < 0),
#             neg_bluebcc = sum(blue_bcc < 0),
#             neg_uvbcc = sum(uv_bcc < 0),
#             count = n()) |> 
#   mutate(neg_irbcc_per = neg_irbcc/count*100,
#          neg_bluebcc_per = neg_bluebcc/count*100,
#          neg_uvbcc_per = neg_uvbcc/count*100) 

### negative values - smoothing needed

## apply cma of order 3, 5 and 7 and create a temporary dataset

df_smooth_raw <- df_main

list_df_raw <- list()

for(i in (unique(df_smooth_raw$id))){

  list_df_raw[[i]] <- df_smooth_raw |>
    filter(id == i)

  list_df_raw[[i]] <- list_df_raw[[i]] |>
    mutate(ir_bcc_3 = smooth::cma(list_df_raw[[i]]$ir_bcc, order = 3, silent = TRUE)$fitted,
           blue_bcc_3 = smooth::cma(list_df_raw[[i]]$blue_bcc, order = 3, silent = TRUE)$fitted,
           uv_bcc_3 = smooth::cma(list_df_raw[[i]]$uv_bcc, order = 3, silent = TRUE)$fitted,
           ir_bcc_5 = smooth::cma(list_df_raw[[i]]$ir_bcc, order = 5, silent = TRUE)$fitted,
           blue_bcc_5 = smooth::cma(list_df_raw[[i]]$blue_bcc, order = 5, silent = TRUE)$fitted,
           uv_bcc_5 = smooth::cma(list_df_raw[[i]]$uv_bcc, order = 5, silent = TRUE)$fitted,
           ir_bcc_7 = smooth::cma(list_df_raw[[i]]$ir_bcc, order = 7, silent = TRUE)$fitted,
           blue_bcc_7 = smooth::cma(list_df_raw[[i]]$blue_bcc, order = 7, silent = TRUE)$fitted,
           uv_bcc_7 = smooth::cma(list_df_raw[[i]]$uv_bcc, order = 7, silent = TRUE)$fitted)
}

df_smooth_temp <- bind_rows(list_df_raw)

## count number of negative values after smoothing

# df_negative_count <- df_smooth_temp |>
#   group_by(exp_type) |>
#   summarise(neg_irbcc = sum(ir_bcc < 0)/n()*100,
#             neg_irbcc_3 = sum(ir_bcc_3 < 0)/n()*100,
#             neg_irbcc_5 = sum(ir_bcc_5 < 0)/n()*100,
#             neg_irbcc_7 = sum(ir_bcc_7 < 0)/n()*100) |>
#   mutate_if(is.numeric,
#             round,
#             digits = 1) 

### create a bar plot of the percentage of negative values
### number of negative values decreases with increase in the order
### of smoothing. we select the order of 5, as the decrease in the
### number of negative values between order 5 and 7 is not more than 1%

## smooth df with selected cma order (now 5)

df_smooth <- df_main

list_df <- list()

for (i in (unique(df_smooth$id))) {

  list_df[[i]] <- df_smooth |>
    filter(id == i)

  list_df[[i]] <- list_df[[i]] |>
    mutate(ir_bcc = smooth::cma(list_df[[i]]$ir_bcc, order = 5, silent = TRUE)$fitted,
           blue_bcc = smooth::cma(list_df[[i]]$blue_bcc, order = 5, silent = TRUE)$fitted,
           ir_babs = smooth::cma(list_df[[i]]$ir_babs, order = 5, silent = TRUE)$fitted,
           blue_babs = smooth::cma(list_df[[i]]$blue_babs, order = 5, silent = TRUE)$fitted,
           uv_bcc = smooth::cma(list_df[[i]]$uv_bcc, order = 5, silent = TRUE)$fitted,
           uv_babs = smooth::cma(list_df[[i]]$uv_babs, order = 5, silent = TRUE)$fitted)
}

### add aae at each observation of smooth dataframe

df_smooth <- bind_rows(list_df) |> 
  mutate(aae_uv_ir = -log(uv_babs/ir_babs)/log((parameters$wavelength[1])/(parameters$wavelength[5])),
         aae_blue_ir = -log(blue_babs/ir_babs)/log((parameters$wavelength[2])/(parameters$wavelength[5])))

### again divide each dataframe

df_collocation <- df_smooth |>
  filter(exp_type == "sensor_collocation")

df_aae <- df_smooth |>
  filter(exp_type %in% c("waste_burning", "cooking", "vehicles"))

df_sm <- df_smooth |>
  filter(exp_type == "stationary_monitoring")

df_sm_hourly <- df_sm |>
  mutate(hour = hour(time)) |>
  group_by(hour, date, settlement_id, day_type, exp_type) |>
  summarise(across(c(ir_bcc, uv_bcc, ir_babs, uv_babs, blue_babs), mean)) |>
  mutate(aae_uv_ir = -log(uv_babs/ir_babs)/log((parameters$wavelength[1])/(parameters$wavelength[5])),
         aae_blue_ir = -log(blue_babs/ir_babs)/log((parameters$wavelength[2])/(parameters$wavelength[5]))) 

df_mm <- df_smooth |>
  filter(exp_type == "mobile_monitoring")

df_pm <- df_smooth |>
  filter(exp_type == "personal_monitoring")

# # calculate aae -----------------------------------------------------------
# 
# # use blue and ir wavelength
# 
# ## the aae is calculated with and without background correction and raw data
# 
# ## correct bc abs data for background absorption
# 
# ### calculate background absorption from stationary monitoring data
# 
# df_backgr <- df_smooth |>
#   filter(exp_type == "stationary_monitoring") |>
#   summarise(mean_ir = mean(ir_babs),
#             mean_blue = mean(blue_babs))
# 
# ### extract mean bc abs values at blue and IR wavelengths
# 
# mean_irbabs = df_backgr$mean_ir
# mean_bluebabs = df_backgr$mean_blue
# 
# ### calculate bc abs by removing the background
# 
# df_aae_raw_blue_ir <- df_smooth |>
#   filter(exp_type %in% c("waste_burning", "cooking", "vehicles"),
#          ir_babs  > mean_irbabs,
#          blue_babs  > mean_bluebabs) |>
#   mutate(ir_babs_bg_corr  = ir_babs  - mean_irbabs,
#          blue_babs_bg_corr  = blue_babs - mean_bluebabs) |>
#   group_by(id, emission_source, exp_type) |>
#   summarise(across(c(blue_babs, ir_babs, ir_babs_bg_corr, blue_babs_bg_corr), mean))|>
#   mutate(aae_wo_bg_corr = -log(blue_babs/ir_babs)/log((parameƒters$wavelength[2])/(parameters$wavelength[5])),
#          aae_bg_corr = -log(blue_babs_bg_corr/ir_babs_bg_corr)/log((parameters$wavelength[2])/(parameters$wavelength[5])))|>
#   select(-blue_babs, -ir_babs, -ir_babs_bg_corr, -blue_babs_bg_corr) |>
#   arrange(by = exp_type)
# 
# ### calculate mean absorption of each experiment
# 
# #### select the aae experiments from data structure
# 
# data_structure_aae <- data_structure |>
#   filter(exp_type %in% c("waste_burning", "cooking", "vehicles"))
# 
# # calculate aae with raw data
# df_aae_blue_ir <- df_smooth |>
#   filter(exp_type %in% c("waste_burning", "cooking", "vehicles")) |>
#   group_by(id, emission_source, exp_type) |>
#   summarise(across(c(blue_babs, ir_babs), mean))|>
#   mutate(aae_raw_data = -log(blue_babs/ir_babs)/log((parameters$wavelength[2])/(parameters$wavelength[5])))|>
#   select(-blue_babs, -ir_babs) |>
#   arrange(by = exp_type) |>
#   merge(df_aae_raw_blue_ir)
# 
# # use uv and ir wavelength
# 
# ## the aae is calculated with and without background correction
# 
# ## correct bc abs data for background absorption
# 
# ### calculate background absorption from stationary monitoring data
# 
# df_backgr_uv_ir <- df_smooth |>
#   filter(exp_type == "stationary_monitoring") |>
#   summarise(mean_ir = mean(ir_babs),
#             mean_uv = mean(uv_babs))
# 
# ### extract mean bc abs values at uv and IR wavelengths
# 
# mean_irbabs = df_backgr_uv_ir$mean_ir
# mean_uvbabs = df_backgr_uv_ir$mean_uv
# 
# ### calculate bc abs by removing the background
# 
# df_aae_raw_uv_ir <- df_smooth |>
#   filter(exp_type %in% c("waste_burning", "cooking", "vehicles"),
#          ir_babs  > mean_irbabs,
#          uv_babs  > mean_uvbabs) |>
#   mutate(ir_babs_bg_corr  = ir_babs  - mean_irbabs,
#          uv_babs_bg_corr  = uv_babs - mean_uvbabs) |>
#   group_by(id, emission_source, exp_type) |>
#   summarise(across(c(uv_babs, ir_babs, ir_babs_bg_corr, uv_babs_bg_corr), mean))|>
#   mutate(aae_wo_bg_corr = -log(uv_babs/ir_babs)/log((parameters$wavelength[1])/(parameters$wavelength[5])),
#          aae_bg_corr = -log(uv_babs_bg_corr/ir_babs_bg_corr)/log((parameters$wavelength[1])/(parameters$wavelength[5])))|>
#   select(-uv_babs, -ir_babs, -ir_babs_bg_corr, -uv_babs_bg_corr) |>
#   arrange(by = exp_type)
# 
# # calculate aae with raw data
# aae_uv_ir <- df_smooth |>
#   filter(exp_type %in% c("waste_burning", "cooking", "vehicles")) |>
#   group_by(id, emission_source, exp_type) |>
#   summarise(across(c(uv_babs, ir_babs), mean))|>
#   mutate(aae_raw_data = -log(uv_babs/ir_babs)/log((parameters$wavelength[1])/(parameters$wavelength[5])))|>
#   select(-uv_babs, -ir_babs) |>
#   arrange(by = exp_type) |>
#   merge(df_aae_raw_uv_ir)
# 
# 
# df_aae_exp <- df_aae_raw
# 
# ## aae calculated for each aae experiment
# 
# aae_calculated <- aae
# 
# 
# # meteorology -------------------------------------------------------------
# 
# wind_df <- read_csv("data-raw/meteorology/meteorology.csv") |>
#   rename(windspeed = "windspeed (m/s)",
#          winddirection = "winddirection (degrees)",
#          date_time = "timestamp") |>
#   mutate(date_time = dmy_hm(date_time))
# 
# wind_df_time <- as_tbl_time(wind_df, index = date_time) |>
#   arrange(date_time)
# 
# df_temp_met <- tibble()
# 
# for(i in seq_along(unique(data_structure$id))){
#   test <- wind_df_time                         |>
#     filter_time(
#       data_structure$start_time[i]
#       ~ data_structure$end_time[i])
# 
#   test <- test |>
#     mutate(
#       id = rep(
#         data_structure$id[i],nrow(test)))
# 
#   df_temp_met <- df_temp_met |>
#     bind_rows(test)
# }
# 
# df_met <- df_temp_met |>
#   left_join(data_structure, by = "id")
# 
# usethis::use_data(df_aae_exp,
#                   df_aae,
#                   aae_calculated,
#                   aae_uv_ir,
#                   df_mm_road_type,
#                   df_mm,
#                   df_pm,
#                   df_sm,
#                   df_collocation,
#                   df_monitoring,
#                   df_negative_count,
#                   df_met,
#                   df_pm_trips,
#                   overwrite = TRUE)
# 
# 
# df_mm_map <- bcsa::df_pm |>
#   #bind_rows(df_pm) |>
#   drop_na("aae_blue_ir") |>
#   filter(settlement_id %in% c("Ndirande"),
#          id == 90)
# 
# df_mm_map |>
#   summarise(min_aae = min(aae_blue_ir),
#             max_aae = max(aae_blue_ir))
# 
# 
# library(tidyverse)
# library(lubridate)
# library(hms)
# 
# lat_mzedi <- -15.780930
# long_mzedi <- 35.094042
# 
# df_mzedi <- data.frame(cbind(lat_mzedi, long_mzedi))
# df_mzedi_map <- st_as_sf(df_mzedi, coords = c("long_mzedi", "lat_mzedi"))
# 
# map_data_sf <- st_as_sf(df_mm_map, coords = c("long", "lat"))
# 
# 
# library(RColorBrewer) # https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=9
# tmap_mode("view")
# 
# #color_palette <- alpha(colorRampPalette(c("black", "brown"))(12), alpha = 0.5)
# 
# colors_manual <- c('black', '#012B40', '#023858', '#045a8d', '#0570b0', '#3690c0', '#74a9cf', '#a6bddb', '#d0d1e6', '#ece7f2', '#fff7fb', 'brown')
# 
# colors_manual <- c('black',  '#3690c0', "brown")
# #
# aae_map <- tm_shape(map_data_sf) +
#   tm_basemap(server = "OpenStreetMap") +
#   tm_dots(col = "aae_blue_ir",
#           size = 0.1,
#           alpha = 1.5,
#           #border.col = "aae_blue_ir",
#           breaks = c(0.79, 1.29, 1.63, 2.15),
#           palette = colors_manual,
#           title = "AAE (475/880nm)") +
#   tm_facets(by = "id", nrow = 2, free.coords = FALSE)
# #tm_shape(df_mzedi_map) +
# #tm_basemap(server = "OpenStreetMap") +
# #tm_dots(col = "black",
# # size = 1,
# # alpha = 1)
# 
# ## using k-means
# 
# #### three clusters of aae were created using mobile
# #### monitoring data. the three clusters should represent
# #### dominant ff, dominant bb, mix of both
# 
# ### prepare dataset to find clusters
# 
# # df_k_means <-  df_main |>
# #   filter(exp_type == "mobile_monitoring") |>
# #   filter_all(all_vars(!is.infinite(.))) |>
# #   left_join(id_mm, by = "id") |>
# #   mutate(aae = -log(uv_babs/ir_babs)/log((parameters$wavelength[1])/(parameters$wavelength[5]))) |>
# #   filter_all(all_vars(!is.infinite(.))) |>
# #   filter(!time_of_day %in% "Morning") |>
# #   select(aae) |>
# #   drop_na()
# 
# 
# # df_k_means <- df_mm |>
# #   filter(!time_of_day %in% "Morning") |>
# #   bind_rows(df_pm) |>
# #   mutate(aae = -log(uv_babs/ir_babs)/log((parameters$wavelength[1])/(parameters$wavelength[5]))) |>
# #   filter_all(all_vars(!is.infinite(.))) |>
# #   select(aae) |>
# #   drop_na()
# 
# 
# ### perform k-means clustering
# 
# # k <- 3
# #
# # result <- kmeans(df_k_means$aae, centers = k)
# #
# # cluster_centers <- tapply(df_k_means$aae, result$cluster, mean)
# #
# # ### calculate the range of each cluster ('aae' max - 'aae' min)
# #
# # cluster_ranges <- tapply(df_k_means$aae, result$cluster, function(cluster_aae) {
# #   max_aae <- max(cluster_aae)
# #   min_aae <- min(cluster_aae)
# #   cluster_range <- cbind(max_aae, min_aae)
# #   return(cluster_range)
# # })
# #
# # unlist(cluster_ranges)
# 
# ## the clusters interpretation is - 25% of ff means ff dominating
# ## and vice-versa
# # Create a new variable 'aae_range' based on 'aae'
# 
# # data <- df_k_means %>%
# #   mutate(aae_range = case_when(
# #     aae >= 0 & aae <= 1.328 ~ "ff_dominant",
# #     aae > 1.328 & aae <= 1.759 ~ "mixed",
# #     aae > 1.759 ~ "bb_dominant"
# #   )) |>
# #   drop_na()  |>
# # group_by(settlement_id, aae_range)  |>
# #   summarize(observation_count = n(),
# #             bc_ff = mean(ir_bcc),
# #             bc_bb = mean(ir_bcc))
# 
# 
# 
