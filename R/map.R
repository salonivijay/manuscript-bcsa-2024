# description -------------------------------------------------------------

# this file contains the code for map (Figure 1)

# r packages --------------------------------------------------------------

library(sf)
library(basemapR)
library(patchwork)
library(ggplot2)

source(here::here("R/data-smoothing.R"))

# define a common map theme ----------------------------------------------

theme_map <- theme(axis.text.y   = element_text(size=12, angle = 90),
               axis.text.x   = element_text(size=12),
               axis.title.y  = element_blank(),
               axis.title.x  = element_blank(),
               legend.title = element_text(size = 13),# Legend title
               legend.text = element_text(size = 12),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.line = element_line(),
               axis.line.x = element_blank(),
               axis.line.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               plot.margin = margin(0.5,0.5,0.5,0.5))


# Figure 1: Study area map ------------------------------------------------

# map of malawi 

# data from: https://data.humdata.org/dataset/cod-ab-mwi

malawi_shp_adm2 <- read_sf("data/raw-data/mwi_adm_nso_20181016_shp/mwi_admbnda_adm2_nso_20181016.shp") 

malawi_shp_adm2_4326 <- st_transform(malawi_shp_adm2, crs = 4326)

malawi_shp_adm2_4326

df_stationary_coords <- tibble(
  settlement_id = c("Sunnyside", "Ndirande"),
  lat = c(-15.79563, -15.78504),
  long = c(35.00273, 35.03723))

df_coord_map <- df_stationary_coords |> 
  filter(settlement_id %in% "Sunnyside") 

map_malawi <- ggplot()  +
  geom_sf(data = malawi_shp_adm2_4326,  
          size = 0.2) +
  geom_sf(data = malawi_shp_adm2_4326 %>% filter(ADM2_EN == "Blantyre City"), fill = "black", color = "black") +
  geom_text(data = df_coord_map, aes(x = long, y = lat, label = "Blantyre"), size = 4, vjust = -0.5, hjust = 1.3) + 
  scale_x_continuous(breaks = c(33.05, 35.5)) +
  scale_y_continuous(breaks = c(-10, -13, -16)) +
  theme_void() +
  theme_map +
  theme(panel.border = element_rect(color = "black", 
                                    size = 1, 
                                    linetype = "solid", 
                                    fill = alpha("black", 0)))

# dataset to create study area map 

df_mm_road_type <- df_mm_road_type

df_study_area <- df_mm_road_type |> 
    select(id, lat, long, exp_type, time_of_day, settlement_id, type_of_road, time) |> 
    filter(time_of_day != "Morning") %>% 
  filter(id %in% c(9:12, 33:35, 89, 92, 95, 96, 28)) 

df_study_area_mm <- df_study_area |> 
  filter(exp_type %in% c("mobile_monitoring"))

df_bound_box_min <- tibble(lat = -15.50, long = 34.90)
df_bound_box_max <- tibble(lat = -16.00, long = 35.13)

df_bound_box <- bind_rows(df_bound_box_min,
                          df_bound_box_max)
map_data_point <- st_as_sf(df_bound_box,
                           coords = c("long", "lat"),
                           crs = 4326)

df_hill_coords <- tibble(
  hills = c("Ndirande", "Soche", "Bangwe", "Michiru", "Mpingwe"),
  lat = c(-15.755035169860944, -15.83992057745279, -15.821992758388108, -15.74921472763114, -15.81588163476441),
  long = c(35.05728286319883, 35.02640111468117, 35.10297669597419, 34.96670954360923, 35.08556482611086))

map_data_hill <- st_as_sf(df_hill_coords, 
                          coords = c("long", "lat"), 
                          crs = 4326)

# Define the color-blind friendly palette
cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
  "#D55E00", "#CC79A7", "#999999"
)

# Define hill colors (color-blind friendly)
hill_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"
)

# Create a custom shape for hills (triangle)
hill_shape <- 24  # Filled triangle

map_blantyre <- ggplot() +
  base_map(st_bbox(map_data_point),
           basemap = "google-satellite",
           increase_zoom = 3) +
  geom_sf(data = malawi_shp_adm2_4326 %>% 
            filter(ADM2_EN == "Blantyre City"), 
          alpha = 0,
          color = "white",
          linewidth = 1) +
  geom_path(data = df_study_area_mm,
            aes(x = long,
                y = lat,
                group = as.factor(id),
                color = as.factor(settlement_id)),
            size = 1.5) +
  geom_point(data = df_stationary_coords,
             aes(x = long,
                 y = lat,
                 shape = factor(settlement_id)),
             colour = "red", 
             size = 6,
             stroke=1.5) +
  geom_point(data = df_stationary_coords,
             aes(x = long,
                 y = lat,
                 shape = factor(settlement_id)),
             color = "yellow",
             stroke=1.5) +
  geom_point(data = df_hill_coords,
             aes(x = long,
                 y = lat,
                 fill = hills),
             size = 5,
             color = "white",
             shape = hill_shape) +
  scale_fill_manual(name = "Hills", values = hill_colors) +
  scale_shape_manual(name = "Stationary monitoring",
                     values = c(23, 22)) +
  scale_color_manual(name = "Mobile route", values = cb_palette) +
  scale_x_continuous(breaks = c(35.0, 35.1)) +  
  scale_y_continuous(breaks = c(-15.85, -15.75)) +  
  ggspatial::annotation_scale(location = "tl", 
                              text_col = "white") +
  ggspatial::annotation_north_arrow(location = "bl", 
                                    which_north = "true", 
                                    height = unit(1, "cm"), 
                                    width = unit(1, "cm")) +
  xlab("Longitude") + ylab("Latitude") +
  theme_void() +
  theme(panel.border = element_rect(color = "black", 
                                    size = 1, 
                                    linetype = "solid", 
                                    fill = alpha("black", 0))) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme_map


p_map_study <- map_malawi + map_blantyre

p_map_routes <- ggplot() +
  base_map(st_bbox(map_data_point), 
           basemap = "google-satellite", 
           increase_zoom = 7) +
  geom_path(data = df_study_area_mm,
            aes(x = long,
                y = lat,
                group = as.factor(id),
                color = as.factor(settlement_id)),
            size = 1.5) +
  scale_color_manual(name = "Mobile route", values = cb_palette) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~settlement_id, scales = "free", nrow = 2) +
  theme(panel.border = element_rect(color = "black", size = 1, linetype = "solid", fill = alpha("black", 0))) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 13),
        legend.position = "none")

ggsave("figures/p_map_routes.jpeg",
       plot = p_map_routes,
       width = 8.9,
       height = 6,
       dpi = 300
)

ggsave("figures/p_map_study.jpeg",
       plot = p_map_study,
       width = 8.9,
       height = 5,                           # Height in cm (can be adjusted as needed)
       dpi = 300,
)


library(sf)
library(ggplot2)
library(dplyr)
library(stars)
library(viridis)

# Load your mobile monitoring data
data <- df_mm %>% 
  filter(!time_of_day %in% "Morning")

# Convert data to sf object
data_sf <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)

# Define grid resolution (100m x 100m)
grid <- st_make_grid(data_sf, cellsize = c(0.001, 0.001))  # Approx. 100m at equator
grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)

# Spatial join to assign points to grid cells
joined_data <- st_join(data_sf, grid_sf)

# Compute average concentration over 8 days per grid cell
avg_concentration <- joined_data %>%
  group_by(grid_id) %>%
  summarise(avg_conc = mean(ir_bcc, na.rm = TRUE)/1000,
            median_conc = median(ir_bcc, na.rm = TRUE)/1000,
            median_aae = median(aae_blue_ir, na.rm = TRUE))

# Ensure grid_sf has grid_id
grid_sf <- grid_sf %>% mutate(grid_id = seq_along(geometry))

# Merge with grid to retain spatial structure
final_map <- st_join(grid_sf, avg_concentration)

# Ensure avg_conc column exists and is not empty
#final_map$avg_conc[is.na(final_map$avg_conc)] <- 0
# Categorize concentrations into bins
final_map$conc_category <- cut(final_map$median_conc, 
                               breaks = c(-Inf, 5, 10, 15, 20, Inf),
                               labels = c("<5", "5-10", "10-15", "15-20", ">20"))

final_map$aae_category <- cut(final_map$median_aae, 
                               breaks = c(-Inf, 1.29, 1.63, Inf),
                               labels = c("< 1.29 (fossil-fuel-based)", 
                                          "1.29-1.63 (mix of two)", 
                                          "> 1.63 (biomass-based)"))

# Plot the result
pollution_map <- ggplot() +
  base_map(st_bbox(map_data_point), 
           basemap = "mapnik", 
           increase_zoom = 7) +
  geom_sf(data = final_map, aes(fill = conc_category),color = NA) +
  scale_fill_manual(na.value = "0",
                    na.translate = FALSE,
                    values = c("<5"     = "#006d77",
                               "5-10"   = "#66c2a4", 
                               "10-15"  = "#9e4f96",
                               "15-20"  = "#cc4c02", 
                               ">20"    = "#a50f15"   )) +
  theme(legend.position = "right") +
  labs(fill = expression("eBC concentration (µg m"^-3*")")) +
  theme_map  

print(pollution_map)

# ggsave("figures/p_concentration_map_1.jpeg",
#        plot = pollution_map,
#        width = 8.9,
#        height = 5,                           # Height in cm (can be adjusted as needed)
#        dpi = 300,
# )

aae_map <- ggplot() +
  base_map(st_bbox(map_data_point), 
           basemap = "mapnik", 
           increase_zoom = 7) +
  geom_sf(data = final_map, aes(fill = aae_category),color = NA) +
  scale_fill_manual(na.value = "0",
                    na.translate = FALSE,
                     values = c("> 1.63 (biomass-based)" = "brown", 
                                "1.29-1.63 (mix of two)" = "blue", 
                                "< 1.29 (fossil-fuel-based)" = "black")) +
  theme(legend.position = "right") +
  labs(fill = "AAE470/880 (Emission source)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme_map  
  # theme(legend.position = "bottom",
  #       legend.box="horizontal",
  #       legend.margin = margin(0.05, 0.05, 0.05, 0.05, "cm"),
  #       legend.spacing.y = unit(0, 'cm'),
  #       legend.key.height = unit(0.3, "cm"), 
  #       legend.box.background = element_rect(color = "black", 
  #                                            size = 0.1),
  #       legend.text = element_text(margin = margin(t = 0, b = 0)))

aae_map

# ggsave("figures/p_aae_map.jpeg",
#        plot = aae_map,
#        width = 8.9,
#        height = 5,                           # Height in cm (can be adjusted as needed)
#        dpi = 300,
# )


library(magick)
library(grid)
library(jpeg)
library(cowplot)
pollution_map + aae_map

img_aae <- readJPEG(here::here(("figures/p_aae_map.jpeg")))  # or use readJPEG if it's a .jpg
p_image_aae <- ggdraw() + draw_image(img) 

img_pollution <- readJPEG(here::here(("figures/p_concentration_map_1.jpeg")))  # or use readJPEG if it's a .jpg
p_image_pollution <- ggdraw() + draw_image(img_pollution) 

p_image_pollution / p_image_aae +
  plot_layout(guides = "keep")

aligned_plot <- plot_grid(
  p_image_pollution, 
  p_image_aae, 
  ncol = 1, 
  align = "v",         # vertical alignment
  axis = "l",         # align left and right axes
  rel_widths = c(1, 1)
)

