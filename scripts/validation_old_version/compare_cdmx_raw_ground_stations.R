# ============================================================================================
# IDB: Air monitoring
# ============================================================================================
# @Goal: COMPARE LASTEST VERSION AND THE NEW ONE
# 
# @Description: From 2000 to 2023 (NEED TO FINISH DOCUMENTATION)
# 
# @Summary: 
#   I.   Load libraries, utility functions and necessary data
#   II.  
#   III. 
# 
# @Date: May 2025
# @Author: Marcos
# ============================================================================================
source(here::here("src", "general_utilities", "config_utils_validation_old_version.R"))

# ============================================================================================
# I: Import  data
# ============================================================================================
# Import the raw panel of ground stations
cdmx_stations_new <- read_parquet(here::here("data", "raw",
                                             "pollution_ground_stations",
                                             "Mexico_city",
                                             "mexico_stations_2000_2023.parquet"))

possible_stations <- read.csv(here::here("data", "raw",
                                         "pollution_ground_stations",
                                         "Mexico_city",
                                         "all_station_on_metro_area.csv"))

metro_area        <- sf::st_read(here::here("data", "raw",
                                            "cities_shapefiles",
                                            "cdmx_metro.gpkg"))

metro_area_old   <- sf::st_read(here::here("data", "raw",
                                           "cities_shapefiles",
                                           "Mexico_city"))

# Define the directory with legacy data and import it
legacy_dir        <- here::here("data", "_legacy", "pollution", "Mexico_city")
cdmx_stations_old <- read_dta(here::here(legacy_dir,
                                         "Air_Pollution_Mexico_2010_2023.dta"))

# ============================================================================================
# II: Process  data
# ============================================================================================
# Apply function harmonize values, types and the order of the old dataframe
cdmx_old <- prepare_legacy_cdmx(cdmx_stations_old)

# Apply function to harmonize the new dataframe making it like the legacy one
# - restrict to 2010–2023
cdmx_new <- prepare_new_cdmx_like_legacy(
  new_df            = cdmx_stations_new,
  stations_keep_df  = possible_stations,  # filters to rows whose station_code is in `code`
  station_code_col  = "code",
  year_keep         = 2010:2023,
  tz                = "UTC"
)

# Apply function to Compare both datasets
res <- compare_panels(
  old_df  = cdmx_old,
  new_df  = cdmx_new,
  keys    = c("station_code", "year", "month", "day", "hour"),
  values  = c("pm10", "pm25", "no2", "o3", "co"),
  tol     = c(pm10 = 0, pm25 = 0, no2 = 0, o3 = 0, co = 0)
)

# Quick summaries for the console
message("\nRows only in legacy: ", nrow(res$only_old))
message("Rows only in new    : ", nrow(res$only_new))
print(res$diff_summary)

# Separate values to save
differences       <- res$diffs_long
differences_2023_pm10  <- differences %>% 
  filter(year == 2023) %>% 
  filter(variable == "pm10")
check             <- res$diff_summary

# Quick check on the non missing data
differences_no_na <- differences %>%
  filter(within_tol == FALSE)

# Quick check on the missing data from before
new_only <- res$only_new %>%
  distinct(station_code, year, month, day, hour) %>%
  arrange(station_code, year, month, day, hour)

missing_values_in_2023 <- 

# Check the rows that exist only on the old data
old_only <- res$only_old



# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
# ================= CHECKS ====================================================================
cdmx_new_2023 <- cdmx_new %>% 
  filter(year == 2023) %>%
  select(datehour, station_code, pm25, pm10)

stations_2023_with_data <- cdmx_new_2023 %>%
  group_by(station_code) %>%
  summarise(has_data = any(!is.na(pm10) | !is.na(pm25)), .groups = "drop") %>%
  filter(has_data) %>%
  pull(station_code)

stations_2023_with_pm10_data <- cdmx_new_2023 %>%
  group_by(station_code) %>%
  filter() %>%
  pull(station_code)


library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)

possible_stations_2023_filtered <- possible_stations %>% 
  filter(code %in% stations_2023_with_data)

# -- make stations an sf from lon/lat (WGS84), then project to metro_area CRS
stations_sf <- st_as_sf(
  possible_stations_2023_filtered,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(st_crs(metro_area))

# (optional) keep only stations inside the metro polygon (uncomment if desired)
# metro_union   <- st_union(metro_area)
# stations_sf   <- st_filter(stations_sf, metro_union)

# get XY for labels
lab_xy <- st_coordinates(stations_sf)
labs   <- cbind(st_drop_geometry(stations_sf), lab_xy)

# nice bounding box for coord_sf
bb <- st_bbox(metro_area)

p <- ggplot() +
  # metro polygons
  geom_sf(
    data  = metro_area,
    fill  = "#F5F7FA",
    color = "#8C9199",
    linewidth = 0.3
  ) +
  # station points
  geom_sf(
    data  = stations_sf,
    aes(fill = entity),
    shape = 21, size = 3, alpha = 0.95, color = "white", linewidth = 0.3
  ) +
  # station codes as labels
  geom_text_repel(
    data = labs,
    aes(x = X, y = Y, label = code),
    size = 3, min.segment.length = 0, seed = 42,
    box.padding = 0.25, point.padding = 0.2, max.overlaps = Inf
  ) +
  coord_sf(
    xlim = c(bb["xmin"]-10, bb["xmax"]+10),
    ylim = c(bb["ymin"]-10, bb["ymax"]+10),
    expand = FALSE
  ) +
  scale_fill_brewer(palette = "Set2", name = "Entidad") +
  labs(
    title = "Estaciones dentro del Área Metropolitana de la CDMX",
    subtitle = "Puntos = estaciones; etiquetas = códigos",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.key.size = unit(10, "pt")
  )

p


stations_sf <- st_as_sf(
  possible_stations_2023_filtered,
  coords = c("lon", "lat"),
  crs = 4674,
  remove = FALSE
) %>%
  st_transform(st_crs(metro_area_old))

bb <- st_bbox(metro_area_old)

p_old <- ggplot() +
  # metro polygons
  geom_sf(
    data  = metro_area_old,
    fill  = "#F5F7FA",
    color = "#8C9199",
    linewidth = 0.3
  ) +
  # station points
  geom_sf(
    data  = stations_sf,
    aes(fill = entity),
    shape = 21, size = 3, alpha = 0.95, color = "white", linewidth = 0.3
  ) +
  # station codes as labels
  geom_text_repel(
    data = labs,
    aes(x = X, y = Y, label = code),
    size = 3, min.segment.length = 0, seed = 42,
    box.padding = 0.25, point.padding = 0.2, max.overlaps = Inf
  ) +
  coord_sf(
    xlim = c(bb["xmin"]-10, bb["xmax"]+10),
    ylim = c(bb["ymin"]-10, bb["ymax"]+10),
    expand = FALSE
  ) +
  scale_fill_brewer(palette = "Set2", name = "Entidad") +
  labs(
    title = "Estaciones dentro del Área Metropolitana de la CDMX",
    subtitle = "Puntos = estaciones; etiquetas = códigos",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.key.size = unit(10, "pt")
  )

p_old
