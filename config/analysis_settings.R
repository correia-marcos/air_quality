# Shared analytical choices. Source this file explicitly in scripts and _targets.R.
# City definitions and source locations remain in src/city_specific/.

# Distances use the metro-centred AEQD projection and an internal polygon point.
distance_metric <- "aeqd"
distance_representative_point <- "point_on_surface"

# Preserve the manuscript target seed during the structural migration.
manuscript_seed <- 20230901L

# Outliers use every available year, including temporal windows crossing year boundaries.
outlier_missing_temporal <- "continue"
outlier_missing_neighbor <- "second"

# IDW uses 2023 at 3/5/20 km; regression results use only the 3 and 5 km estimates.
analysis_year <- 2023L
idw_buffers_km <- c(3, 5, 20)
idw_distance_power <- 1
exposure_buffers_km <- c(3L, 5L)
individual_exposure_buffer_km <- 3L

# The imputed robustness specification uses education quintiles at 3 km in 2023.
imputation_year <- 2023L
imputed_exposure_buffer_km <- 3
imputation_pollutants <- c("pm10", "pm25")

# Exposure rendering keeps the manuscript's city labels, panel order and filenames.
exposure_city_labels <- c(Bogota = "Bogotá", CDMX = "Mexico City",
  Santiago = "Santiago", `Sao Paulo` = "São Paulo",
  `Santiago (comuna, 2024)` = "Santiago (commune, 2024 census)")
exposure_city_files <- c(Bogota = "bogota", CDMX = "mexico_city",
  Santiago = "santiago", `Sao Paulo` = "sao_paulo",
  `Santiago (comuna, 2024)` = "santiago_comuna_2024")
exposure_paper_files <- c(bogota = "bogota", mexico_city = "mexico",
                         santiago = "santiago", sao_paulo = "saopaulo")
exposure_table_cities_edu <- c("Bogota", "CDMX", "Santiago", "Sao Paulo")
exposure_table_cities_inc <- c("CDMX", "Sao Paulo")
exposure_table_labels_edu <- c(Bogota = "Bogota", CDMX = "Mexico City",
                              Santiago = "Santiago", `Sao Paulo` = "Sao Paulo")
exposure_table_labels_inc <- c(CDMX = "Mexico City (income quintiles)",
                              `Sao Paulo` = "Sao Paulo (income deciles)")

# Missingness tables retain the original reported-hour panel and its three dimensions.
missing_table_panel <- "raw"
missing_table_dimensions <- c("station", "month", "hour")
