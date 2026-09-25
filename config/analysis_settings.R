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
