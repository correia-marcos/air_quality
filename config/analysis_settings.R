# Shared analytical choices. Source this file explicitly in scripts and _targets.R.
# City definitions and source locations remain in src/city_specific/. Sections follow the
# analysis folders of scripts/ and config/targets/, in pipeline order.

# ========================================================================================
# Shared
# ========================================================================================
# The year analysed, the seed of the manuscript targets graph and the pollutants reported
# by the descriptive, monitoring and density summaries.
analysis_year <- 2023L
manuscript_seed <- 20230901L
summary_pollutants <- c("pm10", "pm25")

# ========================================================================================
# Inputs
# ========================================================================================
# Distances use the metro-centred AEQD projection and an internal polygon point.
distance_metric <- "aeqd"
distance_representative_point <- "point_on_surface"

# Gross-value screening precedes statistical cleaning; NULL disables bounds.
pollution_upper_bounds <- c(pm25 = 2000, pm10 = 6000) # Equality passes

# Outliers use every available year, including temporal windows crossing year boundaries.
outlier_missing_temporal <- "continue"
outlier_missing_neighbor <- "second"

# ========================================================================================
# Descriptives
# ========================================================================================
# Missingness uses the cleaned panels, which hold every calendar hour. Proportions are
# computed for every dimension; the station, month and hour tables are rendered, outside
# the manuscript.
missing_dimensions <- c("station", "month", "hour", "day_of_week")
missing_table_dimensions <- c("station", "month", "hour")

# Readings by education quintile, reported as available shares. "nearest_unit" gives each
# station the quintile of its nearest census unit and pools those stations' hours. "idw"
# follows the paper draft: each station's share is averaged into the census units within
# the buffer with inverse-distance weights (idw_distance_power), then into individuals
# and their education quintiles. Both use only the stations reporting each pollutant in
# analysis_year, as in the station table.
availability_report        <- "available"
missing_quintile_method    <- "nearest_unit"
missing_quintile_buffer_km <- 3

# Distance-band descriptions use cumulative radii and each census's own indicators.
distance_band_radii_km <- c(1, 3, 5, 10, 20)

# Bogotá 2018: these summaries do not use household-head or ethnicity indicators.
band_shares_bogota <- c("Share of adults" = "adult",
                        "Share of women" = "women",
                        "Share of employed" = "employed",
                        "Share with no education" = "no_education",
                        "Share with graduate education" = "graduate_educ")
band_means_bogota <- c("Mean age" = "age",
                       "Mean years of schooling" = "educ_years")

# Mexico City 2020: include household head, indigenous identity and income.
band_shares_cdmx <- c("Share of adults" = "adult",
                      "Share of women" = "women",
                      "Share of HH women" = "hh_head_women",
                      "Share of indigenous" = "indigena",
                      "Share of employed" = "employed",
                      "Share with no education" = "no_education",
                      "Share with graduate education" = "graduate_educ")
band_means_cdmx <- c("Mean age" = "age",
                     "Mean years of schooling" = "educ_years",
                     "Mean income" = "income")

# Santiago 2017: age is stored in raw_p09; this census supplies no income measure.
band_shares_santiago <- c("Share of adults" = "adult",
                          "Share of women" = "women",
                          "Share of HH women" = "hh_head_women",
                          "Share of indigenous" = "indigena",
                          "Share of employed" = "employed",
                          "Share with no education" = "no_education",
                          "Share with graduate education" = "graduate_educ")
band_means_santiago <- c("Mean age" = "raw_p09",
                         "Mean years of schooling" = "educ_years")

# São Paulo 2010: include race, formal/informal employment and income.
band_shares_sp <- c("Share of adults" = "adult",
                    "Share of women" = "women",
                    "Share of whites" = "white",
                    "Share of blacks" = "black_pardo",
                    "Share of formal employees" = "formal_emp",
                    "Share of informal employees" = "informal_emp",
                    "Share with no education" = "no_education",
                    "Share with graduate education" = "graduate_educ")
band_means_sp <- c("Mean age" = "age",
                   "Mean years of schooling" = "educ_years",
                   "Mean income" = "income")

# ========================================================================================
# Monitoring
# ========================================================================================
# Station context and coverage figures: buffer radii and the WHO interim targets IT1, IT2.
station_context_buffer_km <- 3
monitoring_radii_km <- c(3, 5, 10)
station_who_thresholds <- list(pm10 = c(it1 = 150, it2 = 100),
                               pm25 = c(it1 = 75, it2 = 50))

# Scatter outcomes, with the filename suffixes the manuscript uses.
station_scatter_labels <- c(
  hrs_d_pm10_it1 = "Hours above PM10 IT1 in 2023",
  hrs_d_pm25_it1 = "Hours above PM2.5 IT1 in 2023",
  hrs_d_pm10_it2 = "Hours above PM10 IT2 in 2023",
  hrs_d_pm25_it2 = "Hours above PM2.5 IT2 in 2023",
  avg_pm10 = "Mean annual PM10 concentration in 2023",
  avg_pm25 = "Mean annual PM2.5 concentration in 2023")
station_scatter_tags <- c(
  hrs_d_pm10_it1 = "2023",
  hrs_d_pm25_it1 = "pm25_2023",
  hrs_d_pm10_it2 = "IT2_2023",
  hrs_d_pm25_it2 = "IT2_pm25_2023",
  avg_pm10 = "2023_pm10mean",
  avg_pm25 = "2023_pm25mean")

# ========================================================================================
# Exposure
# ========================================================================================
# IDW runs every buffer. Regressions use exposure_buffers_km: 3 km is the main
# specification and 5 km a robustness check. 20 km feeds the density figures and the
# specification sensitivity.
idw_buffers_km <- c(3, 5, 20)
idw_distance_power <- 1
exposure_buffers_km <- c(3L, 5L)
exposure_density_radii_km <- c(3, 20)

# City labels, panel order and file names of the exposure figures and tables. The
# Santiago (comuna, 2024) entries serve figure_santiago_2024_exposure.R.
exposure_city_labels <- c(Bogota = "Bogotá", CDMX = "Mexico City", 
                          Santiago = "Santiago", `Sao Paulo` = "São Paulo",
                          `Santiago (comuna, 2024)` = "Santiago (commune, 2024 census)")
exposure_city_files   <- c(Bogota = "bogota", CDMX = "mexico_city",
                          Santiago = "santiago", `Sao Paulo` = "sao_paulo",
                          `Santiago (comuna, 2024)` = "santiago_comuna_2024")
exposure_paper_files  <- c(bogota = "bogota", mexico_city = "mexico",
                          santiago = "santiago", sao_paulo = "saopaulo")
exposure_table_cities_edu <- c("Bogota", "CDMX", "Santiago", "Sao Paulo")
exposure_table_cities_inc <- c("CDMX", "Sao Paulo")
exposure_table_labels_edu <- c(Bogota = "Bogota", CDMX = "Mexico City",
                              Santiago = "Santiago", `Sao Paulo` = "Sao Paulo")
exposure_table_labels_inc <- c(CDMX = "Mexico City (income quintiles)",
                              `Sao Paulo` = "Sao Paulo (income deciles)")

# ========================================================================================
# Imputation
# ========================================================================================
# Exposure by education quintile with imputed hourly readings.
imputation_year <- 2023L
imputed_exposure_buffer_km <- 3
imputation_pollutants <- c("pm10", "pm25")

# ========================================================================================
# Temporal
# ========================================================================================
# Density evaluation limits and styling; bandwidths retain all finite observations.
kernel_reference_years <- c(2019L, 2022L)
kernel_pm10_limit <- 500
kernel_pm25_limit <- 100
kernel_pm25_reference_limit <- 250
kernel_city_colours <- c("Bogotá" = "black", "Mexico City" = "black",
                         "São Paulo" = "black", "Santiago" = "red")
kernel_city_linetypes <- c("Bogotá" = "solid", "Mexico City" = "dashed",
                           "São Paulo" = "dotdash", "Santiago" = "solid")

# ========================================================================================
# Robustness
# ========================================================================================
# Individual-level regressions use the exposure at this buffer.
individual_exposure_buffer_km <- 3L

# ========================================================================================
# Specification sensitivity
# ========================================================================================
# Designs A/B/C run for three cities on each declared variant: one IDW buffer and one
# individual education grouping. Each id names its target and fixes its output folder
# and file prefix (resolution_specification()). Other groupings reuse the quintile run's
# Design B exposure at the same buffer, so the quintile runs come first.
resolution_cities <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
resolution_specifications <- list(
  quintile_3km          = list(buffer_km = 3,  grouping = "edu_quintile"),
  quintile_20km         = list(buffer_km = 20, grouping = "edu_quintile"),
  quintile_split_3km    = list(buffer_km = 3,  grouping = "edu_quintile_split"),
  quintile_split_20km   = list(buffer_km = 20, grouping = "edu_quintile_split"),
  education_group3_3km  = list(buffer_km = 3,  grouping = "edu_group3"),
  education_group3_20km = list(buffer_km = 20, grouping = "edu_group3"),
  education_level_3km   = list(buffer_km = 3,  grouping = "edu_level"),
  education_level_20km  = list(buffer_km = 20, grouping = "edu_level"))
# Review figures are drawn for the 3 km variants.
resolution_figure_specifications <- c("quintile_3km", "quintile_split_3km",
                                      "education_group3_3km", "education_level_3km")
# DuckDB memory limit (GB) of the Design B rebuild. Choose based on your machine's limits.
resolution_duckdb_mem_gb <- 32

# IDW buffer x exponent grid on which Design A is estimated.
resolution_grid_buffers_km      <- c(1, 2, 3, 5, 8, 10, 12, 15, 17, 20)
resolution_grid_distance_powers <- c(1, 2)
# Exponent 2 runs on a subset of the buffers, a reduction for the runtime budget of the
# 14 October 2026 run; exponent 1 runs on every buffer.
resolution_grid_power2_buffers_km <- c(1, 2, 3, 5, 10, 20)

# Station sets keep stations with valid hours in analysis_year. A station enters a set
# for a pollutant when that pollutant and every pollutant in `require` have valid hours
# in at least `min_months` months. Sets run with exponent 1.
resolution_station_sets <- list(
  both_pollutants = list(require = c("pm10", "pm25"), min_months = 1L),
  three_months    = list(require = character(),       min_months = 3L))
resolution_station_set_buffers_km <- c(1, 2, 3, 5, 10, 20)

# Harmonized education levels: the six bands each census module derives from educ_years,
# lowest to highest. Bogota's "Some tertiary" includes completed tecnica and normalista.
education_level_bands <- c("no_education", "high_school_incomplete",
                           "high_school_complete", "college_incomplete",
                           "college_complete", "graduate_educ")
education_level_labels <- c("None", "Below secondary", "Secondary", "Some tertiary",
                            "Bachelor's", "Graduate")

# Three attainment groups merge the six bands: the group of each band, lowest band first.
education_group3_of_level <- c(1L, 1L, 2L, 2L, 3L, 3L)
education_group3_labels <- c("Below secondary", "Secondary to some tertiary",
                             "Bachelor's or more")

# Figure labels, headline contrast and caption for each grouping.
resolution_grouping_labels <- list(
  edu_quintile = list(
    group_labels = paste0("Q", 1:5),
    group_name   = "quintiles",
    gap_label    = "Q1 - Q5",
    profile_note = paste("Group means use existing person weights;",
                         "consult coverage tables for each population.")),
  edu_quintile_split = list(
    group_labels = paste0("Q", 1:5),
    group_name   = "quintiles",
    gap_label    = "Q1 - Q5 (split ties)",
    profile_note = paste("Quintiles with tied schooling values split proportionally",
                         "across adjacent quintiles; group means use person weights.")),
  edu_group3 = list(
    group_labels = education_group3_labels,
    group_name   = "groups",
    gap_label    = "Below secondary - Bachelor's+",
    profile_note = paste("Three harmonized attainment groups; group means use person",
                         "weights. Bogota codes the highest level attended, so its top",
                         "group includes adults who did not complete a degree.")),
  edu_level = list(
    group_labels = education_level_labels,
    group_name   = "levels",
    gap_label    = "None - Graduate",
    profile_note = paste("Harmonized education levels; group means use existing person",
                         "weights. Sao Paulo's census cannot code 'Some tertiary'.",
                         "Bogota's 'Some tertiary' includes completed tecnica and",
                         "normalista.")))

# ========================================================================================
# Not in the paper
# ========================================================================================
# Santiago 2024 groups residents by 2024 commune; IDW and exposure use these buffers.
santiago_2024_buffers_km <- c(3L, 5L)

# Satellite extraction choices live in config/merra2_settings.R.
