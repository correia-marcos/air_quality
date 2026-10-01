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

# Descriptive summaries use stored rows before and after outlier removal.
summary_pollutants <- c("pm10", "pm25")
missing_dimensions <- c("station", "month", "hour", "day_of_week")
availability_report <- "available"

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

# Station context and coverage figures retain their existing radii and WHO thresholds.
station_context_buffer_km <- 3
monitoring_radii_km <- c(3, 5, 10)
station_who_thresholds <- list(pm10 = c(it1 = 150, it2 = 100),
                               pm25 = c(it1 = 75, it2 = 50))
exposure_density_radii_km <- c(3, 20)

# Scatter outcomes and the manuscript's existing filename suffixes.
station_scatter_labels <- c(
  hrs_d_pm10_it1 = "Hours above PM10 IT1 in 2023",
  hrs_d_pm25_it1 = "Hours above PM2.5 IT1 in 2023",
  hrs_d_pm10_it2 = "Hours above PM10 IT2 in 2023",
  hrs_d_pm25_it2 = "Hours above PM2.5 IT2 in 2023",
  avg_pm10 = "Mean annual PM10 concentration in 2023",
  avg_pm25 = "Mean annual PM2.5 concentration in 2023")
station_scatter_tags <- c(hrs_d_pm10_it1 = "2023", hrs_d_pm25_it1 = "pm25_2023",
  hrs_d_pm10_it2 = "IT2_2023", hrs_d_pm25_it2 = "IT2_pm25_2023",
  avg_pm10 = "2023_pm10mean", avg_pm25 = "2023_pm25mean")

# Density evaluation limits and styling; bandwidths retain all finite observations.
kernel_reference_years <- c(2019L, 2022L)
kernel_pm10_limit <- 500
kernel_pm25_limit <- 100
kernel_pm25_reference_limit <- 250
kernel_city_colours <- c("Bogotá" = "black", "Mexico City" = "black",
                         "São Paulo" = "black", "Santiago" = "red")
kernel_city_linetypes <- c("Bogotá" = "solid", "Mexico City" = "dashed",
                           "São Paulo" = "dotdash", "Santiago" = "solid")

# Original MERRA-2 extraction: polygon means, with the existing RAM-based parallel choice.
# merra2_extraction_fun <- "mean"
# merra2_parallel <- TRUE
# merra2_num_cores <- NULL
