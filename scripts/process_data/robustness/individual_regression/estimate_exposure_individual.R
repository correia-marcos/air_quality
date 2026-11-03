# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate the education-quintile exposure gaps with individual-level regressions.
#
#' @Description: Read the IDW exposure at individual_exposure_buffer_km and the
# individual education quintiles written by scripts/process_data/exposure/estimate_idw.R.
# compute_exposure_regressions_individual() regresses each person's exposure on their
# quintile, a check on the group-level estimates of estimate_exposure.R. Save the
# combined table to data/processed/idw_regressions/.
#
#' @Summary:
#   I.   Import data: source functions, set paths and read exposure and populations.
#   II.  Process data: estimate each city's regressions and combine them.
#   III. Save outputs: write the combined table.
#
#' @Date: November 2026
#' @Author: Marcos
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and their required packages
source(here::here("src", "general_utilities", "base_utils.R"))
source(here::here("src", "general_utilities", "process", "geo_ids.R"))
source(here::here("src", "general_utilities", "process", "exposure_regressions.R"))
source(here::here("config", "analysis_settings.R"))

# Set the input folder and the output folder
dir_idw <- here::here("data", "processed", "idw_estimates")
dir_out <- here::here("data", "processed", "idw_regressions")

# Read individual education quintiles
individual_bogota   <- read_idw_artifact(dir_idw, "bogota_2018", "indiv_groups")
individual_cdmx     <- read_idw_artifact(dir_idw, "cdmx_2020", "indiv_groups")
individual_santiago <- read_idw_artifact(dir_idw, "santiago_2017", "indiv_groups")
individual_sp       <- read_idw_artifact(dir_idw, "sao_paulo_2010", "indiv_groups")

# Read the exposure at the individual-regression buffer
exposure_bogota   <- read_idw_artifact(dir_idw, "bogota_2018", "idw_exposure",
                                       individual_exposure_buffer_km)
exposure_cdmx     <- read_idw_artifact(dir_idw, "cdmx_2020", "idw_exposure",
                                       individual_exposure_buffer_km)
exposure_santiago <- read_idw_artifact(dir_idw, "santiago_2017", "idw_exposure",
                                       individual_exposure_buffer_km)
exposure_sp       <- read_idw_artifact(dir_idw, "sao_paulo_2010", "idw_exposure",
                                       individual_exposure_buffer_km)

# ========================================================================================
# II: Process data
# ========================================================================================
# Regress each person's exposure on their education quintile, city by city
ci_ind_bogota   <- compute_exposure_regressions_individual(
  exposure_dt   = exposure_bogota,
  individual_dt = individual_bogota,
  group_col     = "edu_quintile",
  year_filter   = analysis_year)
ci_ind_cdmx     <- compute_exposure_regressions_individual(
  exposure_dt   = exposure_cdmx,
  individual_dt = individual_cdmx,
  group_col     = "edu_quintile",
  year_filter   = analysis_year)
ci_ind_santiago <- compute_exposure_regressions_individual(
  exposure_dt   = exposure_santiago,
  individual_dt = individual_santiago,
  group_col     = "edu_quintile",
  year_filter   = analysis_year)
ci_ind_sp       <- compute_exposure_regressions_individual(
  exposure_dt   = exposure_sp,
  individual_dt = individual_sp,
  group_col     = "edu_quintile",
  year_filter   = analysis_year)

# Label each run and combine the individual-regression estimates.
ci_ind_bogota[,   `:=`(city = "Bogota", city_id = "bogota_2018")]
ci_ind_cdmx[,     `:=`(city = "CDMX", city_id = "cdmx_2020")]
ci_ind_santiago[, `:=`(city = "Santiago", city_id = "santiago_2017")]
ci_ind_sp[,       `:=`(city = "Sao Paulo", city_id = "sao_paulo_2010")]

ci_individual <- data.table::rbindlist(
  list(ci_ind_bogota, ci_ind_cdmx, ci_ind_santiago, ci_ind_sp),
  fill = TRUE)

ci_individual[, `:=`(year = analysis_year, buffer_km = individual_exposure_buffer_km,
                     socioeconomic_var = "education", group_type = "quintile")]

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save the combined estimates as Parquet with a CSV copy
individual_files <- save_exposure_tables(
    tables    = list(ci_estimates_education_individual = ci_individual),
    out_dir   = dir_out,
    buffer_km = individual_exposure_buffer_km,
    year      = analysis_year)
