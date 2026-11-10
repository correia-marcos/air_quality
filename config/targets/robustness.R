# Robustness target declarations: the 5 km buffer and the individual-level regression.
# Scientific functions live in src/; the 3 km exposure and the individual groups come
# from exposure.R.
list(
  targets::tar_target(bogota_2018_idw_5km,
    {
      micro <- arrow::read_parquet(bogota_census[
        basename(bogota_census) == "census_2018_metro_individual.parquet"])
      geo <- arrow::read_parquet(bogota_census[
        basename(bogota_census) == "census_2018_metro_collapsed.parquet"])
      matrix <- bogota_2018_distances[basename(bogota_2018_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(bogota_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      runs <- list(education =
        run_idw_city(city_label = "Bogota",
          city_id = "bogota_2018",
          arrow_dir = panel,
          geo_sta_pq = matrix,
          geo_census = geo,
          micro_census = micro,
          socio_var = "education",
          n_groups = 5L,
          group_name = "edu_quintile",
          buffer_km = robustness_buffer_km,
          distance_power = idw_distance_power,
          outdir_exp = dir_idw,
          return_data = FALSE))
      # The individual groups belong to bogota_2018_idw; keep the 5 km files.
      files <- unlist(runs, use.names = FALSE)
      unique(files[grepl(paste0("_", robustness_buffer_km, "km_"), basename(files))])
    },
    format = "file"),

  targets::tar_target(cdmx_2020_idw_5km,
    {
      micro <- arrow::read_parquet(cdmx_census[
        basename(cdmx_census) == "census_metro_individual_2020.parquet"])
      geo <- arrow::read_parquet(cdmx_census[
        basename(cdmx_census) == "collapse_metro_area_2020.parquet"])
      matrix <- cdmx_2020_distances[basename(cdmx_2020_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(cdmx_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      runs <- list(education =
        run_idw_city(city_label = "CDMX",
          city_id = "cdmx_2020",
          arrow_dir = panel,
          geo_sta_pq = matrix,
          geo_census = geo,
          micro_census = micro,
          socio_var = "education",
          n_groups = 5L,
          group_name = "edu_quintile",
          buffer_km = robustness_buffer_km,
          distance_power = idw_distance_power,
          outdir_exp = dir_idw,
          return_data = FALSE),
        income =
        run_idw_city(city_label = "CDMX",
          city_id = "cdmx_2020",
          arrow_dir = panel,
          geo_sta_pq = matrix,
          geo_census = geo,
          micro_census = micro,
          socio_var = "income",
          n_groups = 5L,
          group_name = "income_quintile",
          buffer_km = robustness_buffer_km,
          distance_power = idw_distance_power,
          outdir_exp = dir_idw,
          out_suffix = "income",
          reuse_exposure = TRUE,
          return_data = FALSE))
      # The individual groups belong to cdmx_2020_idw; keep the 5 km files.
      files <- unlist(runs, use.names = FALSE)
      unique(files[grepl(paste0("_", robustness_buffer_km, "km_"), basename(files))])
    },
    format = "file"),

  targets::tar_target(santiago_2017_idw_5km,
    {
      micro <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_individual_2017.parquet"])
      geo <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_collapsed_2017.parquet"])
      matrix <- santiago_2017_distances[basename(santiago_2017_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(santiago_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      runs <- list(education =
        run_idw_city(city_label = "Santiago (zona 2017)",
          city_id = "santiago_2017",
          arrow_dir = panel,
          geo_sta_pq = matrix,
          geo_census = geo,
          micro_census = micro,
          socio_var = "education",
          n_groups = 5L,
          group_name = "edu_quintile",
          buffer_km = robustness_buffer_km,
          distance_power = idw_distance_power,
          outdir_exp = dir_idw,
          return_data = FALSE))
      # The individual groups belong to santiago_2017_idw; keep the 5 km files.
      files <- unlist(runs, use.names = FALSE)
      unique(files[grepl(paste0("_", robustness_buffer_km, "km_"), basename(files))])
    },
    format = "file"),

  targets::tar_target(sao_paulo_2010_idw_5km,
    {
      micro <- arrow::read_parquet(sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_individual_2010.parquet"])
      geo <- arrow::read_parquet(sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_collapsed_2010.parquet"])
      matrix <- sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(sao_paulo_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      runs <- list(education =
        run_idw_city(city_label = "Sao Paulo",
          city_id = "sao_paulo_2010",
          arrow_dir = panel,
          geo_sta_pq = matrix,
          geo_census = geo,
          micro_census = micro,
          socio_var = "education",
          n_groups = 5L,
          group_name = "edu_quintile",
          buffer_km = robustness_buffer_km,
          distance_power = idw_distance_power,
          outdir_exp = dir_idw,
          return_data = FALSE),
        income =
        run_idw_city(city_label = "Sao Paulo",
          city_id = "sao_paulo_2010",
          arrow_dir = panel,
          geo_sta_pq = matrix,
          geo_census = geo,
          micro_census = micro,
          socio_var = "income",
          n_groups = 10L,
          group_name = "income_decile",
          buffer_km = robustness_buffer_km,
          distance_power = idw_distance_power,
          outdir_exp = dir_idw,
          out_suffix = "income",
          reuse_exposure = TRUE,
          return_data = FALSE))
      # The individual groups belong to sao_paulo_2010_idw; keep the 5 km files.
      files <- unlist(runs, use.names = FALSE)
      unique(files[grepl(paste0("_", robustness_buffer_km, "km_"), basename(files))])
    },
    format = "file"),

  targets::tar_target(idw_5km,
    c(bogota_2018_idw_5km, cdmx_2020_idw_5km, santiago_2017_idw_5km,
      sao_paulo_2010_idw_5km),
    format = "file"),

  targets::tar_target(bogota_2018_exposure_inputs_5km,
    arrow::read_parquet(bogota_2018_idw_5km[basename(bogota_2018_idw_5km) ==
      paste0("bogota_2018_", robustness_buffer_km, "km_idw_exposure.parquet")])),

  targets::tar_target(bogota_2018_education_estimates_5km,
    run_city_exposure(city = "Bogota",
        city_id = "bogota_2018",
        exposure_dt = bogota_2018_exposure_inputs_5km,
        individual_dt = data.table::as.data.table(
          arrow::read_parquet(bogota_2018_education_population)),
        geo_station_pq = bogota_2018_distances[basename(bogota_2018_distances) ==
          "matrix_geo_station_distances.parquet"],
        socio_var = "education",
        group_col = "edu_quintile",
        n_groups = 5L,
        year = analysis_year,
        buffer_km = robustness_buffer_km)),

  targets::tar_target(cdmx_2020_exposure_inputs_5km,
    arrow::read_parquet(cdmx_2020_idw_5km[basename(cdmx_2020_idw_5km) ==
      paste0("cdmx_2020_", robustness_buffer_km, "km_idw_exposure.parquet")])),

  targets::tar_target(cdmx_2020_education_estimates_5km,
    run_city_exposure(city = "CDMX",
        city_id = "cdmx_2020",
        exposure_dt = cdmx_2020_exposure_inputs_5km,
        individual_dt = data.table::as.data.table(
          arrow::read_parquet(cdmx_2020_education_population)),
        geo_station_pq = cdmx_2020_distances[basename(cdmx_2020_distances) ==
          "matrix_geo_station_distances.parquet"],
        socio_var = "education",
        group_col = "edu_quintile",
        n_groups = 5L,
        year = analysis_year,
        buffer_km = robustness_buffer_km)),

  targets::tar_target(cdmx_2020_income_estimates_5km,
    run_city_exposure(city = "CDMX",
        city_id = "cdmx_2020",
        exposure_dt = cdmx_2020_exposure_inputs_5km,
        individual_dt = data.table::as.data.table(
          arrow::read_parquet(cdmx_2020_income_population)),
        geo_station_pq = cdmx_2020_distances[basename(cdmx_2020_distances) ==
          "matrix_geo_station_distances.parquet"],
        socio_var = "income",
        group_col = "income_quintile",
        n_groups = 5L,
        year = analysis_year,
        buffer_km = robustness_buffer_km)),

  targets::tar_target(santiago_2017_exposure_inputs_5km,
    arrow::read_parquet(santiago_2017_idw_5km[basename(santiago_2017_idw_5km) ==
      paste0("santiago_2017_", robustness_buffer_km, "km_idw_exposure.parquet")])),

  targets::tar_target(santiago_2017_education_estimates_5km,
    run_city_exposure(city = "Santiago",
        city_id = "santiago_2017",
        exposure_dt = santiago_2017_exposure_inputs_5km,
        individual_dt = data.table::as.data.table(
          arrow::read_parquet(santiago_2017_education_population)),
        geo_station_pq = santiago_2017_distances[basename(santiago_2017_distances) ==
          "matrix_geo_station_distances.parquet"],
        socio_var = "education",
        group_col = "edu_quintile",
        n_groups = 5L,
        year = analysis_year,
        buffer_km = robustness_buffer_km)),

  targets::tar_target(sao_paulo_2010_exposure_inputs_5km,
    arrow::read_parquet(sao_paulo_2010_idw_5km[basename(sao_paulo_2010_idw_5km) ==
      paste0("sao_paulo_2010_", robustness_buffer_km, "km_idw_exposure.parquet")])),

  targets::tar_target(sao_paulo_2010_education_estimates_5km,
    run_city_exposure(city = "Sao Paulo",
        city_id = "sao_paulo_2010",
        exposure_dt = sao_paulo_2010_exposure_inputs_5km,
        individual_dt = data.table::as.data.table(
          arrow::read_parquet(sao_paulo_2010_education_population)),
        geo_station_pq = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
          "matrix_geo_station_distances.parquet"],
        socio_var = "education",
        group_col = "edu_quintile",
        n_groups = 5L,
        year = analysis_year,
        buffer_km = robustness_buffer_km)),

  targets::tar_target(sao_paulo_2010_income_estimates_5km,
    run_city_exposure(city = "Sao Paulo",
        city_id = "sao_paulo_2010",
        exposure_dt = sao_paulo_2010_exposure_inputs_5km,
        individual_dt = data.table::as.data.table(
          arrow::read_parquet(sao_paulo_2010_income_population)),
        geo_station_pq = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
          "matrix_geo_station_distances.parquet"],
        socio_var = "income",
        group_col = "income_decile",
        n_groups = 10L,
        year = analysis_year,
        buffer_km = robustness_buffer_km)),

  targets::tar_target(exposure_5km_group_tables,
    stack_exposure_runs(
      edu_runs = list(bogota_2018_education_estimates_5km,
        cdmx_2020_education_estimates_5km, santiago_2017_education_estimates_5km,
        sao_paulo_2010_education_estimates_5km),
      inc_runs = list(cdmx_2020_income_estimates_5km,
        sao_paulo_2010_income_estimates_5km))),

  targets::tar_target(estimate_exposure_5km,
    save_exposure_tables(
        tables = exposure_5km_group_tables,
        out_dir = here::here("data", "processed", "idw_regressions"),
        buffer_km = robustness_buffer_km,
        year = analysis_year),
    format = "file"),

  targets::tar_target(exposure_5km_plot_data,
    {
      files <- estimate_exposure_5km
      read <- function(table) data.table::as.data.table(arrow::read_parquet(
        files[basename(files) == sprintf("exposure_%s_%dkm_%d.parquet", table,
          robustness_buffer_km, analysis_year)]))
      list(ci_edu = read("ci_estimates_education"),
           summary_edu = read("group_summaries_education"),
           ci_inc = read("ci_estimates_income"),
           summary_inc = read("group_summaries_income"))
    }),

  targets::tar_target(exposure_5km_plots,
    {
      paper_font
      set_paper_theme()
      ci_edu <- exposure_5km_plot_data$ci_edu
      ci_inc <- exposure_5km_plot_data$ci_inc
      list(
        ci_edu = build_exposure_ci_figures(ci_dt = ci_edu[outcome != "avg"],
          tag = "education", city_labels = exposure_city_labels,
          city_files = exposure_city_files),
        levels_edu = build_exposure_level_figures(
          sum_dt = exposure_5km_plot_data$summary_edu, tag = "education",
          city_labels = exposure_city_labels, city_files = exposure_city_files),
        ci_inc = build_exposure_ci_figures(ci_dt = ci_inc[outcome != "avg"],
          tag = "income", city_labels = exposure_city_labels,
          city_files = exposure_city_files),
        levels_inc = build_exposure_level_figures(
          sum_dt = exposure_5km_plot_data$summary_inc, tag = "income",
          city_labels = exposure_city_labels, city_files = exposure_city_files))
    }),

  targets::tar_target(figure_exposure_5km,
    {
      paper_font
      set_paper_theme()
      unlist(lapply(exposure_5km_plots, save_exposure_plot_family,
        out_dir = here::here("results", "figures", "robustness", "buffer_5km"),
        paper_files = exposure_paper_files,
        year = analysis_year), use.names = FALSE)
    }, format = "file"),

  targets::tar_target(exposure_individual_table,
    {
      results <- list()
      result <- compute_exposure_regressions_individual(
          exposure_dt = bogota_2018_exposure_inputs[[
            as.character(individual_exposure_buffer_km)]],
          individual_dt = arrow::read_parquet(bogota_2018_education_population),
          group_col = "edu_quintile",
          year_filter = analysis_year)
      result[, `:=`(city = "Bogota", city_id = "bogota_2018")]
      results[["bogota_2018"]] <- result
      result <- compute_exposure_regressions_individual(
          exposure_dt = cdmx_2020_exposure_inputs[[
            as.character(individual_exposure_buffer_km)]],
          individual_dt = arrow::read_parquet(cdmx_2020_education_population),
          group_col = "edu_quintile",
          year_filter = analysis_year)
      result[, `:=`(city = "CDMX", city_id = "cdmx_2020")]
      results[["cdmx_2020"]] <- result
      result <- compute_exposure_regressions_individual(
          exposure_dt = santiago_2017_exposure_inputs[[
            as.character(individual_exposure_buffer_km)]],
          individual_dt = arrow::read_parquet(santiago_2017_education_population),
          group_col = "edu_quintile",
          year_filter = analysis_year)
      result[, `:=`(city = "Santiago", city_id = "santiago_2017")]
      results[["santiago_2017"]] <- result
      result <- compute_exposure_regressions_individual(
          exposure_dt = sao_paulo_2010_exposure_inputs[[
            as.character(individual_exposure_buffer_km)]],
          individual_dt = arrow::read_parquet(sao_paulo_2010_education_population),
          group_col = "edu_quintile",
          year_filter = analysis_year)
      result[, `:=`(city = "Sao Paulo", city_id = "sao_paulo_2010")]
      results[["sao_paulo_2010"]] <- result
      combined <- data.table::rbindlist(results, fill = TRUE)
      combined[, `:=`(year = analysis_year, buffer_km = individual_exposure_buffer_km,
                      socioeconomic_var = "education", group_type = "quintile")]
      combined
    }),

  targets::tar_target(estimate_exposure_individual,
    save_exposure_tables(
        tables = list(ci_estimates_education_individual = exposure_individual_table),
        out_dir = here::here("data", "processed", "idw_regressions"),
        buffer_km = individual_exposure_buffer_km,
        year = analysis_year),
    format = "file"),

  targets::tar_target(robustness,
    c(idw_5km, estimate_exposure_5km, figure_exposure_5km, estimate_exposure_individual),
    format = "file")
)
