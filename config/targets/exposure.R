# Exposure target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_2018_exposure_inputs,
    lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
      arrow::read_parquet(bogota_2018_idw[basename(bogota_2018_idw) ==
        paste0("bogota_2018_", buffer_km, "km_idw_exposure.parquet")])
    })),

  targets::tar_target(bogota_2018_education_population,
    bogota_2018_idw[
      basename(bogota_2018_idw) == "bogota_2018_indiv_groups.parquet"],
    format = "file"),

  targets::tar_target(bogota_2018_education_estimates,
    {
      population <- data.table::as.data.table(
        arrow::read_parquet(bogota_2018_education_population))
      lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
        run_city_exposure(city = "Bogota",
            city_id = "bogota_2018",
            exposure_dt = bogota_2018_exposure_inputs[[as.character(buffer_km)]],
            individual_dt = population,
            geo_station_pq = bogota_2018_distances[basename(bogota_2018_distances) ==
              "matrix_geo_station_distances.parquet"],
            socio_var = "education",
            group_col = "edu_quintile",
            n_groups = 5L,
            year = analysis_year,
            buffer_km = buffer_km)
      })
    }),

  targets::tar_target(cdmx_2020_exposure_inputs,
    lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
      arrow::read_parquet(cdmx_2020_idw[basename(cdmx_2020_idw) ==
        paste0("cdmx_2020_", buffer_km, "km_idw_exposure.parquet")])
    })),

  targets::tar_target(cdmx_2020_education_population,
    cdmx_2020_idw[
      basename(cdmx_2020_idw) == "cdmx_2020_indiv_groups.parquet"],
    format = "file"),

  targets::tar_target(cdmx_2020_education_estimates,
    {
      population <- data.table::as.data.table(
        arrow::read_parquet(cdmx_2020_education_population))
      lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
        run_city_exposure(city = "CDMX",
            city_id = "cdmx_2020",
            exposure_dt = cdmx_2020_exposure_inputs[[as.character(buffer_km)]],
            individual_dt = population,
            geo_station_pq = cdmx_2020_distances[basename(cdmx_2020_distances) ==
              "matrix_geo_station_distances.parquet"],
            socio_var = "education",
            group_col = "edu_quintile",
            n_groups = 5L,
            year = analysis_year,
            buffer_km = buffer_km)
      })
    }),

  targets::tar_target(cdmx_2020_income_population,
    cdmx_2020_idw[
      basename(cdmx_2020_idw) == "cdmx_2020_income_indiv_groups.parquet"],
    format = "file"),

  targets::tar_target(cdmx_2020_income_estimates,
    {
      population <- data.table::as.data.table(
        arrow::read_parquet(cdmx_2020_income_population))
      lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
        run_city_exposure(city = "CDMX",
            city_id = "cdmx_2020",
            exposure_dt = cdmx_2020_exposure_inputs[[as.character(buffer_km)]],
            individual_dt = population,
            geo_station_pq = cdmx_2020_distances[basename(cdmx_2020_distances) ==
              "matrix_geo_station_distances.parquet"],
            socio_var = "income",
            group_col = "income_quintile",
            n_groups = 5L,
            year = analysis_year,
            buffer_km = buffer_km)
      })
    }),

  targets::tar_target(santiago_2017_exposure_inputs,
    lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
      arrow::read_parquet(santiago_2017_idw[basename(santiago_2017_idw) ==
        paste0("santiago_2017_", buffer_km, "km_idw_exposure.parquet")])
    })),

  targets::tar_target(santiago_2017_education_population,
    santiago_2017_idw[
      basename(santiago_2017_idw) == "santiago_2017_indiv_groups.parquet"],
    format = "file"),

  targets::tar_target(santiago_2017_education_estimates,
    {
      population <- data.table::as.data.table(
        arrow::read_parquet(santiago_2017_education_population))
      lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
        run_city_exposure(city = "Santiago",
            city_id = "santiago_2017",
            exposure_dt = santiago_2017_exposure_inputs[[as.character(buffer_km)]],
            individual_dt = population,
            geo_station_pq = santiago_2017_distances[basename(santiago_2017_distances) ==
              "matrix_geo_station_distances.parquet"],
            socio_var = "education",
            group_col = "edu_quintile",
            n_groups = 5L,
            year = analysis_year,
            buffer_km = buffer_km)
      })
    }),

  targets::tar_target(santiago_2024_exposure_inputs,
    lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
      arrow::read_parquet(santiago_2024_idw[basename(santiago_2024_idw) ==
        paste0("santiago_2024_", buffer_km, "km_idw_exposure.parquet")])
    })),

  targets::tar_target(santiago_2024_education_population,
    santiago_2024_idw[
      basename(santiago_2024_idw) == "santiago_2024_indiv_groups.parquet"],
    format = "file"),

  targets::tar_target(santiago_2024_education_estimates,
    {
      population <- data.table::as.data.table(
        arrow::read_parquet(santiago_2024_education_population))
      lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
        run_city_exposure(city = "Santiago (comuna, 2024)",
            city_id = "santiago_2024",
            exposure_dt = santiago_2024_exposure_inputs[[as.character(buffer_km)]],
            individual_dt = population,
            geo_station_pq = santiago_2024_distances[basename(santiago_2024_distances) ==
              "matrix_geo_station_distances.parquet"],
            socio_var = "education",
            group_col = "edu_quintile",
            n_groups = 5L,
            year = analysis_year,
            buffer_km = buffer_km)
      })
    }),

  targets::tar_target(sao_paulo_2010_exposure_inputs,
    lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
      arrow::read_parquet(sao_paulo_2010_idw[basename(sao_paulo_2010_idw) ==
        paste0("sao_paulo_2010_", buffer_km, "km_idw_exposure.parquet")])
    })),

  targets::tar_target(sao_paulo_2010_education_population,
    sao_paulo_2010_idw[
      basename(sao_paulo_2010_idw) == "sao_paulo_2010_indiv_groups.parquet"],
    format = "file"),

  targets::tar_target(sao_paulo_2010_education_estimates,
    {
      population <- data.table::as.data.table(
        arrow::read_parquet(sao_paulo_2010_education_population))
      lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
        run_city_exposure(city = "Sao Paulo",
            city_id = "sao_paulo_2010",
            exposure_dt = sao_paulo_2010_exposure_inputs[[as.character(buffer_km)]],
            individual_dt = population,
            geo_station_pq = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
              "matrix_geo_station_distances.parquet"],
            socio_var = "education",
            group_col = "edu_quintile",
            n_groups = 5L,
            year = analysis_year,
            buffer_km = buffer_km)
      })
    }),

  targets::tar_target(sao_paulo_2010_income_population,
    sao_paulo_2010_idw[
      basename(sao_paulo_2010_idw) == "sao_paulo_2010_income_indiv_groups.parquet"],
    format = "file"),

  targets::tar_target(sao_paulo_2010_income_estimates,
    {
      population <- data.table::as.data.table(
        arrow::read_parquet(sao_paulo_2010_income_population))
      lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
        run_city_exposure(city = "Sao Paulo",
            city_id = "sao_paulo_2010",
            exposure_dt = sao_paulo_2010_exposure_inputs[[as.character(buffer_km)]],
            individual_dt = population,
            geo_station_pq = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
              "matrix_geo_station_distances.parquet"],
            socio_var = "income",
            group_col = "income_decile",
            n_groups = 10L,
            year = analysis_year,
            buffer_km = buffer_km)
      })
    }),

  targets::tar_target(exposure_group_tables,
    lapply(setNames(exposure_buffers_km, exposure_buffers_km), function(buffer_km) {
      key <- as.character(buffer_km)
      stack_exposure_runs(
        edu_runs = list(
          bogota_2018_education_estimates[[key]],
          cdmx_2020_education_estimates[[key]],
          santiago_2017_education_estimates[[key]],
          santiago_2024_education_estimates[[key]],
          sao_paulo_2010_education_estimates[[key]]),
        inc_runs = list(
          cdmx_2020_income_estimates[[key]],
          sao_paulo_2010_income_estimates[[key]]))
    })),

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
          exposure_dt = santiago_2024_exposure_inputs[[
            as.character(individual_exposure_buffer_km)]],
          individual_dt = arrow::read_parquet(santiago_2024_education_population),
          group_col = "edu_quintile",
          year_filter = analysis_year)
      result[, `:=`(city = "Santiago (comuna, 2024)", city_id = "santiago_2024")]
      results[["santiago_2024"]] <- result
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

  targets::tar_target(exposure_group_files,
    {
      files <- character()
      for (buffer_km in exposure_buffers_km) {
        files <- c(files, save_exposure_tables(
            tables = exposure_group_tables[[as.character(buffer_km)]],
            out_dir = here::here("data", "processed", "idw_regressions"),
            buffer_km = buffer_km,
            year = analysis_year))
      }
      files
    },
    format = "file"),

  targets::tar_target(exposure_individual_files,
    save_exposure_tables(
        tables = list(ci_estimates_education_individual = exposure_individual_table),
        out_dir = here::here("data", "processed", "idw_regressions"),
        buffer_km = individual_exposure_buffer_km,
        year = analysis_year),
    format = "file"),

  targets::tar_target(estimate_exposure,
    c(exposure_group_files, exposure_individual_files),
    format = "file"),

  targets::tar_target(exposure_plot_data,
    {
      observed <- estimate_exposure
      imputed <- estimate_exposure_imputed
      names_ci_edu <- sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
        exposure_buffers_km, analysis_year)
      files_ci_edu <- observed[match(names_ci_edu, basename(observed))]
      names_summary_edu <- sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
        exposure_buffers_km, analysis_year)
      files_summary_edu <- observed[match(names_summary_edu, basename(observed))]
      names_ci_inc <- sprintf("exposure_ci_estimates_income_%dkm_%d.parquet",
        exposure_buffers_km, analysis_year)
      files_ci_inc <- observed[match(names_ci_inc, basename(observed))]
      names_summary_inc <- sprintf("exposure_group_summaries_income_%dkm_%d.parquet",
        exposure_buffers_km, analysis_year)
      files_summary_inc <- observed[match(names_summary_inc, basename(observed))]
      ci_edu <- data.table::rbindlist(lapply(files_ci_edu, arrow::read_parquet))
      summary_edu <- data.table::rbindlist(lapply(files_summary_edu, arrow::read_parquet))
      ci_inc <- summary_inc <- NULL
      if (!anyNA(c(files_ci_inc, files_summary_inc))) {
        ci_inc <- data.table::rbindlist(lapply(files_ci_inc, arrow::read_parquet))
        summary_inc <- data.table::rbindlist(lapply(files_summary_inc, arrow::read_parquet))
      }
      file_ci_imputed <- imputed[basename(imputed) ==
        sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
          imputed_exposure_buffer_km, imputation_year)]
      ci_imputed <- data.table::as.data.table(arrow::read_parquet(file_ci_imputed))
      file_summary_imputed <- imputed[basename(imputed) ==
        sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
          imputed_exposure_buffer_km, imputation_year)]
      summary_imputed <- data.table::as.data.table(
        arrow::read_parquet(file_summary_imputed))
      list(ci_edu = ci_edu, summary_edu = summary_edu,
           ci_inc = ci_inc, summary_inc = summary_inc,
           ci_imputed = ci_imputed, summary_imputed = summary_imputed)
    }),

  targets::tar_target(exposure_plots,
    {
      paper_font
      set_paper_theme()
      ci_edu <- exposure_plot_data$ci_edu
      plots_ci_edu <- build_exposure_ci_figures(ci_dt = ci_edu[outcome != "avg"],
        tag = "education", city_labels = exposure_city_labels,
        city_files = exposure_city_files)
      plots_levels_edu <- build_exposure_level_figures(
        sum_dt = exposure_plot_data$summary_edu, tag = "education",
        city_labels = exposure_city_labels, city_files = exposure_city_files)
      plots_ci_inc <- plots_levels_inc <- list()
      if (!is.null(exposure_plot_data$ci_inc)) {
        ci_inc <- exposure_plot_data$ci_inc
        plots_ci_inc <- build_exposure_ci_figures(ci_dt = ci_inc[outcome != "avg"],
          tag = "income", city_labels = exposure_city_labels,
          city_files = exposure_city_files)
        plots_levels_inc <- build_exposure_level_figures(
          sum_dt = exposure_plot_data$summary_inc, tag = "income",
          city_labels = exposure_city_labels, city_files = exposure_city_files)
      }
      ci_imputed <- exposure_plot_data$ci_imputed
      plots_ci_imputed <- build_exposure_ci_figures(ci_dt = ci_imputed[outcome != "avg"],
        tag = "education", city_labels = exposure_city_labels,
        city_files = exposure_city_files)
      plots_levels_imputed <- build_exposure_level_figures(
        sum_dt = exposure_plot_data$summary_imputed, tag = "education",
        city_labels = exposure_city_labels, city_files = exposure_city_files)
      list(ci_edu = plots_ci_edu, levels_edu = plots_levels_edu,
           ci_inc = plots_ci_inc, levels_inc = plots_levels_inc,
           ci_imputed = plots_ci_imputed, levels_imputed = plots_levels_imputed)
    }),

  targets::tar_target(generate_exposure_plots,
    {
      paper_font
      set_paper_theme()
      c(
        save_exposure_plot_family(exposure_plots$ci_edu,
          out_dir = here::here("results", "figures"), paper_files = exposure_paper_files,
          year = analysis_year),
        save_exposure_plot_family(exposure_plots$levels_edu,
          out_dir = here::here("results", "figures"), paper_files = exposure_paper_files,
          year = analysis_year),
        save_exposure_plot_family(exposure_plots$ci_inc,
          out_dir = here::here("results", "figures"), paper_files = exposure_paper_files,
          year = analysis_year),
        save_exposure_plot_family(exposure_plots$levels_inc,
          out_dir = here::here("results", "figures"), paper_files = exposure_paper_files,
          year = analysis_year),
        save_exposure_plot_family(exposure_plots$ci_imputed,
          out_dir = here::here("results", "figures"), paper_files = exposure_paper_files,
          year = imputation_year, imputed = TRUE),
        save_exposure_plot_family(exposure_plots$levels_imputed,
          out_dir = here::here("results", "figures"), paper_files = exposure_paper_files,
          year = imputation_year, imputed = TRUE))
    }, format = "file")
)
