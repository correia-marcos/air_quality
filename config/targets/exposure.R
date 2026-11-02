# Exposure target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_2018_idw,
    {
      micro <- arrow::read_parquet(bogota_census[
        basename(bogota_census) == "census_2018_metro_individual.parquet"])
      geo <- arrow::read_parquet(bogota_census[
        basename(bogota_census) == "census_2018_metro_collapsed.parquet"])
      matrix <- bogota_2018_distances[basename(bogota_2018_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(bogota_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      files <- character()
      for (buffer_km in idw_buffers_km) {
        education <- run_idw_city(city_label = "Bogota",
            city_id = "bogota_2018",
            arrow_dir = panel,
            geo_sta_pq = matrix,
            geo_census = geo,
            micro_census = micro,
            socio_var = "education",
            n_groups = 5L,
            group_name = "edu_quintile",
            buffer_km = buffer_km,
            distance_power = idw_distance_power,
            outdir_exp = dir_idw,
            return_data = FALSE)
        files <- c(files, unlist(education, use.names = FALSE))
      }
      unique(files)
    },
    format = "file"),

  targets::tar_target(cdmx_2020_idw,
    {
      micro <- arrow::read_parquet(cdmx_census[
        basename(cdmx_census) == "census_metro_individual_2020.parquet"])
      geo <- arrow::read_parquet(cdmx_census[
        basename(cdmx_census) == "collapse_metro_area_2020.parquet"])
      matrix <- cdmx_2020_distances[basename(cdmx_2020_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(cdmx_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      files <- character()
      for (buffer_km in idw_buffers_km) {
        education <- run_idw_city(city_label = "CDMX",
            city_id = "cdmx_2020",
            arrow_dir = panel,
            geo_sta_pq = matrix,
            geo_census = geo,
            micro_census = micro,
            socio_var = "education",
            n_groups = 5L,
            group_name = "edu_quintile",
            buffer_km = buffer_km,
            distance_power = idw_distance_power,
            outdir_exp = dir_idw,
            return_data = FALSE)
        files <- c(files, unlist(education, use.names = FALSE))
        income <- run_idw_city(city_label = "CDMX",
            city_id = "cdmx_2020",
            arrow_dir = panel,
            geo_sta_pq = matrix,
            geo_census = geo,
            micro_census = micro,
            socio_var = "income",
            n_groups = 5L,
            group_name = "income_quintile",
            buffer_km = buffer_km,
            distance_power = idw_distance_power,
            outdir_exp = dir_idw,
            out_suffix = "income",
            reuse_exposure = TRUE,
            return_data = FALSE)
        files <- c(files, unlist(income, use.names = FALSE))
      }
      unique(files)
    },
    format = "file"),

  targets::tar_target(santiago_2017_idw,
    {
      micro <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_individual_2017.parquet"])
      geo <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_collapsed_2017.parquet"])
      matrix <- santiago_2017_distances[basename(santiago_2017_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(santiago_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      files <- character()
      for (buffer_km in idw_buffers_km) {
        education <- run_idw_city(city_label = "Santiago (zona 2017)",
            city_id = "santiago_2017",
            arrow_dir = panel,
            geo_sta_pq = matrix,
            geo_census = geo,
            micro_census = micro,
            socio_var = "education",
            n_groups = 5L,
            group_name = "edu_quintile",
            buffer_km = buffer_km,
            distance_power = idw_distance_power,
            outdir_exp = dir_idw,
            return_data = FALSE)
        files <- c(files, unlist(education, use.names = FALSE))
      }
      unique(files)
    },
    format = "file"),

  targets::tar_target(sao_paulo_2010_idw,
    {
      micro <- arrow::read_parquet(sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_individual_2010.parquet"])
      geo <- arrow::read_parquet(sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_collapsed_2010.parquet"])
      matrix <- sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(sao_paulo_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      files <- character()
      for (buffer_km in idw_buffers_km) {
        education <- run_idw_city(city_label = "Sao Paulo",
            city_id = "sao_paulo_2010",
            arrow_dir = panel,
            geo_sta_pq = matrix,
            geo_census = geo,
            micro_census = micro,
            socio_var = "education",
            n_groups = 5L,
            group_name = "edu_quintile",
            buffer_km = buffer_km,
            distance_power = idw_distance_power,
            outdir_exp = dir_idw,
            return_data = FALSE)
        files <- c(files, unlist(education, use.names = FALSE))
        income <- run_idw_city(city_label = "Sao Paulo",
            city_id = "sao_paulo_2010",
            arrow_dir = panel,
            geo_sta_pq = matrix,
            geo_census = geo,
            micro_census = micro,
            socio_var = "income",
            n_groups = 10L,
            group_name = "income_decile",
            buffer_km = buffer_km,
            distance_power = idw_distance_power,
            outdir_exp = dir_idw,
            out_suffix = "income",
            reuse_exposure = TRUE,
            return_data = FALSE)
        files <- c(files, unlist(income, use.names = FALSE))
      }
      unique(files)
    },
    format = "file"),

  targets::tar_target(idw,
    c(bogota_2018_idw, cdmx_2020_idw, santiago_2017_idw, sao_paulo_2010_idw),
    format = "file"),

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
    }, format = "file"),

  targets::tar_target(exposure,
    c(idw, estimate_exposure),
    format = "file"),

  targets::tar_target(exposure_table_data,
    {
      file_summary_edu <- estimate_exposure[basename(estimate_exposure) ==
        sprintf("exposure_group_summaries_education_%dkm_%d.parquet",
          individual_exposure_buffer_km, analysis_year)]
      summary_edu <- data.table::as.data.table(arrow::read_parquet(file_summary_edu))
      file_summary_inc <- estimate_exposure[basename(estimate_exposure) ==
        sprintf("exposure_group_summaries_income_%dkm_%d.parquet",
          individual_exposure_buffer_km, analysis_year)]
      summary_inc <- data.table::as.data.table(arrow::read_parquet(file_summary_inc))
      file_ci_edu <- estimate_exposure[basename(estimate_exposure) ==
        sprintf("exposure_ci_estimates_education_%dkm_%d.parquet",
          individual_exposure_buffer_km, analysis_year)]
      ci_edu <- data.table::as.data.table(arrow::read_parquet(file_ci_edu))
      file_ci_inc <- estimate_exposure[basename(estimate_exposure) ==
        sprintf("exposure_ci_estimates_income_%dkm_%d.parquet",
          individual_exposure_buffer_km, analysis_year)]
      ci_inc <- data.table::as.data.table(arrow::read_parquet(file_ci_inc))
      list(summary_edu = summary_edu, summary_inc = summary_inc,
           ci_edu = ci_edu, ci_inc = ci_inc)
    }),

  targets::tar_target(exposure_table_tex,
    {
      list(
        means_edu = latex_exposure_means_by_group(
          summary_dt = exposure_table_data$summary_edu, ci_dt = exposure_table_data$ci_edu,
          panel_cities = exposure_table_cities_edu,
          panel_labels = exposure_table_labels_edu,
          n_groups = 5L),
        means_inc = latex_exposure_means_by_group(
          summary_dt = exposure_table_data$summary_inc, ci_dt = exposure_table_data$ci_inc,
          panel_cities = exposure_table_cities_inc,
          panel_labels = exposure_table_labels_inc,
          n_groups = 10L),
        hours_edu = latex_exposure_hours_by_group(
          summary_dt = exposure_table_data$summary_edu,
          panel_cities = exposure_table_cities_edu,
          panel_labels = exposure_table_labels_edu),
        hours_inc = latex_exposure_hours_by_group(
          summary_dt = exposure_table_data$summary_inc,
          panel_cities = exposure_table_cities_inc,
          panel_labels = exposure_table_labels_inc))
    }),

  targets::tar_target(render_exposure_tables,
    {
      c(
        write_latex_table(exposure_table_tex$means_edu,
          here::here("results", "tables", "table_means_education_quintiles.tex"),
          use_bytes = TRUE),
        write_latex_table(exposure_table_tex$means_inc,
          here::here("results", "tables", "table_means_income_groups.tex"),
          use_bytes = TRUE),
        write_latex_table(exposure_table_tex$hours_edu,
          here::here("results", "tables", "table_hours_above_education_quintiles.tex"),
          use_bytes = TRUE),
        write_latex_table(exposure_table_tex$hours_inc,
          here::here("results", "tables", "table_hours_above_income_groups.tex"),
          use_bytes = TRUE))
    }, format = "file"),

  targets::tar_target(bogota_exposure_quintile_weights,
    {
      compute_exposure_quintile_weights(
        groups_file = bogota_2018_idw[basename(bogota_2018_idw) ==
          "bogota_2018_indiv_groups.parquet"])
    }),

  targets::tar_target(bogota_density_exposure,
    {
      exposure <- list()
      for (radius_km in exposure_density_radii_km) {
        filename <- sprintf("bogota_2018_%dkm_idw_exposure.parquet", radius_km)
        exposure[[as.character(radius_km)]] <- arrow::read_parquet(
          bogota_2018_idw[basename(bogota_2018_idw) == filename])
      }
      exposure
    }),

  targets::tar_target(bogota_exposure_density_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in exposure_density_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          plots[[key]] <- plot_exposure_density_by_quintile(
            exposure = bogota_density_exposure[[as.character(radius_km)]],
            quintile_weights = bogota_exposure_quintile_weights,
            city_id = "bogota_2018",
            buffer_km = radius_km,
            pollutant = pollutant,
            city_label = "Bogotá",
            year_filter = analysis_year)
        }
      }
      plots
    }),

  targets::tar_target(bogota_exposure_density_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "exposure")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in exposure_density_radii_km) {
        tag <- if (radius_km == 3) "" else "_v2"
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          file <- here::here(out_dir, paste0("distribution_3km_bogota_",
            pollutant, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = bogota_exposure_density_plots[[key]],
            path = file,
            width = 8,
            height = 5.5,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      files
    }, format = "file"),

  targets::tar_target(cdmx_exposure_quintile_weights,
    {
      compute_exposure_quintile_weights(
        groups_file = cdmx_2020_idw[basename(cdmx_2020_idw) ==
          "cdmx_2020_indiv_groups.parquet"])
    }),

  targets::tar_target(cdmx_density_exposure,
    {
      exposure <- list()
      for (radius_km in exposure_density_radii_km) {
        filename <- sprintf("cdmx_2020_%dkm_idw_exposure.parquet", radius_km)
        exposure[[as.character(radius_km)]] <- arrow::read_parquet(
          cdmx_2020_idw[basename(cdmx_2020_idw) == filename])
      }
      exposure
    }),

  targets::tar_target(cdmx_exposure_density_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in exposure_density_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          plots[[key]] <- plot_exposure_density_by_quintile(
            exposure = cdmx_density_exposure[[as.character(radius_km)]],
            quintile_weights = cdmx_exposure_quintile_weights,
            city_id = "cdmx_2020",
            buffer_km = radius_km,
            pollutant = pollutant,
            city_label = "Mexico City",
            year_filter = analysis_year)
        }
      }
      plots
    }),

  targets::tar_target(cdmx_exposure_density_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "exposure")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in exposure_density_radii_km) {
        tag <- if (radius_km == 3) "" else "_v2"
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          file <- here::here(out_dir, paste0("distribution_3km_mexico_",
            pollutant, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = cdmx_exposure_density_plots[[key]],
            path = file,
            width = 8,
            height = 5.5,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      files
    }, format = "file"),

  targets::tar_target(santiago_exposure_quintile_weights,
    {
      compute_exposure_quintile_weights(
        groups_file = santiago_2017_idw[basename(santiago_2017_idw) ==
          "santiago_2017_indiv_groups.parquet"])
    }),

  targets::tar_target(santiago_density_exposure,
    {
      exposure <- list()
      for (radius_km in exposure_density_radii_km) {
        filename <- sprintf("santiago_2017_%dkm_idw_exposure.parquet", radius_km)
        exposure[[as.character(radius_km)]] <- arrow::read_parquet(
          santiago_2017_idw[basename(santiago_2017_idw) == filename])
      }
      exposure
    }),

  targets::tar_target(santiago_exposure_density_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in exposure_density_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          plots[[key]] <- plot_exposure_density_by_quintile(
            exposure = santiago_density_exposure[[as.character(radius_km)]],
            quintile_weights = santiago_exposure_quintile_weights,
            city_id = "santiago_2017",
            buffer_km = radius_km,
            pollutant = pollutant,
            city_label = "Santiago",
            year_filter = analysis_year)
        }
      }
      plots
    }),

  targets::tar_target(santiago_exposure_density_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "exposure")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in exposure_density_radii_km) {
        tag <- if (radius_km == 3) "" else "_v2"
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          file <- here::here(out_dir, paste0("distribution_3km_santiago_",
            pollutant, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = santiago_exposure_density_plots[[key]],
            path = file,
            width = 8,
            height = 5.5,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      files
    }, format = "file"),

  targets::tar_target(sao_paulo_exposure_quintile_weights,
    {
      compute_exposure_quintile_weights(
        groups_file = sao_paulo_2010_idw[basename(sao_paulo_2010_idw) ==
          "sao_paulo_2010_indiv_groups.parquet"])
    }),

  targets::tar_target(sao_paulo_density_exposure,
    {
      exposure <- list()
      for (radius_km in exposure_density_radii_km) {
        filename <- sprintf("sao_paulo_2010_%dkm_idw_exposure.parquet", radius_km)
        exposure[[as.character(radius_km)]] <- arrow::read_parquet(
          sao_paulo_2010_idw[basename(sao_paulo_2010_idw) == filename])
      }
      exposure
    }),

  targets::tar_target(sao_paulo_exposure_density_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in exposure_density_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          plots[[key]] <- plot_exposure_density_by_quintile(
            exposure = sao_paulo_density_exposure[[as.character(radius_km)]],
            quintile_weights = sao_paulo_exposure_quintile_weights,
            city_id = "sao_paulo_2010",
            buffer_km = radius_km,
            pollutant = pollutant,
            city_label = "São Paulo",
            year_filter = analysis_year)
        }
      }
      plots
    }),

  targets::tar_target(sao_paulo_exposure_density_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "exposure")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in exposure_density_radii_km) {
        tag <- if (radius_km == 3) "" else "_v2"
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          file <- here::here(out_dir, paste0("distribution_3km_saopaulo_",
            pollutant, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = sao_paulo_exposure_density_plots[[key]],
            path = file,
            width = 8,
            height = 5.5,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      files
    }, format = "file"),

  targets::tar_target(figure_quintile_kernel_distributions,
    {
      c(
        bogota_exposure_density_files,
        cdmx_exposure_density_files,
        santiago_exposure_density_files,
        sao_paulo_exposure_density_files)
    }, format = "file")
)
