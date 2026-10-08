# Imputation target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_2018_imputed_idw,
    {
      panel <- bogota_imputation[basename(bogota_imputation) == "bogota_imputed"]
      micro <- arrow::read_parquet(bogota_census[
        basename(bogota_census) == "census_2018_metro_individual.parquet"])
      geo <- arrow::read_parquet(bogota_census[
        basename(bogota_census) == "census_2018_metro_collapsed.parquet"])
      matrix <- bogota_2018_distances[basename(bogota_2018_distances) ==
        "matrix_geo_station_distances.parquet"]
      result <- run_idw_city(city_label = "Bogota", city_id = "bogota_2018",
        arrow_dir = here::here(panel, paste0("year=", imputation_year)),
        geo_sta_pq = matrix, geo_census = geo, micro_census = micro,
        socio_var = "education", n_groups = 5L, group_name = "edu_quintile",
        buffer_km = imputed_exposure_buffer_km, distance_power = idw_distance_power,
        outdir_exp = here::here("data", "processed", "idw_estimates_imputed"))
      unlist(result, use.names = FALSE)
    }, format = "file"),

  targets::tar_target(bogota_2018_imputed_estimates,
    {
      exposure_file <- paste0("bogota_2018_", imputed_exposure_buffer_km,
        "km_idw_exposure.parquet")
      exposure <- arrow::read_parquet(bogota_2018_imputed_idw[
        basename(bogota_2018_imputed_idw) == exposure_file])
      individual <- arrow::read_parquet(bogota_2018_imputed_idw[
        basename(bogota_2018_imputed_idw) == "bogota_2018_indiv_groups.parquet"])
      matrix <- bogota_2018_distances[basename(bogota_2018_distances) ==
        "matrix_geo_station_distances.parquet"]
      run_city_exposure(city = "Bogota", city_id = "bogota_2018",
        exposure_dt = exposure, individual_dt = individual, geo_station_pq = matrix,
        socio_var = "education", group_col = "edu_quintile", n_groups = 5L,
        year = imputation_year, buffer_km = imputed_exposure_buffer_km)
    }),

  targets::tar_target(cdmx_2020_imputed_idw,
    {
      panel <- cdmx_imputation[basename(cdmx_imputation) == "cdmx_imputed"]
      micro <- arrow::read_parquet(cdmx_census[
        basename(cdmx_census) == "census_metro_individual_2020.parquet"])
      geo <- arrow::read_parquet(cdmx_census[
        basename(cdmx_census) == "collapse_metro_area_2020.parquet"])
      matrix <- cdmx_2020_distances[basename(cdmx_2020_distances) ==
        "matrix_geo_station_distances.parquet"]
      result <- run_idw_city(city_label = "CDMX", city_id = "cdmx_2020",
        arrow_dir = here::here(panel, paste0("year=", imputation_year)),
        geo_sta_pq = matrix, geo_census = geo, micro_census = micro,
        socio_var = "education", n_groups = 5L, group_name = "edu_quintile",
        buffer_km = imputed_exposure_buffer_km, distance_power = idw_distance_power,
        outdir_exp = here::here("data", "processed", "idw_estimates_imputed"))
      unlist(result, use.names = FALSE)
    }, format = "file"),

  targets::tar_target(cdmx_2020_imputed_estimates,
    {
      exposure_file <- paste0("cdmx_2020_", imputed_exposure_buffer_km,
        "km_idw_exposure.parquet")
      exposure <- arrow::read_parquet(cdmx_2020_imputed_idw[
        basename(cdmx_2020_imputed_idw) == exposure_file])
      individual <- arrow::read_parquet(cdmx_2020_imputed_idw[
        basename(cdmx_2020_imputed_idw) == "cdmx_2020_indiv_groups.parquet"])
      matrix <- cdmx_2020_distances[basename(cdmx_2020_distances) ==
        "matrix_geo_station_distances.parquet"]
      run_city_exposure(city = "CDMX", city_id = "cdmx_2020",
        exposure_dt = exposure, individual_dt = individual, geo_station_pq = matrix,
        socio_var = "education", group_col = "edu_quintile", n_groups = 5L,
        year = imputation_year, buffer_km = imputed_exposure_buffer_km)
    }),

  targets::tar_target(santiago_2017_imputed_idw,
    {
      panel <- santiago_imputation[basename(santiago_imputation) == "santiago_imputed"]
      micro <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_individual_2017.parquet"])
      geo <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_collapsed_2017.parquet"])
      matrix <- santiago_2017_distances[basename(santiago_2017_distances) ==
        "matrix_geo_station_distances.parquet"]
      result <- run_idw_city(city_label = "Santiago", city_id = "santiago_2017",
        arrow_dir = here::here(panel, paste0("year=", imputation_year)),
        geo_sta_pq = matrix, geo_census = geo, micro_census = micro,
        socio_var = "education", n_groups = 5L, group_name = "edu_quintile",
        buffer_km = imputed_exposure_buffer_km, distance_power = idw_distance_power,
        outdir_exp = here::here("data", "processed", "idw_estimates_imputed"))
      unlist(result, use.names = FALSE)
    }, format = "file"),

  targets::tar_target(santiago_2017_imputed_estimates,
    {
      exposure_file <- paste0("santiago_2017_", imputed_exposure_buffer_km,
        "km_idw_exposure.parquet")
      exposure <- arrow::read_parquet(santiago_2017_imputed_idw[
        basename(santiago_2017_imputed_idw) == exposure_file])
      individual <- arrow::read_parquet(santiago_2017_imputed_idw[
        basename(santiago_2017_imputed_idw) == "santiago_2017_indiv_groups.parquet"])
      matrix <- santiago_2017_distances[basename(santiago_2017_distances) ==
        "matrix_geo_station_distances.parquet"]
      run_city_exposure(city = "Santiago", city_id = "santiago_2017",
        exposure_dt = exposure, individual_dt = individual, geo_station_pq = matrix,
        socio_var = "education", group_col = "edu_quintile", n_groups = 5L,
        year = imputation_year, buffer_km = imputed_exposure_buffer_km)
    }),

  targets::tar_target(sao_paulo_2010_imputed_idw,
    {
      panel <- sao_paulo_imputation[
        basename(sao_paulo_imputation) == "sao_paulo_imputed"]
      micro <- arrow::read_parquet(sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_individual_2010.parquet"])
      geo <- arrow::read_parquet(sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_collapsed_2010.parquet"])
      matrix <- sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
        "matrix_geo_station_distances.parquet"]
      result <- run_idw_city(city_label = "Sao Paulo", city_id = "sao_paulo_2010",
        arrow_dir = here::here(panel, paste0("year=", imputation_year)),
        geo_sta_pq = matrix, geo_census = geo, micro_census = micro,
        socio_var = "education", n_groups = 5L, group_name = "edu_quintile",
        buffer_km = imputed_exposure_buffer_km, distance_power = idw_distance_power,
        outdir_exp = here::here("data", "processed", "idw_estimates_imputed"))
      unlist(result, use.names = FALSE)
    }, format = "file"),

  targets::tar_target(sao_paulo_2010_imputed_estimates,
    {
      exposure_file <- paste0("sao_paulo_2010_", imputed_exposure_buffer_km,
        "km_idw_exposure.parquet")
      exposure <- arrow::read_parquet(sao_paulo_2010_imputed_idw[
        basename(sao_paulo_2010_imputed_idw) == exposure_file])
      individual <- arrow::read_parquet(sao_paulo_2010_imputed_idw[
        basename(sao_paulo_2010_imputed_idw) == "sao_paulo_2010_indiv_groups.parquet"])
      matrix <- sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
        "matrix_geo_station_distances.parquet"]
      run_city_exposure(city = "Sao Paulo", city_id = "sao_paulo_2010",
        exposure_dt = exposure, individual_dt = individual, geo_station_pq = matrix,
        socio_var = "education", group_col = "edu_quintile", n_groups = 5L,
        year = imputation_year, buffer_km = imputed_exposure_buffer_km)
    }),

  targets::tar_target(imputed_exposure_tables,
    {
      runs <- list(bogota_2018_imputed_estimates, cdmx_2020_imputed_estimates,
        santiago_2017_imputed_estimates, sao_paulo_2010_imputed_estimates)
      list(ci_estimates_education = stack_city_tables(runs, "ci"),
        group_summaries_education = stack_city_tables(runs, "summary"),
        coverage = stack_city_tables(runs, "coverage"))
    }),

  targets::tar_target(imputed_exposure_files,
    {
      save_exposure_tables(tables = imputed_exposure_tables,
        out_dir = here::here("data", "processed", "idw_regressions_imputed"),
        buffer_km = imputed_exposure_buffer_km, year = imputation_year)
    }, format = "file"),

  targets::tar_target(estimate_exposure_imputed,
    {
      c(bogota_2018_imputed_idw, cdmx_2020_imputed_idw, santiago_2017_imputed_idw,
        sao_paulo_2010_imputed_idw, imputed_exposure_files)
    }, format = "file"),

  targets::tar_target(bogota_imputation_predictions,
    {
      arrow::read_parquet(bogota_imputation[
        basename(bogota_imputation) == "bogota_imputed_predictions.parquet"])
    }),

  targets::tar_target(bogota_imputation_station_data,
    {
      station <- arrow::read_parquet(bogota_station_socio_file)
      rescale_station_education(station, "Bogota")
    }),

  targets::tar_target(bogota_imputation_ratios,
    {
      ratios <- list()
      for (pollutant in imputation_pollutants) {
        ratios[[pollutant]] <- summarize_imputation_ratios(
          pred_dt = bogota_imputation_predictions,
          station_dt = bogota_imputation_station_data, pollutant = pollutant)
      }
      ratios
    }),

  targets::tar_target(bogota_imputation_plots,
    {
      paper_font
      set_paper_theme()
      series <- ratios <- list()
      for (pollutant in imputation_pollutants) {
        series[[pollutant]] <- plot_imputation_series(
          pred_dt = bogota_imputation_predictions, pollutant = pollutant,
          city_label = "Bogotá")
        ratios[[pollutant]] <- plot_imputation_ratio_by_station(
          ratio_dt = bogota_imputation_ratios[[pollutant]], pollutant = pollutant,
          city_label = "Bogotá")
      }
      list(series = series, ratios = ratios)
    }),

  targets::tar_target(bogota_imputation_figures,
    {
      paper_font
      set_paper_theme()
      dir <- here::here("results", "figures", "imputation")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (pollutant in imputation_pollutants) {
        tag <- if (pollutant == "pm10") "" else "_pm25"
        series_file <- here::here(dir, paste0("model2_bogota", tag, ".pdf"))
        ratio_file <- here::here(dir, paste0("model2_bogota_scatter", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = bogota_imputation_plots$series[[pollutant]],
          path = series_file,
          width = 12,
          height = 8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        save_plot_pdf(
          plot_obj = bogota_imputation_plots$ratios[[pollutant]],
          path = ratio_file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, series_file, ratio_file)
      }
      files
    }, format = "file"),

  targets::tar_target(cdmx_imputation_predictions,
    {
      arrow::read_parquet(cdmx_imputation[
        basename(cdmx_imputation) == "cdmx_imputed_predictions.parquet"])
    }),

  targets::tar_target(cdmx_imputation_station_data,
    {
      station <- arrow::read_parquet(cdmx_station_socio_file)
      rescale_station_education(station, "CDMX")
    }),

  targets::tar_target(cdmx_imputation_ratios,
    {
      ratios <- list()
      for (pollutant in imputation_pollutants) {
        ratios[[pollutant]] <- summarize_imputation_ratios(
          pred_dt = cdmx_imputation_predictions,
          station_dt = cdmx_imputation_station_data, pollutant = pollutant)
      }
      ratios
    }),

  targets::tar_target(cdmx_imputation_plots,
    {
      paper_font
      set_paper_theme()
      series <- ratios <- list()
      for (pollutant in imputation_pollutants) {
        series[[pollutant]] <- plot_imputation_series(
          pred_dt = cdmx_imputation_predictions, pollutant = pollutant,
          city_label = "Mexico City")
        ratios[[pollutant]] <- plot_imputation_ratio_by_station(
          ratio_dt = cdmx_imputation_ratios[[pollutant]], pollutant = pollutant,
          city_label = "Mexico City")
      }
      list(series = series, ratios = ratios)
    }),

  targets::tar_target(cdmx_imputation_figures,
    {
      paper_font
      set_paper_theme()
      dir <- here::here("results", "figures", "imputation")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (pollutant in imputation_pollutants) {
        tag <- if (pollutant == "pm10") "" else "_pm25"
        series_file <- here::here(dir, paste0("model2_mexico", tag, ".pdf"))
        ratio_file <- here::here(dir, paste0("model2_mexico_scatter", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = cdmx_imputation_plots$series[[pollutant]],
          path = series_file,
          width = 12,
          height = 8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        save_plot_pdf(
          plot_obj = cdmx_imputation_plots$ratios[[pollutant]],
          path = ratio_file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, series_file, ratio_file)
      }
      files
    }, format = "file"),

  targets::tar_target(santiago_imputation_predictions,
    {
      arrow::read_parquet(santiago_imputation[
        basename(santiago_imputation) == "santiago_imputed_predictions.parquet"])
    }),

  targets::tar_target(santiago_imputation_station_data,
    {
      station <- arrow::read_parquet(santiago_station_socio_file)
      rescale_station_education(station, "Santiago")
    }),

  targets::tar_target(santiago_imputation_ratios,
    {
      ratios <- list()
      for (pollutant in imputation_pollutants) {
        ratios[[pollutant]] <- summarize_imputation_ratios(
          pred_dt = santiago_imputation_predictions,
          station_dt = santiago_imputation_station_data, pollutant = pollutant)
      }
      ratios
    }),

  targets::tar_target(santiago_imputation_plots,
    {
      paper_font
      set_paper_theme()
      series <- ratios <- list()
      for (pollutant in imputation_pollutants) {
        series[[pollutant]] <- plot_imputation_series(
          pred_dt = santiago_imputation_predictions, pollutant = pollutant,
          city_label = "Santiago")
        ratios[[pollutant]] <- plot_imputation_ratio_by_station(
          ratio_dt = santiago_imputation_ratios[[pollutant]], pollutant = pollutant,
          city_label = "Santiago")
      }
      list(series = series, ratios = ratios)
    }),

  targets::tar_target(santiago_imputation_figures,
    {
      paper_font
      set_paper_theme()
      dir <- here::here("results", "figures", "imputation")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (pollutant in imputation_pollutants) {
        tag <- if (pollutant == "pm10") "" else "_pm25"
        series_file <- here::here(dir, paste0("model2_santiago", tag, ".pdf"))
        ratio_file <- here::here(dir, paste0("model2_santiago_scatter", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = santiago_imputation_plots$series[[pollutant]],
          path = series_file,
          width = 12,
          height = 8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        save_plot_pdf(
          plot_obj = santiago_imputation_plots$ratios[[pollutant]],
          path = ratio_file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, series_file, ratio_file)
      }
      files
    }, format = "file"),

  targets::tar_target(sao_paulo_imputation_predictions,
    {
      arrow::read_parquet(sao_paulo_imputation[
        basename(sao_paulo_imputation) == "sao_paulo_imputed_predictions.parquet"])
    }),

  targets::tar_target(sao_paulo_imputation_station_data,
    {
      station <- arrow::read_parquet(sao_paulo_station_socio_file)
      rescale_station_education(station, "Sao Paulo")
    }),

  targets::tar_target(sao_paulo_imputation_ratios,
    {
      ratios <- list()
      for (pollutant in imputation_pollutants) {
        ratios[[pollutant]] <- summarize_imputation_ratios(
          pred_dt = sao_paulo_imputation_predictions,
          station_dt = sao_paulo_imputation_station_data, pollutant = pollutant)
      }
      ratios
    }),

  targets::tar_target(sao_paulo_imputation_plots,
    {
      paper_font
      set_paper_theme()
      series <- ratios <- list()
      for (pollutant in imputation_pollutants) {
        series[[pollutant]] <- plot_imputation_series(
          pred_dt = sao_paulo_imputation_predictions, pollutant = pollutant,
          city_label = "São Paulo")
        ratios[[pollutant]] <- plot_imputation_ratio_by_station(
          ratio_dt = sao_paulo_imputation_ratios[[pollutant]], pollutant = pollutant,
          city_label = "São Paulo")
      }
      list(series = series, ratios = ratios)
    }),

  targets::tar_target(sao_paulo_imputation_figures,
    {
      paper_font
      set_paper_theme()
      dir <- here::here("results", "figures", "imputation")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (pollutant in imputation_pollutants) {
        tag <- if (pollutant == "pm10") "" else "_pm25"
        series_file <- here::here(dir, paste0("model2_saopaulo", tag, ".pdf"))
        ratio_file <- here::here(dir, paste0("model2_saopaulo_scatter", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = sao_paulo_imputation_plots$series[[pollutant]],
          path = series_file,
          width = 12,
          height = 8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        save_plot_pdf(
          plot_obj = sao_paulo_imputation_plots$ratios[[pollutant]],
          path = ratio_file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, series_file, ratio_file)
      }
      files
    }, format = "file"),

  targets::tar_target(figure_imputation_diagnostics,
    {
      c(bogota_imputation_figures, cdmx_imputation_figures, santiago_imputation_figures,
        sao_paulo_imputation_figures)
    }, format = "file")
)
