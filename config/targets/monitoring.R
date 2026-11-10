# Monitoring target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_collapsed_census,
    {
      data.table::as.data.table(arrow::read_parquet(bogota_census[basename(bogota_census) ==
        "census_2018_metro_collapsed.parquet"]))
    }),

  targets::tar_target(bogota_station_pollution_summary,
    {
      compute_station_pollution_summary(
        arrow_dir = bogota_outliers,
        year_filter = analysis_year,
        station_col = "station",
        pollutants = summary_pollutants,
        who_it = station_who_thresholds)
    }),

  targets::tar_target(bogota_station_context,
    {
      sf::sf_use_s2(TRUE)
      compute_station_socio_context(
        stations_sf = bogota_distance_stations,
        geo_sf = bogota_2018_distance_geography,
        census_col = bogota_collapsed_census,
        station_id_col = "station_name",
        geo_sf_id_col = "GEO_ID",
        socio_vars = "education_mean",
        context_method = "buffer",
        buffer_km = station_context_buffer_km)
    }),

  targets::tar_target(bogota_station_socio,
    {
      join_station_scatter_inputs(
        pollution_dt = bogota_station_pollution_summary,
        context_dt = bogota_station_context,
        socio_vars = "education_mean",
        year_filter = analysis_year)
    }),

  targets::tar_target(bogota_station_socio_file,
    {
      path <- here::here("data", "processed", "station_socio_exposure",
        "bogota_2018", "bogota_2018_2023_3km_station_socio.parquet")
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(bogota_station_socio, path)
      path
    }, format = "file"),

  targets::tar_target(cdmx_collapsed_census,
    {
      data.table::as.data.table(arrow::read_parquet(cdmx_census[basename(cdmx_census) ==
        "collapse_metro_area_2020.parquet"]))
    }),

  targets::tar_target(cdmx_station_pollution_summary,
    {
      compute_station_pollution_summary(
        arrow_dir = cdmx_outliers,
        year_filter = analysis_year,
        station_col = "station",
        pollutants = summary_pollutants,
        who_it = station_who_thresholds)
    }),

  targets::tar_target(cdmx_station_context,
    {
      sf::sf_use_s2(TRUE)
      compute_station_socio_context(
        stations_sf = cdmx_distance_stations,
        geo_sf = cdmx_2020_distance_geography,
        census_col = cdmx_collapsed_census,
        station_id_col = "station",
        geo_sf_id_col = "CVE_MUN",
        socio_vars = c("education_mean", "income_mean"),
        context_method = "containing_geo")
    }),

  targets::tar_target(cdmx_station_socio,
    {
      join_station_scatter_inputs(
        pollution_dt = cdmx_station_pollution_summary,
        context_dt = cdmx_station_context,
        socio_vars = c("education_mean", "income_mean"),
        year_filter = analysis_year)
    }),

  targets::tar_target(cdmx_station_socio_file,
    {
      path <- here::here("data", "processed", "station_socio_exposure",
        "cdmx_2020", "cdmx_2020_2023_station_socio.parquet")
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(cdmx_station_socio, path)
      path
    }, format = "file"),

  targets::tar_target(santiago_collapsed_census,
    {
      data.table::as.data.table(arrow::read_parquet(santiago_census[
        basename(santiago_census) ==
        "census_collapsed_2017.parquet"]))
    }),

  targets::tar_target(santiago_station_pollution_summary,
    {
      compute_station_pollution_summary(
        arrow_dir = santiago_outliers,
        year_filter = analysis_year,
        station_col = "station",
        pollutants = summary_pollutants,
        who_it = station_who_thresholds)
    }),

  targets::tar_target(santiago_station_context,
    {
      sf::sf_use_s2(TRUE)
      compute_station_socio_context(
        stations_sf = santiago_distance_stations,
        geo_sf = santiago_2017_distance_geography,
        census_col = santiago_collapsed_census,
        station_id_col = "station_name",
        geo_sf_id_col = "zona_id",
        socio_vars = "education_mean",
        context_method = "containing_geo")
    }),

  targets::tar_target(santiago_station_socio,
    {
      join_station_scatter_inputs(
        pollution_dt = santiago_station_pollution_summary,
        context_dt = santiago_station_context,
        socio_vars = "education_mean",
        year_filter = analysis_year)
    }),

  targets::tar_target(santiago_station_socio_file,
    {
      path <- here::here("data", "processed", "station_socio_exposure",
        "santiago_2017", "santiago_2017_2023_station_socio.parquet")
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(santiago_station_socio, path)
      path
    }, format = "file"),

  targets::tar_target(sao_paulo_collapsed_census,
    {
      data.table::as.data.table(arrow::read_parquet(sao_paulo_census[
        basename(sao_paulo_census) ==
        "census_sp_collapsed_2010.parquet"]))
    }),

  targets::tar_target(sao_paulo_station_pollution_summary,
    {
      compute_station_pollution_summary(
        arrow_dir = sao_paulo_outliers,
        year_filter = analysis_year,
        station_col = "station",
        pollutants = summary_pollutants,
        who_it = station_who_thresholds)
    }),

  targets::tar_target(sao_paulo_station_context,
    {
      sf::sf_use_s2(TRUE)
      compute_station_socio_context(
        stations_sf = sao_paulo_distance_stations,
        geo_sf = sao_paulo_2010_distance_geography,
        census_col = sao_paulo_collapsed_census,
        station_id_col = "station_name",
        geo_sf_id_col = "code_weighting",
        socio_vars = c("education_mean", "income_mean"),
        context_method = "containing_geo")
    }),

  targets::tar_target(sao_paulo_station_socio,
    {
      join_station_scatter_inputs(
        pollution_dt = sao_paulo_station_pollution_summary,
        context_dt = sao_paulo_station_context,
        socio_vars = c("education_mean", "income_mean"),
        year_filter = analysis_year)
    }),

  targets::tar_target(sao_paulo_station_socio_file,
    {
      path <- here::here("data", "processed", "station_socio_exposure",
        "sao_paulo_2010", "sao_paulo_2010_2023_station_socio.parquet")
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(sao_paulo_station_socio, path)
      path
    }, format = "file"),

  targets::tar_target(compute_station_scatter_inputs,
    {
      c(bogota_station_socio_file, cdmx_station_socio_file,
        santiago_station_socio_file, sao_paulo_station_socio_file)
    }, format = "file"),

  targets::tar_target(scatter,
    compute_station_scatter_inputs,
    format = "file"),

  targets::tar_target(bogota_station_plot_data,
    {
      rescale_station_education(
        station_dt = safe_read_parquet(bogota_station_socio_file),
        city_label = "Bogota")
    }),

  targets::tar_target(bogota_station_scatter_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (outcome in names(station_scatter_labels)) {
        plots[[outcome]] <- plot_station_scatter(
          station_dt = bogota_station_plot_data,
          y_col = outcome,
          x_col = "education_mean",
          y_label = station_scatter_labels[[outcome]],
          x_label = "Average years of schooling")
      }
      plots
    }),

  targets::tar_target(bogota_station_scatter_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (outcome in names(station_scatter_tags)) {
        tag <- station_scatter_tags[[outcome]]
        file <- here::here(out_dir, paste0("scatter_plot_bogota_", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = bogota_station_scatter_plots[[outcome]],
          path = file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, file)
      }
      files
    }, format = "file"),

  targets::tar_target(bogota_station_coverage,
    {
      coverage <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          active <- get_active_station_ids(bogota_station_plot_data, pollutant)
          coverage[[key]] <- build_station_distance_trend_data(
            dist_pq = bogota_2018_distances[basename(bogota_2018_distances) ==
              "matrix_geo_station_distances.parquet"],
            census_dt = bogota_collapsed_census,
            active_ids = active,
            radius_km = radius_km)
        }
      }
      coverage
    }),

  targets::tar_target(bogota_station_distance_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          label <- if (pollutant == "pm10") "PM10" else "PM2.5"
          plots[[key]] <- plot_station_distance_trend(
            dt = bogota_station_coverage[[key]],
            city_label = "Bogota",
            pollutant = label,
            radius_km = radius_km)
        }
      }
      plots
    }),

  targets::tar_target(bogota_station_education_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      plots$avg_pollution <- plot_dual_pollutant_station_scatter(
        station_dt = bogota_station_plot_data,
        city_label = "Bogota",
        y_pm10 = "avg_pm10",
        y_pm25 = "avg_pm25",
        title = "Annual average concentration in 2023",
        y_left = "PM10 annual average",
        y_right = "PM2.5 annual average")
      plots$hours_it1 <- plot_dual_pollutant_station_scatter(
        station_dt = bogota_station_plot_data,
        city_label = "Bogota",
        y_pm10 = "hrs_d_pm10_it1",
        y_pm25 = "hrs_d_pm25_it1",
        title = "Hours above WHO IT1 threshold in 2023",
        y_left = "PM10 hours above IT1",
        y_right = "PM2.5 hours above IT1")
      plots$hours_it2 <- plot_dual_pollutant_station_scatter(
        station_dt = bogota_station_plot_data,
        city_label = "Bogota",
        y_pm10 = "hrs_d_pm10_it2",
        y_pm25 = "hrs_d_pm25_it2",
        title = "Hours above WHO IT2 threshold in 2023",
        y_left = "PM10 hours above IT2",
        y_right = "PM2.5 hours above IT2")
      plots
    }),

  targets::tar_target(bogota_station_monitoring_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in monitoring_radii_km) {
        radius_tag <- if (radius_km == 3) "3km_v2" else paste0(radius_km, "km")
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          tag <- if (pollutant == "pm10") "" else "_pm25"
          file <- here::here(out_dir, paste0("stations_dis_num_bogota_",
            radius_tag, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = bogota_station_distance_plots[[key]],
            path = file,
            width = 8.5,
            height = 5.8,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      file <- here::here(out_dir, "bogota_2018_avg_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = bogota_station_education_plots$avg_pollution,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "bogota_2018_hours_it1_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = bogota_station_education_plots$hours_it1,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "bogota_2018_hours_it2_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = bogota_station_education_plots$hours_it2,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(bogota_population_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_population_density_map(
        metro_sf = bogota_2018_distance_geography,
        stations_sf = bogota_distance_stations,
        arrow_dir = bogota_pollution_parquet[basename(bogota_pollution_parquet) ==
          "bogota_metro_dataset"],
        census_df = bogota_collapsed_census,
        join_sf_col = "GEO_ID",
        join_df_col = "geo_id",
        station_col = "station_name",
        year_filter = analysis_year,
        city_label = "Bogotá")
    }),

  targets::tar_target(bogota_education_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_inequality_pollution(
        metro_sf = bogota_2018_distance_geography,
        stations_sf = bogota_distance_stations,
        arrow_dir = bogota_pollution_parquet[basename(bogota_pollution_parquet) ==
          "bogota_metro_dataset"],
        census_df = bogota_collapsed_census,
        join_sf_col = "GEO_ID",
        join_df_col = "geo_id",
        station_col = "station_name",
        year_filter = analysis_year,
        ed_col = "education_mean",
        pop_col = "pop_total",
        buffer_km = station_context_buffer_km,
        city_label = "")
    }),

  targets::tar_target(cdmx_station_plot_data,
    {
      rescale_station_education(
        station_dt = safe_read_parquet(cdmx_station_socio_file),
        city_label = "Mexico City")
    }),

  targets::tar_target(cdmx_station_scatter_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (outcome in names(station_scatter_labels)) {
        plots[[outcome]] <- plot_station_scatter(
          station_dt = cdmx_station_plot_data,
          y_col = outcome,
          x_col = "education_mean",
          y_label = station_scatter_labels[[outcome]],
          x_label = "Average years of schooling")
      }
      plots$income_pm10 <- plot_station_scatter(
        station_dt = cdmx_station_plot_data,
        y_col = "hrs_d_pm10_it1",
        x_col = "income_mean",
        y_label = station_scatter_labels[["hrs_d_pm10_it1"]],
        x_label = "Average monthly labour income")
      plots$income_pm25 <- plot_station_scatter(
        station_dt = cdmx_station_plot_data,
        y_col = "hrs_d_pm25_it1",
        x_col = "income_mean",
        y_label = station_scatter_labels[["hrs_d_pm25_it1"]],
        x_label = "Average monthly labour income")
      plots
    }),

  targets::tar_target(cdmx_station_scatter_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (outcome in names(station_scatter_tags)) {
        tag <- station_scatter_tags[[outcome]]
        file <- here::here(out_dir, paste0("scatter_plot_mexico_", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = cdmx_station_scatter_plots[[outcome]],
          path = file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, file)
      }
      file <- here::here(out_dir, "scatter_plot_mexico_2023_income.pdf")
      save_plot_pdf(
        plot_obj = cdmx_station_scatter_plots$income_pm10,
        path = file,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "scatter_plot_mexico_pm25_2023_income.pdf")
      save_plot_pdf(
        plot_obj = cdmx_station_scatter_plots$income_pm25,
        path = file,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(cdmx_station_coverage,
    {
      coverage <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          active <- get_active_station_ids(cdmx_station_plot_data, pollutant)
          coverage[[key]] <- build_station_distance_trend_data(
            dist_pq = cdmx_2020_distances[basename(cdmx_2020_distances) ==
              "matrix_geo_station_distances.parquet"],
            census_dt = cdmx_collapsed_census,
            active_ids = active,
            radius_km = radius_km)
        }
      }
      coverage
    }),

  targets::tar_target(cdmx_station_distance_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          label <- if (pollutant == "pm10") "PM10" else "PM2.5"
          plots[[key]] <- plot_station_distance_trend(
            dt = cdmx_station_coverage[[key]],
            city_label = "Mexico City",
            pollutant = label,
            radius_km = radius_km)
        }
      }
      plots
    }),

  targets::tar_target(cdmx_station_education_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      plots$avg_pollution <- plot_dual_pollutant_station_scatter(
        station_dt = cdmx_station_plot_data,
        city_label = "Mexico City",
        y_pm10 = "avg_pm10",
        y_pm25 = "avg_pm25",
        title = "Annual average concentration in 2023",
        y_left = "PM10 annual average",
        y_right = "PM2.5 annual average")
      plots$hours_it1 <- plot_dual_pollutant_station_scatter(
        station_dt = cdmx_station_plot_data,
        city_label = "Mexico City",
        y_pm10 = "hrs_d_pm10_it1",
        y_pm25 = "hrs_d_pm25_it1",
        title = "Hours above WHO IT1 threshold in 2023",
        y_left = "PM10 hours above IT1",
        y_right = "PM2.5 hours above IT1")
      plots$hours_it2 <- plot_dual_pollutant_station_scatter(
        station_dt = cdmx_station_plot_data,
        city_label = "Mexico City",
        y_pm10 = "hrs_d_pm10_it2",
        y_pm25 = "hrs_d_pm25_it2",
        title = "Hours above WHO IT2 threshold in 2023",
        y_left = "PM10 hours above IT2",
        y_right = "PM2.5 hours above IT2")
      plots
    }),

  targets::tar_target(cdmx_station_monitoring_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in monitoring_radii_km) {
        radius_tag <- if (radius_km == 3) "3km_v2" else paste0(radius_km, "km")
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          tag <- if (pollutant == "pm10") "" else "_pm25"
          file <- here::here(out_dir, paste0("stations_dis_num_mexico_",
            radius_tag, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = cdmx_station_distance_plots[[key]],
            path = file,
            width = 8.5,
            height = 5.8,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      file <- here::here(out_dir, "cdmx_2020_avg_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = cdmx_station_education_plots$avg_pollution,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "cdmx_2020_hours_it1_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = cdmx_station_education_plots$hours_it1,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "cdmx_2020_hours_it2_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = cdmx_station_education_plots$hours_it2,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(cdmx_population_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_population_density_map(
        metro_sf = cdmx_2020_distance_geography,
        stations_sf = cdmx_distance_stations,
        arrow_dir = cdmx_pollution_parquet[basename(cdmx_pollution_parquet) ==
          "cdmx_metro_dataset"],
        census_df = cdmx_collapsed_census,
        join_sf_col = "CVE_MUN",
        join_df_col = "geo_id",
        station_col = "station",
        year_filter = analysis_year,
        city_label = "Mexico City")
    }),

  targets::tar_target(cdmx_education_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_inequality_pollution(
        metro_sf = cdmx_2020_distance_geography,
        stations_sf = cdmx_distance_stations,
        arrow_dir = cdmx_pollution_parquet[basename(cdmx_pollution_parquet) ==
          "cdmx_metro_dataset"],
        census_df = cdmx_collapsed_census,
        join_sf_col = "CVE_MUN",
        join_df_col = "geo_id",
        station_col = "station",
        year_filter = analysis_year,
        ed_col = "education_mean",
        pop_col = "pop_total",
        buffer_km = station_context_buffer_km,
        city_label = "")
    }),

  targets::tar_target(santiago_station_plot_data,
    {
      rescale_station_education(
        station_dt = safe_read_parquet(santiago_station_socio_file),
        city_label = "Gran Santiago")
    }),

  targets::tar_target(santiago_station_scatter_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (outcome in names(station_scatter_labels)) {
        plots[[outcome]] <- plot_station_scatter(
          station_dt = santiago_station_plot_data,
          y_col = outcome,
          x_col = "education_mean",
          y_label = station_scatter_labels[[outcome]],
          x_label = "Average years of schooling")
      }
      plots
    }),

  targets::tar_target(santiago_station_scatter_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (outcome in names(station_scatter_tags)) {
        tag <- station_scatter_tags[[outcome]]
        if (tag == "IT2_pm25_2023") tag <- "pm25_IT2_2023"
        file <- here::here(out_dir, paste0("scatter_plot_santiago_", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = santiago_station_scatter_plots[[outcome]],
          path = file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, file)
      }
      files
    }, format = "file"),

  targets::tar_target(santiago_station_coverage,
    {
      coverage <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          active <- get_active_station_ids(santiago_station_plot_data, pollutant)
          coverage[[key]] <- build_station_distance_trend_data(
            dist_pq = santiago_2017_distances[basename(santiago_2017_distances) ==
              "matrix_geo_station_distances.parquet"],
            census_dt = santiago_collapsed_census,
            active_ids = active,
            radius_km = radius_km)
        }
      }
      coverage
    }),

  targets::tar_target(santiago_station_distance_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          label <- if (pollutant == "pm10") "PM10" else "PM2.5"
          plots[[key]] <- plot_station_distance_trend(
            dt = santiago_station_coverage[[key]],
            city_label = "Gran Santiago",
            pollutant = label,
            radius_km = radius_km)
        }
      }
      plots
    }),

  targets::tar_target(santiago_station_education_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      plots$avg_pollution <- plot_dual_pollutant_station_scatter(
        station_dt = santiago_station_plot_data,
        city_label = "Gran Santiago",
        y_pm10 = "avg_pm10",
        y_pm25 = "avg_pm25",
        title = "Annual average concentration in 2023",
        y_left = "PM10 annual average",
        y_right = "PM2.5 annual average")
      plots$hours_it1 <- plot_dual_pollutant_station_scatter(
        station_dt = santiago_station_plot_data,
        city_label = "Gran Santiago",
        y_pm10 = "hrs_d_pm10_it1",
        y_pm25 = "hrs_d_pm25_it1",
        title = "Hours above WHO IT1 threshold in 2023",
        y_left = "PM10 hours above IT1",
        y_right = "PM2.5 hours above IT1")
      plots$hours_it2 <- plot_dual_pollutant_station_scatter(
        station_dt = santiago_station_plot_data,
        city_label = "Gran Santiago",
        y_pm10 = "hrs_d_pm10_it2",
        y_pm25 = "hrs_d_pm25_it2",
        title = "Hours above WHO IT2 threshold in 2023",
        y_left = "PM10 hours above IT2",
        y_right = "PM2.5 hours above IT2")
      plots
    }),

  targets::tar_target(santiago_station_monitoring_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in monitoring_radii_km) {
        radius_tag <- if (radius_km == 3) "3km_v2" else paste0(radius_km, "km")
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          tag <- if (pollutant == "pm10") "" else "_pm25"
          file <- here::here(out_dir, paste0("stations_dis_num_santiago_",
            radius_tag, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = santiago_station_distance_plots[[key]],
            path = file,
            width = 8.5,
            height = 5.8,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      file <- here::here(out_dir, "santiago_2017_avg_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = santiago_station_education_plots$avg_pollution,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "santiago_2017_hours_it1_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = santiago_station_education_plots$hours_it1,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "santiago_2017_hours_it2_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = santiago_station_education_plots$hours_it2,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(santiago_population_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_population_density_map(
        metro_sf = santiago_2017_distance_geography,
        stations_sf = santiago_distance_stations,
        arrow_dir = santiago_pollution_parquet[basename(santiago_pollution_parquet) ==
          "santiago_metro_dataset"],
        census_df = santiago_collapsed_census,
        join_sf_col = "zona_id",
        join_df_col = "geo_id",
        station_col = "station_name",
        year_filter = analysis_year,
        city_label = "Santiago")
    }),

  targets::tar_target(santiago_education_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_inequality_pollution(
        metro_sf = santiago_2017_distance_geography,
        stations_sf = santiago_distance_stations,
        arrow_dir = santiago_pollution_parquet[basename(santiago_pollution_parquet) ==
          "santiago_metro_dataset"],
        census_df = santiago_collapsed_census,
        join_sf_col = "zona_id",
        join_df_col = "geo_id",
        station_col = "station_name",
        year_filter = analysis_year,
        ed_col = "education_mean",
        pop_col = "pop_total",
        buffer_km = station_context_buffer_km,
        city_label = "")
    }),

  targets::tar_target(sao_paulo_station_plot_data,
    {
      rescale_station_education(
        station_dt = safe_read_parquet(sao_paulo_station_socio_file),
        city_label = "Sao Paulo")
    }),

  targets::tar_target(sao_paulo_station_scatter_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (outcome in names(station_scatter_labels)) {
        plots[[outcome]] <- plot_station_scatter(
          station_dt = sao_paulo_station_plot_data,
          y_col = outcome,
          x_col = "education_mean",
          y_label = station_scatter_labels[[outcome]],
          x_label = "Average years of schooling")
      }
      plots$income_pm10 <- plot_station_scatter(
        station_dt = sao_paulo_station_plot_data,
        y_col = "hrs_d_pm10_it1",
        x_col = "income_mean",
        y_label = station_scatter_labels[["hrs_d_pm10_it1"]],
        x_label = "Average monthly labour income")
      plots$income_pm25 <- plot_station_scatter(
        station_dt = sao_paulo_station_plot_data,
        y_col = "hrs_d_pm25_it1",
        x_col = "income_mean",
        y_label = station_scatter_labels[["hrs_d_pm25_it1"]],
        x_label = "Average monthly labour income")
      plots
    }),

  targets::tar_target(sao_paulo_station_scatter_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (outcome in names(station_scatter_tags)) {
        tag <- station_scatter_tags[[outcome]]
        file <- here::here(out_dir, paste0("scatter_plot_saopaulo_", tag, ".pdf"))
        save_plot_pdf(
          plot_obj = sao_paulo_station_scatter_plots[[outcome]],
          path = file,
          width = 8.5,
          height = 5.8,
          dpi = 300,
          bg = "white",
          limitsize = FALSE)
        files <- c(files, file)
      }
      file <- here::here(out_dir, "scatter_plot_saopaulo_2023_income.pdf")
      save_plot_pdf(
        plot_obj = sao_paulo_station_scatter_plots$income_pm10,
        path = file,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "scatter_plot_saopaulo_pm25_2023_income.pdf")
      save_plot_pdf(
        plot_obj = sao_paulo_station_scatter_plots$income_pm25,
        path = file,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(sao_paulo_station_coverage,
    {
      coverage <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          active <- get_active_station_ids(sao_paulo_station_plot_data, pollutant)
          coverage[[key]] <- build_station_distance_trend_data(
            dist_pq = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
              "matrix_geo_station_distances.parquet"],
            census_dt = sao_paulo_collapsed_census,
            active_ids = active,
            radius_km = radius_km)
        }
      }
      coverage
    }),

  targets::tar_target(sao_paulo_station_distance_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      for (radius_km in monitoring_radii_km) {
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          label <- if (pollutant == "pm10") "PM10" else "PM2.5"
          plots[[key]] <- plot_station_distance_trend(
            dt = sao_paulo_station_coverage[[key]],
            city_label = "Sao Paulo",
            pollutant = label,
            radius_km = radius_km)
        }
      }
      plots
    }),

  targets::tar_target(sao_paulo_station_education_plots,
    {
      paper_font
      set_paper_theme()
      plots <- list()
      plots$avg_pollution <- plot_dual_pollutant_station_scatter(
        station_dt = sao_paulo_station_plot_data,
        city_label = "Sao Paulo",
        y_pm10 = "avg_pm10",
        y_pm25 = "avg_pm25",
        title = "Annual average concentration in 2023",
        y_left = "PM10 annual average",
        y_right = "PM2.5 annual average")
      plots$hours_it1 <- plot_dual_pollutant_station_scatter(
        station_dt = sao_paulo_station_plot_data,
        city_label = "Sao Paulo",
        y_pm10 = "hrs_d_pm10_it1",
        y_pm25 = "hrs_d_pm25_it1",
        title = "Hours above WHO IT1 threshold in 2023",
        y_left = "PM10 hours above IT1",
        y_right = "PM2.5 hours above IT1")
      plots$hours_it2 <- plot_dual_pollutant_station_scatter(
        station_dt = sao_paulo_station_plot_data,
        city_label = "Sao Paulo",
        y_pm10 = "hrs_d_pm10_it2",
        y_pm25 = "hrs_d_pm25_it2",
        title = "Hours above WHO IT2 threshold in 2023",
        y_left = "PM10 hours above IT2",
        y_right = "PM2.5 hours above IT2")
      plots
    }),

  targets::tar_target(sao_paulo_station_monitoring_files,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      for (radius_km in monitoring_radii_km) {
        radius_tag <- if (radius_km == 3) "3km_v2" else paste0(radius_km, "km")
        for (pollutant in summary_pollutants) {
          key <- paste(radius_km, pollutant, sep = "_")
          tag <- if (pollutant == "pm10") "" else "_pm25"
          file <- here::here(out_dir, paste0("stations_dis_num_saopaulo_",
            radius_tag, tag, ".pdf"))
          save_plot_pdf(
            plot_obj = sao_paulo_station_distance_plots[[key]],
            path = file,
            width = 8.5,
            height = 5.8,
            dpi = 300,
            bg = "white",
            limitsize = FALSE)
          files <- c(files, file)
        }
      }
      file <- here::here(out_dir, "sao_paulo_2010_avg_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = sao_paulo_station_education_plots$avg_pollution,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "sao_paulo_2010_hours_it1_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = sao_paulo_station_education_plots$hours_it1,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "sao_paulo_2010_hours_it2_pm10_pm25_vs_education.png")
      ggplot2::ggsave(
        filename = file,
        plot = sao_paulo_station_education_plots$hours_it2,
        width = 8.5,
        height = 5.8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(sao_paulo_population_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_population_density_map(
        metro_sf = sao_paulo_2010_distance_geography,
        stations_sf = sao_paulo_distance_stations,
        arrow_dir = sao_paulo_pollution_parquet[basename(sao_paulo_pollution_parquet) ==
          "sao_paulo_metro_dataset"],
        census_df = sao_paulo_collapsed_census,
        join_sf_col = "code_weighting",
        join_df_col = "geo_id",
        station_col = "station_name",
        year_filter = analysis_year,
        city_label = "São Paulo")
    }),

  targets::tar_target(sao_paulo_education_map,
    {
      paper_font
      set_paper_theme()
      sf::sf_use_s2(TRUE)
      plot_inequality_pollution(
        metro_sf = sao_paulo_2010_distance_geography,
        stations_sf = sao_paulo_distance_stations,
        arrow_dir = sao_paulo_pollution_parquet[basename(sao_paulo_pollution_parquet) ==
          "sao_paulo_metro_dataset"],
        census_df = sao_paulo_collapsed_census,
        join_sf_col = "code_weighting",
        join_df_col = "geo_id",
        station_col = "station_name",
        year_filter = analysis_year,
        ed_col = "education_mean",
        pop_col = "pop_total",
        buffer_km = station_context_buffer_km,
        city_label = "")
    }),

  targets::tar_target(figure_station_scatter,
    {
      c(
        bogota_station_scatter_files,
        cdmx_station_scatter_files,
        santiago_station_scatter_files,
        sao_paulo_station_scatter_files)
    }, format = "file"),

  targets::tar_target(plot_station_monitoring_figures,
    {
      c(
        bogota_station_monitoring_files,
        cdmx_station_monitoring_files,
        santiago_station_monitoring_files,
        sao_paulo_station_monitoring_files)
    }, format = "file"),

  targets::tar_target(figure_population_density_maps,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      file <- here::here(out_dir, "bogota_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = bogota_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "mexico_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = cdmx_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "santiago_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = santiago_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "saopaulo_population_density_map.pdf")
      save_plot_pdf(
        plot_obj = sao_paulo_population_map,
        path = file,
        width = 8,
        height = 8,
        dpi = 300,
        bg = NULL,
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file"),

  targets::tar_target(figure_pollution_quintile_maps,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "monitoring")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      files <- character()
      file <- here::here(out_dir, "map_bogota_3km.pdf")
      save_plot_pdf(
        plot_obj = bogota_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "map_mexico_3km.pdf")
      save_plot_pdf(
        plot_obj = cdmx_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "map_santiago_3km_dc.pdf")
      save_plot_pdf(
        plot_obj = santiago_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      file <- here::here(out_dir, "map_saopaulo_3km.pdf")
      save_plot_pdf(
        plot_obj = sao_paulo_education_map,
        path = file,
        width = 12,
        height = 8,
        dpi = 300,
        bg = "white",
        limitsize = FALSE)
      files <- c(files, file)
      files
    }, format = "file")
)
