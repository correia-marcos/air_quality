# Tables export target declarations. Scientific functions live in src/.
list(
  targets::tar_target(geography,
    c(bogota_geography, cdmx_geography, santiago_geography, sao_paulo_geography),
    format = "file"),

  targets::tar_target(census,
    c(bogota_census, cdmx_census, santiago_census, sao_paulo_census),
    format = "file"),

  targets::tar_target(stations,
    c(bogota_stations_filter, cdmx_stations_filter, santiago_stations_filter,
      sao_paulo_stations_filter),
    format = "file"),

  targets::tar_target(pollution,
    c(bogota_pollution_parquet, cdmx_pollution_parquet, santiago_pollution_parquet,
        sao_paulo_pollution_parquet),
    format = "file"),

  targets::tar_target(distances,
    c(bogota_2018_distances, cdmx_2020_distances, santiago_2017_distances,
      santiago_2024_distances,
        sao_paulo_2010_distances),
    format = "file"),

  targets::tar_target(outliers,
    c(bogota_outliers, cdmx_outliers, santiago_outliers, sao_paulo_outliers,
      bogota_quality_report, cdmx_quality_report, santiago_quality_report,
      sao_paulo_quality_report),
    format = "file"),

  targets::tar_target(idw,
    c(bogota_2018_idw, cdmx_2020_idw, santiago_2017_idw, santiago_2024_idw,
      sao_paulo_2010_idw),
    format = "file"),

  targets::tar_target(process,
    c(geography, stations, pollution, census),
    format = "file"),

  targets::tar_target(paper_manifest,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        here::here("config", "paper_artifacts.csv")
    },
    format = "file"),

  targets::tar_target(paper_font,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        here::here("fonts", "texgyrepagella-regular.otf")
    },
    format = "file"),

  targets::tar_target(station_table_data,
    {
      list(
        counts = arrow::read_parquet(station_counts_files[
          basename(station_counts_files) ==
            sprintf("stations_by_pollutant_%d.parquet", analysis_year)]),
        who = arrow::read_parquet(who_exceedance_file),
        thresholds = arrow::read_parquet(threshold_exceedance_files[
          basename(threshold_exceedance_files) ==
            sprintf("days_and_hours_%d.parquet", analysis_year)]))
    }),

  targets::tar_target(station_table_tex,
    {
      who_table <- table_who_exceedances(exceedances_dt = station_table_data$who)
      list(counts = latex_station_counts(station_counts = station_table_data$counts),
           days = latex_threshold_exceedance_table(
             exceed_dt = station_table_data$thresholds, measure = "days"),
           hours = latex_threshold_exceedance_table(
             exceed_dt = station_table_data$thresholds, measure = "hours"),
           who = latex_who_exceedances(wide = who_table,
             caption = paste("Annual PM concentrations vs. WHO AQG 2021",
                             "(interim and long-term targets)."),
             label = "tab:who_exceedances"))
    }),

  targets::tar_target(render_station_tables,
    {
      c(
        write_latex_table(station_table_tex$counts,
          here::here("results", "tables",
            sprintf("stations_by_pollutant_%d.tex", analysis_year)),
          use_bytes = TRUE),
        write_latex_table(station_table_tex$days,
          here::here("results", "tables", "table_days_above_thresholds.tex"),
          use_bytes = TRUE),
        write_latex_table(station_table_tex$hours,
          here::here("results", "tables", "table_avg_hours_above_thresholds.tex"),
          use_bytes = TRUE),
        write_latex_table(station_table_tex$who,
          here::here("results", "tables", "who_exceedances_all_cities.tex")))
    }, format = "file"),

  targets::tar_target(missing_table_data,
    {
      city_files <- c("bogota", "cdmx", "santiago", "sao_paulo_metro")
      stems <- unlist(lapply(missing_table_dimensions, function(dimension) {
        sprintf("%s_%s_missing_by_%s", city_files, missing_table_panel, dimension)
      }))
      files <- missing_dimension_files[
        match(paste0(stems, ".parquet"), basename(missing_dimension_files))]
      quintile_file <- quintile_availability_files[
        basename(quintile_availability_files) ==
          sprintf("missing_by_education_quintile_%d.parquet", analysis_year)]
      list(dimensions = setNames(lapply(files, arrow::read_parquet), stems),
           quintiles = arrow::read_parquet(quintile_file))
    }),

  targets::tar_target(missing_table_tex,
    {
      tex <- list()
      for (dimension in missing_table_dimensions) {
        for (city in c("bogota", "cdmx", "santiago", "sao_paulo_metro")) {
          name <- sprintf("%s_%s_missing_by_%s", city, missing_table_panel, dimension)
          table <- table_missing_by_dimension(
            missing_list = setNames(list(missing_table_data$dimensions[[name]]), dimension),
            dim = dimension, city_label = city)
          tex[[name]] <- latex_missing_dimension(dt = table, dim = dimension,
            city_label = city)
        }
      }
      list(dimensions = tex,
        quintiles = latex_missing_by_quintile(missing_table_data$quintiles))
    }),

  targets::tar_target(render_missing_tables,
    {
      written <- character()
      for (name in names(missing_table_tex$dimensions)) {
        written <- c(written, write_latex_table(missing_table_tex$dimensions[[name]],
          here::here("results", "tables", paste0(name, ".tex"))))
      }
      c(written, write_latex_table(missing_table_tex$quintiles,
        here::here("results", "tables",
          sprintf("missing_by_education_quintile_%d.tex", analysis_year))))
    }, format = "file"),

  targets::tar_target(census_table_data,
    {
      list(census = arrow::read_parquet(census_summary_files[
             basename(census_summary_files) == "census_summary.parquet"]),
           bands = arrow::read_parquet(compute_distance_band_descriptives[
             basename(compute_distance_band_descriptives) ==
               "distance_band_descriptives.parquet"]))
    }),

  targets::tar_target(census_table_tex,
    {
      list(census = latex_census_summary(census_table_data$census),
           bands_a = latex_distance_band_table(bands_dt = census_table_data$bands,
             panel_cities = c("Bogota", "Mexico City")),
           bands_b = latex_distance_band_table(bands_dt = census_table_data$bands,
             panel_cities = c("Santiago", "Sao Paulo")))
    }),

  targets::tar_target(render_census_tables,
    {
      c(
        write_latex_table(census_table_tex$census,
          here::here("results", "tables", "census_summary_table.tex")),
        write_latex_table(census_table_tex$bands_a,
          here::here("results", "tables", "table_descriptives_a.tex")),
        write_latex_table(census_table_tex$bands_b,
          here::here("results", "tables", "table_descriptives_b.tex")))
    }, format = "file"),

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

  targets::tar_target(figures,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        c(generate_exposure_plots, figure_imputation_diagnostics,
          plot_station_monitoring_figures,
            figure_station_scatter, figure_population_density_maps,
              figure_pollution_quintile_maps,
            figure_kernel_distributions, figure_quintile_kernel_distributions,
              figure_station_temporal)
    },
    format = "file"),

  targets::tar_target(tables,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        c(render_station_tables, render_missing_tables, render_census_tables,
          render_exposure_tables)
    },
    format = "file"),

  targets::tar_target(exposure,
    c(idw, estimate_exposure),
    format = "file"),

  targets::tar_target(descriptives,
    c(compute_descriptive_tables, compute_distance_band_descriptives),
    format = "file"),

  targets::tar_target(scatter,
    compute_station_scatter_inputs,
    format = "file"),

  targets::tar_target(imputed,
    c(impute_missing_hourly, estimate_exposure_imputed),
    format = "file"),

  targets::tar_target(temporal,
    prepare_station_hourly,
    format = "file"),

  targets::tar_target(paper_export,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_paper_export(c(figures, tables), paper_manifest)
    },
    format = "file")
)
