# Manuscript export target declarations. Scientific functions live in src/.
list(
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

  targets::tar_target(figures,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        c(generate_exposure_plots, figure_exposure_imputed, figure_imputation_diagnostics,
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

  targets::tar_target(paper_export,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_paper_export(c(figures, tables), paper_manifest)
    },
    format = "file"),

  # The data each figure draws: every plot target built under the paper theme, as the
  # figure writers build them in ggsave(), one Parquet file per target.
  targets::tar_target(plot_layer_records,
    {
      paper_font
      set_paper_theme()
      suppressMessages(sf::sf_use_s2(TRUE))
      plots <- list(
        bogota_station_scatter_plots = bogota_station_scatter_plots,
        cdmx_station_scatter_plots = cdmx_station_scatter_plots,
        santiago_station_scatter_plots = santiago_station_scatter_plots,
        sao_paulo_station_scatter_plots = sao_paulo_station_scatter_plots,
        bogota_station_distance_plots = bogota_station_distance_plots,
        cdmx_station_distance_plots = cdmx_station_distance_plots,
        santiago_station_distance_plots = santiago_station_distance_plots,
        sao_paulo_station_distance_plots = sao_paulo_station_distance_plots,
        bogota_station_education_plots = bogota_station_education_plots,
        cdmx_station_education_plots = cdmx_station_education_plots,
        santiago_station_education_plots = santiago_station_education_plots,
        sao_paulo_station_education_plots = sao_paulo_station_education_plots,
        bogota_population_map = bogota_population_map,
        cdmx_population_map = cdmx_population_map,
        santiago_population_map = santiago_population_map,
        sao_paulo_population_map = sao_paulo_population_map,
        bogota_education_map = bogota_education_map,
        cdmx_education_map = cdmx_education_map,
        santiago_education_map = santiago_education_map,
        sao_paulo_education_map = sao_paulo_education_map,
        exposure_plots = exposure_plots,
        bogota_exposure_density_plots = bogota_exposure_density_plots,
        cdmx_exposure_density_plots = cdmx_exposure_density_plots,
        santiago_exposure_density_plots = santiago_exposure_density_plots,
        sao_paulo_exposure_density_plots = sao_paulo_exposure_density_plots,
        bogota_imputation_plots = bogota_imputation_plots,
        cdmx_imputation_plots = cdmx_imputation_plots,
        santiago_imputation_plots = santiago_imputation_plots,
        sao_paulo_imputation_plots = sao_paulo_imputation_plots,
        exposure_imputed_plots = exposure_imputed_plots,
        bogota_station_temporal_plots = bogota_station_temporal_plots,
        cdmx_station_temporal_plots = cdmx_station_temporal_plots,
        santiago_station_temporal_plots = santiago_station_temporal_plots,
        sao_paulo_station_temporal_plots = sao_paulo_station_temporal_plots,
        station_temporal_episode_plots = station_temporal_episode_plots,
        kernel_distribution_plots = kernel_distribution_plots,
        exposure_5km_plots = exposure_5km_plots)
      out_dir <- here::here("data", "processed", "plot_layers")
      files <- character()
      for (name in names(plots)) {
        files <- c(files, write_plot_layers(plots = plots[[name]],
          file = here::here(out_dir, paste0(name, ".parquet"))))
      }
      files
    },
    format = "file"),

  # Records the verification registry compares that no figure or table reads: each city's
  # PM screening, the imputation info, the IT2 episodes and the data each figure draws.
  targets::tar_target(verification_records,
    c(bogota_quality_report, cdmx_quality_report, santiago_quality_report,
      sao_paulo_quality_report, imputation_count_file, station_temporal_episode_file,
      plot_layer_records),
    format = "file")
)
