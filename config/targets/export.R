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

  # Records the verification registry compares that no figure or table reads: each
  # city's particulate screening, the imputation counts and the IT2 episodes.
  targets::tar_target(verification_records,
    c(bogota_quality_report, cdmx_quality_report, santiago_quality_report,
      sao_paulo_quality_report, imputation_count_file, station_temporal_episode_file),
    format = "file")
)
