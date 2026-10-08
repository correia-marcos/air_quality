# Station temporal target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_station_hourly_summary,
    summarize_city_hourly_pm25(station_data = arrow::open_dataset(bogota_outliers),
      year = analysis_year)),

  targets::tar_target(bogota_station_hourly_file,
    write_station_hourly(hourly = bogota_station_hourly_summary,
      file = here::here("data", "processed", "station_hourly",
        sprintf("bogota_pm25_%d.parquet", analysis_year))), format = "file"),

  targets::tar_target(bogota_station_temporal_data,
    arrow::read_parquet(bogota_station_hourly_file)),

  targets::tar_target(bogota_station_temporal_plots,
    {
      paper_font
      set_paper_theme()
      ridge <- plot_hourly_ridgeline_pollution(df = bogota_station_temporal_data,
        region_name = "Bogota", pollution_var = "pm25_stations") +
        ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())
      list(ridge = ridge)
    }),

  targets::tar_target(cdmx_station_hourly_summary,
    summarize_city_hourly_pm25(station_data = arrow::open_dataset(cdmx_outliers),
      year = analysis_year)),

  targets::tar_target(cdmx_station_hourly_file,
    write_station_hourly(hourly = cdmx_station_hourly_summary,
      file = here::here("data", "processed", "station_hourly",
        sprintf("cdmx_pm25_%d.parquet", analysis_year))), format = "file"),

  targets::tar_target(cdmx_station_temporal_data,
    arrow::read_parquet(cdmx_station_hourly_file)),

  targets::tar_target(cdmx_station_temporal_plots,
    {
      paper_font
      set_paper_theme()
      ridge <- plot_hourly_ridgeline_pollution(df = cdmx_station_temporal_data,
        region_name = "Ciudad de México", pollution_var = "pm25_stations") +
        ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())
      list(ridge = ridge)
    }),

  targets::tar_target(santiago_station_hourly_summary,
    summarize_city_hourly_pm25(station_data = arrow::open_dataset(santiago_outliers),
      year = analysis_year)),

  targets::tar_target(santiago_station_hourly_file,
    write_station_hourly(hourly = santiago_station_hourly_summary,
      file = here::here("data", "processed", "station_hourly",
        sprintf("santiago_pm25_%d.parquet", analysis_year))), format = "file"),

  targets::tar_target(santiago_station_temporal_data,
    arrow::read_parquet(santiago_station_hourly_file)),

  targets::tar_target(santiago_station_temporal_plots,
    {
      paper_font
      set_paper_theme()
      ridge <- plot_hourly_ridgeline_pollution(df = santiago_station_temporal_data,
        region_name = "Santiago", pollution_var = "pm25_stations") +
        ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())
      list(ridge = ridge)
    }),

  targets::tar_target(sao_paulo_station_hourly_summary,
    summarize_city_hourly_pm25(station_data = arrow::open_dataset(sao_paulo_outliers),
      year = analysis_year)),

  targets::tar_target(sao_paulo_station_hourly_file,
    write_station_hourly(hourly = sao_paulo_station_hourly_summary,
      file = here::here("data", "processed", "station_hourly",
        sprintf("sao_paulo_pm25_%d.parquet", analysis_year))), format = "file"),

  targets::tar_target(sao_paulo_station_temporal_data,
    arrow::read_parquet(sao_paulo_station_hourly_file)),

  targets::tar_target(sao_paulo_station_temporal_plots,
    {
      paper_font
      set_paper_theme()
      ridge <- plot_hourly_ridgeline_pollution(df = sao_paulo_station_temporal_data,
        region_name = "São Paulo", pollution_var = "pm25_stations") +
        ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())
      list(ridge = ridge)
    }),

  targets::tar_target(prepare_station_hourly,
    c(bogota_station_hourly_file, cdmx_station_hourly_file,
      santiago_station_hourly_file, sao_paulo_station_hourly_file), format = "file"),

  targets::tar_target(station_temporal_episodes,
    dplyr::bind_rows(
      compute_time_spans_above_target(bogota_station_temporal_data, "Bogota", "IT2"),
      compute_time_spans_above_target(cdmx_station_temporal_data,
        "Ciudad de México", "IT2"),
      compute_time_spans_above_target(santiago_station_temporal_data, "Santiago", "IT2"),
      compute_time_spans_above_target(sao_paulo_station_temporal_data,
        "São Paulo", "IT2"))),

  targets::tar_target(station_temporal_episode_file,
    write_station_episodes(station_temporal_episodes,
      here::here("data", "processed", "station_hourly",
        sprintf("episodes_it2_%d.csv", analysis_year))), format = "file"),

  targets::tar_target(station_temporal_episode_plots,
    {
      paper_font
      set_paper_theme()
      it2 <- plot_episode_spans_ridgeline(station_temporal_episodes, target = "IT2") +
        ggplot2::labs(title = NULL) + ggplot2::theme(plot.title = ggplot2::element_blank())
      list(IT2 = it2)
    }),

  targets::tar_target(figure_station_temporal,
    {
      paper_font
      set_paper_theme()
      out_dir <- here::here("results", "figures", "temporal")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      c(
        save_plot_pdf(bogota_station_temporal_plots$ridge,
          here::here(out_dir, "bogota_ridge_plot.pdf"), width = 16, height = 9,
          bg = NULL, limitsize = FALSE),
        save_plot_pdf(cdmx_station_temporal_plots$ridge,
          here::here(out_dir, "ciudad_mexico_ridge_plot.pdf"), width = 16, height = 9,
          bg = NULL, limitsize = FALSE),
        save_plot_pdf(santiago_station_temporal_plots$ridge,
          here::here(out_dir, "santiago_ridge_plot.pdf"), width = 16, height = 9,
          bg = NULL, limitsize = FALSE),
        save_plot_pdf(sao_paulo_station_temporal_plots$ridge,
          here::here(out_dir, "sao_paulo_ridge_plot.pdf"), width = 16, height = 9,
          bg = NULL, limitsize = FALSE),
        save_plot_pdf(station_temporal_episode_plots$IT2,
          here::here(out_dir, "distribution_hours_above_IT2.pdf"), width = 16,
          height = 9, bg = NULL, limitsize = FALSE))
    }, format = "file")
)
