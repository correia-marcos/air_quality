# Outlier and quality-screen target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = bogota_pollution_parquet[basename(bogota_pollution_parquet) ==
            "bogota_metro_dataset"],
        station_dist_path = bogota_2018_distances[basename(bogota_2018_distances) ==
            "matrix_station_distances.parquet"],
        pct_flag = outlier_quantile,
        n_sd = outlier_sd_band,
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
        neighbor_eligibility = outlier_neighbor_eligibility,
        out_dir = here::here("data", "processed", "monitoring_stations_outliers"),
        out_name = "bogota_metro",
        overwrite = TRUE),
    format = "file"),

  targets::tar_target(bogota_quality_summary,
    summarize_pollution_quality(bogota_outliers)),

  targets::tar_target(bogota_quality_review_records,
    collect_pollution_screening_records(bogota_outliers)),

  targets::tar_target(bogota_quality_report,
    c(write_pollution_quality_summary(bogota_quality_summary,
        here::here("data", "processed", "pollution_quality", "bogota_quality_summary.csv")),
      write_pollution_quality_summary(bogota_quality_review_records,
        here::here("data", "processed", "pollution_quality", "bogota_review_records.csv"))),
    format = "file"),

  targets::tar_target(cdmx_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = cdmx_pollution_parquet[basename(cdmx_pollution_parquet) ==
            "cdmx_metro_dataset"],
        station_dist_path = cdmx_2020_distances[basename(cdmx_2020_distances) ==
            "matrix_station_distances.parquet"],
        pct_flag = outlier_quantile,
        n_sd = outlier_sd_band,
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
        neighbor_eligibility = outlier_neighbor_eligibility,
        out_dir = here::here("data", "processed", "monitoring_stations_outliers"),
        out_name = "cdmx_metro",
        overwrite = TRUE),
    format = "file"),

  targets::tar_target(cdmx_quality_summary,
    summarize_pollution_quality(cdmx_outliers)),

  targets::tar_target(cdmx_quality_review_records,
    collect_pollution_screening_records(cdmx_outliers)),

  targets::tar_target(cdmx_quality_report,
    c(write_pollution_quality_summary(cdmx_quality_summary,
        here::here("data", "processed", "pollution_quality", "cdmx_quality_summary.csv")),
      write_pollution_quality_summary(cdmx_quality_review_records,
        here::here("data", "processed", "pollution_quality", "cdmx_review_records.csv"))),
    format = "file"),

  targets::tar_target(santiago_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = santiago_pollution_parquet[basename(santiago_pollution_parquet) ==
            "santiago_metro_dataset"],
        station_dist_path = santiago_2017_distances[basename(santiago_2017_distances) ==
            "matrix_station_distances.parquet"],
        pct_flag = outlier_quantile,
        n_sd = outlier_sd_band,
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
        neighbor_eligibility = outlier_neighbor_eligibility,
        out_dir = here::here("data", "processed", "monitoring_stations_outliers"),
        out_name = "santiago_metro",
        overwrite = TRUE),
    format = "file"),

  targets::tar_target(santiago_quality_summary,
    summarize_pollution_quality(santiago_outliers)),

  targets::tar_target(santiago_quality_review_records,
    collect_pollution_screening_records(santiago_outliers)),

  targets::tar_target(santiago_quality_report,
    c(write_pollution_quality_summary(santiago_quality_summary,
        here::here("data", "processed", "pollution_quality",
          "santiago_quality_summary.csv")),
      write_pollution_quality_summary(santiago_quality_review_records,
        here::here("data", "processed", "pollution_quality",
          "santiago_review_records.csv"))),
    format = "file"),

  targets::tar_target(sao_paulo_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = sao_paulo_pollution_parquet[basename(sao_paulo_pollution_parquet) ==
            "sao_paulo_metro_dataset"],
        station_dist_path = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
            "matrix_station_distances.parquet"],
        pct_flag = outlier_quantile,
        n_sd = outlier_sd_band,
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
        neighbor_eligibility = outlier_neighbor_eligibility,
        out_dir = here::here("data", "processed", "monitoring_stations_outliers"),
        out_name = "sao_paulo_metro",
        overwrite = TRUE),
    format = "file"),

  targets::tar_target(sao_paulo_quality_summary,
    summarize_pollution_quality(sao_paulo_outliers)),

  targets::tar_target(sao_paulo_quality_review_records,
    collect_pollution_screening_records(sao_paulo_outliers)),

  targets::tar_target(sao_paulo_quality_report,
    c(write_pollution_quality_summary(sao_paulo_quality_summary,
        here::here("data", "processed", "pollution_quality",
          "sao_paulo_quality_summary.csv")),
      write_pollution_quality_summary(sao_paulo_quality_review_records,
        here::here("data", "processed", "pollution_quality",
          "sao_paulo_review_records.csv"))),
    format = "file"),

  targets::tar_target(outliers,
    c(bogota_outliers, cdmx_outliers, santiago_outliers, sao_paulo_outliers,
      bogota_quality_report, cdmx_quality_report, santiago_quality_report,
      sao_paulo_quality_report),
    format = "file")
)
