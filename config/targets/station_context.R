# Station context target declarations. Scientific functions live in src/.
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


  targets::tar_target(bogota_imputation,
    {
      result <- impute_missing_hourly_ols(arrow_dir = bogota_outliers,
        out_dir = here::here("data", "processed", "imputed_ols"),
        out_name = "bogota_imputed", pollutants = imputation_pollutants,
        id_col = "station", years = imputation_year, diag_year = imputation_year)
      files <- c(result$out_path, result$diag_path, result$summary_path)
      files[!is.na(files)]
    }, format = "file"),

  targets::tar_target(cdmx_imputation,
    {
      result <- impute_missing_hourly_ols(arrow_dir = cdmx_outliers,
        out_dir = here::here("data", "processed", "imputed_ols"),
        out_name = "cdmx_imputed", pollutants = imputation_pollutants,
        id_col = "station_code", years = imputation_year, diag_year = imputation_year)
      files <- c(result$out_path, result$diag_path, result$summary_path)
      files[!is.na(files)]
    }, format = "file"),

  targets::tar_target(santiago_imputation,
    {
      result <- impute_missing_hourly_ols(arrow_dir = santiago_outliers,
        out_dir = here::here("data", "processed", "imputed_ols"),
        out_name = "santiago_imputed", pollutants = imputation_pollutants,
        id_col = "station", years = imputation_year, diag_year = imputation_year)
      files <- c(result$out_path, result$diag_path, result$summary_path)
      files[!is.na(files)]
    }, format = "file"),

  targets::tar_target(sao_paulo_imputation,
    {
      result <- impute_missing_hourly_ols(arrow_dir = sao_paulo_outliers,
        out_dir = here::here("data", "processed", "imputed_ols"),
        out_name = "sao_paulo_imputed", pollutants = imputation_pollutants,
        id_col = "station", years = imputation_year, diag_year = imputation_year)
      files <- c(result$out_path, result$diag_path, result$summary_path)
      files[!is.na(files)]
    }, format = "file"),

  targets::tar_target(imputation_counts,
    {
      data.table::rbindlist(list(
        "Bogota" = arrow::read_parquet(bogota_imputation[
          basename(bogota_imputation) == "bogota_imputed_counts.parquet"]),
        "CDMX" = arrow::read_parquet(cdmx_imputation[
          basename(cdmx_imputation) == "cdmx_imputed_counts.parquet"]),
        "Santiago" = arrow::read_parquet(santiago_imputation[
          basename(santiago_imputation) == "santiago_imputed_counts.parquet"]),
        "Sao Paulo" = arrow::read_parquet(sao_paulo_imputation[
          basename(sao_paulo_imputation) == "sao_paulo_imputed_counts.parquet"])),
        idcol = "city")
    }),

  targets::tar_target(imputation_count_file,
    {
      file <- here::here("data", "processed", "imputed_ols",
        paste0("imputation_summary_", imputation_year, ".parquet"))
      arrow::write_parquet(imputation_counts, file)
      file
    }, format = "file"),

  targets::tar_target(impute_missing_hourly,
    {
      c(bogota_imputation, cdmx_imputation, santiago_imputation, sao_paulo_imputation,
        imputation_count_file)
    }, format = "file")
)
