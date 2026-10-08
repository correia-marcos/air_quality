# Descriptives target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_missing_raw,
    compute_missing_proportions(
      arrow_dir = bogota_pollution_parquet[
        basename(bogota_pollution_parquet) == "bogota_metro_dataset"],
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(cdmx_missing_raw,
    compute_missing_proportions(
      arrow_dir = cdmx_pollution_parquet[
        basename(cdmx_pollution_parquet) == "cdmx_metro_dataset"],
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(santiago_missing_raw,
    compute_missing_proportions(
      arrow_dir = santiago_pollution_parquet[
        basename(santiago_pollution_parquet) == "santiago_metro_dataset"],
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(sao_paulo_missing_raw,
    compute_missing_proportions(
      arrow_dir = sao_paulo_pollution_parquet[
        basename(sao_paulo_pollution_parquet) == "sao_paulo_metro_dataset"],
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(bogota_missing_clean,
    compute_missing_proportions(
      arrow_dir = bogota_outliers,
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(cdmx_missing_clean,
    compute_missing_proportions(
      arrow_dir = cdmx_outliers,
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(santiago_missing_clean,
    compute_missing_proportions(
      arrow_dir = santiago_outliers,
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(sao_paulo_missing_clean,
    compute_missing_proportions(
      arrow_dir = sao_paulo_outliers,
      pollutants = summary_pollutants,
      dims = missing_dimensions,
      year_filter = analysis_year)),

  targets::tar_target(bogota_station_counts,
    count_stations_reporting(
      arrow_dir = bogota_pollution_parquet[
        basename(bogota_pollution_parquet) == "bogota_metro_dataset"],
      pollutants = summary_pollutants,
      year_filter = analysis_year,
      mem_gb = 8)),

  targets::tar_target(cdmx_station_counts,
    count_stations_reporting(
      arrow_dir = cdmx_pollution_parquet[
        basename(cdmx_pollution_parquet) == "cdmx_metro_dataset"],
      pollutants = summary_pollutants,
      year_filter = analysis_year,
      mem_gb = 8)),

  targets::tar_target(santiago_station_counts,
    count_stations_reporting(
      arrow_dir = santiago_pollution_parquet[
        basename(santiago_pollution_parquet) == "santiago_metro_dataset"],
      pollutants = summary_pollutants,
      year_filter = analysis_year,
      mem_gb = 8)),

  targets::tar_target(sao_paulo_station_counts,
    count_stations_reporting(
      arrow_dir = sao_paulo_pollution_parquet[
        basename(sao_paulo_pollution_parquet) == "sao_paulo_metro_dataset"],
      pollutants = summary_pollutants,
      year_filter = analysis_year,
      mem_gb = 8)),

  targets::tar_target(bogota_who_exceedances,
    compute_who_exceedances(
      arrow_dir = bogota_outliers,
      city_label = "bogota",
      pollutants = summary_pollutants,
      year_filter = NULL)),

  targets::tar_target(cdmx_who_exceedances,
    compute_who_exceedances(
      arrow_dir = cdmx_outliers,
      city_label = "cdmx",
      pollutants = summary_pollutants,
      year_filter = NULL)),

  targets::tar_target(santiago_who_exceedances,
    compute_who_exceedances(
      arrow_dir = santiago_outliers,
      city_label = "santiago",
      pollutants = summary_pollutants,
      year_filter = NULL)),

  targets::tar_target(sao_paulo_who_exceedances,
    compute_who_exceedances(
      arrow_dir = sao_paulo_outliers,
      city_label = "sao_paulo_metro",
      pollutants = summary_pollutants,
      year_filter = NULL)),

  targets::tar_target(bogota_threshold_exceedances,
    compute_threshold_exceedance_days(
      arrow_dir = bogota_outliers,
      city_label = "Bogota",
      year_filter = analysis_year,
      pollutants = summary_pollutants)),

  targets::tar_target(santiago_threshold_exceedances,
    compute_threshold_exceedance_days(
      arrow_dir = santiago_outliers,
      city_label = "Santiago",
      year_filter = analysis_year,
      pollutants = summary_pollutants)),

  targets::tar_target(cdmx_threshold_exceedances,
    compute_threshold_exceedance_days(
      arrow_dir = cdmx_outliers,
      city_label = "Mexico City",
      year_filter = analysis_year,
      pollutants = summary_pollutants)),

  targets::tar_target(sao_paulo_threshold_exceedances,
    compute_threshold_exceedance_days(
      arrow_dir = sao_paulo_outliers,
      city_label = "Sao Paulo",
      year_filter = analysis_year,
      pollutants = summary_pollutants)),

  targets::tar_target(bogota_quintile_availability,
    compute_missing_by_quintile(
      city = "Bogota",
      city_order = 1L,
      pollution_dir = bogota_outliers,
      dist_pq = bogota_2018_distances[
        basename(bogota_2018_distances) == "matrix_geo_station_distances.parquet"],
      census_file = bogota_census[
        basename(bogota_census) == "census_2018_metro_individual.parquet"],
      geo_id_col = "geo_id",
      pollutants = summary_pollutants,
      year = analysis_year,
      report = availability_report)),

  targets::tar_target(cdmx_quintile_availability,
    compute_missing_by_quintile(
      city = "Mexico City",
      city_order = 2L,
      pollution_dir = cdmx_outliers,
      dist_pq = cdmx_2020_distances[
        basename(cdmx_2020_distances) == "matrix_geo_station_distances.parquet"],
      census_file = cdmx_census[
        basename(cdmx_census) == "census_metro_individual_2020.parquet"],
      geo_id_col = "geo_id",
      pollutants = summary_pollutants,
      year = analysis_year,
      report = availability_report)),

  targets::tar_target(santiago_quintile_availability,
    compute_missing_by_quintile(
      city = "Santiago",
      city_order = 3L,
      pollution_dir = santiago_outliers,
      dist_pq = santiago_2017_distances[
        basename(santiago_2017_distances) == "matrix_geo_station_distances.parquet"],
      census_file = santiago_census[
        basename(santiago_census) == "census_individual_2017.parquet"],
      geo_id_col = "geo_id",
      pollutants = summary_pollutants,
      year = analysis_year,
      report = availability_report)),

  targets::tar_target(sao_paulo_quintile_availability,
    compute_missing_by_quintile(
      city = "Sao Paulo",
      city_order = 4L,
      pollution_dir = sao_paulo_outliers,
      dist_pq = sao_paulo_2010_distances[
        basename(sao_paulo_2010_distances) == "matrix_geo_station_distances.parquet"],
      census_file = sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_individual_2010.parquet"],
      geo_id_col = "geo_id",
      pollutants = summary_pollutants,
      year = analysis_year,
      report = availability_report)),

  targets::tar_target(bogota_census_summary,
    compute_city_census_summary(
      census_path = bogota_census[
        basename(bogota_census) == "census_2018_metro_individual.parquet"],
      city = "Bogota",
      city_latex = "Bogot\\'a",
      census_year = 2018L,
      census_level = "Census tract",
      geo_id_col = "geo_id",
      pop_col = "person_weight")),

  targets::tar_target(cdmx_census_summary,
    compute_city_census_summary(
      census_path = cdmx_census[
        basename(cdmx_census) == "census_metro_individual_2020.parquet"],
      city = "Mexico City",
      city_latex = "Mexico City",
      census_year = 2020L,
      census_level = "Municipality",
      geo_id_col = "geo_id",
      pop_col = "person_weight")),

  targets::tar_target(santiago_census_summary,
    compute_city_census_summary(
      census_path = santiago_census[
        basename(santiago_census) == "census_individual_2017.parquet"],
      city = "Gran Santiago",
      city_latex = "Gran Santiago",
      census_year = 2017L,
      census_level = "Census tract",
      geo_id_col = "geo_id",
      pop_col = "person_weight")),

  targets::tar_target(sao_paulo_census_summary,
    compute_city_census_summary(
      census_path = sao_paulo_census[
        basename(sao_paulo_census) == "census_sp_individual_2010.parquet"],
      city = "Sao Paulo",
      city_latex = "S\\~ao Paulo",
      census_year = 2010L,
      census_level = "Weighting area",
      geo_id_col = "geo_id",
      pop_col = "person_weight")),

  targets::tar_target(station_counts_summary,
    {
      counts_santiago <- data.table::copy(santiago_station_counts)
      counts_bogota <- data.table::copy(bogota_station_counts)
      counts_cdmx <- data.table::copy(cdmx_station_counts)
      counts_sp <- data.table::copy(sao_paulo_station_counts)
      counts_santiago[, city := "Santiago"]
      counts_bogota[,   city := "Bogotá"]
      counts_cdmx[,     city := "Mexico City"]
      counts_sp[,       city := "São Paulo"]

      station_counts <- data.table::rbindlist(list(counts_santiago, counts_bogota,
                                                  counts_cdmx, counts_sp))
      station_counts <- station_counts[, .(city, pm10, pm25)]
      station_counts
    }),

  targets::tar_target(who_exceedance_summary,
    data.table::rbindlist(list(
      bogota_who_exceedances,
      cdmx_who_exceedances,
      santiago_who_exceedances,
      sao_paulo_who_exceedances), fill = TRUE)),

  targets::tar_target(threshold_exceedance_summary,
    data.table::rbindlist(list(
      bogota_threshold_exceedances,
      santiago_threshold_exceedances,
      cdmx_threshold_exceedances,
      sao_paulo_threshold_exceedances))),

  targets::tar_target(quintile_availability_summary,
    data.table::rbindlist(list(
      bogota_quintile_availability,
      cdmx_quintile_availability,
      santiago_quintile_availability,
      sao_paulo_quintile_availability))),

  targets::tar_target(census_summary,
    data.table::rbindlist(list(
      bogota_census_summary,
      cdmx_census_summary,
      santiago_census_summary,
      sao_paulo_census_summary))),

  targets::tar_target(bogota_missing_raw_files,
    write_missing_proportions(
      tables = bogota_missing_raw,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "bogota_raw"),
    format = "file"),

  targets::tar_target(cdmx_missing_raw_files,
    write_missing_proportions(
      tables = cdmx_missing_raw,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "cdmx_raw"),
    format = "file"),

  targets::tar_target(santiago_missing_raw_files,
    write_missing_proportions(
      tables = santiago_missing_raw,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "santiago_raw"),
    format = "file"),

  targets::tar_target(sao_paulo_missing_raw_files,
    write_missing_proportions(
      tables = sao_paulo_missing_raw,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "sao_paulo_metro_raw"),
    format = "file"),

  targets::tar_target(bogota_missing_clean_files,
    write_missing_proportions(
      tables = bogota_missing_clean,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "bogota_clean"),
    format = "file"),

  targets::tar_target(cdmx_missing_clean_files,
    write_missing_proportions(
      tables = cdmx_missing_clean,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "cdmx_clean"),
    format = "file"),

  targets::tar_target(santiago_missing_clean_files,
    write_missing_proportions(
      tables = santiago_missing_clean,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "santiago_clean"),
    format = "file"),

  targets::tar_target(sao_paulo_missing_clean_files,
    write_missing_proportions(
      tables = sao_paulo_missing_clean,
      out_dir = here::here("data", "processed", "missing_proportions"),
      out_name = "sao_paulo_metro_clean"),
    format = "file"),

  targets::tar_target(station_counts_files,
    save_table_parquet_csv(
      dt = station_counts_summary,
      out_dir = here::here("data", "processed", "station_counts"),
      name = paste0("stations_by_pollutant_", analysis_year)),
    format = "file"),

  targets::tar_target(who_exceedance_file,
    save_raw_data_tidy_formatted(
      data = who_exceedance_summary,
      out_dir = here::here("data", "processed", "who_exceedances"),
      out_name = "who_exceedances_all_cities",
      write_rds = FALSE,
      write_parquet = TRUE,
      write_csv_gz = FALSE)$parquet,
    format = "file"),

  targets::tar_target(threshold_exceedance_files,
    save_table_parquet_csv(
      dt = threshold_exceedance_summary,
      out_dir = here::here("data", "processed", "threshold_exceedances"),
      name = paste0("days_and_hours_", analysis_year)),
    format = "file"),

  targets::tar_target(quintile_availability_files,
    save_table_parquet_csv(
      dt = quintile_availability_summary,
      out_dir = here::here("data", "processed", "missing_proportions"),
      name = paste0("missing_by_education_quintile_", analysis_year)),
    format = "file"),

  targets::tar_target(census_summary_files,
    save_table_parquet_csv(
      dt = census_summary,
      out_dir = here::here("data", "processed", "census_summary"),
      name = "census_summary"),
    format = "file"),

  targets::tar_target(missing_dimension_files,
    c(
      bogota_missing_raw_files,
      cdmx_missing_raw_files,
      santiago_missing_raw_files,
      sao_paulo_missing_raw_files,
      bogota_missing_clean_files,
      cdmx_missing_clean_files,
      santiago_missing_clean_files,
      sao_paulo_missing_clean_files), format = "file"),

  targets::tar_target(compute_descriptive_tables,
    c(missing_dimension_files, station_counts_files, who_exceedance_file,
      threshold_exceedance_files, quintile_availability_files, census_summary_files),
    format = "file"),

  targets::tar_target(bogota_distance_band_area,
    compute_geo_area_km2(geo_sf = bogota_2018_distance_geography,
                         geo_id_col = "GEO_ID")),

  targets::tar_target(bogota_distance_bands,
    compute_distance_band_summary(
      dist_pq = bogota_2018_distances[basename(bogota_2018_distances) ==
        "matrix_geo_station_distances.parquet"],
      census_path = bogota_census[basename(bogota_census) ==
        "census_2018_metro_individual.parquet"],
      area_dt = bogota_distance_band_area,
      city = "Bogota",
      unit_label = "census tracts",
      share_vars = band_shares_bogota,
      mean_vars = band_means_bogota,
      radii_km = distance_band_radii_km)),

  targets::tar_target(cdmx_distance_band_area,
    compute_geo_area_km2(geo_sf = cdmx_2020_distance_geography,
                         geo_id_col = "CVE_MUN")),

  targets::tar_target(cdmx_distance_bands,
    compute_distance_band_summary(
      dist_pq = cdmx_2020_distances[basename(cdmx_2020_distances) ==
        "matrix_geo_station_distances.parquet"],
      census_path = cdmx_census[basename(cdmx_census) ==
        "census_metro_individual_2020.parquet"],
      area_dt = cdmx_distance_band_area,
      city = "Mexico City",
      unit_label = "municipalities",
      share_vars = band_shares_cdmx,
      mean_vars = band_means_cdmx,
      radii_km = distance_band_radii_km)),

  targets::tar_target(santiago_distance_band_area,
    compute_geo_area_km2(geo_sf = santiago_2017_distance_geography,
                         geo_id_col = "zona_id")),

  targets::tar_target(santiago_distance_bands,
    compute_distance_band_summary(
      dist_pq = santiago_2017_distances[basename(santiago_2017_distances) ==
        "matrix_geo_station_distances.parquet"],
      census_path = santiago_census[basename(santiago_census) ==
        "census_individual_2017.parquet"],
      area_dt = santiago_distance_band_area,
      city = "Santiago",
      unit_label = "census tracts",
      share_vars = band_shares_santiago,
      mean_vars = band_means_santiago,
      radii_km = distance_band_radii_km)),

  targets::tar_target(sao_paulo_distance_band_area,
    compute_geo_area_km2(geo_sf = sao_paulo_2010_distance_geography,
                         geo_id_col = "code_weighting")),

  targets::tar_target(sao_paulo_distance_bands,
    compute_distance_band_summary(
      dist_pq = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
        "matrix_geo_station_distances.parquet"],
      census_path = sao_paulo_census[basename(sao_paulo_census) ==
        "census_sp_individual_2010.parquet"],
      area_dt = sao_paulo_distance_band_area,
      city = "Sao Paulo",
      unit_label = "weighting areas",
      share_vars = band_shares_sp,
      mean_vars = band_means_sp,
      radii_km = distance_band_radii_km)),

  targets::tar_target(distance_band_summary,
    data.table::rbindlist(list(bogota_distance_bands, cdmx_distance_bands,
      santiago_distance_bands, sao_paulo_distance_bands), fill = TRUE)),

  targets::tar_target(compute_distance_band_descriptives,
    save_table_parquet_csv(dt = distance_band_summary,
      out_dir = here::here("data", "processed", "distance_band_descriptives"),
      name = "distance_band_descriptives"), format = "file")
)
