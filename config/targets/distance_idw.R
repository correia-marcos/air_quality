# Distance idw target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_distance_stations,
    sf::st_read(
      bogota_stations_filter[basename(bogota_stations_filter) ==
        "bogota_2018_stations_buffer_metro.gpkg"],
      quiet = TRUE)),

  targets::tar_target(bogota_2018_distance_geography,
    sf::st_read(
      bogota_geography[basename(bogota_geography) ==
        "bogota_area_metro_census_tracts_2018.gpkg"],
      quiet = TRUE)),

  targets::tar_target(bogota_2018_distance_matrices,
    {
      sf::sf_use_s2(TRUE)
      compute_distance_matrices(
        stations_sf = bogota_distance_stations,
        station_id_col = "station_name",
        geo_sf = bogota_2018_distance_geography,
        geo_id_col = "GEO_ID",
        distance_metric = distance_metric,
        representative_point = distance_representative_point)
    }),

  targets::tar_target(bogota_2018_distances,
    write_distance_matrices(
      result = bogota_2018_distance_matrices,
      out_dir = here::here("data", "processed", "distances_matrices", "bogota_2018")),
    format = "file"),

  targets::tar_target(bogota_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = bogota_pollution_parquet[basename(bogota_pollution_parquet) ==
            "bogota_metro_dataset"],
        station_dist_path = bogota_2018_distances[basename(bogota_2018_distances) ==
            "matrix_station_distances.parquet"],
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
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

  targets::tar_target(cdmx_distance_stations,
    sf::st_read(
      cdmx_stations_filter[basename(cdmx_stations_filter) ==
        "cdmx_stations_buffer_metro.gpkg"],
      quiet = TRUE)),

  targets::tar_target(cdmx_2020_distance_geography,
    sf::st_read(
      cdmx_geography[basename(cdmx_geography) ==
        "cdmx_area_metro_municipalities_2024.gpkg"],
      quiet = TRUE)),

  targets::tar_target(cdmx_2020_distance_matrices,
    {
      sf::sf_use_s2(TRUE)
      compute_distance_matrices(
        stations_sf = cdmx_distance_stations,
        station_id_col = "station",
        geo_sf = cdmx_2020_distance_geography,
        geo_id_col = "CVE_MUN",
        distance_metric = distance_metric,
        representative_point = distance_representative_point)
    }),

  targets::tar_target(cdmx_2020_distances,
    write_distance_matrices(
      result = cdmx_2020_distance_matrices,
      out_dir = here::here("data", "processed", "distances_matrices", "cdmx_2020")),
    format = "file"),

  targets::tar_target(cdmx_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = cdmx_pollution_parquet[basename(cdmx_pollution_parquet) ==
            "cdmx_metro_dataset"],
        station_dist_path = cdmx_2020_distances[basename(cdmx_2020_distances) ==
            "matrix_station_distances.parquet"],
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
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

  targets::tar_target(santiago_distance_stations,
    sf::st_read(
      santiago_stations_filter[basename(santiago_stations_filter) ==
        "gran_santiago_stations_buffer_metro_2017.gpkg"],
      quiet = TRUE)),

  targets::tar_target(santiago_2017_distance_geography,
    sf::st_read(
      santiago_geography[basename(santiago_geography) ==
        "gran_santiago_zonas_2017.gpkg"],
      quiet = TRUE)),

  targets::tar_target(santiago_2017_distance_matrices,
    {
      sf::sf_use_s2(TRUE)
      compute_distance_matrices(
        stations_sf = santiago_distance_stations,
        station_id_col = "station_name",
        geo_sf = santiago_2017_distance_geography,
        geo_id_col = "zona_id",
        distance_metric = distance_metric,
        representative_point = distance_representative_point)
    }),

  targets::tar_target(santiago_2017_distances,
    write_distance_matrices(
      result = santiago_2017_distance_matrices,
      out_dir = here::here("data", "processed", "distances_matrices", "santiago_2017")),
    format = "file"),

  targets::tar_target(santiago_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = santiago_pollution_parquet[basename(santiago_pollution_parquet) ==
            "santiago_metro_dataset"],
        station_dist_path = santiago_2017_distances[basename(santiago_2017_distances) ==
            "matrix_station_distances.parquet"],
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
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

  targets::tar_target(santiago_2024_distance_geography,
    sf::st_read(
      santiago_geography[basename(santiago_geography) ==
        "gran_santiago_area_2024.gpkg"],
      quiet = TRUE)),

  targets::tar_target(santiago_2024_distance_matrices,
    {
      sf::sf_use_s2(TRUE)
      compute_distance_matrices(
        stations_sf = santiago_distance_stations,
        station_id_col = "station_name",
        geo_sf = santiago_2024_distance_geography,
        geo_id_col = "CUT",
        distance_metric = distance_metric,
        representative_point = distance_representative_point)
    }),

  targets::tar_target(santiago_2024_distances,
    write_distance_matrices(
      result = santiago_2024_distance_matrices,
      out_dir = here::here("data", "processed", "distances_matrices", "santiago_2024")),
    format = "file"),

  targets::tar_target(santiago_2024_idw,
    {
      micro <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_santiago_individual_2024.parquet"])
      geo <- arrow::read_parquet(santiago_census[
        basename(santiago_census) == "census_santiago_collapsed_2024.parquet"])
      matrix <- santiago_2024_distances[basename(santiago_2024_distances) ==
        "matrix_geo_station_distances.parquet"]
      panel <- here::here(santiago_outliers, paste0("year=", analysis_year))
      dir_idw <- here::here("data", "processed", "idw_estimates")
      files <- character()
      for (buffer_km in idw_buffers_km) {
        education <- run_idw_city(city_label = "Santiago (comuna 2024)",
            city_id = "santiago_2024",
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

  targets::tar_target(sao_paulo_distance_stations,
    sf::st_read(
      sao_paulo_stations_filter[basename(sao_paulo_stations_filter) ==
        "sao_paulo_stations_buffer_metro_2010.gpkg"],
      quiet = TRUE)),

  targets::tar_target(sao_paulo_2010_distance_geography,
    sf::st_read(
      sao_paulo_geography[basename(sao_paulo_geography) ==
        "sao_paulo_metro_2010_weighting_areas.gpkg"],
      quiet = TRUE)),

  targets::tar_target(sao_paulo_2010_distance_matrices,
    {
      sf::sf_use_s2(TRUE)
      compute_distance_matrices(
        stations_sf = sao_paulo_distance_stations,
        station_id_col = "station_name",
        geo_sf = sao_paulo_2010_distance_geography,
        geo_id_col = "code_weighting",
        distance_metric = distance_metric,
        representative_point = distance_representative_point)
    }),

  targets::tar_target(sao_paulo_2010_distances,
    write_distance_matrices(
      result = sao_paulo_2010_distance_matrices,
      out_dir = here::here("data", "processed", "distances_matrices", "sao_paulo_2010")),
    format = "file"),

  targets::tar_target(sao_paulo_outliers,
    detect_pollution_outliers(
        upper_bounds = pollution_upper_bounds,
        arrow_dir = sao_paulo_pollution_parquet[basename(sao_paulo_pollution_parquet) ==
            "sao_paulo_metro_dataset"],
        station_dist_path = sao_paulo_2010_distances[basename(sao_paulo_2010_distances) ==
            "matrix_station_distances.parquet"],
        on_missing_temporal = outlier_missing_temporal,
        on_missing_neighbor = outlier_missing_neighbor,
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
    format = "file")
)
