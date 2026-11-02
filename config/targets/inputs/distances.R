# Distance target declarations. Scientific functions live in src/.
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

  targets::tar_target(distances,
    c(bogota_2018_distances, cdmx_2020_distances, santiago_2017_distances,
      sao_paulo_2010_distances),
    format = "file")
)
