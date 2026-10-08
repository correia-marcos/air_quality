# Santiago target declarations. Scientific functions live in src/.
list(
  targets::tar_target(santiago_config,
    manuscript_city_config(santiago_cfg)),

  targets::tar_target(santiago_geography_inputs,
    c(
      here::here(santiago_config$dl_dir, "metro_area", "Cartografia_censo2024_Pais.zip"),
      here::here(santiago_config$dl_dir, "metro_area", "2017",
        "GRAN_SANTIAGO_13_metro.geojson"),
      here::here(santiago_config$dl_dir, "metro_area", "2017",
        "GRAN_SANTIAGO_13_zonas.geojson"),
      here::here(santiago_config$dl_dir, "metro_area", "2017",
        "GRAN_SANTIAGO_13_count.json")),
    format = "file"),

  targets::tar_target(santiago_zones_2017_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- santiago_geography_inputs
      santiago_prepare_metro_area_2017(
        metro_file = sources[basename(sources) == "GRAN_SANTIAGO_13_metro.geojson"],
        zones_file = sources[basename(sources) == "GRAN_SANTIAGO_13_zonas.geojson"],
        count_file = sources[basename(sources) == "GRAN_SANTIAGO_13_count.json"])
    }),

  targets::tar_target(santiago_metro_2024_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- santiago_geography_inputs
      santiago_prepare_metro_area_2024(
        source_zip = sources[basename(sources) == "Cartografia_censo2024_Pais.zip"],
        type = "gran_santiago", level = "mpio",
        keep_municipality = santiago_config$cities_in_metro, dissolve_by = "CUT")
    }),

  targets::tar_target(santiago_geography,
    c(
      write_geopackage(santiago_zones_2017_sf,
        here::here(santiago_config$out_dir, "geospatial_data", "santiago",
        "gran_santiago_zonas_2017.gpkg")),
      write_geopackage(santiago_metro_2024_sf,
        here::here(santiago_config$out_dir, "geospatial_data", "santiago",
        "gran_santiago_area_2024.gpkg"))),
    format = "file"),

  targets::tar_target(santiago_stations_filter_inputs,
    c(
      here::here(santiago_config$dl_dir, "stations_metadata",
        "SINCA_metadata_stations_20260113_1616.csv")),
    format = "file"),

  targets::tar_target(santiago_station_locations,
    {
      files <- santiago_stations_filter_inputs
      read.csv(files[basename(files) == "SINCA_metadata_stations_20260113_1616.csv"])
    }),

  targets::tar_target(santiago_stations_sf,
    {
      sf::sf_use_s2(TRUE)
      santiago_filter_stations_in_metro(
        stations_df = santiago_station_locations,
        metro_area = santiago_zones_2017_sf,
        radius_km = santiago_config$station_buffer_km,
        out_file = NULL)
    }),

  targets::tar_target(santiago_stations_filter,
    c(
      write_geopackage(santiago_stations_sf,
        here::here(santiago_config$out_dir, "geospatial_data", "santiago",
        "gran_santiago_stations_buffer_metro_2017.gpkg"))),
    format = "file"),

  targets::tar_target(santiago_pollution_parquet_inputs,
    c(
      here::here(santiago_config$dl_dir, "ground_stations")),
    format = "file"),

  targets::tar_target(santiago_pollution_parquet,
    {
      sources <- santiago_pollution_parquet_inputs
      primary <- sources[basename(sources) == "ground_stations"]
      files <- santiago_stations_filter
      stations <- sf::st_read(files[basename(files) ==
        "gran_santiago_stations_buffer_metro_2017.gpkg"], quiet = TRUE)
      destination <- here::here(santiago_config$out_dir, "monitoring_stations")
      pollution <- santiago_process_stations_data_to_parquet(data_folder = primary,
          stations_sf = stations,
          tz = santiago_config$processing_tz,
          years = santiago_config$years,
          out_dir = destination,
          out_name = "santiago_metro")
      city_pollution_outputs("santiago", santiago_config)
    },
    format = "file"),

  targets::tar_target(santiago_census_inputs,
    c(
      here::here(santiago_config$dl_dir, "census", "2017", "censo2017.duckdb"),
      here::here(santiago_config$dl_dir, "census", "2024")),
    format = "file"),

  targets::tar_target(santiago_census,
    {
      files <- santiago_census_inputs
      file_census_2017 <- files[basename(files) == "censo2017.duckdb"]
      dir_census_2024 <- files[basename(files) == "2024"]
      preflight_census_inputs("santiago", c(census_2017 = file_census_2017,
        census_2024 = dir_census_2024))
      geography <- santiago_geography
      zones_2017 <- sf::st_read(geography[basename(geography) ==
        "gran_santiago_zonas_2017.gpkg"], quiet = TRUE)
      metro_2024 <- sf::st_read(geography[basename(geography) ==
        "gran_santiago_area_2024.gpkg"], quiet = TRUE)
      out_census_2017 <- here::here(santiago_config$out_dir, "census", "santiago_2017")
      out_census_2024 <- here::here(santiago_config$out_dir, "census", "santiago_2024")
      dir_work_2017   <- here::here(santiago_config$out_dir, "census_extracted",
        "santiago", "2017")
      census_2017 <- santiago_process_census_2017(sf_data = zones_2017,
          match_col = "zona_id",
          source_db = file_census_2017,
          work_dir = dir_work_2017,
          out_dir = out_census_2017,
          return_data = FALSE)

      census_2024 <- santiago_process_census_2024(census_dir = dir_census_2024,
          sf_data = metro_2024,
          match_col = "CUT",
          out_dir = out_census_2024,
          overwrite = TRUE,
          return_data = FALSE)
      census_members <- utils::unzip(here::here(dir_census_2024,
                                              "chile_census_2024_people.zip"),
        list = TRUE)$Name
      census_csv <- grep("\\.csv$", census_members, value = TRUE, ignore.case = TRUE)[1]
      census_files <- c(here::here(dir_work_2017, "censo2017.duckdb"),
        here::here(out_census_2024, basename(census_csv)),
        here::here(out_census_2017,
          c("census_individual_2017.parquet", "census_collapsed_2017.parquet")),
        here::here(out_census_2024, c("census_santiago_individual_2024.parquet",
                                    "census_santiago_collapsed_2024.parquet")))
      processing_files(census_files)
    },
    format = "file")
)
