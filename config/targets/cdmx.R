# Cdmx target declarations. Scientific functions live in src/.
list(
  targets::tar_target(cdmx_config,
    manuscript_city_config(cdmx_cfg)),

  targets::tar_target(cdmx_geography_inputs,
    c(
      here::here(cdmx_config$dl_dir, "metro_area", "mg_2024_integrado.zip")),
    format = "file"),

  targets::tar_target(cdmx_municipalities_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- cdmx_geography_inputs
      cdmx_prepare_metro_area(
        source_zip = sources[basename(sources) == "mg_2024_integrado.zip"],
        level = "municipality", keep_municipality = cdmx_config$cities_in_metro)
    }),

  targets::tar_target(cdmx_metro_area_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- cdmx_geography_inputs
      cdmx_prepare_metro_area(
        source_zip = sources[basename(sources) == "mg_2024_integrado.zip"],
        level = "ageb", keep_municipality = cdmx_config$cities_in_metro)
    }),

  targets::tar_target(cdmx_geography,
    c(
      write_geopackage(cdmx_municipalities_sf,
        here::here(cdmx_config$out_dir, "geospatial_data", "cdmx",
        "cdmx_area_metro_municipalities_2024.gpkg")),
      write_geopackage(cdmx_metro_area_sf,
        here::here(cdmx_config$out_dir, "geospatial_data", "cdmx",
        "cdmx_area_metro_2024.gpkg"))),
    format = "file"),

  targets::tar_target(cdmx_stations_filter_inputs,
    c(
      here::here(cdmx_config$dl_dir, "ground_stations_geolocation",
        "all_station_location.csv")),
    format = "file"),

  targets::tar_target(cdmx_station_locations,
    {
      files <- cdmx_stations_filter_inputs
      read.csv(files[basename(files) == "all_station_location.csv"])
    }),

  targets::tar_target(cdmx_stations_sf,
    {
      sf::sf_use_s2(TRUE)
      locations <- cdmx_correct_station_names(cdmx_station_locations,
        cdmx_config$station_nme_map)
      cdmx_filter_stations_in_metro(
        station_location = locations,
        metro_area = cdmx_metro_area_sf,
        dissolve = TRUE,
        radius_km = cdmx_config$station_buffer_km,
        out_file = NULL)
    }),

  targets::tar_target(cdmx_stations_filter,
    c(
      write_geopackage(cdmx_stations_sf,
        here::here(cdmx_config$out_dir, "geospatial_data", "cdmx",
        "cdmx_stations_buffer_metro.gpkg"))),
    format = "file"),

  targets::tar_target(cdmx_pollution_parquet_inputs,
    c(
      here::here(cdmx_config$dl_dir, "ground_stations"),
      here::here(cdmx_config$dl_dir, "ground_stations_raw_missing_data")),
    format = "file"),

  targets::tar_target(cdmx_pollution_parquet,
    {
      sources <- cdmx_pollution_parquet_inputs
      primary <- sources[basename(sources) == "ground_stations"]
      secondary <- sources[basename(sources) == "ground_stations_raw_missing_data"]
      files <- cdmx_stations_filter
      stations <- sf::st_read(files[basename(files) ==
        "cdmx_stations_buffer_metro.gpkg"], quiet = TRUE)
      destination <- here::here(cdmx_config$out_dir, "monitoring_stations")
      pollution <- cdmx_merge_pollution_data(primary_data_dir = primary,
          secondary_data_dir = secondary,
          stations_sf = stations,
          tz = cdmx_config$processing_tz,
          years = cdmx_config$years,
          cleanup = FALSE,
          include_source_metadata = TRUE,
          out_dir = destination,
          out_name = "cdmx_metro")
      city_pollution_outputs("cdmx", cdmx_config)
    },
    format = "file"),

  targets::tar_target(cdmx_census_inputs,
    c(
      here::here(cdmx_config$dl_dir, "census")),
    format = "file"),

  targets::tar_target(cdmx_census,
    {
      dir_census <- cdmx_census_inputs
      preflight_census_inputs("cdmx", c(census_2020 = dir_census))
      out_extracted <- here::here(cdmx_config$out_dir, "census_extracted", "cdmx",
        "CPV2020_EXTENDED")
      out_census    <- here::here(cdmx_config$out_dir, "census", "cdmx_extended_2020")
      extracted <- mexico_filter_census(census_dir = dir_census,
          out_dir = out_extracted,
          overwrite = TRUE)
      archives <- list.files(dir_census, pattern = "Censo2020_CA_.*_csv\\.zip$",
                             ignore.case = TRUE)
      if (nrow(extracted) != length(archives) || !all(file.exists(extracted$file))) {
        stop("Incomplete extended census extraction")
      }
      census <- mexico_harmonize_census_data(extract_index = extracted,
          metro_codes = cdmx_config$cities_in_metro,
          out_dir = out_census,
          return_data = FALSE)
      census_files <- c(out_extracted, here::here(out_census,
        c("census_metro_individual_2020.parquet", "collapse_metro_area_2020.parquet")))
      processing_files(census_files)
    },
    format = "file")
)
