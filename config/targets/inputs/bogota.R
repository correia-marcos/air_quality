# Bogota target declarations. Scientific functions live in src/.
list(
  targets::tar_target(bogota_config,
    manuscript_city_config(bogota_cfg)),

  targets::tar_target(bogota_geography_inputs,
    c(
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2005_COLOMBIA.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2018_INTGRD_MPIO.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2018_INTGRD_MANZ.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2018_INTGRD_SECCR.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "bogota_loca.gpkg")),
    format = "file"),

  targets::tar_target(bogota_metro_2005_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- bogota_geography_inputs
      archives <- sources[basename(sources) == "SHP_MGN2005_COLOMBIA.zip"]
      bogota_prepare_metro_area(source_zips = archives,
        level = "mpio_localidad",
        mgn_year = 2005,
        municipality_codes = bogota_config$city_code_metro,
        localities_file = sources[basename(sources) == "bogota_loca.gpkg"])
    }),

  targets::tar_target(bogota_municipalities_2005_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- bogota_geography_inputs
      archives <- sources[basename(sources) == "SHP_MGN2005_COLOMBIA.zip"]
      bogota_prepare_metro_area(source_zips = archives,
        level = "mpio",
        mgn_year = 2005,
        municipality_codes = bogota_config$city_code_metro)
    }),

  targets::tar_target(bogota_tracts_2005_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- bogota_geography_inputs
      archives <- sources[basename(sources) == "SHP_MGN2005_COLOMBIA.zip"]
      bogota_prepare_metro_area(source_zips = archives,
        level = "manzana",
        mgn_year = 2005,
        municipality_codes = bogota_config$city_code_metro)
    }),

  targets::tar_target(bogota_metro_2018_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- bogota_geography_inputs
      archives <- sources[basename(sources) == "SHP_MGN2018_INTGRD_MPIO.zip"]
      bogota_prepare_metro_area(source_zips = archives,
        level = "mpio_localidad",
        mgn_year = 2018,
        municipality_codes = bogota_config$city_code_metro,
        localities_file = sources[basename(sources) == "bogota_loca.gpkg"])
    }),

  targets::tar_target(bogota_tracts_2018_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- bogota_geography_inputs
      archives <- sources[basename(sources) %in%
        c("SHP_MGN2018_INTGRD_MANZ.zip", "SHP_MGN2018_INTGRD_SECCR.zip")]
      bogota_prepare_metro_area(source_zips = archives,
        level = "manzana",
        mgn_year = 2018,
        municipality_codes = bogota_config$city_code_metro)
    }),

  targets::tar_target(bogota_geography,
    c(
      write_geopackage(bogota_metro_2005_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_area_metro_2005.gpkg")),
      write_geopackage(bogota_municipalities_2005_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_area_metro_municipalities_2005.gpkg")),
      write_geopackage(bogota_tracts_2005_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_area_metro_census_tracts_2005.gpkg")),
      write_geopackage(bogota_metro_2018_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_area_metro_2018.gpkg")),
      write_geopackage(bogota_tracts_2018_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_area_metro_census_tracts_2018.gpkg"))),
    format = "file"),

  targets::tar_target(bogota_stations_filter_inputs,
    c(
      here::here(bogota_config$dl_dir, "ground_stations_geolocation",
        "bogota_stations_location.csv"),
      here::here(bogota_config$dl_dir, "stations_metadata")),
    format = "file"),

  targets::tar_target(bogota_station_locations,
    {
      files <- bogota_stations_filter_inputs
      read.csv(files[basename(files) == "bogota_stations_location.csv"])
    }),

  targets::tar_target(bogota_stations_2018_sf,
    {
      sf::sf_use_s2(TRUE)
      files <- bogota_stations_filter_inputs
      metadata <- files[basename(files) == "stations_metadata"]
      bogota_filter_stations_in_metro(
        rmcab_df = bogota_station_locations,
        metadata_dir = metadata,
        metro_area = bogota_metro_2018_sf,
        radius_km = bogota_config$station_buffer_km,
        out_file = NULL)
    }),

  targets::tar_target(bogota_stations_2005_sf,
    {
      sf::sf_use_s2(TRUE)
      files <- bogota_stations_filter_inputs
      metadata <- files[basename(files) == "stations_metadata"]
      bogota_filter_stations_in_metro(
        rmcab_df = bogota_station_locations,
        metadata_dir = metadata,
        metro_area = bogota_metro_2005_sf,
        radius_km = bogota_config$station_buffer_km,
        out_file = NULL)
    }),

  targets::tar_target(bogota_stations_filter,
    c(
      write_geopackage(bogota_stations_2018_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_2018_stations_buffer_metro.gpkg")),
      write_geopackage(bogota_stations_2005_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_2005_stations_buffer_metro.gpkg"))),
    format = "file"),

  targets::tar_target(bogota_pollution_parquet_inputs,
    c(
      here::here(bogota_config$dl_dir, "ground_stations"),
      here::here(bogota_config$dl_dir, "metro_ground_stations_hourly")),
    format = "file"),

  targets::tar_target(bogota_pollution_parquet,
    {
      sources <- bogota_pollution_parquet_inputs
      primary <- sources[basename(sources) == "ground_stations"]
      secondary <- sources[basename(sources) == "metro_ground_stations_hourly"]
      files <- bogota_stations_filter
      stations <- sf::st_read(files[basename(files) ==
        "bogota_2018_stations_buffer_metro.gpkg"], quiet = TRUE)
      destination <- here::here(bogota_config$out_dir, "monitoring_stations")
      pollution <- bogota_process_stations_data_to_parquet(rmcab_folder = primary,
          sisaire_folder = secondary,
          stations_sf = stations,
          tz = bogota_config$processing_tz,
          years = bogota_config$years,
          out_dir = destination,
          out_name = "bogota_metro")
      city_pollution_outputs("bogota", bogota_config)
    },
    format = "file"),

  targets::tar_target(bogota_census_inputs,
    c(
      here::here(bogota_config$dl_dir, "census", "CG2005_AMPLIADO.zip"),
      here::here(bogota_config$dl_dir, "census", "CG2005_BASICO.zip"),
      here::here(bogota_config$dl_dir, "census")),
    format = "file"),

  targets::tar_target(bogota_census,
    {
      files <- bogota_census_inputs
      file_census_extended <- files[basename(files) == "CG2005_AMPLIADO.zip"]
      file_census_basic <- files[basename(files) == "CG2005_BASICO.zip"]
      dir_census <- files[basename(files) == "census"]
      preflight_census_inputs("bogota", c(extended = file_census_extended,
        basic = file_census_basic, census_2018 = dir_census))
      out_extract_extended <- here::here(bogota_config$out_dir, "census_extracted",
        "bogota",
                                        "CG2005_EXTENDED")
      out_census_extended  <- here::here(bogota_config$out_dir, "census",
        "bogota_extended_2005")
      extracted_extended <- bogota_filter_census_2005(census_zip = file_census_extended,
          out_dir = out_extract_extended,
          overwrite = TRUE)
      census_extended <-
        bogota_harmonize_census_2005_data(extract_list = extracted_extended,
          is_extended = TRUE,
          metro_codes = bogota_config$city_code_metro,
          out_dir = out_census_extended,
          return_data = FALSE)

      out_extract_basic <- here::here(bogota_config$out_dir, "census_extracted", "bogota",
                                        "CG2005_BASIC")
      out_census_basic  <- here::here(bogota_config$out_dir, "census",
        "bogota_basic_2005")
      extracted_basic <- bogota_filter_census_2005(census_zip = file_census_basic,
          out_dir = out_extract_basic,
          overwrite = TRUE)
      census_basic <- bogota_harmonize_census_2005_data(extract_list = extracted_basic,
          is_extended = FALSE,
          metro_codes = bogota_config$city_code_metro,
          out_dir = out_census_basic,
          return_data = FALSE)

      out_extract_2018 <- here::here(bogota_config$out_dir, "census_extracted",
        "bogota", "CNPV_2018")
      out_census_2018  <- here::here(bogota_config$out_dir, "census", "bogota_2018")
      extracted_2018 <- bogota_filter_census_2018(census_folder = dir_census,
          out_dir = out_extract_2018,
          overwrite = TRUE)
      census_2018 <- bogota_harmonize_census_2018_data(extract_paths = extracted_2018,
          metro_codes = bogota_config$city_code_metro,
          out_dir = out_census_2018,
          return_data = FALSE)
      census_files <- c(out_extract_extended, out_extract_basic, out_extract_2018,
        here::here(out_census_extended, c("census_metro_individual_extended.parquet",
                                         "collapse_metro_area_extended.parquet")),
        here::here(out_census_basic, c("census_metro_individual_basic.parquet",
                                      "collapse_metro_area_basic.parquet")),
        here::here(out_census_2018, c("census_2018_metro_individual.parquet",
                                     "census_2018_metro_collapsed.parquet")))
      processing_files(census_files)
    },
    format = "file")
)
