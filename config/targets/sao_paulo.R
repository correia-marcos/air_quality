# Sao paulo target declarations. Scientific functions live in src/.
list(
  targets::tar_target(sao_paulo_config,
    manuscript_city_config(sao_paulo_cfg)),

  targets::tar_target(sao_paulo_geography_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "metro_area", "sp_municipios.zip"),
      here::here(sao_paulo_config$dl_dir, "metro_area", "sp_setores_censitarios.zip"),
      here::here(sao_paulo_config$dl_dir, "metro_area", "sp_weighting_areas_2010.rds")),
    format = "file"),

  targets::tar_target(sao_paulo_municipalities_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- sao_paulo_geography_inputs
      sao_paulo_prepare_metro_area(
        source_zip = sources[basename(sources) == "sp_municipios.zip"],
        level = "mpio", keep_municipality = sao_paulo_config$cities_in_metro)
    }),

  targets::tar_target(sao_paulo_tracts_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- sao_paulo_geography_inputs
      sao_paulo_prepare_metro_area(
        source_zip = sources[basename(sources) == "sp_setores_censitarios.zip"],
        level = "setor_censitario", keep_municipality = sao_paulo_config$cities_in_metro)
    }),

  targets::tar_target(sao_paulo_weighting_areas_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- sao_paulo_geography_inputs
      sao_paulo_prepare_weighting_areas(
        source_file = sources[basename(sources) == "sp_weighting_areas_2010.rds"],
        keep_municipality = sao_paulo_config$cities_in_metro)
    }),

  targets::tar_target(sao_paulo_geography,
    c(
      write_geopackage(sao_paulo_municipalities_sf,
        here::here(sao_paulo_config$out_dir, "geospatial_data", "sao_paulo",
        "sao_paulo_metro_2010.gpkg")),
      write_geopackage(sao_paulo_tracts_sf,
        here::here(sao_paulo_config$out_dir, "geospatial_data", "sao_paulo",
        "sao_paulo_metro_2010_census_tracts.gpkg")),
      write_geopackage(sao_paulo_weighting_areas_sf,
        here::here(sao_paulo_config$out_dir, "geospatial_data", "sao_paulo",
        "sao_paulo_metro_2010_weighting_areas.gpkg"))),
    format = "file"),

  targets::tar_target(sao_paulo_stations_filter_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "stations_metadata", "stations_metadata.csv")),
    format = "file"),

  targets::tar_target(sao_paulo_station_locations,
    {
      files <- sao_paulo_stations_filter_inputs
      read.csv(files[basename(files) == "stations_metadata.csv"])
    }),

  targets::tar_target(sao_paulo_stations_sf,
    {
      sf::sf_use_s2(TRUE)
      sp_filter_stations_in_metro(
        stations_sp = sao_paulo_station_locations,
        metro_area = sao_paulo_municipalities_sf,
        radius_km = sao_paulo_config$station_buffer_km,
        out_file = NULL)
    }),

  targets::tar_target(sao_paulo_stations_filter,
    c(
      write_geopackage(sao_paulo_stations_sf,
        here::here(sao_paulo_config$out_dir, "geospatial_data", "sao_paulo",
        "sao_paulo_stations_buffer_metro_2010.gpkg"))),
    format = "file"),

  targets::tar_target(sao_paulo_pollution_parquet_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "ground_stations")),
    format = "file"),

  targets::tar_target(sao_paulo_pollution_parquet,
    {
      sources <- sao_paulo_pollution_parquet_inputs
      primary <- sources[basename(sources) == "ground_stations"]
      files <- sao_paulo_stations_filter
      stations <- sf::st_read(files[basename(files) ==
        "sao_paulo_stations_buffer_metro_2010.gpkg"], quiet = TRUE)
      destination <- here::here(sao_paulo_config$out_dir, "monitoring_stations")
      pollution <- sp_process_stations_data_to_parquet(data_folder = primary,
          stations_sf = stations,
          tz = sao_paulo_config$processing_tz,
          years = sao_paulo_config$years,
          out_dir = destination,
          out_name = "sao_paulo_metro")
      city_pollution_outputs("sao_paulo", sao_paulo_config)
    },
    format = "file"),

  targets::tar_target(sao_paulo_census_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "census", "2010_population.parquet")),
    format = "file"),

  targets::tar_target(sao_paulo_census,
    {
      file_census <- sao_paulo_census_inputs
      geography <- sao_paulo_geography
      weighting_areas <- sf::st_read(geography[basename(geography) ==
        "sao_paulo_metro_2010_weighting_areas.gpkg"], quiet = TRUE)
      out_census <- here::here(sao_paulo_config$out_dir, "census", "sao_paulo_2010")
      census <- sp_process_census_2010(sf_data = weighting_areas,
          source_file = file_census,
          out_dir = out_census,
          return_data = FALSE)
      census_files <- here::here(out_census,
        c("census_sp_individual_2010.parquet", "census_sp_collapsed_2010.parquet"))
      processing_files(census_files)
    },
    format = "file")
)
