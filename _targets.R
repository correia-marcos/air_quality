# Manuscript targets. Distance calculations and their saved checkpoints are separate.
# Remaining analytical stages retain transitional adapters during migration.
source(here::here("src", "pipeline", "load.R"), local = TRUE)
load_manuscript_functions()
source(here::here("src", "general_utilities", "process", "spatial_files.R"), local = TRUE)
source(here::here("config", "analysis_settings.R"), local = TRUE)
targets::tar_option_set(error = "stop", seed = manuscript_seed)

list(
  # bogota: explicit sources, spatial objects and saved checkpoints

  targets::tar_target(bogota_config,
    manuscript_city_config(bogota_cfg), packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_geography_inputs,
    c(
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2005_COLOMBIA.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2018_INTGRD_MPIO.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2018_INTGRD_MANZ.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "SHP_MGN2018_INTGRD_SECCR.zip"),
      here::here(bogota_config$dl_dir, "metro_area", "bogota_loca.gpkg")),
    format = "file", packages = c("dplyr", "data.table")),

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
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_municipalities_2005_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- bogota_geography_inputs
      archives <- sources[basename(sources) == "SHP_MGN2005_COLOMBIA.zip"]
      bogota_prepare_metro_area(source_zips = archives,
        level = "mpio",
        mgn_year = 2005,
        municipality_codes = bogota_config$city_code_metro)
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_tracts_2005_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- bogota_geography_inputs
      archives <- sources[basename(sources) == "SHP_MGN2005_COLOMBIA.zip"]
      bogota_prepare_metro_area(source_zips = archives,
        level = "manzana",
        mgn_year = 2005,
        municipality_codes = bogota_config$city_code_metro)
    }, packages = c("dplyr", "data.table")),

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
    }, packages = c("dplyr", "data.table")),

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
    }, packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_stations_filter_inputs,
    c(
      here::here(bogota_config$dl_dir, "ground_stations_geolocation",
        "bogota_stations_location.csv"),
      here::here(bogota_config$dl_dir, "stations_metadata")),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_station_locations,
    {
      files <- bogota_stations_filter_inputs
      read.csv(files[basename(files) == "bogota_stations_location.csv"])
    }, packages = c("dplyr", "data.table")),

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
    }, packages = c("dplyr", "data.table")),

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
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_stations_filter,
    c(
      write_geopackage(bogota_stations_2018_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_2018_stations_buffer_metro.gpkg")),
      write_geopackage(bogota_stations_2005_sf,
        here::here(bogota_config$out_dir, "geospatial_data", "bogota",
        "bogota_2005_stations_buffer_metro.gpkg"))),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_pollution_parquet_inputs,
    c(
      here::here(bogota_config$dl_dir, "ground_stations"),
      here::here(bogota_config$dl_dir, "metro_ground_stations_hourly")),
    format = "file", packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(bogota_census_inputs,
    c(
      here::here(bogota_config$dl_dir, "census", "CG2005_AMPLIADO.zip"),
      here::here(bogota_config$dl_dir, "census", "CG2005_BASICO.zip"),
      here::here(bogota_config$dl_dir, "census")),
    format = "file", packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  # cdmx: explicit sources, spatial objects and saved checkpoints

  targets::tar_target(cdmx_config,
    manuscript_city_config(cdmx_cfg), packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_geography_inputs,
    c(
      here::here(cdmx_config$dl_dir, "metro_area", "mg_2024_integrado.zip")),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_municipalities_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- cdmx_geography_inputs
      cdmx_prepare_metro_area(
        source_zip = sources[basename(sources) == "mg_2024_integrado.zip"],
        level = "municipality", keep_municipality = cdmx_config$cities_in_metro)
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_metro_area_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- cdmx_geography_inputs
      cdmx_prepare_metro_area(
        source_zip = sources[basename(sources) == "mg_2024_integrado.zip"],
        level = "ageb", keep_municipality = cdmx_config$cities_in_metro)
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_geography,
    c(
      write_geopackage(cdmx_municipalities_sf,
        here::here(cdmx_config$out_dir, "geospatial_data", "cdmx",
        "cdmx_area_metro_municipalities_2024.gpkg")),
      write_geopackage(cdmx_metro_area_sf,
        here::here(cdmx_config$out_dir, "geospatial_data", "cdmx",
        "cdmx_area_metro_2024.gpkg"))),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_stations_filter_inputs,
    c(
      here::here(cdmx_config$dl_dir, "ground_stations_geolocation",
        "all_station_location.csv")),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_station_locations,
    {
      files <- cdmx_stations_filter_inputs
      read.csv(files[basename(files) == "all_station_location.csv"])
    }, packages = c("dplyr", "data.table")),

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
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_stations_filter,
    c(
      write_geopackage(cdmx_stations_sf,
        here::here(cdmx_config$out_dir, "geospatial_data", "cdmx",
        "cdmx_stations_buffer_metro.gpkg"))),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_pollution_parquet_inputs,
    c(
      here::here(cdmx_config$dl_dir, "ground_stations"),
      here::here(cdmx_config$dl_dir, "ground_stations_raw_missing_data")),
    format = "file", packages = c("dplyr", "data.table")),

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
          out_dir = destination,
          out_name = "cdmx_metro")
      city_pollution_outputs("cdmx", cdmx_config)
    },
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(cdmx_census_inputs,
    c(
      here::here(cdmx_config$dl_dir, "census")),
    format = "file", packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  # santiago: explicit sources, spatial objects and saved checkpoints

  targets::tar_target(santiago_config,
    manuscript_city_config(santiago_cfg), packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_geography_inputs,
    c(
      here::here(santiago_config$dl_dir, "metro_area", "Cartografia_censo2024_Pais.zip"),
      here::here(santiago_config$dl_dir, "metro_area", "2017",
        "GRAN_SANTIAGO_13_metro.geojson"),
      here::here(santiago_config$dl_dir, "metro_area", "2017",
        "GRAN_SANTIAGO_13_zonas.geojson"),
      here::here(santiago_config$dl_dir, "metro_area", "2017",
        "GRAN_SANTIAGO_13_count.json")),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_zones_2017_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- santiago_geography_inputs
      santiago_prepare_metro_area_2017(
        metro_file = sources[basename(sources) == "GRAN_SANTIAGO_13_metro.geojson"],
        zones_file = sources[basename(sources) == "GRAN_SANTIAGO_13_zonas.geojson"],
        count_file = sources[basename(sources) == "GRAN_SANTIAGO_13_count.json"])
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_metro_2024_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- santiago_geography_inputs
      santiago_prepare_metro_area_2024(
        source_zip = sources[basename(sources) == "Cartografia_censo2024_Pais.zip"],
        type = "gran_santiago", level = "mpio",
        keep_municipality = santiago_config$cities_in_metro, dissolve_by = "CUT")
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_geography,
    c(
      write_geopackage(santiago_zones_2017_sf,
        here::here(santiago_config$out_dir, "geospatial_data", "santiago",
        "gran_santiago_zonas_2017.gpkg")),
      write_geopackage(santiago_metro_2024_sf,
        here::here(santiago_config$out_dir, "geospatial_data", "santiago",
        "gran_santiago_area_2024.gpkg"))),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_stations_filter_inputs,
    c(
      here::here(santiago_config$dl_dir, "stations_metadata",
        "SINCA_metadata_stations_20260113_1616.csv")),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_station_locations,
    {
      files <- santiago_stations_filter_inputs
      read.csv(files[basename(files) == "SINCA_metadata_stations_20260113_1616.csv"])
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_stations_sf,
    {
      sf::sf_use_s2(TRUE)
      santiago_filter_stations_in_metro(
        stations_df = santiago_station_locations,
        metro_area = santiago_zones_2017_sf,
        radius_km = santiago_config$station_buffer_km,
        out_file = NULL)
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_stations_filter,
    c(
      write_geopackage(santiago_stations_sf,
        here::here(santiago_config$out_dir, "geospatial_data", "santiago",
        "gran_santiago_stations_buffer_metro_2017.gpkg"))),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_pollution_parquet_inputs,
    c(
      here::here(santiago_config$dl_dir, "ground_stations")),
    format = "file", packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(santiago_census_inputs,
    c(
      here::here(santiago_config$dl_dir, "census", "2017", "censo2017.duckdb"),
      here::here(santiago_config$dl_dir, "census", "2024")),
    format = "file", packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  # sao_paulo: explicit sources, spatial objects and saved checkpoints

  targets::tar_target(sao_paulo_config,
    manuscript_city_config(sao_paulo_cfg), packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_geography_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "metro_area", "sp_municipios.zip"),
      here::here(sao_paulo_config$dl_dir, "metro_area", "sp_setores_censitarios.zip"),
      here::here(sao_paulo_config$dl_dir, "metro_area", "sp_weighting_areas_2010.rds")),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_municipalities_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- sao_paulo_geography_inputs
      sao_paulo_prepare_metro_area(
        source_zip = sources[basename(sources) == "sp_municipios.zip"],
        level = "mpio", keep_municipality = sao_paulo_config$cities_in_metro)
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_tracts_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- sao_paulo_geography_inputs
      sao_paulo_prepare_metro_area(
        source_zip = sources[basename(sources) == "sp_setores_censitarios.zip"],
        level = "setor_censitario", keep_municipality = sao_paulo_config$cities_in_metro)
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_weighting_areas_sf,
    {
      sf::sf_use_s2(TRUE)
      sources <- sao_paulo_geography_inputs
      sao_paulo_prepare_weighting_areas(
        source_file = sources[basename(sources) == "sp_weighting_areas_2010.rds"],
        keep_municipality = sao_paulo_config$cities_in_metro)
    }, packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_stations_filter_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "stations_metadata", "stations_metadata.csv")),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_station_locations,
    {
      files <- sao_paulo_stations_filter_inputs
      read.csv(files[basename(files) == "stations_metadata.csv"])
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_stations_sf,
    {
      sf::sf_use_s2(TRUE)
      sp_filter_stations_in_metro(
        stations_sp = sao_paulo_station_locations,
        metro_area = sao_paulo_municipalities_sf,
        radius_km = sao_paulo_config$station_buffer_km,
        out_file = NULL)
    }, packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_stations_filter,
    c(
      write_geopackage(sao_paulo_stations_sf,
        here::here(sao_paulo_config$out_dir, "geospatial_data", "sao_paulo",
        "sao_paulo_stations_buffer_metro_2010.gpkg"))),
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_pollution_parquet_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "ground_stations")),
    format = "file", packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  targets::tar_target(sao_paulo_census_inputs,
    c(
      here::here(sao_paulo_config$dl_dir, "census", "2010_population.parquet")),
    format = "file", packages = c("dplyr", "data.table")),

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
    format = "file", packages = c("dplyr", "data.table")),

  # City/vintage matrices, complete outlier datasets and IDW families
  targets::tar_target(bogota_2018_spec,
    manuscript_city_specs()[1L, , drop = FALSE],
    format = "rds", packages = pipeline_packages("process")),

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
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_outlier_files("bogota", bogota_pollution_parquet, bogota_2018_distances)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(bogota_2018_idw,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_idw_files(bogota_2018_spec, bogota_outliers, bogota_2018_distances,
            bogota_census)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(cdmx_2020_spec,
    manuscript_city_specs()[2L, , drop = FALSE],
    format = "rds", packages = pipeline_packages("process")),

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
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_outlier_files("cdmx", cdmx_pollution_parquet, cdmx_2020_distances)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(cdmx_2020_idw,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_idw_files(cdmx_2020_spec, cdmx_outliers, cdmx_2020_distances,
          cdmx_census)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(santiago_2017_spec,
    manuscript_city_specs()[3L, , drop = FALSE],
    format = "rds", packages = pipeline_packages("process")),

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
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_outlier_files("santiago", santiago_pollution_parquet,
          santiago_2017_distances)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(santiago_2017_idw,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_idw_files(santiago_2017_spec, santiago_outliers,
          santiago_2017_distances,
            santiago_census)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(santiago_2024_spec,
    manuscript_city_specs()[4L, , drop = FALSE],
    format = "rds", packages = pipeline_packages("process")),

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
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_idw_files(santiago_2024_spec, santiago_outliers,
          santiago_2024_distances,
            santiago_census)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(sao_paulo_2010_spec,
    manuscript_city_specs()[5L, , drop = FALSE],
    format = "rds", packages = pipeline_packages("process")),

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
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_outlier_files("sao_paulo", sao_paulo_pollution_parquet,
          sao_paulo_2010_distances)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(sao_paulo_2010_idw,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_idw_files(sao_paulo_2010_spec, sao_paulo_outliers,
          sao_paulo_2010_distances,
            sao_paulo_census)
    },
    format = "file", packages = pipeline_packages("process")),

  # Selections forward tracked files, preserving content hashes across these edges.
  # Only the upstream scientific targets write these files.
  targets::tar_target(geography,
    c(bogota_geography, cdmx_geography, santiago_geography, sao_paulo_geography),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(census,
    c(bogota_census, cdmx_census, santiago_census, sao_paulo_census),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(stations,
    c(bogota_stations_filter, cdmx_stations_filter, santiago_stations_filter,
      sao_paulo_stations_filter),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(pollution,
    c(bogota_pollution_parquet, cdmx_pollution_parquet, santiago_pollution_parquet,
        sao_paulo_pollution_parquet),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(distances,
    c(bogota_2018_distances, cdmx_2020_distances, santiago_2017_distances,
      santiago_2024_distances,
        sao_paulo_2010_distances),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(outliers,
    c(bogota_outliers, cdmx_outliers, santiago_outliers, sao_paulo_outliers),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(idw,
    c(bogota_2018_idw, cdmx_2020_idw, santiago_2017_idw, santiago_2024_idw,
      sao_paulo_2010_idw),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(process,
    c(geography, stations, pollution, census),
    format = "file", packages = pipeline_packages("process")),

  # Preserved temporal inputs and manuscript export configuration
  targets::tar_target(temporal_aerosol_inputs,
    pipeline_stage_inputs("generate_panel_air_quality"),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(temporal_station_inputs,
    here::here("data", "raw", c("pollution_ground_stations", "cities_shapefiles")),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(paper_manifest,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        here::here("config", "paper_artifacts.csv")
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(paper_font,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        here::here("fonts", "texgyrepagella-regular.otf")
    },
    format = "file", packages = pipeline_packages("process")),

  # Analytical products and shared intermediate summaries
  targets::tar_target(estimate_exposure,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        run_estimate_exposure(c(idw, distances))
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(compute_descriptive_tables,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        run_compute_descriptive_tables(c(pollution, outliers, distances, census))
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(compute_distance_band_descriptives,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        run_compute_distance_band_descriptives(c(distances, census, geography))
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(compute_station_scatter_inputs,
    run_compute_station_scatter_inputs(c(outliers, geography, census)),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(impute_missing_hourly,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        run_impute_missing_hourly(c(outliers))
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(estimate_exposure_imputed,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        run_estimate_exposure_imputed(c(impute_missing_hourly, distances, census))
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(generate_panel_air_quality,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        run_generate_panel_air_quality(c(temporal_aerosol_inputs))
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(prepare_station_temporal,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        run_prepare_station_temporal(c(generate_panel_air_quality,
          temporal_station_inputs))
    },
    format = "file", packages = pipeline_packages("process")),

  # Inspectable rendering data and owned figure files
  targets::tar_target(exposure_plot_data,
    read_generate_exposure_plots_data(
      c(estimate_exposure, estimate_exposure_imputed, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(generate_exposure_plots,
    write_generate_exposure_plots(
      exposure_plot_data,
      inputs = c(estimate_exposure,
        estimate_exposure_imputed, paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(figure_imputation_diagnostics_data,
    read_figure_imputation_diagnostics_data(
      c(impute_missing_hourly, compute_station_scatter_inputs, paper_manifest,
          paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(figure_imputation_diagnostics,
    write_figure_imputation_diagnostics(
      figure_imputation_diagnostics_data,
      inputs = c(impute_missing_hourly,
        compute_station_scatter_inputs, paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(plot_station_monitoring_figures_data,
    read_plot_station_monitoring_figures_data(
      c(distances, compute_station_scatter_inputs, census, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(plot_station_monitoring_figures,
    write_plot_station_monitoring_figures(
      plot_station_monitoring_figures_data,
        inputs = c(distances, compute_station_scatter_inputs, census, paper_manifest,
            paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(figure_station_scatter_data,
    read_figure_station_scatter_data(
      c(compute_station_scatter_inputs, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(figure_station_scatter,
    write_figure_station_scatter(
      figure_station_scatter_data,
      inputs = c(compute_station_scatter_inputs,
        paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(figure_population_density_maps_data,
    read_figure_population_density_maps_data(
      c(geography, stations, census, pollution, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(figure_population_density_maps,
    write_figure_population_density_maps(
      figure_population_density_maps_data,
      inputs = c(geography,
        stations, census, pollution, paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(figure_pollution_quintile_maps_data,
    read_figure_pollution_quintile_maps_data(
      c(geography, stations, census, pollution, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(figure_pollution_quintile_maps,
    write_figure_pollution_quintile_maps(
      figure_pollution_quintile_maps_data,
      inputs = c(geography,
        stations, census, pollution, paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(figure_kernel_distributions_data,
    read_figure_kernel_distributions_data(
      c(outliers, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(figure_kernel_distributions,
    write_figure_kernel_distributions(
      figure_kernel_distributions_data,
      inputs = c(outliers,
        paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(figure_quintile_kernel_distributions_data,
    read_figure_quintile_kernel_distributions_data(
      c(idw, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(figure_quintile_kernel_distributions,
    write_figure_quintile_kernel_distributions(
      figure_quintile_kernel_distributions_data,
        inputs = c(idw, paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(figure_station_temporal_data,
    read_figure_station_temporal_data(
      c(prepare_station_temporal, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(figure_station_temporal,
    write_figure_station_temporal(
      figure_station_temporal_data,
      inputs = c(prepare_station_temporal,
        paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  # Inspectable table data and owned LaTeX files
  targets::tar_target(station_table_data,
    read_render_station_tables_data(
      c(compute_descriptive_tables, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(render_station_tables,
    write_render_station_tables(
      station_table_data,
      inputs = c(compute_descriptive_tables,
        paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(missing_table_data,
    read_render_missing_tables_data(
      c(compute_descriptive_tables, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(render_missing_tables,
    write_render_missing_tables(
      missing_table_data,
      inputs = c(compute_descriptive_tables,
        paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(census_table_data,
    read_render_census_tables_data(
      c(compute_descriptive_tables, compute_distance_band_descriptives, paper_manifest,
          paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(render_census_tables,
    write_render_census_tables(
      census_table_data,
      inputs = c(compute_descriptive_tables,
        compute_distance_band_descriptives, paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  targets::tar_target(exposure_table_data,
    read_render_exposure_tables_data(
      c(estimate_exposure, paper_manifest, paper_font)
    ), format = "rds", packages = pipeline_packages("plot")),

  targets::tar_target(render_exposure_tables,
    write_render_exposure_tables(
      exposure_table_data,
      inputs = c(estimate_exposure,
        paper_manifest, paper_font)),
    format = "file", packages = pipeline_packages("plot")),

  # Aggregate file selections and manuscript export; selections never write products.
  targets::tar_target(figures,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        c(generate_exposure_plots, figure_imputation_diagnostics,
          plot_station_monitoring_figures,
            figure_station_scatter, figure_population_density_maps,
              figure_pollution_quintile_maps,
            figure_kernel_distributions, figure_quintile_kernel_distributions,
              figure_station_temporal)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(tables,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        c(render_station_tables, render_missing_tables, render_census_tables,
          render_exposure_tables)
    },
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(exposure,
    c(idw, estimate_exposure),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(descriptives,
    c(compute_descriptive_tables, compute_distance_band_descriptives),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(scatter,
    compute_station_scatter_inputs,
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(imputed,
    c(impute_missing_hourly, estimate_exposure_imputed),
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(temporal,
    prepare_station_temporal,
    format = "file", packages = pipeline_packages("process")),

  targets::tar_target(paper_export,
    {
        suppressMessages(sf::sf_use_s2(TRUE))
        prepare_paper_export(c(figures, tables), paper_manifest)
    },
    format = "file", packages = pipeline_packages("process"))

)
