# inputs for prepare station temporal.
#' @param inputs Upstream files/directories; reads resolve from these paths.
#' @return Named objects for inspection, including written paths when applicable.
#' @details Preserves the manuscript specification; writing calls are kept explicit.
prepare_station_temporal_inputs <- function(inputs) {
  # Define input and output folders
  dir_panels   <- pipeline_input_root(inputs, "cities_m2_aerosols")
  dir_stations <- pipeline_input_root(inputs, "pollution_ground_stations")

  outdir_pm25         <- here::here("data", "processed", "merra2_pm25")
  outdir_m2_stations  <- here::here("data", "processed", "merra2_stations_pm25")

  # City aerosol panels, written by generate_panel_air_quality.R
  bogota_panel        <- read.csv(file.path(dir_panels, "bogota_panel.csv"))
  ciudad_mexico_panel <- read.csv(file.path(dir_panels, "ciudad_mexico_panel.csv"))
  santiago_panel      <- read.csv(file.path(dir_panels, "santiago_panel.csv"))
  sao_paulo_panel     <- read.csv(file.path(dir_panels, "sao_paulo_panel.csv"))

  # Ground-station readings. Each city names its datetime and PM2.5 columns differently, so
  # those names are passed explicitly in section III rather than harmonized here.
  bogota_stations        <- readRDS(file.path(
    dir_stations, "Bogota", "pollution_pm10_pm25_data_balanced_2023.rds"))
  ciudad_mexico_stations <- readRDS(file.path(
    dir_stations, "Mexico_city", "pollution_pm25_data_balanced_2023.rds"))
  santiago_stations      <- readRDS(file.path(
    dir_stations, "Santiago", "pollution_data_balanced_2023_pm25.rds"))
  sao_paulo_stations     <- readRDS(file.path(
    dir_stations, "Sao_paulo", "pollution_data_balanced_2023_pm25.rds"))

  # Sao Paulo's station file covers the whole state; section III cuts it to the metro area.
  stations_in_sp_metro <- sf::st_read(here::here(
    pipeline_input_root(inputs, "cities_shapefiles"), "Sao_Paulo_metro_stations"))
  list(
    dir_panels = dir_panels,
    dir_stations = dir_stations,
    outdir_pm25 = outdir_pm25,
    outdir_m2_stations = outdir_m2_stations,
    bogota_panel = bogota_panel,
    ciudad_mexico_panel = ciudad_mexico_panel,
    santiago_panel = santiago_panel,
    sao_paulo_panel = sao_paulo_panel,
    bogota_stations = bogota_stations,
    ciudad_mexico_stations = ciudad_mexico_stations,
    santiago_stations = santiago_stations,
    sao_paulo_stations = sao_paulo_stations,
    stations_in_sp_metro = stations_in_sp_metro
  )
}

# aerosols for prepare station temporal.
#' @param outdir_pm25 Directory path for the named data family.
#' @param bogota_panel Named data, setting or result from the preceding operation.
#' @param ciudad_mexico_panel Named data, setting or result from the preceding operation.
#' @param santiago_panel Named data, setting or result from the preceding operation.
#' @param sao_paulo_panel Named data, setting or result from the preceding operation.
#' @return Named objects for inspection, including written paths when applicable.
#' @details Preserves the manuscript specification; writing calls are kept explicit.
prepare_station_temporal_aerosols <- function(
  outdir_pm25,
  bogota_panel,
  ciudad_mexico_panel,
  santiago_panel,
  sao_paulo_panel) {
  dir.create(outdir_pm25, recursive = TRUE, showWarnings = FALSE)

  bogota_pm25        <- convert_and_add_pm25(bogota_panel)
  ciudad_mexico_pm25 <- convert_and_add_pm25(ciudad_mexico_panel)
  santiago_pm25      <- convert_and_add_pm25(santiago_panel)
  sao_paulo_pm25     <- convert_and_add_pm25(sao_paulo_panel)

  write.csv(bogota_pm25, file.path(outdir_pm25, "bogota_pm25.csv"), row.names = FALSE)
  write.csv(ciudad_mexico_pm25, file.path(outdir_pm25, "ciudad_mexico_pm25.csv"),
            row.names = FALSE)
  write.csv(santiago_pm25, file.path(outdir_pm25, "santiago_pm25.csv"), row.names = FALSE)
  write.csv(sao_paulo_pm25, file.path(outdir_pm25, "sao_paulo_pm25.csv"),
    row.names = FALSE)
  list(
    bogota_pm25 = bogota_pm25,
    ciudad_mexico_pm25 = ciudad_mexico_pm25,
    santiago_pm25 = santiago_pm25,
    sao_paulo_pm25 = sao_paulo_pm25
  )
}

# station series for prepare station temporal.
#' @param outdir_m2_stations Directory path for the named data family.
#' @param bogota_stations Named data, setting or result from the preceding operation.
#' @param ciudad_mexico_stations Named data, setting or result from the preceding operation.
#' @param santiago_stations Named data, setting or result from the preceding operation.
#' @param sao_paulo_stations Named data, setting or result from the preceding operation.
#' @param stations_in_sp_metro Named data, setting or result from the preceding operation.
#' @param bogota_pm25 Named data, setting or result from the preceding operation.
#' @param ciudad_mexico_pm25 Named data, setting or result from the preceding operation.
#' @param santiago_pm25 Named data, setting or result from the preceding operation.
#' @param sao_paulo_pm25 Named data, setting or result from the preceding operation.
#' @return Named objects for inspection, including written paths when applicable.
#' @details Preserves the manuscript specification; writing calls are kept explicit.
prepare_station_temporal_station_series <- function(
  outdir_m2_stations,
  bogota_stations,
  ciudad_mexico_stations,
  santiago_stations,
  sao_paulo_stations,
  stations_in_sp_metro,
  bogota_pm25,
  ciudad_mexico_pm25,
  santiago_pm25,
  sao_paulo_pm25) {
  dir.create(outdir_m2_stations, recursive = TRUE, showWarnings = FALSE)

  # Keep only the Sao Paulo stations that sit inside the metro area.
  sao_paulo_stations <- sao_paulo_stations %>%
    filter(station_code %in% stations_in_sp_metro$sttn_cd)

  bogota_pollution <- combine_station_merra2_pm25(
    station_df           = bogota_stations,
    station_datetime_col = "datetime",
    station_pm25_col     = "pm25",
    merra2_df            = bogota_pm25)

  ciudad_mexico_pollution <- combine_station_merra2_pm25(
    station_df           = ciudad_mexico_stations,
    station_datetime_col = "datetime",
    station_pm25_col     = "pm25",
    merra2_df            = ciudad_mexico_pm25)

  santiago_pollution <- combine_station_merra2_pm25(
    station_df           = santiago_stations,
    station_datetime_col = "date2_hour",
    station_pm25_col     = "pm25_validated",
    merra2_df            = santiago_pm25)

  sao_paulo_pollution <- combine_station_merra2_pm25(
    station_df           = sao_paulo_stations,
    station_datetime_col = "datetime",
    station_pm25_col     = "pm25",
    merra2_df            = sao_paulo_pm25)

  write.csv(bogota_pollution,
            file.path(outdir_m2_stations, "bogota_pm25_stations_merra2.csv"),
            row.names = FALSE)
  write.csv(ciudad_mexico_pollution,
            file.path(outdir_m2_stations, "ciudad_mexico_pm25_stations_merra2.csv"),
            row.names = FALSE)
  write.csv(santiago_pollution,
            file.path(outdir_m2_stations, "santiago_pm25_stations_merra2.csv"),
            row.names = FALSE)
  write.csv(sao_paulo_pollution,
            file.path(outdir_m2_stations, "sao_paulo_pm25_stations_merra2.csv"),
            row.names = FALSE)
  list(
    sao_paulo_stations = sao_paulo_stations,
    bogota_pollution = bogota_pollution,
    ciudad_mexico_pollution = ciudad_mexico_pollution,
    santiago_pollution = santiago_pollution,
    sao_paulo_pollution = sao_paulo_pollution
  )
}

# Execute the same scientific operations as the interactive recipe.
#' @param inputs Explicit upstream files/directories.
#' @return Every owned output path, validated after processing.
run_prepare_station_temporal <- function(inputs) {
  processing_files(inputs, "prepare_station_temporal inputs")
  inputs_result <- prepare_station_temporal_inputs(
  inputs = inputs)
  aerosols_result <- prepare_station_temporal_aerosols(
  outdir_pm25 = inputs_result$outdir_pm25,
  bogota_panel = inputs_result$bogota_panel,
  ciudad_mexico_panel = inputs_result$ciudad_mexico_panel,
  santiago_panel = inputs_result$santiago_panel,
  sao_paulo_panel = inputs_result$sao_paulo_panel)
  station_series_result <- prepare_station_temporal_station_series(
  outdir_m2_stations = inputs_result$outdir_m2_stations,
  bogota_stations = inputs_result$bogota_stations,
  ciudad_mexico_stations = inputs_result$ciudad_mexico_stations,
  santiago_stations = inputs_result$santiago_stations,
  sao_paulo_stations = inputs_result$sao_paulo_stations,
  stations_in_sp_metro = inputs_result$stations_in_sp_metro,
  bogota_pm25 = aerosols_result$bogota_pm25,
  ciudad_mexico_pm25 = aerosols_result$ciudad_mexico_pm25,
  santiago_pm25 = aerosols_result$santiago_pm25,
  sao_paulo_pm25 = aerosols_result$sao_paulo_pm25)
  pipeline_stage_outputs("prepare_station_temporal")
}
