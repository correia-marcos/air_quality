# inputs for generate panel air quality.
#' @return Named objects for inspection, including written paths when applicable.
#' @details Preserves the manuscript specification; writing calls are kept explicit.
generate_panel_air_quality_inputs <- function(inputs) {
  # Create list of all raster files from MERRA2
  nc_files             <- list.files(pipeline_input_root(inputs,
    "merra2_aerosol_products"),
                                     full.names = TRUE)

  # Open city shapefiles
  bogota               <- sf::st_read(here::here(pipeline_input_root(inputs,
    "cities_shapefiles"), "Bogota_metro"))
  ciudad_mexico        <- sf::st_read(here::here(pipeline_input_root(inputs,
    "cities_shapefiles"), "Mexico_city"))
  santiago             <- sf::st_read(here::here(pipeline_input_root(inputs,
    "cities_shapefiles"), "Santiago"))
  sao_paulo            <- sf::st_read(here::here(pipeline_input_root(inputs,
    "cities_shapefiles"), "Sao_Paulo"))
  list(
    nc_files = nc_files,
    bogota = bogota,
    ciudad_mexico = ciudad_mexico,
    santiago = santiago,
    sao_paulo = sao_paulo
  )
}

# panels for generate panel air quality.
#' @param nc_files Named data, setting or result from the preceding operation.
#' @param bogota Named data, setting or result from the preceding operation.
#' @param ciudad_mexico Named data, setting or result from the preceding operation.
#' @param santiago Named data, setting or result from the preceding operation.
#' @param sao_paulo Named data, setting or result from the preceding operation.
#' @return Named objects for inspection, including written paths when applicable.
#' @details Preserves the manuscript specification; writing calls are kept explicit.
generate_panel_air_quality_panels <- function(
  nc_files,
  bogota,
  ciudad_mexico,
  santiago,
  sao_paulo) {
  # Apply function to crop data for each city and generate a panel - parallel possible
  bogota_results <- process_merra2_region_hourly(
    shapefile      = bogota,
    nc_files       = nc_files,
    region_name    = "Bogotá",
    num_cores      = NULL,
    extraction_fun = "mean",
    parallel       = TRUE)

  ciudad_mexico_results <- process_merra2_region_hourly(
    shapefile      = ciudad_mexico,
    nc_files       = nc_files,
    region_name    = "Ciudad de México",
    num_cores      = NULL,
    extraction_fun = "mean",
    parallel       = TRUE)

  santiago_results <- process_merra2_region_hourly(
    shapefile      = santiago,
    nc_files       = nc_files,
    region_name    = "Santiago",
    num_cores      = NULL,
    extraction_fun = "mean",
    parallel       = TRUE)

  sao_paulo_results <- process_merra2_region_hourly(
    shapefile      = sao_paulo,
    nc_files       = nc_files,
    region_name    = "São Paulo",
    num_cores      = NULL,
    extraction_fun = "mean",
    parallel       = TRUE)
  list(
    bogota_results = bogota_results,
    ciudad_mexico_results = ciudad_mexico_results,
    santiago_results = santiago_results,
    sao_paulo_results = sao_paulo_results
  )
}

# save for generate panel air quality.
#' @param bogota_results Named data, setting or result from the preceding operation.
#' @param ciudad_mexico_results Named data, setting or result from the preceding operation.
#' @param santiago_results Named data, setting or result from the preceding operation.
#' @param sao_paulo_results Named data, setting or result from the preceding operation.
#' @return Named objects for inspection, including written paths when applicable.
#' @details Preserves the manuscript specification; writing calls are kept explicit.
generate_panel_air_quality_save <- function(
  bogota_results,
  ciudad_mexico_results,
  santiago_results,
  sao_paulo_results) {
  # Ensure output folder exists
  outdir <- here("data", "interim", "cities_m2_aerosols")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  # Save processed dataframes
  write.csv(bogota_results,
            file      = here::here(outdir, "bogota_panel.csv"),
            row.names = FALSE)
  write.csv(ciudad_mexico_results,
            file      = here::here(outdir, "ciudad_mexico_panel.csv"),
            row.names = FALSE)
  write.csv(santiago_results,
            file      = here::here(outdir, "santiago_panel.csv"),
            row.names = FALSE)
  write.csv(sao_paulo_results,
            file      = here::here(outdir, "sao_paulo_panel.csv"),
            row.names = FALSE)

  list(
    outdir = outdir
  )
}

# Execute the same scientific operations as the interactive recipe.
#' @param inputs Explicit upstream files/directories.
#' @return Every owned output path, validated after processing.
run_generate_panel_air_quality <- function(inputs) {
  processing_files(inputs, "generate_panel_air_quality inputs")
  inputs_result <- generate_panel_air_quality_inputs(
  inputs = inputs)
  panels_result <- generate_panel_air_quality_panels(
  nc_files = inputs_result$nc_files,
  bogota = inputs_result$bogota,
  ciudad_mexico = inputs_result$ciudad_mexico,
  santiago = inputs_result$santiago,
  sao_paulo = inputs_result$sao_paulo)
  save_result <- generate_panel_air_quality_save(
  bogota_results = panels_result$bogota_results,
  ciudad_mexico_results = panels_result$ciudad_mexico_results,
  santiago_results = panels_result$santiago_results,
  sao_paulo_results = panels_result$sao_paulo_results)
  pipeline_stage_outputs("generate_panel_air_quality")
}
