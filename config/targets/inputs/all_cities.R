# Target declarations that combine the four city input modules.
list(
  targets::tar_target(geography,
    c(bogota_geography, cdmx_geography, santiago_geography, sao_paulo_geography),
    format = "file"),

  targets::tar_target(census,
    c(bogota_census, cdmx_census, santiago_census, sao_paulo_census),
    format = "file"),

  targets::tar_target(stations,
    c(bogota_stations_filter, cdmx_stations_filter, santiago_stations_filter,
      sao_paulo_stations_filter),
    format = "file"),

  targets::tar_target(pollution,
    c(bogota_pollution_parquet, cdmx_pollution_parquet, santiago_pollution_parquet,
        sao_paulo_pollution_parquet),
    format = "file"),

  targets::tar_target(process,
    c(geography, stations, pollution, census),
    format = "file")
)
