# Portable contract checks: declared packages, without real analytical datasets.
#' @return Number of independent contract assertions exercised.
check_targets_contracts <- function() {
  e <- new.env(parent = globalenv())
  sys.source(here::here("src", "pipeline", "load.R"), e)
  e$load_manuscript_functions(envir = e)
  count <- 0L
  check <- function(condition) {
    stopifnot(isTRUE(condition))
    count <<- count + 1L
  }
  fails <- function(expr, pattern) {
    message <- tryCatch({ force(expr); "" }, error = conditionMessage)
    check(grepl(pattern, message))
  }
  cities <- c("bogota", "cdmx", "santiago", "sao_paulo")
  check(identical(e$list_cities(), cities))
  for (city in cities) {
    check(identical(names(formals(e$get_city(city)$process)),
                    c("cfg", "steps", "inputs", "quiet")))
  }
  fails(e$city_process("missing"), "Unknown city")
  fails(e$city_download("cdmx"), "No acquisition wrapper")
  fails(e$register_city("bogota", e$bogota_cfg, process = e$bogota_process),
        "already registered")
  fails(e$register_city("invalid", list(), process = e$bogota_process), "Invalid")
  fails(e$register_city("invalid", e$bogota_cfg, process = function(cfg) NULL), "Invalid")
  fails(e$city_processing_steps("cdmx", "census_2005"), "Unknown")
  fails(e$city_processing_steps("bogota", character()), "empty")
  for (city in cities) {
    check(identical(e$city_processing_steps(city, "pollution_parquet")$stages,
      c("geography", "stations_filter", "pollution_parquet")))
  }
  check(identical(e$city_processing_steps("bogota", "census_2005")$variants,
                  "census_2005"))
  check(identical(e$city_processing_steps("santiago", "census")$stages,
                  c("geography", "census")))
  check(identical(e$city_processing_steps("cdmx", "census")$stages, "census"))
  e$load_city_modules(e)
  check(identical(e$list_cities(), cities))

  cache <- here::here("tests", "_cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("city-contracts-", tmpdir = cache)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  write_source <- function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("preserved fixture", path)
  }
  calls <- list()
  fake_geography <- function(...) {
    args <- list(...)
    calls[[length(calls) + 1L]] <<- args
    data.frame(value = 1)
  }
  e$write_geopackage <- function(x, path, ...) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("derived fixture", path)
    path
  }
  builders <- c("bogota_prepare_metro_area", "cdmx_prepare_metro_area",
    "santiago_prepare_metro_area_2017", "santiago_prepare_metro_area_2024",
    "sao_paulo_prepare_metro_area", "sao_paulo_prepare_weighting_areas")
  for (name in builders) assign(name, fake_geography, envir = e)
  expected_counts <- c(bogota = 5L, cdmx = 2L, santiago = 2L, sao_paulo = 3L)
  for (city in cities) {
    cfg <- e$city_cfg(city)
    cfg$dl_dir <- file.path(work, "sources", city)
    cfg$out_dir <- file.path(work, "derived", city)
    inputs <- e$city_processing_inputs(city, cfg)$geography
    invisible(lapply(inputs, write_source))
    before <- tools::md5sum(inputs)
    calls <- list()
    prepare <- get(paste0(city, "_prepare_geography"), envir = e)
    outputs <- prepare(cfg, inputs, quiet = TRUE)
    check(length(outputs) == expected_counts[city])
    check(length(calls) == expected_counts[city])
    check(all(vapply(calls, function(x) {
      source_names <- intersect(names(x), c("source_zips", "source_zip", "source_file",
        "metro_file", "zones_file", "count_file", "localities_file"))
      paths <- unlist(x[source_names], use.names = FALSE)
      length(paths) > 0L && all(file.exists(paths)) &&
        is.null(x$allow_download) && is.null(x$out_file)
    }, logical(1))))
    check(all(file.exists(outputs)))
    check(identical(tools::md5sum(inputs), before))
    if (city == "bogota") {
      check(identical(vapply(calls, function(x) x$mgn_year, numeric(1)),
        c(2005, 2005, 2005, 2018, 2018)))
      check(identical(vapply(calls, function(x) x$level, character(1)),
        c("mpio_localidad", "mpio", "manzana", "mpio_localidad", "manzana")))
    }
    missing <- inputs
    missing[1] <- paste0(inputs[1], ".absent")
    calls <- list()
    fails(prepare(cfg, missing), "Missing")
    check(length(calls) == 0L)
  }

  # Test orchestration independently of the numerical/spatial implementations.
  cfg <- e$bogota_cfg
  cfg$dl_dir <- file.path(work, "orchestration")
  cfg$out_dir <- file.path(work, "outputs")
  inputs <- e$city_processing_inputs("bogota", cfg)
  for (path in unlist(inputs)) {
    if (grepl("\\.(zip|csv|gpkg)$", path)) write_source(path) else {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      write_source(file.path(path, "fixture.csv"))
    }
  }
  write_source(file.path(inputs$census["census_2018"], "2018_Bogota.zip"))
  write_source(file.path(inputs$census["census_2018"], "2018_Cundinamarca.zip"))
  sequence <- character()
  make_stage <- function(stage) {
    force(stage)
    function(...) {
      sequence <<- c(sequence, stage)
      c(output = file.path(work, paste0(stage, ".parquet")))
    }
  }
  stages <- c("geography", "stations", "pollution", "census")
  for (stage in stages) {
    assign(paste0("bogota_prepare_", stage), make_stage(stage), envir = e)
  }
  outputs <- e$bogota_process(cfg, inputs = inputs)
  check(identical(sequence, stages))
  check(identical(names(outputs),
    c("geography", "stations_filter", "pollution_parquet", "census")))
  sequence <- character()
  bad <- inputs
  bad$census["basic"] <- file.path(work, "missing.zip")
  fails(e$bogota_process(cfg, inputs = bad), "Missing")
  check(length(sequence) == 0L)
  e$bogota_prepare_pollution <- function(...) stop("fixture failure")
  fails(e$bogota_process(cfg, inputs = inputs), "fixture failure")
  check(identical(sequence, c("geography", "stations")))
  sequence <- character()
  selected <- NULL
  e$bogota_prepare_census <- function(cfg, inputs, ...) {
    selected <<- names(inputs)
    c(individual = "individual.parquet", collapsed = "collapsed.parquet")
  }
  fails(e$bogota_process(cfg, steps = "census_2005", inputs = bad), "Missing")
  alias_inputs <- inputs
  alias_inputs$census["census_2018"] <- file.path(work, "absent-2018")
  outputs <- e$bogota_process(cfg, steps = "census_2005", inputs = alias_inputs)
  check(identical(selected, c("extended", "basic")))
  check(identical(names(outputs), "census"))
  fails(e$resolve_city_inputs("bogota", cfg, list(geography = "incomplete")),
        "every named source")

  manifest <- targets::tar_manifest(fields = c("name", "command", "format"),
    script = here::here("_targets.R"), callr_function = NULL,
    envir = new.env(parent = globalenv()))
  plan <- setNames(lapply(seq_len(nrow(manifest)), function(i) {
    list(command = str2lang(manifest$command[i]), format = manifest$format[i])
  }), manifest$name)
  check(!anyDuplicated(names(plan)))
  selections <- c("geography", "census", "stations", "pollution", "distances",
    "outliers", "idw", "process", "figures", "tables", "exposure", "descriptives",
    "scatter", "imputed", "temporal")
  check(all(vapply(plan[selections], function(x) identical(x$format, "file"), logical(1))))
  dependencies <- lapply(plan, function(x)
    intersect(targets::tar_deps_raw(x$command), names(plan)))
  remaining <- names(plan)
  visited <- character()
  while (length(remaining)) {
    ready <- remaining[vapply(dependencies[remaining], function(x)
      all(x %in% visited), logical(1))]
    check(length(ready) > 0L)
    visited <- c(visited, ready)
    remaining <- setdiff(remaining, ready)
  }
  check(setequal(visited, names(plan)))
  specs <- e$manuscript_city_specs()
  for (city in cities) {
    row <- specs[specs$city == city, , drop = FALSE][1, ]
    check(paste0(row$id, "_distances") %in% dependencies[[paste0(city, "_outliers")]])
    check(paste0(city, "_pollution_parquet") %in%
      dependencies[[paste0(city, "_outliers")]])
    command <- all.names(plan[[paste0(city, "_geography")]]$command)
    check("write_geopackage" %in% command)
    check(identical(plan[[paste0(city, "_geography")]]$format, "file"))
    check(identical(plan[[paste0(city, "_pollution_parquet")]]$format, "file"))
  }
  check(!any(grepl("resolution|download|validation|process_merra2_panels", names(plan))))
  check("generate_panel_air_quality" %in% dependencies$prepare_station_temporal)
  check("prepare_station_temporal" %in% dependencies$figure_station_temporal_data)
  check("estimate_exposure_imputed" %in% dependencies$exposure_plot_data)
  check("paper_manifest" %in% dependencies$paper_export)
  check("census" %in% dependencies$compute_station_scatter_inputs)
  check("geography" %in% dependencies$compute_station_scatter_inputs)
  check("census" %in% dependencies$compute_descriptive_tables)
  check(!"cdmx_config" %in% dependencies$bogota_geography)
  check(!"bogota_config" %in% dependencies$cdmx_geography)
  cfg <- e$bogota_cfg
  check(identical(e$manuscript_city_config(cfg), cfg))
  cfg$out_dir <- work
  fails(e$manuscript_city_config(cfg), "canonical data/interim root")
  check(identical(specs$income_groups, c(0L, 5L, 0L, 0L, 10L)))
  check(identical(specs$geo_id, c("GEO_ID", "CVE_MUN", "zona_id", "CUT", "code_weighting")))
  check(length(e$idw_owned_files(specs[1, ], work)) == 7L)
  check(length(e$idw_owned_files(specs[2, ], work)) == 11L)
  manifest <- read.csv(here::here("config", "paper_artifacts.csv"))
  check(all(sub("[.]R$", "", basename(manifest$producer_script)) %in% names(plan)))
  check(identical(manifest$producer_script[
    manifest$artifact_id == "tables_table_avg_hours_above_thresholds"],
    "scripts/tables_images/render_station_tables.R"))
  owners <- split(manifest$producer_script, manifest$source_path)
  check(all(vapply(owners, function(x) length(unique(x)) == 1L, logical(1))))
  count
}

if (sys.nframe() == 0L) {
  count <- check_targets_contracts()
  cat(count, "portable targets/city contract assertions passed\n")
}
