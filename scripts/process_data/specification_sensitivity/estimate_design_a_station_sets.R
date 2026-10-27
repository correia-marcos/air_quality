# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate Design A for each station set and compare it with the default stations.
#
#' @Description: Specification sensitivity, station-set comparison. For Bogotá 2018,
# Santiago 2017 and São Paulo 2010, each station-set exposure (estimate_idw_station_
# sets.R) defines a Design A sample at each buffer of resolution_station_set_buffers_km,
# exponent 1. Design A compares Q1 and Q5 at every support, for the frozen quintiles and
# for quintiles with tied schooling split proportionally (same people and weights). The
# change from the default stations at the same buffer is split into an exposure part on
# the people both cover and a part from the people the set loses; the change from the
# set's own 3 km estimate is split the same way. Writes samples, station weights,
# contrasts, profiles, both decompositions and checks to data/processed/
# resolution_sensitivity/station_sets/design_a/ and results/tables/
# specification_sensitivity/station_sets/.
#
#' @Summary:
#   I.   Import data: functions, settings, frozen inputs, default and set exposures.
#   II.  Process data: groups, samples, station weights, contrasts, decompositions and
#        checks.
#   III. Save outputs: the tables in the same order.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and the station-set settings
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

# Settings: cities, education groupings (frozen quintiles, then quintiles with split
# ties), outcomes, the station sets, their buffers and the 3 km reference
cities     <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
groupings  <- c("edu_quintile", "edu_quintile_split")
outcomes   <- c("avg_pm10", "hrs_d_pm10_it1", "hrs_d_pm10_it2",
                "avg_pm25", "hrs_d_pm25_it1", "hrs_d_pm25_it2")
sets       <- names(resolution_station_sets)
buffers    <- as.character(resolution_station_set_buffers_km)
ref_buffer <- "3"

# Set the folders
dir_inputs <- here::here("data", "interim", "resolution_sensitivity")
dir_frozen <- here::here("data", "processed", "resolution_sensitivity")
dir_grid   <- here::here(dir_frozen, "idw_grid")
dir_sets   <- here::here(dir_frozen, "station_sets")
dir_out    <- here::here(dir_sets, "design_a")
dir_tables <- here::here("results", "tables", "specification_sensitivity", "station_sets")

# Read each city's frozen input manifest; stop if any input changed after freezing
manifests <- lapply(setNames(cities, cities), function(city) {
  fread(file.path(dir_inputs, city, "input_manifest.csv"))
})
for (city in cities) {
  stopifnot(identical(unname(tools::md5sum(here::here(manifests[[city]]$path))),
                      manifests[[city]]$md5))
}

# Read the frozen crosswalks, adults, quintile cells and support ladders
keys       <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "crosswalk.parquet"))))
population <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "adult_population.parquet"))))
quintiles  <- lapply(setNames(cities, cities), function(city) as.data.table(
  arrow::read_parquet(file.path(dir_inputs, city, "individual_quintile_cells.parquet"))))
ladders    <- lapply(setNames(cities, cities), function(city) {
  fread(file.path(dir_inputs, city, "levels.csv"))
})

# Read the fine distance matrices and the stations each set keeps active per pollutant
distances <- lapply(setNames(cities, cities), function(city) {
  x <- as.data.table(arrow::read_parquet(file.path(dir_frozen, city, "B", "fine",
                                                   "matrix_geo_station_distances.parquet")))
  x[, station_id := normalize_station(station_id)]
})
active <- lapply(setNames(sets, sets), function(set) {
  lapply(setNames(cities, cities), function(city) {
    panel <- as.data.table(arrow::read_parquet(file.path(dir_sets, set, "panels", city,
      "year=2023", "data.parquet"), col_select = c("station", "pm10", "pm25")))
    panel[, station := normalize_station(station)]
    list(pm10 = unique(panel[is.finite(pm10), station]),
         pm25 = unique(panel[is.finite(pm25), station]))
  })
})

# Read the default grid exposures and the set exposures at the set buffers (exponent 1)
grid_manifest <- fread(file.path(dir_grid, "grid_manifest.csv"))
set_manifest  <- fread(file.path(dir_sets, "set_manifest.csv"))
stopifnot(identical(unname(tools::md5sum(here::here(set_manifest$path))),
                    set_manifest$md5))
exposures <- lapply(c(default = "default", setNames(sets, sets)), function(src) {
  lapply(setNames(cities, cities), function(city) {
    rows <- if (src == "default") {
      grid_manifest[city_id == city & distance_power == 1 &
                      buffer_km %in% resolution_station_set_buffers_km]
    } else {
      set_manifest[station_set == src & city_id == city]
    }
    setNames(lapply(here::here(rows$path), function(path) {
      x <- as.data.table(arrow::read_parquet(path))[year == analysis_year]
      for (oc in outcomes) set(x, j = oc, value = as.numeric(x[[oc]]))
      x
    }), rows$buffer_km)
  })
})

# Read the saved default Design A contrasts, recomputed below as a parity check
default_contrasts <- as.data.table(arrow::read_parquet(file.path(dir_frozen,
  "design_a_grid", "design_a_grid_contrasts.parquet")))[grouping %in% groupings &
  distance_power == 1 & buffer_km %in% resolution_station_set_buffers_km]

# ========================================================================================
# II: Process data
# ========================================================================================
# 1. Individual groups: the frozen quintiles, and quintiles with split ties for the same
#    adults (fractional weights), as in the Design A grid
cells <- lapply(setNames(cities, cities), function(city) {
  split <- resolution_group_cells(
    groups_path    = here::here(manifests[[city]][input == "groups", path]),
    quintile_cells = quintiles[[city]],
    grouping       = "edu_quintile_split",
    bands          = education_level_bands)$cells
  list(edu_quintile = quintiles[[city]], edu_quintile_split = split)
})

# Design A samples of the default stations and of each set: fine units with exposure
# and a parent at every support
good_keys <- lapply(keys, function(k) {
  k[, .(complete = all(!is.na(parent_id))), by = geo_id][complete == TRUE, geo_id]
})
sample_ids <- lapply(exposures, function(by_city) {
  lapply(setNames(cities, cities), function(city) {
    lapply(by_city[[city]], function(x) lapply(setNames(outcomes, outcomes), function(oc) {
      design_a_sample(exposure   = x,
                      population = population[[city]],
                      good_keys  = good_keys[[city]],
                      outcome    = oc)
    }))
  })
})

# Every set, city, buffer and pollutant described below
described <- CJ(station_set = sets, city_id = cities, buffer = buffers,
                pollutant = c("pm10", "pm25"), sorted = FALSE)

# Describe each set's sample: people, units, quintile shares and share of the default
samples <- rbindlist(lapply(seq_len(nrow(described)), function(i) {
  r <- described[i]
  q <- quintiles[[r$city_id]]
  oc <- paste0("avg_", r$pollutant)
  ids     <- sample_ids[[r$station_set]][[r$city_id]][[r$buffer]][[oc]]
  default <- sample_ids$default[[r$city_id]][[r$buffer]][[oc]]
  people  <- q[geo_id %chin% ids, .(w = sum(person_weight)), by = edu_quintile]
  data.table(station_set = r$station_set, city_id = r$city_id,
    buffer_km = as.numeric(r$buffer), pollutant = r$pollutant,
    sample_population = sum(people$w), sample_fine_units = length(ids),
    sample_share_city = sum(people$w) / sum(q$person_weight),
    default_population = q[geo_id %chin% default, sum(person_weight)],
    share_of_default = sum(people$w) / q[geo_id %chin% default, sum(person_weight)],
    nested_in_default = all(ids %chin% default),
    share_q1 = people[edu_quintile == 1, sum(w)] / sum(people$w),
    share_q5 = people[edu_quintile == 5, sum(w)] / sum(people$w))
}))

# 2. Station weights of each set, averaged over its sample's adults (as in the grid)
station_weights <- rbindlist(lapply(seq_len(nrow(described)), function(i) {
  r <- described[i]
  q <- quintiles[[r$city_id]][, .(w = sum(person_weight)), by = geo_id]
  stations <- active[[r$station_set]][[r$city_id]][[r$pollutant]]
  ids   <- sample_ids[[r$station_set]][[r$city_id]][[r$buffer]][[
    paste0("avg_", r$pollutant)]]
  units <- idw_station_weights(distances      = distances[[r$city_id]],
                               active         = stations,
                               buffer_km      = as.numeric(r$buffer),
                               distance_power = 1)
  units <- merge(units[geo_id %chin% ids], q, by = "geo_id")
  hours <- merge(exposures[[r$station_set]][[r$city_id]][[r$buffer]][geo_id %chin% ids,
    .(geo_id, hours = get(paste0("total_hrs_", r$pollutant)))], q, by = "geo_id")
  data.table(station_set = r$station_set, city_id = r$city_id,
    buffer_km = as.numeric(r$buffer), pollutant = r$pollutant,
    active_stations = length(stations),
    mean_eligible_stations = units[, weighted.mean(eligible_stations, w)],
    share_adults_multistation = units[, sum(w[eligible_stations >= 2]) / sum(w)],
    mean_static_neff = units[, weighted.mean(static_neff, w)],
    mean_observed_hours = hours[, weighted.mean(hours, w)])
}))

# 3. Design A contrasts and profiles of the default stations and of each set, on their
#    own samples, for every grouping, city, buffer, support and outcome
estimates <- list()
for (src in names(exposures)) for (g in groupings) for (city in cities) for (b in buffers) {
  for (lv in ladders[[city]]$level) for (oc in outcomes) {
    e <- design_a_estimate(exposure   = exposures[[src]][[city]][[b]],
                           sample_ids = sample_ids[[src]][[city]][[b]][[oc]],
                           population = population[[city]],
                           cells      = cells[[city]][[g]],
                           keys       = keys[[city]][level == lv, .(geo_id, parent_id)],
                           outcome    = oc,
                           group_col  = g,
                           groups     = sort(unique(cells[[city]][[g]][[g]])),
                           inference  = ladders[[city]][level == lv, secondary_inference])
    id <- data.table(station_set = src, grouping = g, city_id = city,
                     buffer_km = as.numeric(b), level = lv, outcome = oc)
    estimates[[length(estimates) + 1L]] <- list(contrast = cbind(id, e$contrast),
      profile = cbind(id, setnames(copy(e$profile), g, "group")))
  }
}
all_contrasts <- rbindlist(lapply(estimates, `[[`, "contrast"))
contrasts     <- all_contrasts[station_set != "default"]
profiles      <- rbindlist(lapply(estimates, `[[`, "profile"))[station_set != "default"]

# 4. Change from the default stations at the same buffer: the exposure part on the people
#    both cover and the part from the people the set loses (design_a_decomposition)
keys_cols <- c("grouping", "city_id", "buffer_km", "level", "outcome")
own <- merge(contrasts[, c("station_set", keys_cols, "gap"), with = FALSE],
             all_contrasts[station_set == "default", c(keys_cols, "gap"), with = FALSE],
             by = keys_cols, suffixes = c("", "_ref"))
common_default <- rbindlist(lapply(seq_len(nrow(own)), function(i) {
  r <- own[i]
  b <- as.character(r$buffer_km)
  ids_s <- sample_ids[[r$station_set]][[r$city_id]][[b]][[r$outcome]]
  ids_r <- sample_ids$default[[r$city_id]][[b]][[r$outcome]]
  ids_c <- intersect(ids_s, ids_r)
  cells_g <- cells[[r$city_id]][[r$grouping]]
  gap_on <- function(src, ids) {
    design_a_estimate(exposure   = exposures[[src]][[r$city_id]][[b]],
                      sample_ids = ids,
                      population = population[[r$city_id]],
                      cells      = cells_g,
                      keys       = keys[[r$city_id]][level == r$level,
                                                     .(geo_id, parent_id)],
                      outcome    = r$outcome,
                      group_col  = r$grouping,
                      groups     = sort(unique(cells_g[[r$grouping]])))$contrast$gap
  }
  # Reuse an own-sample gap whenever the common sample is that sample
  data.table(gap_spec_common = if (setequal(ids_c, ids_s)) r$gap else
               gap_on(r$station_set, ids_c),
             gap_ref_common  = if (setequal(ids_c, ids_r)) r$gap_ref else
               gap_on("default", ids_c),
             common_population = quintiles[[r$city_id]][geo_id %chin% ids_c,
                                                        sum(person_weight)],
             nested = setequal(ids_c, ids_s) || setequal(ids_c, ids_r))
}))
decomposition_vs_default <- cbind(own, common_default,
  design_a_decomposition(gap_own         = own$gap,
                         gap_ref_own     = own$gap_ref,
                         gap_spec_common = common_default$gap_spec_common,
                         gap_ref_common  = common_default$gap_ref_common))
setnames(decomposition_vs_default, c("gap", "gap_ref"), c("gap_set", "gap_default"))

# 5. Change from the set's own 3 km estimate, split the same way
own_3km <- merge(contrasts[, c("station_set", keys_cols, "gap"), with = FALSE],
                 contrasts[buffer_km == as.numeric(ref_buffer),
                           c("station_set", setdiff(keys_cols, "buffer_km"), "gap"),
                           with = FALSE],
                 by = c("station_set", setdiff(keys_cols, "buffer_km")),
                 suffixes = c("", "_ref"))
common_3km <- rbindlist(lapply(seq_len(nrow(own_3km)), function(i) {
  r <- own_3km[i]
  b <- as.character(r$buffer_km)
  ids_s <- sample_ids[[r$station_set]][[r$city_id]][[b]][[r$outcome]]
  ids_r <- sample_ids[[r$station_set]][[r$city_id]][[ref_buffer]][[r$outcome]]
  ids_c <- intersect(ids_s, ids_r)
  cells_g <- cells[[r$city_id]][[r$grouping]]
  gap_on <- function(buffer, ids) {
    design_a_estimate(exposure   = exposures[[r$station_set]][[r$city_id]][[buffer]],
                      sample_ids = ids,
                      population = population[[r$city_id]],
                      cells      = cells_g,
                      keys       = keys[[r$city_id]][level == r$level,
                                                     .(geo_id, parent_id)],
                      outcome    = r$outcome,
                      group_col  = r$grouping,
                      groups     = sort(unique(cells_g[[r$grouping]])))$contrast$gap
  }
  data.table(gap_spec_common = if (setequal(ids_c, ids_s)) r$gap else gap_on(b, ids_c),
             gap_ref_common  = if (setequal(ids_c, ids_r)) r$gap_ref else
               gap_on(ref_buffer, ids_c),
             common_population = quintiles[[r$city_id]][geo_id %chin% ids_c,
                                                        sum(person_weight)],
             nested = setequal(ids_c, ids_s) || setequal(ids_c, ids_r))
}))
decomposition_vs_3km <- cbind(own_3km, common_3km,
  design_a_decomposition(gap_own         = own_3km$gap,
                         gap_ref_own     = own_3km$gap_ref,
                         gap_spec_common = common_3km$gap_spec_common,
                         gap_ref_common  = common_3km$gap_ref_common))
setnames(decomposition_vs_3km, c("gap", "gap_ref"), c("gap_own", "gap_3km"))

# 6. Checks: the recomputed default equals the saved grid; set samples sit inside the
#    default samples and nest in the buffer; the identities hold
default_parity <- merge(all_contrasts[station_set == "default",
                                      c(keys_cols, "gap", "population"), with = FALSE],
                        default_contrasts[, c(keys_cols, "gap", "population"),
                                          with = FALSE],
                        by = keys_cols, suffixes = c("", "_saved"))
b_sorted <- as.character(sort(resolution_station_set_buffers_km))
buffer_nesting <- CJ(station_set = sets, city_id = cities,
                     step = seq_len(length(b_sorted) - 1L), outcome = outcomes,
                     sorted = FALSE)
buffer_nesting[, passed := mapply(function(s, city, step, oc) {
  all(sample_ids[[s]][[city]][[b_sorted[step]]][[oc]] %chin%
        sample_ids[[s]][[city]][[b_sorted[step + 1L]]][[oc]])
}, station_set, city_id, step, outcome)]
checks <- rbind(
  data.table(check = "default_parity_with_grid", cases = nrow(default_parity),
             failed = default_parity[, sum(abs(gap - gap_saved) > 1e-10 |
                                           abs(population / population_saved - 1) > 1e-10)],
             max_error = default_parity[, max(abs(gap - gap_saved))]),
  data.table(check = "set_sample_nested_in_default", cases = nrow(samples),
             failed = sum(!samples$nested_in_default), max_error = NA_real_),
  data.table(check = "set_sample_buffer_nesting", cases = nrow(buffer_nesting),
             failed = sum(!buffer_nesting$passed), max_error = NA_real_),
  data.table(check = "variance_identity", cases = nrow(contrasts), failed = NA_integer_,
             max_error = max(abs(contrasts$variance_identity_error), na.rm = TRUE)),
  data.table(check = "decomposition_vs_default_identity",
             cases = nrow(decomposition_vs_default),
             failed = sum(abs(decomposition_vs_default$identity_error) > 1e-10,
                          na.rm = TRUE),
             max_error = max(abs(decomposition_vs_default$identity_error), na.rm = TRUE)),
  data.table(check = "decomposition_vs_3km_identity", cases = nrow(decomposition_vs_3km),
             failed = sum(abs(decomposition_vs_3km$identity_error) > 1e-10, na.rm = TRUE),
             max_error = max(abs(decomposition_vs_3km$identity_error), na.rm = TRUE)))
print(checks)
stopifnot(all(checks$failed == 0, na.rm = TRUE))

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save each table as Parquet and CSV in the processed folder and as CSV in results
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
outputs <- list(samples = samples, station_weights = station_weights,
                contrasts = contrasts, profiles = profiles,
                decomposition_vs_default = decomposition_vs_default,
                decomposition_vs_3km = decomposition_vs_3km, checks = checks)
for (name in names(outputs)) {
  resolution_write(outputs[[name]], file.path(dir_out, paste0("station_sets_", name)))
  fwrite(outputs[[name]], file.path(dir_tables, paste0("station_sets_", name, ".csv")))
}
