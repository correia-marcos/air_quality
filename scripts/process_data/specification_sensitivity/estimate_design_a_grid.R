# ========================================================================================
# IDB: Air monitoring
# ========================================================================================
#' @Goal: Estimate Design A over the IDW buffer x exponent grid, with its decomposition.
#
#' @Description: Primary design of the specification-sensitivity analysis. For Bogotá
# 2018, Santiago 2017 and São Paulo 2010, each grid exposure (estimate_idw_specification_
# grid.R) defines a Design A sample: adults in fine units with exposure. Design A averages
# fine exposure over each support and compares the lowest and highest education groups,
# for four groupings. Each specification is also evaluated on the sample it shares with
# the reference (3 km, exponent 1), which splits the change from the reference into an
# exposure part on fixed people and a sample part. Writes samples, station weights,
# contrasts, profiles, decomposition and checks to data/processed/resolution_sensitivity/
# design_a_grid/ and results/tables/specification_sensitivity/design_a/.
#
#' @Summary:
#   I.   Import data: functions, settings, frozen inputs and grid exposures.
#   II.  Process data: samples, support sizes, station weights, contrasts,
#        decomposition, threshold ties and checks.
#   III. Save outputs: the nine tables in the same order.
#
#' @Date: October 2026
#' @Author: Marcos Paulo
# ========================================================================================

# ========================================================================================
# I: Import data
# ========================================================================================
# Load the functions and the grid settings
source(here::here("src", "general_utilities", "config_utils_resolution.R"))
source(here::here("config", "analysis_settings.R"))

# Settings: cities, education groupings, outcomes and the reference specification
cities     <- c("bogota_2018", "santiago_2017", "sao_paulo_2010")
groupings  <- c("edu_quintile", "edu_quintile_split", "edu_level", "edu_group3")
outcomes   <- c("avg_pm10", "hrs_d_pm10_it1", "hrs_d_pm10_it2",
                "avg_pm25", "hrs_d_pm25_it1", "hrs_d_pm25_it2")
reference  <- c(buffer_km = 3, distance_power = 1)
specs      <- rbind(data.table(buffer_km = resolution_grid_buffers_km, distance_power = 1),
                    data.table(buffer_km = resolution_grid_power2_buffers_km,
                               distance_power = 2))
specs[, spec := paste0("b", buffer_km, "_p", distance_power)]
both_powers <- resolution_grid_power2_buffers_km
ref_spec   <- paste0("b", reference[["buffer_km"]], "_p", reference[["distance_power"]])

# Set the folders
dir_inputs <- here::here("data", "interim", "resolution_sensitivity")
dir_frozen <- here::here("data", "processed", "resolution_sensitivity")
dir_grid   <- here::here(dir_frozen, "idw_grid")
dir_out    <- here::here(dir_frozen, "design_a_grid")
dir_tables <- here::here("results", "tables", "specification_sensitivity", "design_a")

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

# Read the area of every unit at every support (fixed evaluation CRS, km2)
unit_areas <- lapply(setNames(cities, cities), function(city) {
  lvs <- ladders[[city]]$level
  lapply(setNames(lvs, lvs), function(lv) as.data.table(arrow::read_parquet(
    file.path(dir_inputs, city, paste0(lv, "_unit_population.parquet"))))[
      , .(parent_id, area_km2)])
})

# Read the fine distance matrices and the stations that reported each pollutant in 2023
distances <- lapply(setNames(cities, cities), function(city) {
  x <- as.data.table(arrow::read_parquet(file.path(dir_frozen, city, "B", "fine",
                                                   "matrix_geo_station_distances.parquet")))
  x[, station_id := normalize_station(station_id)]
})
panels <- lapply(manifests, function(m) {
  panel <- as.data.table(arrow::read_parquet(here::here(m[input == "pollution", path])))
  panel[, station := normalize_station(station)]
})
active <- lapply(panels, function(panel) {
  list(pm10 = unique(panel[is.finite(pm10), station]),
       pm25 = unique(panel[is.finite(pm25), station]))
})

# Read the grid exposures listed in the grid manifest (one per city and specification)
grid_manifest <- fread(file.path(dir_grid, "grid_manifest.csv"))
stopifnot(identical(unname(tools::md5sum(here::here(grid_manifest$path))),
                    grid_manifest$md5))
exposures <- lapply(setNames(cities, cities), function(city) {
  rows <- merge(specs, grid_manifest[city_id == city],
                by = c("buffer_km", "distance_power"))
  setNames(lapply(here::here(rows$path), function(path) {
    x <- as.data.table(arrow::read_parquet(path))[year == analysis_year]
    for (oc in c(outcomes, "total_hrs_pm10", "total_hrs_pm25")) {
      set(x, j = oc, value = as.numeric(x[[oc]]))
    }
    x
  }), rows$spec)
})

# ========================================================================================
# II: Process data
# ========================================================================================
# 1. Individual groups: frozen quintiles, and three alternatives with the same adults
cells <- lapply(setNames(cities, cities), function(city) {
  alternatives <- lapply(setNames(groupings[-1], groupings[-1]), function(g) {
    resolution_group_cells(groups_path = here::here(manifests[[city]][input == "groups",
                                                                      path]),
                           quintile_cells = quintiles[[city]],
                           grouping       = g,
                           bands          = education_level_bands,
                           group_of_level = education_group3_of_level)$cells
  })
  c(list(edu_quintile = quintiles[[city]]), alternatives)
})

# 2. Design A samples: fine units with exposure and a parent at every support
good_keys <- lapply(keys, function(k) {
  k[, .(complete = all(!is.na(parent_id))), by = geo_id][complete == TRUE, geo_id]
})
sample_ids <- lapply(setNames(cities, cities), function(city) {
  lapply(exposures[[city]], function(x) lapply(setNames(outcomes, outcomes), function(oc) {
    design_a_sample(exposure   = x,
                    population = population[[city]],
                    good_keys  = good_keys[[city]],
                    outcome    = oc)
  }))
})

# Describe each sample: people, units, education composition and overlap with 3 km
samples <- rbindlist(lapply(cities, function(city) {
  q <- quintiles[[city]]
  city_adults <- sum(q$person_weight)
  rbindlist(lapply(seq_len(nrow(specs)), function(i) {
    rbindlist(lapply(c("pm10", "pm25"), function(poll) {
      ids <- sample_ids[[city]][[specs$spec[i]]][[paste0("avg_", poll)]]
      ref <- sample_ids[[city]][[ref_spec]][[paste0("avg_", poll)]]
      people <- q[geo_id %chin% ids, .(w = sum(person_weight)), by = edu_quintile]
      shares <- setNames(numeric(5), paste0("share_q", 1:5))
      shares[paste0("share_q", people$edu_quintile)] <- people$w / sum(people$w)
      data.table(city_id = city, buffer_km = specs$buffer_km[i],
        distance_power = specs$distance_power[i], pollutant = poll,
        sample_population = sum(people$w), sample_fine_units = length(ids),
        sample_all_adults = population[[city]][geo_id %chin% ids, sum(pop)],
        sample_share_city = sum(people$w) / city_adults,
        overlap_with_reference = q[geo_id %chin% intersect(ids, ref), sum(person_weight)] /
          q[geo_id %chin% ref, sum(person_weight)],
        share_in_reference = q[geo_id %chin% intersect(ids, ref), sum(person_weight)] /
          sum(people$w),
        nested_with_reference = all(ids %chin% ref) || all(ref %chin% ids),
        share_q1 = shares[[1]], share_q2 = shares[[2]], share_q3 = shares[[3]],
        share_q4 = shares[[4]], share_q5 = shares[[5]])
    }))
  }))
}))

# Size of the support units each sample reaches, weighted by its education-reporting
# adults: the area and population of the unit the average sampled adult lives in
support_sizes <- rbindlist(lapply(cities, function(city) {
  q <- quintiles[[city]][, .(w = sum(person_weight)), by = geo_id]
  rbindlist(lapply(ladders[[city]]$level, function(lv) {
    k <- keys[[city]][level == lv, .(geo_id, parent_id)]
    rbindlist(lapply(specs$spec, function(s) {
      rbindlist(lapply(c("pm10", "pm25"), function(poll) {
        ids <- sample_ids[[city]][[s]][[paste0("avg_", poll)]]
        u <- merge(q[geo_id %chin% ids], k, by = "geo_id")[, .(w = sum(w)), by = parent_id]
        u <- merge(u, unit_areas[[city]][[lv]], by = "parent_id", all.x = TRUE)
        data.table(city_id = city, spec = s, pollutant = poll, level = lv,
          n_level_units = nrow(unit_areas[[city]][[lv]]), sample_support_units = nrow(u),
          pw_unit_area_km2 = u[, weighted.mean(area_km2, w)],
          median_unit_area_km2 = stats::median(u$area_km2),
          pw_unit_population = u[, sum(w^2) / sum(w)])
      }))
    }))
  }))
}))

# 3. Station weights of each specification, averaged over the sample's adults
station_weights <- rbindlist(lapply(cities, function(city) {
  q <- quintiles[[city]][, .(w = sum(person_weight)), by = geo_id]
  rbindlist(lapply(seq_len(nrow(specs)), function(i) {
    rbindlist(lapply(c("pm10", "pm25"), function(poll) {
      ids <- sample_ids[[city]][[specs$spec[i]]][[paste0("avg_", poll)]]
      units <- idw_station_weights(distances      = distances[[city]],
                                   active         = active[[city]][[poll]],
                                   buffer_km      = specs$buffer_km[i],
                                   distance_power = specs$distance_power[i])
      units <- merge(units[geo_id %chin% ids], q, by = "geo_id")
      hours <- merge(exposures[[city]][[specs$spec[i]]][geo_id %chin% ids,
        .(geo_id, hours = get(paste0("total_hrs_", poll)))], q, by = "geo_id")
      data.table(city_id = city, buffer_km = specs$buffer_km[i],
        distance_power = specs$distance_power[i], pollutant = poll,
        mean_eligible_stations = units[, weighted.mean(eligible_stations, w)],
        share_adults_multistation = units[, sum(w[eligible_stations >= 2]) / sum(w)],
        mean_static_neff = units[, weighted.mean(static_neff, w)],
        mean_max_weight_share = units[, weighted.mean(max_weight_share, w)],
        median_nearest_eligible_km = units[, resolution_weighted_quantile(
          nearest_eligible_km, w, .5)],
        mean_observed_hours = hours[, weighted.mean(hours, w)])
    }))
  }))
}))

# 4. Design A contrasts and profiles: every city, grouping, specification, support and
#    outcome on its own sample (HC1 where the ladder allows secondary inference)
estimates <- list()
for (city in cities) for (g in groupings) for (s in specs$spec) {
  groups <- sort(unique(cells[[city]][[g]][[g]]))
  for (lv in ladders[[city]]$level) for (oc in outcomes) {
    e <- design_a_estimate(exposure   = exposures[[city]][[s]],
                           sample_ids = sample_ids[[city]][[s]][[oc]],
                           population = population[[city]],
                           cells      = cells[[city]][[g]],
                           keys       = keys[[city]][level == lv, .(geo_id, parent_id)],
                           outcome    = oc,
                           group_col  = g,
                           groups     = groups,
                           inference  = ladders[[city]][level == lv, secondary_inference])
    id <- data.table(city_id = city, grouping = g, spec = s, level = lv, outcome = oc)
    estimates[[length(estimates) + 1L]] <- list(
      contrast = cbind(id, e$contrast,
        sample_matches_assignment = setequal(e$assignment$geo_id,
                                             sample_ids[[city]][[s]][[oc]])),
      profile = cbind(id, setnames(copy(e$profile), g, "group")))
  }
}
contrasts <- merge(specs, rbindlist(lapply(estimates, `[[`, "contrast")), by = "spec")
profiles  <- merge(specs, rbindlist(lapply(estimates, `[[`, "profile")), by = "spec")

# 5. Fixed-sample decomposition against the reference: evaluate each specification and
#    the reference on their common sample, then split the change (design_a_decomposition)
keys_cols <- c("city_id", "grouping", "level", "outcome")
own <- contrasts[, c(keys_cols, "spec", "buffer_km", "distance_power", "gap"), with = FALSE]
own <- merge(own, contrasts[spec == ref_spec, c(keys_cols, "gap"), with = FALSE],
             by = keys_cols, suffixes = c("", "_ref"))
common_gaps <- rbindlist(lapply(seq_len(nrow(own)), function(i) {
  r <- own[i]
  ids_s <- sample_ids[[r$city_id]][[r$spec]][[r$outcome]]
  ids_r <- sample_ids[[r$city_id]][[ref_spec]][[r$outcome]]
  ids_c <- intersect(ids_s, ids_r)
  cells_g <- cells[[r$city_id]][[r$grouping]]
  gap_on <- function(s, ids) {
    design_a_estimate(exposure   = exposures[[r$city_id]][[s]],
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
  data.table(gap_spec_common = if (setequal(ids_c, ids_s)) r$gap else gap_on(r$spec, ids_c),
             gap_ref_common  = if (setequal(ids_c, ids_r)) r$gap_ref else
               gap_on(ref_spec, ids_c),
             common_fine_units = length(ids_c),
             common_population = cells[[r$city_id]][[r$grouping]][geo_id %chin% ids_c,
                                                                   sum(person_weight)],
             nested = setequal(ids_c, ids_s) || setequal(ids_c, ids_r))
}))
decomposition <- cbind(own, common_gaps,
                       design_a_decomposition(gap_own         = own$gap,
                                              gap_ref_own     = own$gap_ref,
                                              gap_spec_common = common_gaps$gap_spec_common,
                                              gap_ref_common  = common_gaps$gap_ref_common))
setnames(decomposition, c("gap", "gap_ref"), c("gap_own", "gap_ref_own"))

# 6. Checks: samples do not depend on the exponent or on the outcome of a pollutant,
#    are nested in the buffer, and the estimates satisfy their identities
power_invariance <- rbindlist(lapply(cities, function(city) {
  rbindlist(lapply(both_powers, function(b) {
    ids <- lapply(resolution_grid_distance_powers, function(p) {
      sample_ids[[city]][[paste0("b", b, "_p", p)]]
    })
    data.table(city_id = city, buffer_km = b,
               passed = all(vapply(ids[-1], identical, logical(1), ids[[1]])))
  }))
}))
outcome_invariance <- rbindlist(lapply(cities, function(city) {
  rbindlist(lapply(specs$spec, function(s) {
    x <- sample_ids[[city]][[s]]
    data.table(city_id = city, spec = s,
      passed = identical(x$avg_pm10, x$hrs_d_pm10_it1) &&
        identical(x$avg_pm10, x$hrs_d_pm10_it2) &&
        identical(x$avg_pm25, x$hrs_d_pm25_it1) && identical(x$avg_pm25, x$hrs_d_pm25_it2))
  }))
}))
buffer_nesting <- rbindlist(lapply(cities, function(city) {
  b <- sort(resolution_grid_buffers_km)
  rbindlist(lapply(seq_len(length(b) - 1L), function(i) {
    smaller <- sample_ids[[city]][[paste0("b", b[i], "_p1")]]
    larger  <- sample_ids[[city]][[paste0("b", b[i + 1L], "_p1")]]
    data.table(city_id = city, buffer_km = b[i + 1L],
      passed = all(vapply(outcomes, function(oc) all(smaller[[oc]] %chin% larger[[oc]]),
                          logical(1))))
  }))
}))
single_station <- rbindlist(lapply(cities, function(city) {
  rbindlist(lapply(both_powers, function(b) {
    rbindlist(lapply(c("pm10", "pm25"), function(poll) {
      one <- idw_station_weights(distances[[city]], active[[city]][[poll]], b, 1)[
        eligible_stations == 1, geo_id]
      cols <- grep(paste0("^(avg|hrs_d)_", poll, "_?"), outcomes, value = TRUE)
      x <- merge(exposures[[city]][[paste0("b", b, "_p1")]][geo_id %chin% one,
                   c("geo_id", cols), with = FALSE],
                 exposures[[city]][[paste0("b", b, "_p2")]][geo_id %chin% one,
                   c("geo_id", cols), with = FALSE], by = "geo_id")
      error <- vapply(cols, function(oc) {
        max(abs(x[[paste0(oc, ".x")]] - x[[paste0(oc, ".y")]]), 0, na.rm = TRUE)
      }, numeric(1))
      # Hour counts can differ only where a value sits on a threshold to rounding error
      differs <- Reduce(`|`, lapply(cols[-1], function(oc) {
        x[[paste0(oc, ".x")]] != x[[paste0(oc, ".y")]]
      }))
      data.table(city_id = city, buffer_km = b, pollutant = poll,
                 single_station_units = nrow(x), max_error = error[[paste0("avg_", poll)]],
                 units_with_hour_difference = sum(differs, na.rm = TRUE),
                 max_hour_difference = max(error[-1]),
                 passed = error[[paste0("avg_", poll)]] < 1e-8)
    }))
  }))
}))
# Station-hours exactly at a threshold: where one station sets the value, (v * w) / w
# can round below the threshold, so these hours are counted or not by rounding alone
threshold_ties <- rbindlist(lapply(cities, function(city) {
  rbindlist(lapply(names(station_who_thresholds), function(poll) {
    v <- panels[[city]][[poll]]
    thresholds <- station_who_thresholds[[poll]]
    data.table(city_id = city, pollutant = poll, threshold = names(thresholds),
               value = unname(thresholds), station_hours = sum(is.finite(v)),
               hours_at_threshold = vapply(thresholds,
                                           function(t) sum(v == t, na.rm = TRUE),
                                           numeric(1)))
  }))
}))
threshold_ties[, share_at_threshold := hours_at_threshold / station_hours]

checks <- rbind(
  data.table(check = "sample_power_invariance", cases = nrow(power_invariance),
             failed = sum(!power_invariance$passed), max_error = NA_real_),
  data.table(check = "sample_outcome_invariance", cases = nrow(outcome_invariance),
             failed = sum(!outcome_invariance$passed), max_error = NA_real_),
  data.table(check = "sample_buffer_nesting", cases = nrow(buffer_nesting),
             failed = sum(!buffer_nesting$passed), max_error = NA_real_),
  data.table(check = "single_station_power_invariance", cases = nrow(single_station),
             failed = sum(!single_station$passed),
             max_error = max(single_station$max_error)),
  data.table(check = "single_station_threshold_hours", cases = nrow(single_station),
             failed = NA_integer_, max_error = max(single_station$max_hour_difference)),
  data.table(check = "assignment_equals_sample", cases = nrow(contrasts),
             failed = sum(!contrasts$sample_matches_assignment), max_error = NA_real_),
  data.table(check = "variance_identity", cases = nrow(contrasts), failed = NA_integer_,
             max_error = max(abs(contrasts$variance_identity_error), na.rm = TRUE)),
  data.table(check = "sample_nested_with_reference", cases = nrow(decomposition),
             failed = sum(!decomposition$nested), max_error = NA_real_),
  data.table(check = "decomposition_identity", cases = nrow(decomposition),
             failed = sum(abs(decomposition$identity_error) > 1e-10, na.rm = TRUE),
             max_error = max(abs(decomposition$identity_error), na.rm = TRUE)))
print(checks)

# ========================================================================================
# III: Save outputs
# ========================================================================================
# Save each table as Parquet and CSV in the processed folder and as CSV in results
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
outputs <- list(samples = samples, support_sizes = support_sizes,
                station_weights = station_weights, contrasts = contrasts,
                profiles = profiles, decomposition = decomposition,
                threshold_ties = threshold_ties, single_station = single_station,
                checks = checks)
for (name in names(outputs)) {
  resolution_write(outputs[[name]], file.path(dir_out, paste0("design_a_grid_", name)))
  fwrite(outputs[[name]], file.path(dir_tables, paste0("design_a_grid_", name, ".csv")))
}
