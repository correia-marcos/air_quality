# Specification-sensitivity target declarations: frozen inputs, the A/B/C robustness
# variants declared in config/analysis_settings.R and their review figures. Loaded by
# _targets_specification.R, never by the manuscript graph: these targets read manuscript
# products but never rebuild them. Scientific functions live in src/.
list(
  # The manuscript products the frozen inputs record, at every declared buffer.
  targets::tar_target(resolution_manuscript_files,
    {
      buffers <- unique(vapply(resolution_specifications, `[[`, 0, "buffer_km"))
      c(unlist(lapply(resolution_cities, function(city) {
          resolution_input_paths(city, resolution_city_supports(city))
        }), use.names = FALSE),
        here::here("data", "processed", "idw_estimates", rep(resolution_cities,
          each = length(setdiff(buffers, 3))),
          paste0(rep(resolution_cities, each = length(setdiff(buffers, 3))), "_",
                 setdiff(buffers, 3), "km_idw_exposure.parquet")))
    },
    format = "file"),

  # The retired Bogota reference, archived as the 3 km quintile anchor's input.
  targets::tar_target(resolution_archived_reference,
    here::here("data", "processed", "resolution_sensitivity",
               "resolution_ci_bogota_3km_2023.csv"),
    format = "file"),

  # The code recorded with every variant's tables.
  targets::tar_target(resolution_code_files,
    c("_targets_specification.R", "config/targets/specification_sensitivity.R",
      "src/general_utilities/config_utils_resolution.R",
      "src/general_utilities/process/specification_sensitivity/resolution_sensitivity.R",
      "src/general_utilities/process/specification_sensitivity/resolution_specification.R",
      "src/general_utilities/process/specification_sensitivity/resolution_workflow.R",
      "src/general_utilities/plot/specification_sensitivity/resolution_workflow.R",
      "src/general_utilities/process/idw_exposure.R",
      "src/general_utilities/process/distances.R",
      "src/general_utilities/process/exposure_regressions.R",
      "config/analysis_settings.R"),
    format = "file"),

  # Freeze each city's inputs and prepare its supports (prepare_resolution_inputs.R).
  targets::tar_target(resolution_inputs,
    {
      resolution_manuscript_files
      dir_frozen <- here::here("data", "interim", "resolution_sensitivity")
      dir_prepared <- resolution_buffer_root(3)
      for (city in resolution_cities) {
        frozen_dir <- file.path(dir_frozen, city)
        prepared_dir <- file.path(dir_prepared, city)
        supports <- resolution_city_supports(city)
        paths <- resolution_input_paths(city, supports)
        resolution_freeze_inputs(city, supports, paths, frozen_dir, prepared_dir)
        population <- resolution_population_cells(supports, paths, frozen_dir)
        geometry <- resolution_fine_geometry(city, supports, paths,
                                             population$census$geo_id, frozen_dir)
        resolution_population_denominators(city, supports, paths, population$population,
                                           population$cells, geometry, frozen_dir,
                                           prepared_dir)
        keys <- resolution_support_keys(city, supports, paths, population$census$geo_id,
                                        population$population, geometry, frozen_dir)
        resolution_support_matrices(city, supports, paths, population$population,
                                    population$cells, geometry, keys, frozen_dir,
                                    prepared_dir)
      }
      prepared <- file.path(dir_prepared, resolution_cities)
      written <- "^(population_denominators|geographic_audit)[.]|^matrix_.*[.]parquet$"
      c(file.path(dir_frozen, resolution_cities),
        unlist(lapply(prepared, list.files, pattern = written, recursive = TRUE,
                      full.names = TRUE)))
    },
    format = "file"),

  # One target per declared variant (estimate_resolution_sensitivity.R): designs A/B/C
  # for every city, the combined tables and the code provenance.
  targets::tar_target(resolution_quintile_3km,
    {
      resolution_inputs
      specification <- resolution_specification("quintile_3km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      resolution_reference_anchor(specification, resolution_archived_reference)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  targets::tar_target(resolution_quintile_20km,
    {
      resolution_inputs
      specification <- resolution_specification("quintile_20km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  # Other groupings reuse the quintile run's B exposure at the same buffer.
  targets::tar_target(resolution_quintile_split_3km,
    {
      resolution_quintile_3km
      specification <- resolution_specification("quintile_split_3km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  targets::tar_target(resolution_quintile_split_20km,
    {
      resolution_quintile_20km
      specification <- resolution_specification("quintile_split_20km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  targets::tar_target(resolution_education_group3_3km,
    {
      resolution_quintile_3km
      specification <- resolution_specification("education_group3_3km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  targets::tar_target(resolution_education_group3_20km,
    {
      resolution_quintile_20km
      specification <- resolution_specification("education_group3_20km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  targets::tar_target(resolution_education_level_3km,
    {
      resolution_quintile_3km
      specification <- resolution_specification("education_level_3km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  targets::tar_target(resolution_education_level_20km,
    {
      resolution_quintile_20km
      specification <- resolution_specification("education_level_20km",
        resolution_specifications, resolution_cities)
      lapply(resolution_cities, run_resolution_city, specification = specification,
             education_bands = education_level_bands,
             education_group_map = education_group3_of_level,
             duckdb_mem_gb = resolution_duckdb_mem_gb)
      c(write_resolution_tables(specification, resolution_combine_cities(specification),
                                resolution_verification_summary(specification),
                                resolution_gap_patterns(specification)),
        write_resolution_provenance(specification, resolution_code_files))
    },
    format = "file"),

  targets::tar_target(resolution_robustness,
    c(resolution_quintile_3km, resolution_quintile_20km, resolution_quintile_split_3km,
      resolution_quintile_split_20km, resolution_education_group3_3km,
      resolution_education_group3_20km, resolution_education_level_3km,
      resolution_education_level_20km),
    format = "file"),

  # Review figures of the 3 km variants (figure_resolution_sensitivity.R).
  targets::tar_target(resolution_figures_quintile_3km,
    {
      resolution_quintile_3km
      specification <- resolution_specification("quintile_3km",
        resolution_specifications, resolution_cities)
      labels <- resolution_grouping_labels[[specification$grouping]]
      set_paper_theme(base_size = 11)
      tables <- resolution_figure_tables(specification)
      save_resolution_figures(c(
        plot_resolution_designs(tables, specification$grouping, labels$group_labels,
                                labels$group_name, labels$gap_label, labels$profile_note),
        plot_resolution_monitoring(tables, labels$gap_label)), specification)
    },
    format = "file"),

  targets::tar_target(resolution_figures_quintile_split_3km,
    {
      resolution_quintile_split_3km
      specification <- resolution_specification("quintile_split_3km",
        resolution_specifications, resolution_cities)
      labels <- resolution_grouping_labels[[specification$grouping]]
      set_paper_theme(base_size = 11)
      tables <- resolution_figure_tables(specification)
      save_resolution_figures(c(
        plot_resolution_designs(tables, specification$grouping, labels$group_labels,
                                labels$group_name, labels$gap_label, labels$profile_note),
        plot_resolution_monitoring(tables, labels$gap_label)), specification)
    },
    format = "file"),

  targets::tar_target(resolution_figures_education_group3_3km,
    {
      resolution_education_group3_3km
      specification <- resolution_specification("education_group3_3km",
        resolution_specifications, resolution_cities)
      labels <- resolution_grouping_labels[[specification$grouping]]
      set_paper_theme(base_size = 11)
      tables <- resolution_figure_tables(specification)
      save_resolution_figures(c(
        plot_resolution_designs(tables, specification$grouping, labels$group_labels,
                                labels$group_name, labels$gap_label, labels$profile_note),
        plot_resolution_monitoring(tables, labels$gap_label)), specification)
    },
    format = "file"),

  targets::tar_target(resolution_figures_education_level_3km,
    {
      resolution_education_level_3km
      specification <- resolution_specification("education_level_3km",
        resolution_specifications, resolution_cities)
      labels <- resolution_grouping_labels[[specification$grouping]]
      set_paper_theme(base_size = 11)
      tables <- resolution_figure_tables(specification)
      save_resolution_figures(c(
        plot_resolution_designs(tables, specification$grouping, labels$group_labels,
                                labels$group_name, labels$gap_label, labels$profile_note),
        plot_resolution_monitoring(tables, labels$gap_label)), specification)
    },
    format = "file"),

  targets::tar_target(resolution_figures,
    c(resolution_figures_quintile_3km, resolution_figures_quintile_split_3km,
      resolution_figures_education_group3_3km, resolution_figures_education_level_3km),
    format = "file")
)
