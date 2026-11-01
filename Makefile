# ============================================================================================
# Makefile - Air Monitoring replication pipeline
# ============================================================================================
# A thin, stage-level wrapper around the same R scripts that scripts/run_pipeline.R sources.
# It adds two things without touching any R code: (1) ordered stages, and (2) skip-unchanged
# rebuilds via stamp files. Scientific reproduction needs executed, reviewed verification.
# Docker and renv record the environment. For a dependency-aware, R-native pipeline see
# doc/planning/targets-migration.md. Targets is opt-in until parity is accepted.
#
# Usage:
#   make                 # build the analysis: process -> distances -> outliers -> exposure
#                        #                      -> descriptives + scatter -> figures + tables
#   make process         # run a single stage (and anything it depends on)
#   make DOCKER=1        # run every recipe inside the compose "analysis" service
#   make download        # large, credential-gated raw pulls (never part of `make all`)
#   make merra2          # satellite track: panels, station comparison, aerosol figures
#   make validate        # legacy comparison track
#   make clean           # remove stage stamps (does NOT delete data or results)
#   make help
# ============================================================================================

# ---- Config --------------------------------------------------------------------------------
R      := Rscript
DOCKER ?= 0
ifeq ($(DOCKER),1)
RUN := docker compose run --rm analysis Rscript
else
RUN := $(R)
endif

# Stamp dir lives under data/ so it inherits data/'s .gitignore (no gitignore edit needed).
STAMP := data/.make
# If a src/ function changes, dependent stages should rebuild. `find` rather than `wildcard`
# so the stamps still track src/ once its files sit in subdirectories.
SRC := $(shell find src -name '*.R')

# ---- Phony convenience targets -------------------------------------------------------------
.PHONY: all download process merra2 distances outliers exposure descriptives \
        scatter imputed temporal figures tables context-maps resolution \
        specification-grid specification-panel specification-station-sets \
        validate clean help

all: figures tables

$(STAMP):
	mkdir -p $(STAMP)

# 1. Process city data (data/raw -> data/processed). Assumes data/raw is populated.
process: $(STAMP)/process.stamp
$(STAMP)/process.stamp: scripts/process_data/inputs/process_bogota_data.R \
                        scripts/process_data/inputs/process_cdmx_data.R \
                        scripts/process_data/inputs/process_santiago_data.R \
                        scripts/process_data/inputs/process_sao_paulo_data.R \
                        $(SRC) | $(STAMP)
	$(RUN) scripts/process_data/inputs/process_bogota_data.R
	$(RUN) scripts/process_data/inputs/process_cdmx_data.R
	$(RUN) scripts/process_data/inputs/process_santiago_data.R
	$(RUN) scripts/process_data/inputs/process_sao_paulo_data.R
	touch $@

# 2. Distance matrices (census tracts <-> stations); needs processed data.
distances: $(STAMP)/distances.stamp
$(STAMP)/distances.stamp: scripts/process_data/inputs/generate_distance_matrices.R \
                          $(STAMP)/process.stamp
	$(RUN) scripts/process_data/inputs/generate_distance_matrices.R
	touch $@

# 3. Outlier detection.
outliers: $(STAMP)/outliers.stamp
$(STAMP)/outliers.stamp: scripts/process_data/inputs/detect_outliers.R \
                         $(STAMP)/distances.stamp
	$(RUN) scripts/process_data/inputs/detect_outliers.R
	touch $@

# 4. IDW exposure, then the group regressions the figures read; needs distances and outliers.
exposure: $(STAMP)/exposure.stamp
$(STAMP)/exposure.stamp: scripts/process_data/exposure/estimate_idw.R \
                         scripts/process_data/exposure/estimate_exposure.R \
                         $(STAMP)/distances.stamp $(STAMP)/outliers.stamp
	$(RUN) scripts/process_data/exposure/estimate_idw.R
	$(RUN) scripts/process_data/exposure/estimate_exposure.R
	touch $@

# 5. Descriptive statistics: station counts, missing shares, WHO exceedances, census
#    summary. Needs the cleaned panels, the distance matrices and the processed census.
descriptives: $(STAMP)/descriptives.stamp
$(STAMP)/descriptives.stamp: \
    scripts/process_data/descriptives/compute_descriptive_tables.R \
    scripts/process_data/descriptives/compute_distance_band_descriptives.R \
    $(STAMP)/distances.stamp $(STAMP)/outliers.stamp
	$(RUN) scripts/process_data/descriptives/compute_descriptive_tables.R
	$(RUN) scripts/process_data/descriptives/compute_distance_band_descriptives.R
	touch $@

# 5b. Station-level socioeconomic inputs. plot_station_monitoring_figures.R reads these,
#     so `figures` depends on this target rather than on whatever is left on disk.
scatter: $(STAMP)/scatter.stamp
$(STAMP)/scatter.stamp: scripts/process_data/monitoring/compute_station_scatter_inputs.R \
                        $(STAMP)/outliers.stamp
	$(RUN) scripts/process_data/monitoring/compute_station_scatter_inputs.R
	touch $@

# 5c. Imputed panels for the robustness specification, plus the fitted values its
#     diagnostics figures plot. One OLS per station, so this is the slow descriptive step.
imputed: $(STAMP)/imputed.stamp
$(STAMP)/imputed.stamp: scripts/process_data/imputation/impute_missing_hourly.R \
                        scripts/process_data/imputation/estimate_exposure_imputed.R \
                        $(STAMP)/outliers.stamp $(STAMP)/distances.stamp
	$(RUN) scripts/process_data/imputation/impute_missing_hourly.R
	$(RUN) scripts/process_data/imputation/estimate_exposure_imputed.R
	touch $@

# 6. Publication artefacts. Read only from data/processed; regenerate on demand (phony).
figures: exposure scatter imputed descriptives temporal
	$(RUN) scripts/tables_images/exposure/generate_exposure_plots.R
	$(RUN) scripts/tables_images/imputation/figure_imputation_diagnostics.R
	$(RUN) scripts/tables_images/monitoring/plot_station_monitoring_figures.R
	$(RUN) scripts/tables_images/monitoring/figure_station_scatter.R
	$(RUN) scripts/tables_images/monitoring/figure_population_density_maps.R
	$(RUN) scripts/tables_images/monitoring/figure_pollution_quintile_maps.R
	$(RUN) scripts/tables_images/temporal/figure_kernel_distributions.R
	$(RUN) scripts/tables_images/exposure/figure_quintile_kernel_distributions.R
	$(RUN) scripts/tables_images/temporal/figure_station_temporal.R

tables: exposure descriptives
	$(RUN) scripts/tables_images/descriptives/render_station_tables.R
	$(RUN) scripts/tables_images/descriptives/render_missing_tables.R
	$(RUN) scripts/tables_images/descriptives/render_census_tables.R
	$(RUN) scripts/tables_images/exposure/render_exposure_tables.R

# Observed station-only summaries supply the manuscript appendix.
temporal: $(STAMP)/temporal.stamp
$(STAMP)/temporal.stamp: scripts/process_data/temporal/prepare_station_hourly.R $(SRC) \
                         $(STAMP)/outliers.stamp | $(STAMP)
	$(RUN) scripts/process_data/temporal/prepare_station_hourly.R
	touch $@

# Satellite preparation and comparison are optional and use current city products.
merra2: temporal
	$(RUN) scripts/process_data/not_in_paper/satellite/generate_panel_air_quality_merra2.R
	$(RUN) scripts/process_data/not_in_paper/satellite/prepare_station_temporal.R
	$(RUN) scripts/process_data/not_in_paper/satellite/process_merra2_panels.R
	$(RUN) scripts/tables_images/not_in_paper/satellite/figure_merra2_vs_stations.R
	$(RUN) scripts/tables_images/not_in_paper/satellite/figure_aerosol_composition.R

# Specification sensitivity (geographic support, IDW buffer and exponent, education
# groupings). Optional and deliberately NOT a prerequisite of `all`. File map:
# scripts/process_data/specification_sensitivity/README.md
SPEC_PROCESS := scripts/process_data/specification_sensitivity
SPEC_FIGURES := scripts/tables_images/specification_sensitivity

# Robustness designs A/B/C on the variants declared in config/analysis_settings.R. They
# consume frozen derived inputs and never invoke `exposure`; the Bogota crosswalk
# (build_bogota_localidad_crosswalk.R) is a one-time input. One variant runs alone with
# `make targets STAGE=resolution_<id>`.
resolution:
	$(RUN) $(SPEC_PROCESS)/prepare_resolution_inputs.R
	$(RUN) $(SPEC_PROCESS)/estimate_resolution_sensitivity.R
	$(RUN) $(SPEC_FIGURES)/figure_resolution_sensitivity.R

# Primary Design A: fine IDW over the buffer x exponent grid, then Design A estimates with
# the fixed-sample decomposition. Needs the frozen inputs (prepare step).
specification-grid:
	$(RUN) $(SPEC_PROCESS)/estimate_idw_specification_grid.R
	$(RUN) $(SPEC_PROCESS)/estimate_design_a_grid.R

# One row per Design A specification, the exact variance decomposition and the grid
# figures. The panel's parity check needs the A/B/C robustness runs at 3 and 20 km.
specification-panel:
	$(RUN) $(SPEC_PROCESS)/build_specification_panel.R
	$(RUN) $(SPEC_PROCESS)/estimate_specification_variance_decomposition.R
	$(RUN) $(SPEC_FIGURES)/figure_design_a_grid.R

# Station sets declared in config/analysis_settings.R: IDW with fewer stations, Design A
# against the default stations, and their figures. Needs the frozen inputs and the grid.
specification-station-sets:
	$(RUN) $(SPEC_PROCESS)/estimate_idw_station_sets.R
	$(RUN) $(SPEC_PROCESS)/estimate_design_a_station_sets.R
	$(RUN) $(SPEC_FIGURES)/figure_station_sets.R

# Context maps are not manuscript artifacts; the terrain version may contact Stadia.
context-maps: process
	$(RUN) scripts/tables_images/not_in_paper/supporting/figure_study_area_maps.R

# ---- Optional / manual ---------------------------------------------------------------------
# Large, credential-gated raw pulls. Deliberately NOT a prerequisite of `all`.
download:
	$(RUN) scripts/download_data/inputs/download_bogota_data.R
	$(RUN) scripts/download_data/inputs/download_cdmx_data.R
	$(RUN) scripts/download_data/inputs/download_santiago_data.R
	$(RUN) scripts/download_data/inputs/download_sao_paulo_data.R
	$(RUN) scripts/download_data/not_in_paper/satellite/download_merra2_data.R

# Legacy validation track (internal audit; separate from the paper pipeline).
validate:
	$(RUN) scripts/process_data/not_in_paper/legacy_validation/compare_bogota.R

# Remove stage stamps only. Data and results are left untouched on purpose.
clean:
	rm -rf $(STAMP)

help:
	@echo "Targets: all process distances outliers exposure descriptives scatter"
	@echo "         imputed temporal figures tables"
	@echo "         merra2 context-maps resolution"
	@echo "         specification-grid specification-panel specification-station-sets"
	@echo "         download validate clean"
	@echo "Add DOCKER=1 to run each step inside the compose \"analysis\" service."


.PHONY: test test-release verify verify-full export-dry-run
test:
	$(RUN) tests/testthat.R
test-release:
	$(RUN) tests/testthat.R --mode=release
verify:
	$(RUN) tools/reproduction/verify.R
verify-full:
	$(RUN) tools/reproduction/verify.R --full
export-dry-run:
	$(RUN) tools/reproduction/export_paper.R --destination data/verification/export-preview --dry-run

# Candidate graph: stage prerequisites are declared only in the R graph.
STAGE ?= all
.PHONY: targets targets-outdated
targets:
	$(RUN) scripts/run_targets.R $(STAGE)
targets-outdated:
	$(RUN) scripts/run_targets.R $(STAGE) --outdated
