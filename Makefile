# ============================================================================================
# Makefile - Air Monitoring replication pipeline
# ============================================================================================
# A thin, stage-level wrapper around the same R scripts that scripts/run_pipeline.R sources.
# It adds two things without touching any R code: (1) ordered stages, and (2) skip-unchanged
# rebuilds via stamp files. It is a convenience layer, NOT the reproducibility guarantee
# (that stays Docker + renv + here::here()). For a dependency-aware, R-native pipeline see
# doc/TARGETS_MIGRATION_PLAN.md.
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
        scatter imputed temporal figures tables context-maps resolution validate clean help

all: figures tables

$(STAMP):
	mkdir -p $(STAMP)

# 1. Process city data (data/raw -> data/processed). Assumes data/raw is populated.
process: $(STAMP)/process.stamp
$(STAMP)/process.stamp: scripts/process_data/process_bogota_data.R \
                        scripts/process_data/process_cdmx_data.R \
                        scripts/process_data/process_santiago_data.R \
                        scripts/process_data/process_sao_paulo_data.R \
                        $(SRC) | $(STAMP)
	$(RUN) scripts/process_data/process_bogota_data.R
	$(RUN) scripts/process_data/process_cdmx_data.R
	$(RUN) scripts/process_data/process_santiago_data.R
	$(RUN) scripts/process_data/process_sao_paulo_data.R
	touch $@

# 2. Distance matrices (census tracts <-> stations); needs processed data.
distances: $(STAMP)/distances.stamp
$(STAMP)/distances.stamp: scripts/process_data/generate_distance_matrices.R $(STAMP)/process.stamp
	$(RUN) scripts/process_data/generate_distance_matrices.R
	touch $@

# 3. Outlier detection.
outliers: $(STAMP)/outliers.stamp
$(STAMP)/outliers.stamp: scripts/process_data/detect_outliers.R $(STAMP)/process.stamp
	$(RUN) scripts/process_data/detect_outliers.R
	touch $@

# 4. IDW exposure, then the group regressions the figures read; needs distances and outliers.
exposure: $(STAMP)/exposure.stamp
$(STAMP)/exposure.stamp: scripts/process_data/estimate_idw.R \
                         scripts/process_data/estimate_exposure.R \
                         $(STAMP)/distances.stamp $(STAMP)/outliers.stamp
	$(RUN) scripts/process_data/estimate_idw.R
	$(RUN) scripts/process_data/estimate_exposure.R
	touch $@

# 5. Descriptive statistics: station counts, missing shares, WHO exceedances, census
#    summary. Needs the cleaned panels, the distance matrices and the processed census.
descriptives: $(STAMP)/descriptives.stamp
$(STAMP)/descriptives.stamp: scripts/process_data/compute_descriptive_tables.R \
                             scripts/process_data/compute_distance_band_descriptives.R \
                             $(STAMP)/distances.stamp $(STAMP)/outliers.stamp
	$(RUN) scripts/process_data/compute_descriptive_tables.R
	$(RUN) scripts/process_data/compute_distance_band_descriptives.R
	touch $@

# 5b. Station-level socioeconomic inputs. plot_station_monitoring_figures.R reads these,
#     so `figures` depends on this target rather than on whatever is left on disk.
scatter: $(STAMP)/scatter.stamp
$(STAMP)/scatter.stamp: scripts/process_data/compute_station_scatter_inputs.R \
                        $(STAMP)/outliers.stamp
	$(RUN) scripts/process_data/compute_station_scatter_inputs.R
	touch $@

# 5c. Imputed panels for the robustness specification, plus the fitted values its
#     diagnostics figures plot. One OLS per station, so this is the slow descriptive step.
imputed: $(STAMP)/imputed.stamp
$(STAMP)/imputed.stamp: scripts/process_data/impute_missing_hourly.R \
                        scripts/process_data/estimate_exposure_imputed.R \
                        $(STAMP)/outliers.stamp $(STAMP)/distances.stamp
	$(RUN) scripts/process_data/impute_missing_hourly.R
	$(RUN) scripts/process_data/estimate_exposure_imputed.R
	touch $@

# 6. Publication artefacts. Read only from data/processed; regenerate on demand (phony).
figures: exposure scatter imputed descriptives temporal
	$(RUN) scripts/tables_images/generate_exposure_plots.R
	$(RUN) scripts/tables_images/figure_imputation_diagnostics.R
	$(RUN) scripts/tables_images/plot_station_monitoring_figures.R
	$(RUN) scripts/tables_images/figure_station_scatter.R
	$(RUN) scripts/tables_images/figure_population_density_maps.R
	$(RUN) scripts/tables_images/figure_pollution_quintile_maps.R
	$(RUN) scripts/tables_images/figure_kernel_distributions.R
	$(RUN) scripts/tables_images/figure_quintile_kernel_distributions.R
	$(RUN) scripts/tables_images/figure_station_temporal.R

tables: exposure descriptives
	$(RUN) scripts/tables_images/render_station_tables.R
	$(RUN) scripts/tables_images/render_missing_tables.R
	$(RUN) scripts/tables_images/render_census_tables.R
	$(RUN) scripts/tables_images/render_exposure_tables.R

# Shared temporal inputs preserve MERRA-2 timestamp support for manuscript figures.
temporal: $(STAMP)/temporal.stamp
$(STAMP)/temporal.stamp: scripts/process_data/generate_panel_air_quality.R \
                         scripts/process_data/prepare_station_temporal.R $(SRC) | $(STAMP)
	$(RUN) scripts/process_data/generate_panel_air_quality.R
	$(RUN) scripts/process_data/prepare_station_temporal.R
	touch $@

# Optional comparisons reuse the city series, adding country/NASA and inversion inputs.
merra2: temporal
	$(RUN) scripts/process_data/process_merra2_panels.R
	$(RUN) scripts/tables_images/figure_merra2_vs_stations.R
	$(RUN) scripts/tables_images/figure_aerosol_composition.R

# Resolution sensitivity re-keys the Bogota exposure to five nested census geographies.
# Deliberately NOT a prerequisite of `all`: methodological, and not yet cited by the draft.
resolution: exposure
	$(RUN) scripts/process_data/build_bogota_localidad_crosswalk.R
	$(RUN) scripts/process_data/estimate_resolution_sensitivity.R
	$(RUN) scripts/tables_images/figure_resolution_sensitivity.R

# Context maps are not manuscript artifacts; the terrain version may contact Stadia.
context-maps: process
	$(RUN) scripts/tables_images/figure_study_area_maps.R

# ---- Optional / manual ---------------------------------------------------------------------
# Large, credential-gated raw pulls. Deliberately NOT a prerequisite of `all`.
download:
	$(RUN) scripts/download_data/download_bogota_data.R
	$(RUN) scripts/download_data/download_cdmx_data.R
	$(RUN) scripts/download_data/download_santiago_data.R
	$(RUN) scripts/download_data/download_sao_paulo_data.R
	$(RUN) scripts/download_data/download_merra2_data.R

# Legacy validation track (internal audit; separate from the paper pipeline).
validate:
	$(RUN) scripts/validation_old_version/compare_bogota.R

# Remove stage stamps only. Data and results are left untouched on purpose.
clean:
	rm -rf $(STAMP)

help:
	@echo "Targets: all process distances outliers exposure descriptives scatter"
	@echo "         imputed temporal figures tables"
	@echo "         merra2 context-maps resolution download validate clean"
	@echo "Add DOCKER=1 to run each step inside the compose \"analysis\" service."


.PHONY: test test-release verify verify-full export-dry-run
test:
	$(RUN) tests/testthat.R
test-release:
	$(RUN) tests/testthat.R --mode=release
verify:
	$(RUN) scripts/verification/verify.R
verify-full:
	$(RUN) scripts/verification/verify.R --full
export-dry-run:
	$(RUN) scripts/export/export_paper.R --destination data/verification/export-preview --dry-run
