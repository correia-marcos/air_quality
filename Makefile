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
#   make                 # build the analysis: process -> distances -> outliers -> exposure -> figures + tables
#   make process         # run a single stage (and anything it depends on)
#   make DOCKER=1        # run every recipe inside the compose "analysis" service
#   make download        # large, credential-gated raw pulls (never part of `make all`)
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
# If a src/ function changes, dependent stages should rebuild.
SRC := $(wildcard src/city_specific/*.R) $(wildcard src/general_utilities/*.R)

# ---- Phony convenience targets -------------------------------------------------------------
.PHONY: all download process distances outliers exposure figures tables validate clean help

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
$(STAMP)/distances.stamp: scripts/process_data/generate_distances_matrices.R $(STAMP)/process.stamp
	$(RUN) scripts/process_data/generate_distances_matrices.R
	touch $@

# 3. Outlier detection.
outliers: $(STAMP)/outliers.stamp
$(STAMP)/outliers.stamp: scripts/process_data/detect_outliers.R $(STAMP)/process.stamp
	$(RUN) scripts/process_data/detect_outliers.R
	touch $@

# 4. IDW exposure; needs both distances and outlier flags.
exposure: $(STAMP)/exposure.stamp
$(STAMP)/exposure.stamp: scripts/process_data/estimate_idw_exposure.R \
                         $(STAMP)/distances.stamp $(STAMP)/outliers.stamp
	$(RUN) scripts/process_data/estimate_idw_exposure.R
	touch $@

# 5. Publication artefacts. Read only from data/processed; regenerate on demand (phony).
#    Add each figure/table script here as you finish it.
figures: exposure
	$(RUN) scripts/tables_images/figure_exposure_by_quintile_with_ci.R
	$(RUN) scripts/tables_images/figure_hours_above_target_by_quintile.R

tables: exposure
	$(RUN) scripts/tables_images/table_who_exceedances.R
	$(RUN) scripts/tables_images/table_stations_by_pollutant.R

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
	@echo "Targets: all process distances outliers exposure figures tables download validate clean"
	@echo "Add DOCKER=1 to run each step inside the compose \"analysis\" service."

