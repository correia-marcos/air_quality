################################################################################
# STAGE 1: Base - System Dependencies (Ubuntu Noble)
################################################################################
FROM rocker/r-ver:4.5.1 AS base

# Prevent apt-get prompts
ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC

# Install System Dependencies (Geospatial, Java, Fonts, etc.)
# These rarely change, so we do them first to maximize caching.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    build-essential git curl ca-certificates pkg-config cmake \
    default-jdk \
    # Geospatial Stack
    gdal-bin libgdal-dev libproj-dev proj-bin proj-data \
    libgeos-dev libudunits2-dev libsqlite3-dev \
    # Graphics / Text
    libpng-dev libjpeg-turbo8-dev libtiff-dev \
    libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    pandoc libicu-dev \
    # Compression / Archives
    libarchive-dev libzstd-dev liblz4-dev libsnappy-dev bzip2 xz-utils \
    # Network / Security
    libxml2-dev libssl-dev libcurl4-openssl-dev \
 && rm -rf /var/lib/apt/lists/*

# Configure R Repositories (RSPM + CRAN + R-Universe)
# This ensures fast binary downloads for Linux
ARG PPM_DATE=2025-09-01
ENV RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/${PPM_DATE}"

RUN mkdir -p /usr/local/lib/R/etc \
 && cat >/usr/local/lib/R/etc/Rprofile.site <<'RPROF'
options(repos = c(
  RSPM      = Sys.getenv("RENV_CONFIG_PPM_URL"),
  RUniverse = "https://ropensci.r-universe.dev",
  CRAN      = "https://cran.rstudio.com/"
))
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])))
RPROF

################################################################################
# STAGE 2: Builder - Restore R Packages
################################################################################
FROM base AS builder

WORKDIR /air_monitoring

# 1. Setup renv directory structure
RUN mkdir -p renv

# 2. Copy ONLY the files needed for package restoration
#    (This allows Docker to cache this layer if code changes but packages don't)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# 3. Install renv and Restore
#    We use renv::restore() to install everything into the local project library
ENV RENV_PATHS_LIBRARY=/air_monitoring/renv/library
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')" \
 && R -s -e "renv::restore(clean = TRUE)"

################################################################################
# STAGE 3: Final - Runtime
################################################################################
FROM rocker/rstudio:4.5.1 AS final

# Re-install runtime system dependencies (RStudio image doesn't inherit from Base)
# (Ideally, you'd merge these, but sticking to Rocker structure is safer)
ENV DEBIAN_FRONTEND=noninteractive TZ=UTC
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin libgdal-dev libproj-dev proj-bin libgeos-dev libudunits2-0 \
    default-jdk pandoc libicu74 libarchive13 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /air_monitoring

# 1. Copy the restored project from the Builder stage
COPY --from=builder /air_monitoring /air_monitoring

# 2. Copy the rest of your application code
#    (This layer changes frequently, so it comes last)
COPY . .

# 3. Ensure permissions for RStudio user
RUN chown -R rstudio:rstudio /air_monitoring

# 4. Critical: Ensure RStudio loads the .Rprofile on startup
#    This forces it to recognize the 'renv' library we just restored.
RUN echo "source('/air_monitoring/.Rprofile')" >> /home/rstudio/.Rprofile

# 5. Runtime Config
EXPOSE 8787
CMD ["/init"]