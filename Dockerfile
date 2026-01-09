################################################################################
# STAGE 1: Base - System Dependencies (Ubuntu Noble)
################################################################################
FROM rocker/r-ver:4.5.1 AS base

# Prevent apt-get prompts
ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC

# Install System Dependencies
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

# Configure R Repositories
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

# 2. Copy ONLY files needed for restoration (Caching Layer)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# 3. Restore Packages
#    (Note: This relies on your updated local renv.lock containing all packages)
ENV RENV_PATHS_LIBRARY=/air_monitoring/renv/library
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')" \
 && R -s -e "renv::restore(clean = TRUE)"

################################################################################
# STAGE 3: Final - Runtime
################################################################################
FROM rocker/rstudio:4.5.1 AS final

# Re-install runtime dependencies
ENV DEBIAN_FRONTEND=noninteractive TZ=UTC
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin libgdal-dev libproj-dev proj-bin libgeos-dev libudunits2-0 \
    default-jdk pandoc libicu74 libarchive13 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /air_monitoring

# 1. Copy the restored library from Builder (The heavy lifting)
COPY --from=builder /air_monitoring /air_monitoring

# 2. Copy the rest of the code (The frequent changes)
#    This includes your UPDATED .Rprofile
COPY . .

# 3. Fix Permissions
RUN chown -R rstudio:rstudio /air_monitoring

# 4. Trampoline: Force RStudio to load the project profile
#    This connects /home/rstudio (start) -> /air_monitoring (project)
RUN echo "source('/air_monitoring/.Rprofile')" >> /home/rstudio/.Rprofile

# 5. Runtime Config
EXPOSE 8787

# Copy Entrypoint
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/init"]