################################################################################
# STAGE 1: Base - System Dependencies
################################################################################
FROM rocker/rstudio:4.5.3 AS base

ENV DEBIAN_FRONTEND=noninteractive TZ=UTC

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential git curl ca-certificates pkg-config cmake \
    # Java for rJava/XLConnect
    default-jdk \
    # Geospatial Stack
    gdal-bin libgdal-dev libproj-dev proj-bin proj-data \
    libgeos-dev libudunits2-dev libsqlite3-dev \
    # Graphics & Text
    libpng-dev libjpeg-turbo8-dev libtiff-dev \
    libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    # System Utils
    pandoc libicu-dev libarchive-dev libzstd-dev liblz4-dev libsnappy-dev \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
 && rm -rf /var/lib/apt/lists/*

# IMPORTANT: Register Java with R for rJava/XLConnect
RUN R CMD javareconf

################################################################################
# STAGE 2: Builder - Restore R Packages
################################################################################
FROM base AS builder

WORKDIR /air_monitoring

# Copy renv metadata for caching
COPY renv.lock .
COPY .Rprofile .
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Restore libraries (caching this layer)
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')" \
 && R -s -e "renv::restore(clean = TRUE)"

################################################################################
# STAGE 3: Final - Runtime
################################################################################
FROM base AS final

WORKDIR /air_monitoring

# 1. Copy the pre-built library from builder
COPY --from=builder /air_monitoring/renv /air_monitoring/renv

# 2. Copy the full project code
COPY . .

# 3. RStudio & Permissions Setup
RUN mkdir -p /home/rstudio/.config/rstudio \
 && chown -R rstudio:rstudio /air_monitoring /home/rstudio/.config

# Copy Preferences & Force Project Loading
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
RUN echo "source('/air_monitoring/.Rprofile')" >> /home/rstudio/.Rprofile

# 4. Runtime Configuration
EXPOSE 8787
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/init"]