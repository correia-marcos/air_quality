################################################################################
# STAGE 1: Builder — fast binary installs via PPM + renv restore
################################################################################
FROM rocker/r-ver:4.5.1 AS builder

LABEL \
  org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
  org.opencontainers.image.version="v1.0.0" \
  maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

ARG PPM_DATE=2025-09-01
ENV DEBIAN_FRONTEND=noninteractive

# 0) Build toolchain & headers for source builds (sf/terra/arrow/textshaping/etc.)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential git curl ca-certificates pkg-config cmake libgit2-dev \
    gdal-bin libgdal-dev libproj-dev libgeos-dev libudunits2-dev \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libpng-dev libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libx11-dev xz-utils pandoc libarchive-dev\
 && update-ca-certificates && rm -rf /var/lib/apt/lists/*

# Keep {duckdb} from doing CRAN-specific parallel tuning tricks
ENV NOT_CRAN=true
# Optional: skip building vignettes to speed up renv restore
ENV RENV_CONFIGURE_BUILD_VIGNETTES=FALSE

# 1) PPM snapshot (Ubuntu noble) + CRAN fallback
ENV RENV_CONFIG_PPM_ENABLED=TRUE \
    RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/${PPM_DATE}"
RUN mkdir -p /usr/local/lib/R/etc && \
    printf '%s\n' \
'options(repos = c(' \
'  RSPM = Sys.getenv("RENV_CONFIG_PPM_URL"),' \
'  CRAN = "https://cran.rstudio.com/"' \
'))' \
'options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),' \
'  paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])))' \
    >> /usr/local/lib/R/etc/Rprofile.site

# 2) Project checkout (shallow clone for cache/bandwidth)
ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
ARG GIT_REF=main
RUN git clone --depth 1 --branch ${GIT_REF} ${REPO_URL} /air_monitoring
WORKDIR /air_monitoring

# 3) Prepare renv cache/library (layer-cached)
ENV RENV_PATHS_CACHE=/air_monitoring/renv/.cache
ENV RENV_PATHS_LIBRARY=/air_monitoring/renv/library
RUN mkdir -p renv/.cache renv/library

# 4) Copy minimal renv metadata first (better Docker layer caching)
COPY renv.lock renv/settings.json ./
COPY renv/activate.R renv/activate.R
COPY .Rprofile ./

# 5) Restore packages from lockfile (PPM snapshot + CRAN fallback)
RUN R -q -e "install.packages('renv'); renv::restore(prompt = FALSE)"
# Optional: verify
# RUN R -q -e "renv::status()"

################################################################################
# STAGE 2: Runtime — slim RStudio with runtime libs only
################################################################################
FROM rocker/rstudio:4.5.1 AS final

LABEL \
  org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
  org.opencontainers.image.version="v1.0.0" \
  maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

ENV DEBIAN_FRONTEND=noninteractive

# 0) Runtime deps (shared objects only; no -dev unless you need to compile inside)
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl git ca-certificates \
    locales tzdata \
    # geospatial runtime
    gdal-bin libgdal-dev libproj-dev libgeos-dev libudunits2-0 \
    # text rendering (ragg/systemfonts)
    libpng16-16 libfreetype6 libfontconfig1 libharfbuzz0b libfribidi0 \
    fonts-dejavu fonts-liberation \
    # parquet/arrow codecs
    libzstd1 liblz4-1 libsnappy1v5 libbrotli1 bzip2 \
    # doc rendering (knitr/rmarkdown)
    pandoc \
    # clean PID1 handling
    tini \
    # archive system requirements
    libarchive13 \
 && update-ca-certificates && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/# en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen \
 && sed -i 's/# pt_BR.UTF-8/pt_BR.UTF-8/' /etc/locale.gen \
 && locale-gen

# CA + locale + tz
ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
    CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt \
    GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    TZ=America/Sao_Paulo

# 1) Same PPM snapshot + CRAN fallback in runtime (for interactive installs)
ARG PPM_DATE=2025-09-01
ENV RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/${PPM_DATE}"
RUN mkdir -p /usr/local/lib/R/etc && \
    printf '%s\n' \
'options(repos = c(' \
'  RSPM = Sys.getenv("RENV_CONFIG_PPM_URL"),' \
'  CRAN = "https://cran.rstudio.com/"' \
'))' \
'options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),' \
'  paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])))' \
    >> /usr/local/lib/R/etc/Rprofile.site

# 2) Shared global renv cache (mount as named volume in compose)
ENV RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN mkdir -p /usr/local/lib/R/renv-cache \
 && echo "RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache" \
    >> /usr/local/lib/R/etc/Renviron.site

# 3) Bring in baked project + restored library from builder
COPY --from=builder /air_monitoring /air_monitoring

# 4) RStudio user prefs & profile
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
COPY .Rprofile           /home/rstudio/.Rprofile

# 5) Permissions
RUN chown -R rstudio:staff /air_monitoring \
 && chown -R rstudio:rstudio /home/rstudio/.config/rstudio \
 && chown rstudio:rstudio /home/rstudio/.Rprofile

WORKDIR /air_monitoring

# 6) Expose RStudio + healthcheck
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=5s --retries=5 \
  CMD curl -fsS http://localhost:8787/ || exit 1

# 7) Entrypoint (tini for clean shutdown) + your script
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/bin/tini","-g","--","/usr/local/bin/entrypoint.sh"]

# OPTIONAL: DuckDB CLI — uncomment to install
# ARG DUCKDB_CLI_VERSION=1.3.3
# RUN apt-get update && apt-get install -y --no-install-recommends unzip \
#  && arch="$(dpkg --print-architecture)" \
#  && case "$arch" in \
#       arm64) asset="duckdb_cli-linux-aarch64.zip" ;; \
#       amd64) asset="duckdb_cli-linux-amd64.zip" ;; \
#       *) echo "Unsupported arch: $arch" && exit 1 ;; \
#     esac \
#  && curl -L -o /tmp/duckdb.zip \
#       "https://github.com/duckdb/duckdb/releases/download/v${DUCKDB_CLI_VERSION}/${asset}" \
#  && unzip /tmp/duckdb.zip -d /usr/local/bin \
#  && chmod +x /usr/local/bin/duckdb \
#  && rm -f /tmp/duckdb.zip