################################################################################
# STAGE 1: Builder — clone repo + renv restore (Ubuntu noble)
################################################################################
FROM rocker/r-ver:4.5.1 AS builder
# rocker/r-ver:4.5.1 is Ubuntu noble on arm64 at the moment

LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
ARG GIT_REF=main
ARG PPM_DATE=2025-09-01

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC

# Toolchain & headers for source builds (sf/terra/arrow/stringi/etc.)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential git curl ca-certificates pkg-config cmake libgit2-dev \
    gdal-bin libgdal-dev libproj-dev proj-bin proj-data \
    libgeos-dev libudunits2-dev libsqlite3-dev \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libpng-dev libjpeg-turbo8-dev libtiff-dev \
    libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libx11-dev xz-utils bzip2 libarchive-dev libzstd-dev liblz4-dev libsnappy-dev \
    pandoc libicu-dev \
 && update-ca-certificates && rm -rf /var/lib/apt/lists/*

# PPM (Ubuntu noble) + CRAN + r-universe (for rnaturalearthhires)
ENV RENV_CONFIG_PPM_ENABLED=TRUE \
    RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/${PPM_DATE}" \
    NOT_CRAN=true \
    RENV_CONFIGURE_BUILD_VIGNETTES=FALSE
RUN mkdir -p /usr/local/lib/R/etc && \
    printf '%s\n' \
'options(repos = c(' \
'  RSPM = Sys.getenv("RENV_CONFIG_PPM_URL"),' \
'  RUniverse = "https://ropensci.r-universe.dev",' \
'  CRAN = "https://cran.rstudio.com/"' \
'))' \
'options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),' \
'  paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])))' \
    >> /usr/local/lib/R/etc/Rprofile.site

# Clone project (shallow) and restore with renv
WORKDIR /air_monitoring
RUN git clone --depth 1 --branch "${GIT_REF}" "${REPO_URL}" /air_monitoring

# Local renv cache/library (cached layers)
ENV RENV_PATHS_CACHE=/air_monitoring/renv/.cache \
    RENV_PATHS_LIBRARY=/air_monitoring/renv/library \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN mkdir -p renv/.cache renv/library

RUN R -q -e "install.packages('renv'); renv::restore(prompt = FALSE)"

################################################################################
# STAGE 2: Runtime — RStudio + LaTeX + geospatial + fonts (Ubuntu noble)
################################################################################
FROM rocker/rstudio:4.5.1 AS final
# rocker/rstudio:4.5.1 is Ubuntu noble on arm64 at the moment

LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
    CURL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt \
    GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt

# Runtime libs: geospatial, LaTeX, fonts, codecs, ICU runtime, pandoc
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl git ca-certificates locales tzdata tini \
    gdal-bin libgdal-dev libproj-dev proj-bin proj-data \
    libgeos-dev libudunits2-0 libsqlite3-0 \
    libpng16-16 libjpeg-turbo8 libtiff6 \
    libfreetype6 libfontconfig1 libharfbuzz0b libfribidi0 \
    fonts-dejavu fonts-liberation fonts-noto fonts-noto-color-emoji \
    libzstd1 liblz4-1 libsnappy1v5 libbrotli1 bzip2 \
    pandoc \
    texlive-latex-base texlive-latex-recommended texlive-latex-extra \
    texlive-fonts-recommended texlive-fonts-extra texlive-plain-generic \
    texlive-xetex texlive-luatex lmodern latexmk ghostscript dvipng cm-super \
    texlive-lang-english texlive-lang-spanish texlive-lang-portuguese \
    libarchive13 libicu74 \
 && update-ca-certificates && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/# en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen \
 && locale-gen \
 && fc-cache -f -v

# PPM repo options at runtime too
ARG PPM_DATE=2025-09-01
ENV RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/${PPM_DATE}" \
    RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN mkdir -p /usr/local/lib/R/renv-cache \
 && echo "RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache" \
    >> /usr/local/lib/R/etc/Renviron.site \
 && mkdir -p /usr/local/lib/R/etc \
 && printf '%s\n' \
'options(repos = c(' \
'  RSPM = Sys.getenv("RENV_CONFIG_PPM_URL"),' \
'  RUniverse = "https://ropensci.r-universe.dev",' \
'  CRAN = "https://cran.rstudio.com/"' \
'))' \
'options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),' \
'  paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])))' \
    >> /usr/local/lib/R/etc/Rprofile.site

# Bring in baked project + restored library from builder
COPY --from=builder /air_monitoring /air_monitoring

# Ownership (best-effort on arm64)
RUN chown -R rstudio:staff /air_monitoring || true \
 && chown -R rstudio:rstudio /home/rstudio || true

WORKDIR /air_monitoring

# RStudio server
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=5s --retries=5 \
  CMD curl -fsS http://localhost:8787/ || exit 1

ENTRYPOINT ["/usr/bin/tini","-g","--"]
CMD ["/init"]

# OPTIONAL: DuckDB CLI
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