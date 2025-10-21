################################################################################
# STAGE 1: Builder — clone repo + renv restore (Ubuntu noble)
################################################################################
FROM rocker/r-ver:4.5.1 AS builder
# NOTE: rocker/r-ver:4.5.1 is Ubuntu 24.04 "noble" (multi-arch: amd64/arm64)

# Use bash with pipefail for all RUN instructions in this stage
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Step 1 — Metadata, args, and base env
LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
ARG GIT_REF=main
ARG PPM_DATE=2025-09-01

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    NOT_CRAN=true \
    RENV_CONFIGURE_BUILD_VIGNETTES=FALSE

# Step 2 — Toolchain & headers for source builds (sf/terra/arrow/stringi/etc.)
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    build-essential git curl ca-certificates pkg-config cmake libgit2-dev \
    gdal-bin libgdal-dev libproj-dev proj-bin proj-data \
    libgeos-dev libudunits2-dev libsqlite3-dev \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libpng-dev libjpeg-turbo8-dev libtiff-dev \
    libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libx11-dev xz-utils bzip2 libarchive-dev libzstd-dev liblz4-dev libsnappy-dev \
    pandoc libicu-dev \
 && update-ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Step 3 — Configure R repositories (RSPM + r-universe + CRAN) via heredoc
ENV RENV_CONFIG_PPM_ENABLED=TRUE \
    RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/${PPM_DATE}"

RUN mkdir -p /usr/local/lib/R/etc \
 && cat >/usr/local/lib/R/etc/Rprofile.site <<'RPROF'
options(repos = c(
  RSPM      = Sys.getenv("RENV_CONFIG_PPM_URL"),
  RUniverse = "https://ropensci.r-universe.dev",
  CRAN      = "https://cran.rstudio.com/"
))
options(HTTPUserAgent = sprintf(
  "R/%s R (%s)",
  getRversion(),
  paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])
))
RPROF

# Step 4 — Clone project (shallow) for better cache reuse
WORKDIR /air_monitoring
RUN git clone --depth 1 --branch "${GIT_REF}" "${REPO_URL}" /air_monitoring

# Step 5 — Prepare renv cache/library (layer-cached)
ENV RENV_PATHS_CACHE=/air_monitoring/renv/.cache \
    RENV_PATHS_LIBRARY=/air_monitoring/renv/library \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN mkdir -p renv/.cache renv/library

# Step 6 — Restore packages from renv.lock (PPM snapshot + CRAN + r-universe)
#          Also ensure graphics/font pkgs are compiled here so they load at runtime.
ARG EXTRA_R_PKGS="showtext sysfonts showtextdb svglite zoo"
RUN R -q -e "options(renv.verbose = FALSE); install.packages('renv', quiet = TRUE); \
             renv::restore(prompt = FALSE); \
             ep <- strsplit(Sys.getenv('EXTRA_R_PKGS'), ' +')[[1]]; ep <- ep[nzchar(ep)]; \
             if (length(ep)) renv::install(ep); \
             renv::snapshot(prompt = FALSE)"

################################################################################
# STAGE 2: Runtime — RStudio + LaTeX + geospatial + fonts (Ubuntu noble)
################################################################################
FROM rocker/rstudio:4.5.1 AS final
# NOTE: rocker/rstudio:4.5.1 is Ubuntu 24.04 "noble" (multi-arch: amd64/arm64)

# Use bash with pipefail for all RUN instructions in this stage
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Step 1 — Runtime env (UTC, locale, CA trust)
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

# Step 2 — Runtime libs: geospatial, LaTeX, fonts, codecs, ICU runtime, pandoc
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    curl git ca-certificates locales tzdata tini \
    # geospatial runtime
    gdal-bin libgdal-dev libproj-dev proj-bin proj-data \
    libgeos-dev libudunits2-0 libsqlite3-0 \
    # raster/graphics codecs
    libpng16-16 libjpeg-turbo8 libtiff6 \
    # font stack for ragg/showtext/systemfonts
    libfreetype6 libfontconfig1 libharfbuzz0b libfribidi0 \
    fonts-dejavu fonts-liberation fonts-noto fonts-noto-color-emoji \
    # parquet/arrow codecs
    libzstd1 liblz4-1 libsnappy1v5 libbrotli1 bzip2 \
    # doc rendering (knitr/rmarkdown)
    pandoc \
    # LaTeX (XeLaTeX/LuaLaTeX + language packs)
    texlive-latex-base texlive-latex-recommended texlive-latex-extra \
    texlive-fonts-recommended texlive-fonts-extra texlive-plain-generic \
    texlive-xetex texlive-luatex lmodern latexmk ghostscript dvipng cm-super \
    texlive-lang-english texlive-lang-spanish texlive-lang-portuguese \
    # archives + ICU runtime
    libarchive13 libicu74 \
 && update-ca-certificates \
 && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/# en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen \
 && locale-gen \
 && fc-cache -f -v

# Step 3 — Runtime R repos + shared renv cache (heredoc + Renviron.site)
ARG PPM_DATE=2025-09-01
ENV RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/ubuntu/noble/${PPM_DATE}" \
    RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE

RUN mkdir -p /usr/local/lib/R/renv-cache /usr/local/lib/R/etc \
 && echo "RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache" >> /usr/local/lib/R/etc/Renviron.site \
 && cat >/usr/local/lib/R/etc/Rprofile.site <<'RPROF'
options(repos = c(
  RSPM      = Sys.getenv("RENV_CONFIG_PPM_URL"),
  RUniverse = "https://ropensci.r-universe.dev",
  CRAN      = "https://cran.rstudio.com/"
))
options(HTTPUserAgent = sprintf(
  "R/%s R (%s)",
  getRversion(),
  paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])
))
RPROF

# Step 4 — Copy baked project + restored library from builder
COPY --from=builder /air_monitoring /air_monitoring

# Step 4.1 — RStudio preferences (open in project, etc.)
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json

# Step 5 — Ownership (best-effort across arches) & working directory
RUN chown -R rstudio:staff /air_monitoring /home/rstudio/.config || true \
 && chown -R rstudio:rstudio /home/rstudio || true
WORKDIR /air_monitoring

# Step 6 — RStudio server: port + healthcheck + entrypoint
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=5s --retries=5 \
  CMD curl -fsS http://localhost:8787/ || exit 1

# Step 7 — Entrypoint (tini for clean shutdown) + your script
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/bin/tini","-g","--","/usr/local/bin/entrypoint.sh"]