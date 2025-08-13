################################################################################
# STAGE 1: Builder  — fast binary installs via PPM + renv restore
################################################################################
FROM rocker/r-ver:4.4.2 AS builder

LABEL \
  org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
  org.opencontainers.image.version="v1.0.0" \
  maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

# 0) System libs for building R packages (kept in builder only)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git curl ca-certificates \
    # geospatial / data packages
    libgdal-dev libudunits2-dev libxml2-dev libssl-dev libcurl4-openssl-dev \
    # plotting/text shaping
    libpng-dev libfreetype6-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    cmake pkg-config \
  && rm -rf /var/lib/apt/lists/*

# 1) Use Posit Package Manager (PPM) for Linux binaries (Debian bookworm)
ENV RENV_CONFIG_PPM_ENABLED=TRUE \
    RENV_CONFIG_PPM_URL="https://packagemanager.posit.co/cran/__linux__/bookworm/latest" \
    RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/bookworm/latest"

# Required UA so PPM serves binaries (Arrow docs recommend this)
RUN bash -lc 'cat >> /usr/local/lib/R/etc/Rprofile.site <<EOF
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/bookworm/latest"))
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])))
EOF'

# 2) Project checkout
ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
RUN git clone ${REPO_URL} /air_monitoring
WORKDIR /air_monitoring

# 3) Prepare renv cache/library (layer-cached)
ENV RENV_PATHS_CACHE=/air_monitoring/renv/.cache
ENV RENV_PATHS_LIBRARY=/air_monitoring/renv/library
RUN mkdir -p renv/.cache renv/library

# 4) Copy the minimal renv metadata first (for better Docker layer caching)
COPY renv.lock       renv/settings.json  ./
COPY renv/activate.R renv/activate.R
COPY .Rprofile       ./

# 5) Install renv (binary via PPM) and restore packages from lockfile
RUN R -q -e "install.packages('renv'); renv::restore(prompt = FALSE)"

# NOTE: Do NOT snapshot in builds (keeps builds pure & deterministic)
# RUN R -q -e "renv::snapshot(confirm = FALSE)"   # ← intentionally omitted


################################################################################
# STAGE 2: Runtime — slim RStudio Server with runtime libs only
################################################################################
FROM rocker/rstudio:4.4.2 AS final

LABEL \
  org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
  org.opencontainers.image.version="v1.0.0" \
  maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

# 0) Runtime dependencies only (no browsers, no geckodriver, no JRE)
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl git \
    # locales & tz for reproducible dates/strings
    locales tzdata \
    # shared object libs needed at runtime
    libgdal-dev libudunits2-dev libxml2 libssl3 libcurl4 \
    libpng16-16 libfreetype6 libfontconfig1 libharfbuzz0b libfribidi0 \
    fonts-dejavu fonts-liberation \
  && rm -rf /var/lib/apt/lists/* \
  && sed -i 's/# en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen \
  && sed -i 's/# pt_BR.UTF-8/pt_BR.UTF-8/' /etc/locale.gen \
  && locale-gen

ENV LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 TZ=America/Sao_Paulo

# 1) Keep using PPM in runtime too (for any interactive installs)
RUN bash -lc 'cat >> /usr/local/lib/R/etc/Rprofile.site <<EOF
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/bookworm/latest"))
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(R.version[["platform"]], R.version[["arch"]], R.version[["os"]])))
EOF'

# 2) Shared global renv cache (mount this as a named volume in compose)
ENV RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN mkdir -p /usr/local/lib/R/renv-cache \
 && echo "RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache" >> /usr/local/lib/R/etc/Renviron.site

# 3) Bring in the baked project + restored library from builder
COPY --from=builder /air_monitoring /air_monitoring

# 4) RStudio user prefs & profile
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
COPY .Rprofile           /home/rstudio/.Rprofile

# 5) Permissions (use compose env to set PASSWORD; avoid baking secrets)
RUN chown -R rstudio:staff /air_monitoring \
 && chown -R rstudio:rstudio /home/rstudio/.config/rstudio \
 && chown rstudio:rstudio /home/rstudio/.Rprofile

WORKDIR /air_monitoring

# 6) RStudio port + healthcheck
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=5s --retries=5 \
  CMD curl -fsS http://localhost:8787/ || exit 1

# 7) Entrypoint
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]