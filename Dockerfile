################################################################################
# STAGE 1: Builder
################################################################################
FROM rocker/r-ver:4.4.2 AS builder

LABEL \
    org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
    org.opencontainers.image.version="v1.0.0" \
    maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

# 1) Install build tools and system dependencies (including textshaping prerequisites)
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      git \
      cmake libabsl-dev \
      libx11-dev libxml2-dev libssl-dev libcurl4-openssl-dev \
      libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
      wget unzip xvfb openjdk-11-jre-headless pkg-config \
      libfontconfig1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
 && rm -rf /var/lib/apt/lists/*

# 2) Clone repository
ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
RUN git clone ${REPO_URL} /air_monitoring

# 3) Configure renv cache & library paths
ENV RENV_PATHS_CACHE=/air_monitoring/renv/.cache
ENV RENV_PATHS_LIBRARY=/air_monitoring/renv/library

WORKDIR /air_monitoring

# 4) Copy renv lockfile and activation script
COPY renv.lock       renv/settings.json  ./
COPY renv/activate.R renv/activate.R
COPY .Rprofile       ./

# 5) Bootstrap renv & restore packages
RUN mkdir -p renv/.cache renv/library \
 && R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_1.0.10.tar.gz', type='source')" \
 && R -e "options(repos='https://cran.rstudio.com/'); renv::restore(prompt = FALSE)" \
 && R -e "renv::snapshot(confirm = FALSE)"


################################################################################
# STAGE 2: Runtime with RStudio Server
################################################################################
FROM rocker/rstudio:4.4.2 AS final

LABEL \
    org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
    org.opencontainers.image.version="v1.0.0" \
    maintainer="Marcos Correia <marcospaulorcorreia@gmail.com>"

# 1) Install runtime dependencies, curl & geckodriver
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      curl \
      git \
      cmake libabsl-dev \
      libx11-dev libxml2-dev libssl-dev libcurl4-openssl-dev \
      libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
      libfontconfig1-dev pkg-config \
      libharfbuzz-dev \
      libfribidi-dev \
      wget unzip xvfb openjdk-11-jre-headless \
 && rm -rf /var/lib/apt/lists/* \
 && curl -L https://github.com/mozilla/geckodriver/releases/download/v0.30.0/geckodriver-v0.30.0-linux64.tar.gz \
       -o /tmp/geckodriver.tar.gz \
 && tar xzf /tmp/geckodriver.tar.gz -C /usr/local/bin \
 && chmod +x /usr/local/bin/geckodriver

# 2) Configure a shared global renv cache (for mounting)
RUN mkdir -p /usr/local/lib/R/renv-cache \
 && echo "RENV_PATHS_CACHE=/usr/local/lib/R/renv-cache" \
       >> /usr/local/lib/R/etc/Renviron.site

# 3) Copy the baked project from the builder
COPY --from=builder /air_monitoring /air_monitoring

# 4) RStudio preferences and user .Rprofile
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json
COPY .Rprofile           /home/rstudio/.Rprofile

# 5) Adjust permissions and set default password
RUN chown -R rstudio:staff /air_monitoring \
 && chown -R rstudio:rstudio /home/rstudio/.config/rstudio \
 && chown rstudio:rstudio /home/rstudio/.Rprofile \
 && echo "rstudio:secret123" | chpasswd

WORKDIR /air_monitoring

# 6) Expose RStudio Server port & add healthcheck
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl --fail http://localhost:8787/ || exit 1

# 7) Entrypoint script
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]