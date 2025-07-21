################################################################################
# STAGE 1: Builder
################################################################################
FROM rocker/r-ver:4.4.2 AS builder

LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Paulo Rodrigues Correia <marcospaulorcorreia@gmail.com>"

# 1) System dependencies + Selenium prerequisites
RUN apt-get update && apt-get install -y \
    git \
    cmake libabsl-dev \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
    wget unzip xvfb openjdk-11-jre-headless \
    chromium chromium-chromedriver \
  && rm -rf /var/lib/apt/lists/*

# 2) Clone the repo from GitHub
ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
RUN git clone ${REPO_URL} /air_monitoring

# 3) renv setup
WORKDIR /air_monitoring
COPY renv.lock .Rprofile renv/activate.R ./
RUN mkdir -p renv/.cache \
 && RENV_PATHS_CACHE=renv/.cache \
    R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_1.0.10.tar.gz', type='source')" \
 && R -e "options(repos='https://cran.rstudio.com/'); renv::restore()"

################################################################################
# STAGE 2: Runtime with RStudio Server
################################################################################
FROM rocker/rstudio:4.4.2 AS final

LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Paulo Rodrigues Correia <marcospaulorcorreia@gmail.com>"

# 1) System dependencies (repeat to ensure libs available)
RUN apt-get update && apt-get install -y \
    cmake libabsl-dev \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
    wget unzip xvfb openjdk-11-jre-headless \
    chromium chromium-chromedriver \
  && rm -rf /var/lib/apt/lists/*

# 2) Copy code + renv from builder
COPY --from=builder /air_monitoring /air_monitoring
RUN chown -R rstudio:staff /air_monitoring          # Give studio ownership of the project
WORKDIR /air_monitoring

# 3) Entrypoint setup: normalize line endings & set executable
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN sed -i 's/\r$//' /usr/local/bin/entrypoint.sh \
 && chmod +x /usr/local/bin/entrypoint.sh

# 4) Expose RStudio Server and add a healthcheck
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl --fail http://localhost:8787/ || exit 1

# REMOVED PART:
# 5) Default entrypoint and command to launch RStudio Server
# ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
# CMD ["rserver", "--server-daemonize=0", "--www-port=8787"]

