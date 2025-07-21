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
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
    wget unzip xvfb openjdk-11-jre-headless \
    chromium chromium-chromedriver \
  && rm -rf /var/lib/apt/lists/*

# 2) Copy code + renv from builder
COPY --from=builder /air_monitoring /air_monitoring
WORKDIR /air_monitoring

# 3) Entrypoint setup (still root)
COPY entrypoint.sh /air_monitoring/entrypoint.sh
RUN chmod +x /air_monitoring/entrypoint.sh \
 && chown rstudio:staff /air_monitoring/entrypoint.sh

# 4) Expose RStudio Server and add a healthcheck
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl --fail http://localhost:8787/ || exit 1

# 5) Switch to non-root user
USER rstudio

# 6) Default entrypoint and command
ENTRYPOINT ["/air_monitoring/entrypoint.sh"]
CMD ["bash"]