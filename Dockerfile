################################################################################
# STAGE 1: Builder
################################################################################
FROM rocker/r-ver:4.4.2 AS builder

LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Paulo Rodrigues Correia <marcospaulorcorreia@gmail.com>"

# 1) System/build dependencies (no Chromium here)
RUN apt-get update && apt-get install -y \
    git \
    cmake libabsl-dev \
    libx11-dev libxml2-dev libssl-dev libcurl4-openssl-dev \
    libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
    wget unzip xvfb openjdk-11-jre-headless \
  && rm -rf /var/lib/apt/lists/*

# 2) Clone your repo
ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
RUN git clone ${REPO_URL} /air_monitoring

# 3) Tell renv where to put cache & library, then restore
ENV RENV_PATHS_CACHE=/air_monitoring/renv/.cache
ENV RENV_PATHS_LIBRARY=/air_monitoring/renv/library

WORKDIR /air_monitoring
COPY renv.lock      renv/settings.json  ./ 
COPY .Rprofile                             ./
COPY renv/activate.R                      renv/
RUN mkdir -p renv/.cache renv/library \
 && R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_1.0.10.tar.gz', type='source')" \
 && R -e "options(repos='https://cran.rstudio.com/'); renv::restore()"

################################################################################
# STAGE 2: Runtime with RStudio Server
################################################################################
FROM rocker/rstudio:4.4.2 AS final

LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Paulo Rodrigues Correia <marcospaulorcorreia@gmail.com>"

# 1) System deps + curl, Xvfb, Java headless, etc.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      curl \
      cmake libabsl-dev \
      libx11-dev libxml2-dev libssl-dev libcurl4-openssl-dev \
      libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
      wget unzip xvfb openjdk-11-jre-headless \
      ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && curl -L https://github.com/mozilla/geckodriver/releases/download/v0.30.0/geckodriver-v0.30.0-linux64.tar.gz -o geckodriver.tar.gz \
    && tar xzf geckodriver.tar.gz -C /usr/local/bin \
    && chmod +x /usr/local/bin/geckodriver

# 2) re‐export renv paths
ENV RENV_PATHS_CACHE=/air_monitoring/renv/.cache
ENV RENV_PATHS_LIBRARY=/air_monitoring/renv/library

# 3) copy in your baked project + default.Rprofile
COPY --from=builder /air_monitoring /air_monitoring
COPY .Rprofile /home/rstudio/.Rprofile
RUN chown -R rstudio:staff /air_monitoring \
 && chown rstudio:rstudio /home/rstudio/.Rprofile \
 && echo 'rstudio:secret123' | chpasswd

WORKDIR /air_monitoring

# 4) Expose & healthcheck
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl --fail http://localhost:8787/ || exit 1

# 5) Let entrypoint.sh to be used
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
