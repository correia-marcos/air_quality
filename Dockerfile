################################################################################
# STAGE 1: Builder
################################################################################
FROM rocker/r-ver:4.4.2 AS builder

LABEL org.opencontainers.image.source="https://github.com/correia-marcos/air_quality" \
      org.opencontainers.image.version="v1.0.0" \
      maintainer="Marcos Paulo Rodrigues Correia <marcospaulorcorreia@gmail.com>"

# 1) System dependencies + Selenium prerequisites
RUN apt-get update && apt-get install -y \
    git libxml2-dev libssl-dev libcurl4-openssl-dev \
    libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
    wget unzip xvfb openjdk-11-jre-headless \
  && rm -rf /var/lib/apt/lists/*

# 2) Google Chrome + matching ChromeDriver
ARG CHROME_DRIVER_VERSION=116.0.5845.96
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" \
      > /etc/apt/sources.list.d/google-chrome.list \
 && apt-get update && apt-get install -y google-chrome-stable \
 && wget -O /tmp/chromedriver.zip \
      "https://chromedriver.storage.googleapis.com/${CHROME_DRIVER_VERSION}/chromedriver_linux64.zip" \
 && unzip /tmp/chromedriver.zip -d /usr/local/bin/ \
 && chmod +x /usr/local/bin/chromedriver \
 && rm /tmp/chromedriver.zip \
 && rm -rf /var/lib/apt/lists/*

# 3) Clone the repo from GitHub
ARG REPO_URL=https://github.com/correia-marcos/air_quality.git
RUN git clone ${REPO_URL} /air_monitoring

# 4) renv setup
WORKDIR /air_monitoring
COPY renv.lock .Rprofile renv/activate.R ./
RUN mkdir -p renv/.cache \
 && RENV_PATHS_CACHE=renv/.cache R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_1.0.10.tar.gz', type='source')" \
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
  && rm -rf /var/lib/apt/lists/*

# 2) Chrome + ChromeDriver (reuse builder layer)
ARG CHROME_DRIVER_VERSION=116.0.5845.96
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" \
      > /etc/apt/sources.list.d/google-chrome.list \
 && apt-get update && apt-get install -y google-chrome-stable \
 && rm -rf /var/lib/apt/lists/*

# 3) Copy renv library/cache from builder
COPY --from=builder /air_monitoring /air_monitoring

WORKDIR /air_monitoring

# 4) Expose RStudio Server and add a healthcheck
EXPOSE 8787
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl --fail http://localhost:8787/ || exit 1

# 5) Use non-root user (rocker/rstudio uses 'rstudio' by default)
USER rstudio

# 6) Entrypoint + default command
COPY entrypoint.sh /air_monitoring/entrypoint.sh
RUN chmod +x /air_monitoring/entrypoint.sh
ENTRYPOINT ["/air_monitoring/entrypoint.sh"]
CMD ["bash"]