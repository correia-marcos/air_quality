################################################################################
# STAGE 1: Build base environment + install R packages via renv
################################################################################
FROM rocker/r-ver:4.4.2 AS base

# system libraries for R packages + RSelenium dependencies
RUN apt-get update && apt-get install -y \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
    wget unzip xvfb openjdk-11-jre-headless \
    && rm -rf /var/lib/apt/lists/*

# Install Google Chrome
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - && \
    echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" \
      > /etc/apt/sources.list.d/google-chrome.list && \
    apt-get update && apt-get install -y google-chrome-stable && \
    rm -rf /var/lib/apt/lists/*

# Install matching ChromeDriver
ARG CHROME_DRIVER_VERSION=116.0.5845.96
RUN wget -O /tmp/chromedriver.zip \
       https://chromedriver.storage.googleapis.com/${CHROME_DRIVER_VERSION}/chromedriver_linux64.zip && \
    unzip /tmp/chromedriver.zip -d /usr/local/bin/ && \
    chmod +x /usr/local/bin/chromedriver && \
    rm /tmp/chromedriver.zip

WORKDIR /air_monitoring

# renv setup
COPY renv.lock .Rprofile renv/activate.R ./
RUN mkdir -p renv/.cache && \
    RENV_PATHS_CACHE=renv/.cache R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_1.0.10.tar.gz', type='source')" && \
    R -e "options(repos='https://cran.rstudio.com/'); renv::restore()"



################################################################################
# STAGE 2: Final image
################################################################################
FROM rocker/r-ver:4.4.2

# same system libs + Chrome + ChromeDriver
RUN apt-get update && apt-get install -y \
    libxml2-dev libssl-dev libcurl4-openssl-dev \
    libgdal-dev libudunits2-dev libpng-dev libfreetype6-dev \
    wget unzip xvfb openjdk-11-jre-headless \
    && rm -rf /var/lib/apt/lists/*

# Chrome
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - && \
    echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" \
      > /etc/apt/sources.list.d/google-chrome.list && \
    apt-get update && apt-get install -y google-chrome-stable && \
    rm -rf /var/lib/apt/lists/*

# ChromeDriver
ARG CHROME_DRIVER_VERSION=116.0.5845.96
RUN wget -O /tmp/chromedriver.zip \
       https://chromedriver.storage.googleapis.com/${CHROME_DRIVER_VERSION}/chromedriver_linux64.zip && \
    unzip /tmp/chromedriver.zip -d /usr/local/bin/ && \
    chmod +x /usr/local/bin/chromedriver && \
    rm /tmp/chromedriver.zip

WORKDIR /air_monitoring

# copy renv + installed packages from base
COPY --from=base /air_monitoring /air_monitoring

# copy project files
COPY . /air_monitoring

# entrypoint
COPY entrypoint.sh /air_monitoring/entrypoint.sh
RUN chmod +x /air_monitoring/entrypoint.sh

ENTRYPOINT ["/air_monitoring/entrypoint.sh"]
CMD ["bash"]