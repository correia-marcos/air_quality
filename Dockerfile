################################################################################
# STAGE 1: Build base environment + install packages via renv
################################################################################
FROM rocker/r-ver:4.4.2 AS base

# Install system libraries needed to compile certain R packages
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    # Add more system dependencies if needed...
    && rm -rf /var/lib/apt/lists/*

# Create the air_monitoring directory and set it as working directory
WORKDIR /air_monitoring

# Copy only the files required for renv::restore()
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
# COPY renv/settings.dcf renv/settings.dcf

# (Optional) set a local renv cache inside the project
RUN mkdir -p renv/.cache
ENV RENV_PATHS_CACHE renv/.cache

# 1) Install the archived renv version from the URL you provided
RUN R -e "options(repos = c(CRAN='https://cran.rstudio.com/')); \
          install.packages('https://cran.r-project.org/src/contrib/Archive/renv/renv_1.0.10.tar.gz', \
                           type='source')"

# 2) Restore packages with CRAN = 'https://cran.rstudio.com/'
RUN R -e "options(repos = c(CRAN='https://cran.rstudio.com/')); renv::restore()"


################################################################################
# STAGE 2: Final image
################################################################################
FROM rocker/r-ver:4.4.2

# Reinstall system libraries so the final image has them available at runtime
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    # Add more system dependencies if needed...
    && rm -rf /var/lib/apt/lists/*

# Create the air_monitoring directory and set it as working directory
WORKDIR /air_monitoring

# Copy all files from Stage 1 (including installed R packages) into final image
COPY --from=base /air_monitoring /air_monitoring

# Copy the rest of your project files that change frequently (R scripts, data, etc.)
COPY . /air_monitoring

# Default command: drop into a bash shell
CMD ["/bin/bash"]