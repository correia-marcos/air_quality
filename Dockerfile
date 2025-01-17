# Dockerfile

# 1) Use the official R image from Rocker
FROM rocker/r-ver:4.2.2

# 2) Set a working directory inside the container
WORKDIR /home/rstudio/my-project

# 3) Copy your project files (except what is ignored by .dockerignore)
COPY . /home/rstudio/my-project

# 4) Install Linux packages needed for your R packages (example: libxml2-dev, etc.)
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# 5) If you're using `renv`, restore from your lockfile
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')"
RUN R -e "renv::restore()"  # this will install the packages listed in renv.lock

# 6) Set an entrypoint or command. For interactive sessions:
CMD ["/bin/bash"]
