# IDB Project: Air Pollution Inequality

**Objective:** Investigate disparities in air pollution exposure across socioeconomic groups in Latin American metropolitan areas, linking satellite and ground‑level data with demographic indicators to inform policy.

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Repository Structure](#repository-structure)
3. [Prerequisites](#prerequisites)
4. [Installation](#installation)
   - [Docker (Recommended)](#docker-recommended)
   - [Local Setup with renv](#local-setup-with-renv)
5. [Usage](#usage)
6. [Workflow](#workflow)
7. [Contributing](#contributing)
8. [License & Citation](#license--citation)
9. [Contact](#contact)

---

## 1. Project Overview

**Motivation:** Low‑ and middle‑income regions often bear disproportionate air pollution burdens. This analysis seeks to quantify inequalities by linking pollutant exposure data with socioeconomic metrics.

**Key Goals:**

- Collect and preprocess air quality data (MERRA‑2, ground stations).
- Merge pollution metrics with demographic and economic indicators.
- Perform reproducible statistical analyses to uncover exposure disparities.
- Generate publication‑quality figures and tables for top‑field journals and policy briefs.

**Collaboration:** Inter‑American Development Bank (IDB) Research Team

---

## 2. Earthdata Credentials

In order to download MERRA‑2 files, you must authenticate against NASA’s Earthdata portal.  
Follow these steps once per user:

1. **Create or edit** your R environment file `~/.Renviron`:

   ```bash
   # in your home project directory
   cat <<EOF >> ~/.Renviron
   EARTHDATA_USER=your_nasa_username
   EARTHDATA_PASS=your_nasa_password
   EOF


## 3. Repository Structure

```
│-- .dockerignore           # Files to exclude from Docker build
│-- .gitignore             # Files to ignore in Git
│-- .Rprofile              # RStudio project configuration
│-- Coding.Rproj           # RStudio project file
│-- Dockerfile             # Defines Docker environment
│-- entrypoint.sh          # Launch script for container
│-- renv/                  # Local package library managed by renv
│-- renv.lock              # Locked package versions
│-- data/                  # Data directory
│   ├── raw/               # Original, unmodified data (shapefiles, .nc4, .csv)
│   └── processed/         # Cleaned/aggregated data ready for analysis
│-- doc/                   # Documentation, references, methodology notes
│-- src/                   # Analysis scripts
│   ├── config/            # Configuration (environment variables, constants)
│   ├── process_data/      # Data cleaning and transformation scripts
│   └── tables_images/     # Scripts to generate tables and figures
│-- results/               # Output artifacts
│   ├── figures/           # Plots and visualizations
│   └── tables/            # Summary tables and CSV exports
└-- README.md              # Project overview and setup instructions
```

---

## 4. Prerequisites

- **R** version ≥ 4.2.0
- **Docker** (for containerized execution)
- System libraries (e.g., `libcurl`, `libxml2`, `gdal`, `proj`) — see [Dockerfile](./Dockerfile)

---

## 5. Installation

### Docker (Recommended)

1. Build the image:

   ```bash
   docker build -t idb-air-inequality:latest .
   ```

2. Run with a bind mount:

   ```bash
   docker run --rm -it \
     -v $(pwd):/home/rstudio/project \
     idb-air-inequality:latest
   ```

3. Inside container, use `entrypoint.sh` or start RStudio/server as configured.

### Local Setup with renv

1. Install `renv` if needed:

   ```r
   install.packages("renv")
   ```

2. Restore packages:

   ```r
   renv::restore()
   ```

3. Launch RStudio from project root.

---

## 6. Usage

- **Run individual scripts:**

  ```bash
  ./entrypoint.sh run src/process_data/clean_pollution_data.R
  ```

- **Interactive shell:**

  ```bash
  docker run --rm -it idb-air-inequality:latest bash
  ```

- **Generate all outputs:**

  ```bash
  ./entrypoint.sh run \
    src/config/setup.R \
    src/process_data/*.R \
    src/tables_images/*.R
  ```

---

## 7. Workflow

1. **Data ingestion:** Drop raw files in `data/raw/`.
2. **Preprocessing:** Scripts in `src/process_data/` clean and merge datasets.
3. **Analysis & Visualization:** `src/tables_images/` generates figures in `results/figures/` and tables in `results/tables/`.
4. **Review & Export:** Pull outputs for manuscript drafting or policy brief.

---

## 8. Contributing

1. Fork the repository.
2. Create a feature branch: `git checkout -b feature/your-feature`.
3. Commit changes with descriptive messages.
4. Push and open a Pull Request.
5. Ensure code style consistency and add tests if applicable.

---

## 9. License & Citation

- Licensed under MIT License. See [LICENSE.md](LICENSE.md).
- Please cite this repository or related publications when reusing methods or results.

---

## 10. Contact

- **Project Lead:** [Marcos Paulo Rodrigues Correia]
- **Affiliation:** Inter‑American Development Bank
- **Email:** [marcospaulorcorreia@gmail.com](marcospaulorcorreia@gmail.com)

**Last Updated:** 2025‑04‑21 (YYYY-MM-DD)
