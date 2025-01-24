# IDB Project: Air Pollution Inequality

**Goal**: The objective of this research project is to explore inequality in air pollution monitoring and exposure across different socioeconomic groups. It focuses on data management, processing, and analysis related to air quality in various Latin American metropolitan areas.

---

## 1. Project Overview

- **Motivation**: Many low- and middle-income regions face disproportionate burdens of air pollution. This project aims to identify patterns of inequality by linking socioeconomic indicators with air quality data.
- **Main Tasks**:
  1. Collect and process air pollution data (satellite-derived or ground-level monitoring).
  2. Integrate socioeconomic variables (e.g., income levels, population density).
  3. Generate reproducible analyses of inequalities in air pollution exposure.
  4. Visualize results and produce tables/figures for publication or policy briefs.

This work is conducted in collaboration with the **Inter-American Development Bank (IDB)**.

---

## 2. Repository Structure

Here is a brief outline of the key folders and files in this repository:

\`\`\`
|-- .dockerignore
|-- .git
|-- .gitignore
|-- .Rprofile
|-- .Rproj.user/
|-- Coding.Rproj
|-- data/
|   |-- (raw and processed data files)
|-- doc/
|   |-- (documentation, references, PDFs)
|-- Dockerfile
|-- renv/
|   |-- (local project library managed by renv)
|-- renv.lock
|-- results/
|   |-- figures/
|   |-- tables/
|-- src/
|   |-- config/
|   |-- process_data/
|   |-- tables_images/
\`\`\`

**Brief Descriptions**:

- **Coding.Rproj**: RStudio project file.  
- **data/**:
  - **raw/**: Original or unprocessed datasets (e.g., shapefiles, .nc4, .csv).
  - **processed/**: Cleaned or aggregated data used in analysis.
- **doc/**: Supporting documentation, references (could be methodology notes, PDFs, README files).
- **Dockerfile**: Specifies the Docker image used for reproducible R environment.
- **renv/** and **renv.lock**: R environment management (package versions).
- **results/**:
  - **figures/**: Plots and graphs produced by the analysis.
  - **tables/**: Generated summary tables or CSV outputs.
- **src/**:
  - **config/**: Scripts for setting up configs or environment variables.
  - **process_data/**: Data cleaning, transformation, or analysis scripts.
  - **tables_images/**: Scripts to generate summary tables or figures.

---

## 3. Getting Started

### 3.1. Cloning the Repository

\`\`\`bash
git clone https://github.com/<user>/<repo>.git
cd <repo>
\`\`\`

### 3.2. Dependencies

The project uses **R** (4.2+ recommended) with packages managed by **[renv](https://rstudio.github.io/renv/)**.  
System-level dependencies (like \`libcurl\`, \`libxml2\`, etc.) can be found in the **Dockerfile** or installed manually.

### 3.3. Using Docker (Recommended)

1. **Build** the Docker image:
   \`\`\`bash
   docker build -t air-inequality:latest .
   \`\`\`
2. **Run** the container with a local mount:
   \`\`\`bash
   docker run -it --rm -v \$(pwd):/home/rstudio/project air-inequality:latest
   \`\`\`
3. Inside the container, you can run scripts or launch R.

### 3.4. Using renv Locally (Without Docker)

1. Ensure you have R installed.
2. Install **renv**:
   \`\`\`r
   install.packages(\"renv\")
   \`\`\`
3. From the project root, run:
   \`\`\`r
   renv::restore()
   \`\`\`
   This installs the packages needed in a local project library.

---

## 4. Workflow

1. **Data Preparation**: Place raw files in \`data/raw/\`.
2. **Run Scripts**:
   - **\`src/config/\`**: Scripts for environment or config settings.
   - **\`src/process_data/\`**: Steps to clean and merge data, e.g., \`compare_monthly_country_pm.R\`.
   - **\`src/tables_images/\`**: Scripts generating final tables or figures.
3. **Results**: Output (figures, tables) are stored in \`results/\`.

---

## 5. Contributing

- **Fork** the repository and create a new branch for your feature or fix.
- Submit a **pull request** with a clear description of changes.
- Follow the existing code style and folder structure.

---

## 6. License & Citation

- The code is provided under the [MIT License](LICENSE.md) (or specify whichever license you use).
- If you use these methods or results in your work, please cite appropriately, referencing this repository or any related publications.

---

## 7. Contact

- **Lead Researchers**: (Your name or team name)
- **Institution**: IDB Project Team
- **Email**: [username@example.com](mailto:username@example.com)

For questions or further information, feel free to open an issue or contact the authors directly.

---

**Last Update**: (Month Day, Year)
"

