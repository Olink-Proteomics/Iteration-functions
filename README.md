# Iteration Shiny App – product level Module Runner


This Shiny app provides a unified interface to run individual modules with customizable configuration. It supports real-time output logging, config overrides, and report download.


## Features
- Select and run R-based analysis modules
- Override default module configuration using YAML input
- Live log output with most recent messages at the top
- Status bar and cancel button to manage long-running jobs
- Automatic display of generated PDF reports
- Download button for the final report


## Folder Structure
```
project_root/
├── interface.R                   # The Shiny app
├── modules/                     # Folder containing modules
│   ├── my_module/
│   │   ├── config.yml
│   │   ├── run.R
│   │   └── (optional graphics.R or report.Rmd)
├── reports/                     # Where reports are saved by modules
│   └── <generated_report>.pdf
```


## How to Use
1. **Launch the app:**
   ```r
   shiny::runApp("interface.R")
   ```


2. **In the UI:**
   - Select a module from the dropdown
   - Click "Load Config" to view default YAML
   - Modify the YAML if needed in the "Override Config" box
   - Click "Run Module" to start


3. **While running:**
   - Output appears in real time
   - Progress bar shows current status
   - Cancel button stops execution


4. **After completion:**
   - Status updates to “finished” or “error”
   - If a PDF report is generated, it is shown in the viewer
   - The report can be downloaded via the download button


## Module Requirements
Each module (inside `modules/<module_name>/`) must include:
- `run.R`: the main runner script  
  - Must accept a config path as its first argument:
    ```r
    args <- commandArgs(trailingOnly = TRUE)
    config <- yaml::read_yaml(args[1])
    ```
- `config.yml`: the default configuration
- Optionally:
  - `graphics.R`: sourced by `run.R` (must use global `config`)
  - `report.Rmd`: rendered via `rmarkdown::render(..., params = config)`


## Output Reports
- Must be saved to the top-level `reports/` folder
- Report filename can be dynamic (e.g. include product name or date)


## R Dependencies

These packages must be installed (from CRAN or GitHub):

**CRAN Packages:**
```
arrow, dplyr, extrafont, ggVennDiagram, ggfortify, ggpattern, ggplot2,
ggpubr, ggridges, ggside, ggstatsplot, ggthemes, gplots, gtsummary,
kableExtra, knitr, nloptr, openxlsx, patchwork, purrr, readr, readxl,
rmarkdown, segmented, shiny, shinyWidgets, statsExpressions, tidyverse,
viridis, yaml, RColorBrewer, OlinkAnalyze
```

**GitHub Packages:**
```
Olink-Proteomics/npxexplorer
```

Also ensure `pandoc` and a LaTeX engine (e.g., TinyTeX) are available for PDF rendering.

---


## Docker Setup (Optional)

You can run the app in a fully containerized environment using Docker.

### Build the Docker image:
```bash
docker build -t maximus-app .
```

### Run the app:
```bash
docker run -p 3838:3838 maximus-app
```

Then open your browser at [http://localhost:3838](http://localhost:3838)

### Notes:
- The image is based on `rocker/shiny` and includes Shiny Server
- All CRAN and GitHub dependencies (including `npxexplorer`) are installed
- PDF reports require TinyTeX, which is preinstalled
- The container runs the app from `/srv/shiny-server`


---

## Project Setup Using Makefile (Optional)

This project includes a `Makefile` to simplify setup and execution.

### To get started:

```bash
make install     # Install all required R packages (CRAN + GitHub)
make latex       # Install TinyTeX for PDF report generation
make run         # Launch the Shiny app
```

### Optional:

```bash
make clone       # Clone or update the project repository
make clean       # Remove temporary YAML config files
```

The `Makefile` is recommended for Linux/macOS users with `make` installed. It ensures a consistent, reproducible environment.

If you're using Windows, you can run these commands through Git Bash or WSL.

