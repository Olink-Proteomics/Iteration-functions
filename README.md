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

