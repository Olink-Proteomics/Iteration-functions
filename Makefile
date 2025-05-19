
# Makefile for Maximus iteration Shiny App

.PHONY: all install run clean latex clone

# Git repo settings
REPO_URL = git@github.com:https://github.com/Olink-Proteomics/Iteration-functions
CLONE_DIR = iteration_run_dir

# R interpreter
R = Rscript

# R packages required by interface.R and mylib.R
REQUIRED_PACKAGES = shiny shinyWidgets yaml processx rmarkdown knitr \
dplyr ggplot2 patchwork purrr readr readxl openxlsx ggpubr \
ggridges ggthemes gplots kableExtra ggstatsplot statsExpressions \
tidyverse ggVennDiagram ggfortify ggpattern ggside gtsummary \
viridis extrafont segmented nloptr arrow RColorBrewer \
OlinkAnalyze

all: install

install:
	@echo "Installing required CRAN packages..."
	@$(R) -e 'packages <- c($(REQUIRED_PACKAGES)); \
	installed <- rownames(installed.packages()); \
	missing <- setdiff(packages, installed); \
	if (length(missing)) install.packages(missing, repos="https://cloud.r-project.org")'

	@echo "Installing GitHub packages..."
	@$(R) -e 'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", repos="https://cloud.r-project.org"); \
	remotes::install_github("Olink-Proteomics/npxexplorer", upgrade = "never")'

latex:
	@echo "Installing TinyTeX (LaTeX engine for PDF reports)..."
	@$(R) -e 'if (!requireNamespace("tinytex", quietly = TRUE)) install.packages("tinytex", repos="https://cloud.r-project.org"); \
	tinytex::install_tinytex()'

clone:
	@if [ -d "$(CLONE_DIR)/.git" ]; then \
		echo "Updating existing repo..."; \
		cd $(CLONE_DIR) && git pull; \
	else \
		echo "Cloning repository from $(REPO_URL)..."; \
		git clone $(REPO_URL) $(CLONE_DIR); \
	fi

run:
	@echo "Starting Shiny app..."
	@$(R) -e 'shiny::runApp("interface.R")'

clean:
	@echo "Cleaning up temp config files..."
	@rm -f *.yml
