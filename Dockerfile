
# Use the official Shiny Server base image (Debian + R + Shiny Server)
FROM rocker/shiny:latest

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    pandoc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages from CRAN
RUN Rscript -e "install.packages(c( \
  'shiny', 'shinyWidgets', 'yaml', 'processx', 'rmarkdown', 'knitr', \
  'dplyr', 'ggplot2', 'patchwork', 'purrr', 'readr', 'readxl', 'openxlsx', 'ggpubr', \
  'ggridges', 'ggthemes', 'gplots', 'kableExtra', 'ggstatsplot', 'statsExpressions', \
  'tidyverse', 'ggVennDiagram', 'ggfortify', 'ggpattern', 'ggside', 'gtsummary', \
  'viridis', 'extrafont', 'segmented', 'nloptr', 'arrow', 'RColorBrewer', \
  'OlinkAnalyze' \
), repos='https://cloud.r-project.org')"

# Install GitHub packages
RUN Rscript -e "if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes', repos='https://cloud.r-project.org'); \
                remotes::install_github('Olink-Proteomics/npxexplorer', upgrade='never')"

# Install TinyTeX for PDF rendering
RUN Rscript -e "if (!requireNamespace('tinytex', quietly = TRUE)) install.packages('tinytex', repos='https://cloud.r-project.org'); \
                tinytex::install_tinytex()"

# Copy app into the container
COPY . /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose default Shiny port
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
