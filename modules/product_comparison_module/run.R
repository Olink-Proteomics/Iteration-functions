# HS, 120220205
######################
#
# Source required libraries and functions
sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)

# Load configuration

# Get path to config file passed by the Shiny app
args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(args) > 0 && file.exists(args[1])) args[1] else "modules/instrument_comparison_module/config.yml"
config <- yaml::read_yaml(config_path)

#generate graphs
cat("\nGenerating graphics...\n")
source("modules/product_comparison_module/graphics.R", local = TRUE)

report_filename <- paste0("../../reports/Product_comparison_Report_", parameters$Product, ".pdf")

cat("\nGenerating assay spreadsheet...\n")

#generate assay spreadsheet
if (isTRUE(config$parameters$generate_assay_spreadsheet)) {
  source("modules/product_comparison_module/assay_metrics.R", local = TRUE)  # or whatever script builds the spreadsheet
}




#generate the report

cat(paste("\nRendering the PDF report and saving as", report_filename, "...\n"))
rmarkdown::render(
  input = "modules/product_comparison_module/report.rmd", 
  params = config, 
  output_file = report_filename,
  output_format = "pdf_document"
)


cat(paste0("\nReport for Project '", parameters$Project, "' saved as: ", report_filename, "\n"))
cat("\nAll tasks completed successfully!\n")


message("All tasks completed successfully")
