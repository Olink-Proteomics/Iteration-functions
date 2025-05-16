# HS, 202020205
######################
#
# Source required libraries and functions
sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)

# Load configuration
config <- yaml::read_yaml("modules/product_performance_module/config.yml")

#generate graphs
cat("\nGenerating graphics...\n")
source("modules/product_performance_module/graphics.R")

report_filename <- paste0("../../reports/Product_performance_report_", parameters$Instrument, ".pdf")

#generate the report

cat(paste("\nRendering the PDF report and saving as", report_filename, "...\n"))
rmarkdown::render(
  input = "modules/product_performance_module/report.rmd", 
  params = config, 
  output_file = report_filename,
  output_format = "pdf_document"
)


cat(paste0("\nReport for instrument '", parameters$Instrument, "' saved as: ", report_filename, "\n"))
cat("\nAll tasks completed successfully!\n")


message("All tasks completed successfully")
