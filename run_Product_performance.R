# HS, 202020205
######################
#
# Source required libraries and functions
source("mylib.R")
sapply(list.files("Functions", pattern = "\\.R$", full.names = TRUE), source)

# Load configuration
config <- yaml::read_yaml("config.yml")

#generate graphs
cat("\nGenerating graphics...\n")
source("generate_graphs_product_performance.R")

report_filename <- paste0("Product_performance_report_", parameters$Instrument, ".pdf")

#generate the report

cat(paste("\nRendering the PDF report and saving as", report_filename, "...\n"))
rmarkdown::render(
  input = "Product_Performance_Report.rmd", 
  params = config, 
  output_file = report_filename,
  output_format = "pdf_document"
)


cat(paste0("\nReport for instrument '", parameters$Instrument, "' saved as: ", report_filename, "\n"))
cat("\nAll tasks completed successfully!\n")


message("All tasks completed successfully")
