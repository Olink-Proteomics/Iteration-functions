# HS, 202020205
######################
#
# Source required libraries and functions
sapply(list.files("../functions", pattern = "\\.R$", full.names = TRUE), source)

# Load configuration
config <- yaml::read_yaml("../config/config_product.yml")

#generate graphs
cat("\nGenerating graphics...\n")
source("generate_graphslysate.R")

report_filename <- paste0("Lysate_module_Report_", parameters$Product, ".pdf")

#generate the report

cat(paste("\nRendering the PDF report and saving as", report_filename, "...\n"))
rmarkdown::render(
  input = "../RMD/lysate_module_Report.rmd", 
  params = config, 
  output_file = report_filename,
  output_format = "pdf_document"
)


cat(paste0("\nReport for lysate module saved as: ", report_filename, "\n"))
cat("\nAll tasks completed successfully!\n")


message("All tasks completed successfully")
