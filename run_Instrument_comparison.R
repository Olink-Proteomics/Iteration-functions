# HS, 120220205
######################
#
# Source required libraries and functions
source("mylib.R")
sapply(list.files("Functions", pattern = "\\.R$", full.names = TRUE), source)

# Load configuration
config <- yaml::read_yaml("config_instrument.yml")

#generate graphs
cat("\nGenerating graphics...\n")
source("generate_graphs_instrument_comparison.R")

report_filename <- paste0("Instrument_comparison_Report_", parameters$Product, ".pdf")

#generate the report

cat(paste("\nRendering the PDF report and saving as", report_filename, "...\n"))
rmarkdown::render(
  input = "Instrument_comparison_Report.rmd", 
  params = config, 
  output_file = report_filename,
  output_format = "pdf_document"
)


cat(paste0("\nReport for Project '", parameters$Project, "' saved as: ", report_filename, "\n"))
cat("\nAll tasks completed successfully!\n")


message("All tasks completed successfully")
