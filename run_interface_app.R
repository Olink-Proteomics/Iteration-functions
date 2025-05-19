# Simple launcher for the Shiny interface app
# Use this script instead of clicking "Run App" in RStudio
app_file <- "interface.R"  # 
if (!file.exists(app_file)) {
  stop("App file not found: ", app_file)
}
shiny::runApp(app_file)
