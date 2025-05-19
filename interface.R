library(shiny)
library(yaml)
library(shinyWidgets)
library(processx)


base_dir <- "modules"
process_handle <- reactiveVal(NULL)


ui <- fluidPage(
  titlePanel("Module Runner"),
  sidebarLayout(
    sidebarPanel(
      selectInput("module", "Select a module:",
                  choices = list.files(base_dir)),
      actionButton("load_config", "Load Config"),
      textAreaInput("config_override", "Override Config (YAML format):",
                    value = "", rows = 15),
      actionButton("run_module", "Run Module"),
      actionButton("cancel_run", "Cancel"),
      br(),
      progressBar(id = "progress", value = 0, total = 100, display_pct = TRUE),
      br(),
      h4("Status:"),
      textOutput("status_message"),
      br(),
      uiOutput("download_report_ui")
    ),
    mainPanel(
      h4("Live Output:"),
      verbatimTextOutput("output_log", placeholder = TRUE),
      uiOutput("report_viewer")
    )
  )
)


server <- function(input, output, session) {
  config_path <- reactive({
    file.path(base_dir, input$module, "config.yml")
  })
  
  
  report_file <- reactiveVal(NULL)
  
  
  observeEvent(input$load_config, {
    if (file.exists(config_path())) {
      config_content <- readLines(config_path(), warn = FALSE)
      updateTextAreaInput(session, "config_override", value = paste(config_content, collapse = "\n"))
    }
  })
  
  
  observeEvent(input$run_module, {
    updateProgressBar(session = session, id = "progress", value = 10)
    output$output_log <- renderText("Starting module...")
    output$status_message <- renderText("Running...")
    output$report_viewer <- renderUI(NULL)
    report_file(NULL)
    
    
    temp_config <- tempfile(fileext = ".yml")
    writeLines(input$config_override, temp_config)
    run_script <- file.path(base_dir, input$module, "run.R")
    
    
    if (!file.exists(run_script)) {
      output$output_log <- renderText("Run script not found.")
      output$status_message <- renderText("Error: run script not found.")
      return()
    }
    
    
    log_lines <- character()
    proc <- process$new("Rscript", args = c(run_script, temp_config), stdout = "|", stderr = "|")
    process_handle(proc)
    
    
    observe({
      invalidateLater(500, session)
      proc <- process_handle()
      if (!is.null(proc) && proc$is_alive()) {
        try({
          new_output <- proc$read_output_lines()
          new_error <- proc$read_error_lines()
          log_lines <<- c(log_lines, new_output, new_error)
          updateProgressBar(session = session, id = "progress", value = 50)
          output$output_log <- renderText(paste(rev(log_lines), collapse = "\n"))
        }, silent = TRUE)
      } else if (!is.null(proc)) {
        try({
          final_output <- c(proc$read_all_output_lines(), proc$read_all_error_lines())
          log_lines <<- c(log_lines, final_output)
          updateProgressBar(session = session, id = "progress", value = 100)
          
          
          if (proc$get_exit_status() == 0) {
            output$status_message <- renderText("Execution finished successfully.")
          } else {
            output$status_message <- renderText("Execution finished with errors.")
          }
          
          
          output$output_log <- renderText(paste(rev(log_lines), collapse = "\n"))
          
          
          # Look for the most recent PDF report in the top-level "reports" folder
          reports_dir <- "reports"
          pdf_candidates <- list.files(
            reports_dir,
            pattern = "\\.pdf$", full.names = TRUE
          )
          pdf_path <- if (length(pdf_candidates) > 0) {
            pdf_candidates[which.max(file.info(pdf_candidates)$mtime)]
          } else {
            NULL
          }
          
          
          if (!is.null(pdf_path) && file.exists(pdf_path)) {
            report_file(pdf_path)
            output$report_viewer <- renderUI({
              tags$iframe(src = pdf_path, width = "100%", height = "800px", frameborder = 0)
            })
          } else {
            output$report_viewer <- renderUI({
              tags$p("No PDF report found.")
            })
          }
          
          
        }, silent = TRUE)
        process_handle(NULL)
      }
    })
  })
  
  
  output$download_report_ui <- renderUI({
    if (!is.null(report_file()) && file.exists(report_file())) {
      downloadButton("download_report", "Download Report")
    } else {
      tags$p("No report available yet.")
    }
  })
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("report_", input$module, ".pdf")
    },
    content = function(file) {
      if (!is.null(report_file()) && file.exists(report_file())) {
        file.copy(report_file(), file)
      } else {
        stop("Report not available.")
      }
    }
  )
  
  
  observeEvent(input$cancel_run, {
    proc <- process_handle()
    if (!is.null(proc) && proc$is_alive()) {
      proc$kill()
      process_handle(NULL)
      updateProgressBar(session = session, id = "progress", value = 0)
      output$status_message <- renderText("Process was cancelled by user.")
      output$output_log <- renderText("Process was cancelled by user.")
    }
  })
}


shinyApp(ui = ui, server = server)
