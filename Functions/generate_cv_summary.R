#' Generate CV Summary Table and Optional Histogram Plot
#'
#' This function calculates and summarizes the specified coefficient of variation (CV) type.
#' It also provides an optional histogram plot of CV values.
#'
#' @param df Data frame. The input dataset containing assay information.
#' @param value_col Character. The name of the CV column to calculate (e.g., `"intra_CV"`, `"inter_plate_CV"`, `"inter_run_CV"`).
#' @param comp_var Character. The column used for comparisons (e.g., `"run_id"`). Required for `"intra_CV"` and `"inter_plate_CV"`.
#' @param npx Character. The column representing NPX values.
#' @param threshold Character. The column representing the limit of detection.
#' @param sample_type Character. The value in the `sample_type` column to filter on. Default is `"SAMPLE_CONTROL"`.
#' @param group_vars Character vector. The grouping variables for summarization. Defaults to `"block"`.
#' @param pivot_var Character. The column used to pivot the table. Required for `"intra_CV"` and `"inter_plate_CV"`, ignored for `"inter_run_CV"`.
#' @param caption Character. The caption for the table.
#' @param format Character. The output format of the table (`"latex"` or `"html"`). Default is `"latex"`.
#' @param longtable Logical. Whether to create a longtable (default: `TRUE`).
#' @param booktabs Logical. Whether to use booktabs formatting (default: `TRUE`).
#' @param fontsize Numeric. Font size for LaTeX tables (default: `12`).
#' @param plot Logical. Whether to generate a histogram plot of CV values (default: `FALSE`).
#'
#' @return A list containing:
#' \item{table}{A formatted table summarizing the specified CV type.}
#' \item{plot}{(Optional) A histogram plot of CV values if `plot = TRUE`.}
#'
#' @details
#' - The function automatically calls the correct CV calculation function:
#'   - `calculate_intra_plate_cv()`
#'   - `calculate_inter_plate_cv()`
#'   - `calculate_inter_run_cv()`
#' - If `plot = TRUE`, it generates a histogram of CV values, grouped by `comp_var`, and faceted by `"block"`.
#'
#' @examples
#' # Generate intra-plate CV table with a histogram
#' results <- generate_cv_summary(
#'   df = your_data,
#'   value_col = "intra_CV",
#'   comp_var = "run_id",
#'   npx = "pc_normalized_npx",
#'   threshold = "lod_in_npx",
#'   sample_type = "SAMPLE_CONTROL",
#'   group_vars = c("run_id", "plate_id", "block"),
#'   pivot_var = "run_id",
#'   caption = "Average of intra-plate CV per block, plate, and run",
#'   plot = TRUE
#' )
#'
#' # Access the table
#' results$table
#'
#' # Access and display the plot
#' print(results$plot)
#'
#' @export
generate_cv_summary <- function(df, value_col, comp_var = NULL, npx, threshold, filter_col = "sample_type", filter_value = "SAMPLE_CONTROL",
                                group_vars = "block", pivot_var = NULL, caption, 
                                format = "latex", longtable = TRUE, booktabs = TRUE, fontsize = 12,
                                plot = FALSE) {
  # Validate value_col
  if (!value_col %in% c("intra_CV", "inter_plate_CV", "inter_run_CV")) {
    stop("Invalid value_col. Choose from 'intra_CV', 'inter_plate_CV', or 'inter_run_CV'.")
  }
  
  # Call the appropriate calculation function based on value_col
  cv_data <- switch(value_col,
                    "intra_CV" = {
                      if (is.null(comp_var)) stop("comp_var must be specified for intra-plate CV.")
                      calculate_intra_plate_cv(df, comp_var = comp_var, npx = npx, threshold = threshold,filter_col=filter_col, filter_value = filter_value)
                    },
                    "inter_plate_CV" = {
                      if (is.null(comp_var)) stop("comp_var must be specified for inter-plate CV.")
                      calculate_inter_plate_cv(df, comp_var = comp_var, npx = npx, threshold = threshold, filter_col=filter_col, filter_value = filter_value)
                    },
                    "inter_run_CV" = {
                      calculate_inter_run_cv(df, npx = npx, threshold = threshold, filter_col=filter_col, filter_value = filter_value, comp_var = comp_var)
                    }
  )
  
  # Validate and adjust group_vars
  existing_columns <- colnames(cv_data)
  valid_group_vars <- group_vars[group_vars %in% existing_columns]
  if (length(valid_group_vars) == 0) {
    stop("None of the specified group_vars exist in the resulting data frame.")
  }
  
  # If pivot_var is specified, validate it
  if (!is.null(pivot_var) && !pivot_var %in% existing_columns) {
    stop(paste0("The specified pivot_var '", pivot_var, "' does not exist in the resulting data frame."))
  }
  
  # Generate summary table
  summary_table <- cv_data %>%
    group_by(across(all_of(valid_group_vars))) %>%
    summarise(meanCV = round(mean(!!sym(value_col), na.rm = TRUE), 2), .groups = "drop")
  
  # Pivot the table if pivot_var is specified and relevant
  if (!is.null(pivot_var) && value_col != "inter_run_CV") {
    summary_table <- summary_table %>%
      pivot_wider(names_from = !!sym(pivot_var), values_from = meanCV)
  }
  
  # Generate table output
  table_output <- summary_table %>%
    kbl(format = format, caption = caption, longtable = longtable, booktabs = booktabs)
  
  # Adjust font size for LaTeX tables
  if (format == "latex" && !is.null(fontsize)) {
    table_output <- table_output %>% kable_styling(font_size = fontsize, full_width = FALSE)
  }
  
  # Initialize output list
  output <- list(table = table_output, data=cv_data)
  
  # Generate plot if requested
  if (plot) {
    plot_output <- ggplot(cv_data, aes(x = .data[[value_col]], fill = .data[[comp_var]])) +
      geom_histogram(color = "black", lwd = 0.75, linetype = 1, position = "dodge", bins = 30) +
      scale_x_continuous(limits = c(0, 200)) +
      facet_wrap(~block, scales = "free") +
      theme_bw(base_size = 14) +
      ylab('Number of assays') +
      theme(strip.text = element_text(size = 15), legend.position = "bottom") +
      olink_fill_discrete()
    
    output$plot <- plot_output  # Store plot in the list
  }
  
  return(output)  # Return both table and plot
}
