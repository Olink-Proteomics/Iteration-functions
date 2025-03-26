#' Generate a QC Summary Table
#'
#' This function generates a summary table of QC statuses grouped by a user-specified comparison factor
#' (e.g., instrument, run ID) and formats it for output as a LaTeX or HTML table.
#'
#' @param data A data frame containing the input data.
#' @param compare_by A string specifying the column in the data frame to use for comparison (e.g., "Instrument", "run_id").
#' @param compare_levels A character vector of levels to include from the `compare_by` column. Default is all unique levels in the data.
#' @param sample_type_filter A string to filter rows in the `sample_type` column (default: "SAMPLE").
#' @param qc_var A string specifying the column to use for identifying samples (default: "sample_id").
#' @param qc_col A string specifying the column containing QC statuses (default: "sample_qc").
#' @param caption A string specifying the caption for the table (default: "QC Status of Samples").
#' @param font_size An integer specifying the font size for the table in LaTeX output (default: 10).
#' @param format A string specifying the table format (e.g., "latex" or "html", default: "latex").
#'
#' @return A formatted table as a LaTeX or HTML string.
#'
#' @examples
#' # Example 1: Generate a table comparing by Instrument
#' generate_qc_table(data = both0, compare_by = "Instrument", compare_levels = c("Inst1", "Inst2"),
#'                   qc_var = "sample_id", qc_col = "sample_qc", caption = "QC Status by Instrument")
#'
#' # Example 2: Generate a table comparing by run_id
#' generate_qc_table(data = same, compare_by = "run_id", compare_levels = c("Run1", "Run2", "Run3"),
#'                   qc_var = "sample_id", qc_col = "sample_qc", caption = "QC Status by Run ID", font_size = 8)
#'
#' # Example 3: Generate a table comparing by a custom factor
#' generate_qc_table(data = both0, compare_by = "factor_x", compare_levels = c("Factor1", "Factor2"),
#'                   qc_var = "sample_id", qc_col = "sample_qc", caption = "QC Status by Factor X")
#'
generate_qc_table <- function(data, compare_by, compare_levels = NULL, sample_type_filter = "SAMPLE", qc_var = "sample_id", qc_col = "sample_qc", 
                              caption = "QC Status of Samples", font_size = 10, format = "latex") {
  # Validate inputs
  if (!(compare_by %in% colnames(data))) {
    stop(paste("The column", compare_by, "does not exist in the data frame."))
  }
  
  if (!(qc_col %in% colnames(data))) {
    stop(paste("The QC column", qc_col, "does not exist in the data frame."))
  }
  
  if (!(qc_var %in% colnames(data))) {
    stop(paste("The QC variable", qc_var, "does not exist in the data frame."))
  }
  
  
  # If compare_levels is NULL, use all unique levels from compare_by
  if (is.null(compare_levels)) {
    compare_levels <- unique(data[[compare_by]])
  }
  
  # Ensure all specified levels are present in the data
  actual_levels <- unique(data[[compare_by]])
  missing_levels <- setdiff(compare_levels, actual_levels)
  if (length(missing_levels) > 0) {
    stop(paste("The following levels are missing in the data:", paste(missing_levels, collapse = ", ")))
  }
  
  # Filter and prepare the data
  summary_table <- data %>%
    filter(sample_type == sample_type_filter, !!sym(compare_by) %in% compare_levels) %>%
    distinct(Block = block, SampleID = !!sym(qc_var), PlateID = plate_id, CompareBy = !!sym(compare_by), QC = !!sym(qc_col)) %>%
    group_by(Block, PlateID, CompareBy) %>%
    count(QC) %>%
    pivot_wider(names_from = CompareBy, values_from = n, values_fill = 0) %>%
    pivot_wider(names_from = QC, values_from = all_of(compare_levels), values_fill = 0)
  
  # Dynamically generate column names
  qc_statuses <- unique(data[[qc_col]])
  col_names <- c("Block", "Plate", rep(qc_statuses, length(compare_levels)))
  col_names <- col_names[1:ncol(summary_table)]  # Ensure correct number of column names
  
  # Create headers for the LaTeX or HTML table
  header_labels <- c(" " = 2, setNames(rep(length(qc_statuses), length(compare_levels)), compare_levels))
  
  # Generate and format the table
  table <- kbl(summary_table, format = format, caption = caption, col.names = col_names, position = "!h") %>%
    kable_classic(full_width = FALSE) %>%
    add_header_above(header_labels)
  
  # Add font size to the table
  if (!is.null(font_size)) {
    table <- kable_styling(table, font_size = font_size)
  }
  
  return(table)
}
