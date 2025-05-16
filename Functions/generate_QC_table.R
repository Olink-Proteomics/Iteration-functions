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
generate_qc_table <- function(data, compare_by, compare_levels = NULL,
                              sample_type_filter = "SAMPLE", qc_var = "sample_id",
                              qc_col = "sample_qc", caption = "QC Status of Samples",
                              font_size = 10, format = "latex") {
  # Validate inputs
  if (!(compare_by %in% colnames(data))) stop(paste("The column", compare_by, "does not exist in the data frame."))
  if (!(qc_col %in% colnames(data))) stop(paste("The QC column", qc_col, "does not exist in the data frame."))
  if (!(qc_var %in% colnames(data))) stop(paste("The QC variable", qc_var, "does not exist in the data frame."))
  
  
  # Set default compare_levels
  if (is.null(compare_levels)) {
    compare_levels <- unique(data[[compare_by]])
  }
  
  
  # Filter and prepare data
  filtered_data <- data %>%
    filter(sample_type == sample_type_filter,
           !!sym(compare_by) %in% compare_levels) %>%
    distinct(Block = block, SampleID = !!sym(qc_var), PlateID = plate_id,
             CompareBy = !!sym(compare_by), QC = !!sym(qc_col))
  
  
  # Count and reshape
  summary_counts <- filtered_data %>%
    count(Block, PlateID, CompareBy, QC, name = "Count") %>%
    mutate(Column = paste(CompareBy, QC, sep = "_")) %>%
    select(-CompareBy, -QC) %>%
    pivot_wider(names_from = Column, values_from = Count, values_fill = 0)
  
  
  # Get actual column order based on present combinations
  all_columns <- colnames(summary_counts)
  fixed_cols <- c("Block", "PlateID")
  data_cols <- setdiff(all_columns, fixed_cols)
  
  
  # Reorder columns: group by compare level first
  split_parts <- strsplit(data_cols, "_")
  rep_order <- compare_levels
  status_order <- sort(unique(filtered_data$QC))
  
  
  # Build correct order
  expected_order <- unlist(lapply(rep_order, function(rep) {
    paste(rep, status_order, sep = "_")
  }))
  ordered_cols <- intersect(expected_order, data_cols)
  
  
  # Final column order
  summary_counts <- summary_counts[, c(fixed_cols, ordered_cols)]
  
  
  # Friendly column names
  friendly_names <- colnames(summary_counts)
  friendly_names[1:2] <- c("Block", "Plate")
  friendly_names[-c(1,2)] <- gsub("_", " ", friendly_names[-c(1,2)])
  
  
  # Build header: first level = Rep names, second level = status labels
  rep_names <- gsub(" .*$", "", friendly_names[-c(1,2)])  # e.g., "Rep1 PASS" -> "Rep1"
  rep_rle <- rle(rep_names)
  top_header <- c(" " = 2, setNames(rep_rle$lengths, rep_rle$values))
  
  
  # Now build the table
  table <- kbl(summary_counts, format = format, caption = caption, col.names = friendly_names, position = "!h") %>%
    kable_classic(full_width = FALSE) %>%
    add_header_above(top_header)
  
  
  if (!is.null(font_size)) {
    table <- kable_styling(table, font_size = font_size)
  }
  
  
  return(table)
}
