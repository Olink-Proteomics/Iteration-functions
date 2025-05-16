#' Create a Correlation Summary Table
#'
#' This function generates a formatted LaTeX or HTML table summarizing correlation results
#' across multiple comparisons, ensuring that columns are ordered by metrics (e.g., Min, Q1, Mean)
#' across comparisons. It also dynamically groups headers for each comparison.
#'
#' @param cor_summary A data frame containing the correlation summary output from `correlation_function`.
#'                    It should include columns: `Comparison`, `block`, `Min`, `Q1`, `Mean`, `Median`, `Q3`, `Max`.
#' @param format A string specifying the table format, either `"latex"` or `"html"`. Default is `"latex"`.
#' @param caption A string specifying the caption for the table. Default is `"Summary of log2count correlations per assay"`.
#' @param font_size An integer specifying the font size for the table. Default is `8`.
#'
#' @return A formatted table as a LaTeX or HTML string.
#'
#' @details
#' This function takes the correlation summary (e.g., from `correlation_function`) and:
#' - Dynamically pivots the data to include all comparisons as separate columns.
#' - Ensures that metrics (Min, Q1, Mean, Median, Q3, Max) are ordered sequentially across comparisons.
#' - Adds grouped headers for each comparison.
#'
#' The resulting table is formatted using `kableExtra` and can be rendered in LaTeX or HTML.
#'
#' @examples
#' # Example Usage:
#' result <- correlation_function(
#'   data = filter(yourdata, NPX > eob),
#'   value_column = "log2count",
#'   comparison_column = "run_id",
#'   filter_sample_type = "SAMPLE",
#'   filter_assay_type = "assay"
#' )
#'
#' # Generate the correlation summary table
#' create_correlation_table(result$summary)
#'
#' @export



create_correlation_table <- function(cor_summary, format = "latex", caption = "Summary of correlations per assay", font_size = 8) {
  library(dplyr)
  library(tidyr)
  library(knitr)
  library(kableExtra)
  
  # Define the statistical metrics
  value_metrics <- c("Min", "Q1", "Mean", "Median", "Q3", "Max")
  
  # Pivot data so each comparison appears as its own set of statistics
  corr <- cor_summary %>%
    pivot_wider(names_from = Comparison, values_from = all_of(value_metrics)) %>%
    arrange(block)
  
  
  # Extract unique comparisons
  comparison_pairs <- unique(cor_summary$Comparison)
  
  
  # Create the correct column order: for each comparison, include all metrics sequentially
  ordered_cols <- c("block")
  for (comparison in comparison_pairs) {
    for (metric in value_metrics) {
      ordered_cols <- c(ordered_cols, paste0(metric, "_", comparison))
    }
  }
  
  
  # Reorder columns in the data frame
  corr <- corr %>%
    select(all_of(ordered_cols))
  
  
  # Display-friendly column names (metrics only, without comparison names)
  display_col_names <- c("Block", rep(value_metrics, length(comparison_pairs)))
  
  # Create header mapping for kableExtra::add_header_above
  header_labels <- c(" " = 1)  # "Block" column remains separate
  for (comparison in comparison_pairs) {
    header_labels[comparison] <- length(value_metrics)  # Assign 6 statistics per comparison
  }
  
  
  # Generate and format the kbl table
  table <- corr %>%
    kbl(position = "!h", format = format, caption = caption, col.names = display_col_names) %>%
    kable_classic(full_width = F, font_size = font_size) %>%
    add_header_above(header_labels)
  
  
  return(table)
}
