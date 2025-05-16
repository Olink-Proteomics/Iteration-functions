
#' Generate a count ummary Table with Block and Comparison Variable
#'
#' This function creates a table grouped by `block` and a comparison variable (`comp_var`),
#' and outputs a formatted table.
#'
#' @param data A data frame containing the input data.
#' @param comp_var A string specifying the comparison variable column (e.g., "run_id").
#' @param values_fill The value to fill in for missing combinations in `pivot_wider` (default: 0).
#' @param caption A string specifying the caption for the table (default: "Summary Table").
#' @param font_size An integer specifying the font size for the table (default: 12).
#' @param format A string specifying the table format (e.g., "html", "latex", default: "html").
#'
#' @return A formatted table in the specified format.
#'
#' @examples
#' # Example usage
#' generate_count_table_with_block(
#'   data = your_data,
#'   comp_var = "run_id",
#'   caption = "Total number of counts",
#'   font_size = 12
#' )
generate_count_table_with_block <- function(data, comp_var, 
                                              values_fill = 0, caption = "Total number of counts", 
                                              font_size = 12, format = "html") {
  # Validate inputs
  if (!("block" %in% colnames(data))) {
    stop("The column 'block' does not exist in the data frame.")
  }
  if (!(comp_var %in% colnames(data))) {
    stop(paste("The comparison variable", comp_var, "does not exist in the data frame."))
  }
  
  
  # Pivot the data
  summary_table <- data %>%
    group_by(block, !!sym(comp_var)) %>%
    summarise(Count = sum(count), .groups = "drop") %>%
    pivot_wider(names_from = !!sym(comp_var), values_from = Count, values_fill = values_fill)
  
  # Generate the table
  table <- kable(summary_table, format = format, caption = caption, position="!h") %>%
    kable_classic(full_width = FALSE) %>%
    kable_styling(font_size = font_size)
  
  return(table)
}
