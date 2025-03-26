#' Wrapper Function to Run Detectability Calculation for Each Instrument
#'
#' This function splits the data by instrument, runs `calculate_detectability_percentage()`
#' separately for each instrument, and merges the results into a single data frame.
#'
#' @param df Data frame. The input data containing detectability values and instrument information.
#' @param assay_group Optional data frame. A second data frame containing `olink_id` and grouping variables.
#' @param detectability_col Character. The name of the column representing detectability values.
#' @param thresholds Numeric vector. The thresholds for calculating percentages.
#' @param group_var Character. The primary grouping variable (e.g., "label").
#' @param instrument_col Character. The column indicating instrument type.
#'
#' @return A data frame combining results for all instruments.
#'
#' @examples
#' result <- run_detectability_by_instrument(
#'   df = detect_data, 
#'   assay_group = label_data, 
#'   detectability_col = "median_detectability", 
#'   thresholds = c(0, 0.1, 0.5),
#'   group_var = "label",
#'   instrument_col = "instrument"
#' )
#'
#' @export
calculate_detectability_percentage_per_group <- function(df, assay_group = NULL, detectability_col, thresholds, group_var, instrument_col) {
  
  # Ensure instrument column exists
  if (!instrument_col %in% names(df)) {
    stop("The specified instrument_col does not exist in the data frame.")
  }
  
  # Split data by instrument and run the detectability function
  results_list <- split(df, df[[instrument_col]]) %>%
    map(~ calculate_detectability_percentage(
      df = .x, 
      assay_group = assay_group, 
      detectability_col = detectability_col, 
      thresholds = thresholds, 
      group_var = group_var
    ) %>%
      mutate(!!instrument_col := unique(.x[[instrument_col]])))  # Add instrument column back
  
  # Merge all results together
  final_result <- bind_rows(results_list)
  
  return(final_result)
}
