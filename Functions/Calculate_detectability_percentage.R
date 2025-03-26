#' Calculate Detectability Percentage
#'
#' This function calculates the percentage of assays with detectability above specified thresholds,
#' optionally grouped by a user-specified grouping variable (`group_var`). If `0` is included in the thresholds,
#' the comparison will use `detectability_col > 0` instead of `>= 0`.
#'
#' @param df Data frame. The input data frame containing detectability values.
#' @param assay_group Optional data frame. A second data frame containing `olink_id` and the grouping variable (e.g., `group_var`).
#'   If the grouping variable is not in the primary data frame, the function attempts to merge it using this second data frame.
#' @param detectability_col Character. The name of the column representing detectability values.
#' @param thresholds Numeric vector. The thresholds for calculating percentages.
#'   If `0` is included, the comparison will be `> 0` for that threshold.
#' @param group_var Optional character. The name of the grouping variable. If `NULL`, no grouping is performed.
#'
#' @return A data frame summarizing the percentage of assays with detectability above the thresholds,
#' grouped by `group_var` (if provided). Final values are rounded to 2 decimals.
#'
#' @examples
#' # Example usage with grouping
#' result <- calculate_detectability_percentage(
#'   df = detect_data, 
#'   assay_group = label_data, 
#'   detectability_col = "median_detectability", 
#'   thresholds = c(0, 0.1, 0.5),
#'   group_var = "label"
#' )
#'
#' @export
calculate_detectability_percentage <- function(df, assay_group = NULL, detectability_col, thresholds, group_var = NULL) {
  # Ensure detectability_col exists in df
  if (!detectability_col %in% names(df)) {
    stop("The specified detectability_col does not exist in the primary data frame.")
  }
  
  # Initialize grouping availability
  grouping_available <- FALSE
  
  # Check for group_var in the primary data frame or assay_group
  if (!is.null(group_var)) {
    if (group_var %in% names(df)) {
      grouping_available <- TRUE
    } else if (!is.null(assay_group)) {
      # Check if the second data frame contains the required columns
      if (!all(c("olink_id", group_var) %in% names(assay_group))) {
        stop(paste0("The second data frame (assay_group) must contain 'olink_id' and '", group_var, "' columns."))
      }
      
      # Merge assay_group with df by olink_id
      df <- df %>%
        left_join(assay_group, by = "olink_id")
      
      # Ensure the merge was successful
      if (!(group_var %in% names(df))) {
        stop(paste0("The '", group_var, "' column was not found in the merged data. Ensure 'olink_id' exists in both data frames."))
      }
      
      grouping_available <- TRUE
    }
  }
  
  # Group by group_var if grouping is enabled
  if (!is.null(group_var) && grouping_available) {
    df <- df %>%
      group_by(!!sym(group_var))
  }
  
  # Create a data frame for percentages based on thresholds
  percentages <- map_dfc(thresholds, function(thresh) {
    if (thresh == 0) {
      # Special case for threshold 0: Use >
      tibble(!!paste0("percent_above_", thresh) := round(mean(df[[detectability_col]] > thresh, na.rm = TRUE) * 100, 2))
    } else {
      # Default case: Use >=
      tibble(!!paste0("percent_above_", thresh) := round(mean(df[[detectability_col]] >= thresh, na.rm = TRUE) * 100, 2))
    }
  })
  
  # Combine percentages with grouping variable if applicable
  if (!is.null(group_var) && grouping_available) {
    result <- df %>%
      summarise(percentages, .groups = "drop")
  } else {
    result <- percentages
  }
  
  return(result)
}
