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
  
  # Check for group_var in the primary data frame or assay_group
  if (!is.null(group_var)) {
    if (!(group_var %in% names(df))) {
      if (!is.null(assay_group)) {
        if (!all(c("olink_id", group_var) %in% names(assay_group))) {
          stop(paste0("The second data frame (assay_group) must contain 'olink_id' and '", group_var, "' columns."))
        }
        df <- df %>%
          left_join(assay_group, by = "olink_id")
      } else {
        stop(paste0("Grouping variable '", group_var, "' not found in df or assay_group."))
      }
    }
  }
  
  # Remove duplicate olink_id entries before summarizing
  df <- df %>%
    distinct(olink_id, .keep_all = TRUE)
  
  # Grouped case
  if (!is.null(group_var)) {
    percentages <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        n_olinkid = n_distinct(olink_id),
        !!!setNames(
          lapply(thresholds, function(thresh) {
            expr(round(mean(!!sym(detectability_col) > !!thresh) * 100, 2))
          }),
          paste0("percent_above_", thresholds)
        ),
        .groups = "drop"
      )
  } else {
    # Ungrouped case
    percentages <- tibble(
      n_olinkid = n_distinct(df$olink_id)
    ) %>%
      bind_cols(
        map_dfc(thresholds, function(thresh) {
          tibble(!!paste0("percent_above_", thresh) := round(mean(df[[detectability_col]] > thresh) * 100, 2))
        })
      )
  }
  
  return(percentages)
}
