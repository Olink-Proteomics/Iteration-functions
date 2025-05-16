#' Calculate Intra-plate CV
#'
#' This function calculates the intra-plate coefficient of variation (CV) for a given dataset.
#' If `threshold = "detection"`, the function will use the binary detection column
#' to filter only samples that are detected (`"above EOB"`). 
#' Otherwise, it filters samples where NPX values are above the specified threshold.
#'
#' @param df Data frame. The input data containing assay information.
#' @param comp_var Character. The name of the column representing the comparison variable (e.g., run ID).
#' @param npx Character. The name of the column representing NPX values.
#' @param threshold Character. The name of the column representing the limit of detection, or `"detection"` to filter detected samples.
#' @param filter_col Character. The name of the column used for filtering. Default is `"sample_type"`.
#' @param filter_value Character. The value in `filter_col` to filter on. Default is `"SAMPLE_CONTROL"`.
#' @param additionnal_var Character or NULL. An optional variable to include in the grouping. Default is `NULL`, meaning it is not used.
#'
#' @return A data frame with intra-plate CV values for each `olink_id`, `comp_var`, `plate_id`, and `block`, optionally grouped by `additionnal_var` if provided.
#'
#' @examples
#' # Default filtering on `sample_type`
#' intra_plate_cv <- calculate_intra_plate_cv(
#'   df = your_data,
#'   comp_var = "run_id",
#'   npx = "pc_normalized_npx",
#'   threshold = "lod_in_npx"
#' )
#'
#' # Filtering on another column, e.g., `sample_info`
#' intra_plate_cv <- calculate_intra_plate_cv(
#'   df = your_data,
#'   comp_var = "run_id",
#'   npx = "pc_normalized_npx",
#'   threshold = "lod_in_npx",
#'   filter_col = "sample_info",
#'   filter_value = "CONTROL_SAMPLE"
#' )
#'
#' # Including an additional grouping variable
#' intra_plate_cv <- calculate_intra_plate_cv(
#'   df = your_data,
#'   comp_var = "run_id",
#'   npx = "pc_normalized_npx",
#'   threshold = "lod_in_npx",
#'   additionnal_var = "Product"
#' )
#'
#' @export
calculate_intra_plate_cv <- function(df, comp_var, npx, threshold, 
                                     filter_col = "sample_type", filter_value = "SAMPLE_CONTROL", 
                                     additionnal_var=NULL) {
  df %>%
    filter(!!sym(filter_col) == filter_value, category == 0, assay_type == "assay") %>%
    {   
      if (threshold == "detection") {
        filter(., !!sym(threshold) == "above EOB")  # Keep only detected samples
      } else {
        filter(., !!sym(npx) > !!sym(threshold))  # Default NPX filtering
      }
    } %>%
    {
      # Conditionally group by additionnal_var if it is not NULL
      if (!is.null(additionnal_var)) {
        group_by(., olink_id, !!sym(comp_var), across(all_of(additionnal_var)), plate_id, block)
      } else {
        group_by(., olink_id, !!sym(comp_var), plate_id, block)
      }
    } %>%
    summarise(
      std = sd(!!sym(npx), na.rm = TRUE),
      sln = log(2) * std,
      intra_CV = 100 * sqrt(exp(sln^2) - 1),
      .groups = "drop"
    ) %>%
    select(-std, -sln)
}
