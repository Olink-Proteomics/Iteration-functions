#' Calculate all CV and return a median if multiple runs exist
#'
#' This function calculates intra-plate, inter-plate, and inter-run coefficient of variation (CV) 
#' for a given dataset and merges the results into a single data frame.
#' If multiple runs exist per `comp_var`, the function returns the median CV.
#'
#' @param df Data frame. The input data containing assay information.
#' @param npx Character. The name of the column representing NPX values.
#' @param threshold Character. The name of the column representing the limit of detection or `"detection"` to filter detected samples.
#' @param comp_var Character. The variable used for grouping runs. Default is `"run_id"`.
#' @param filter_col Character. The name of the column used for filtering. Default is `"sample_type"`.
#' @param filter_value Character. The value in `filter_col` to filter on. Default is `"SAMPLE_CONTROL"`.
#' @param additionnal_var Character vector or NULL. Optional variable(s) to include in the grouping. Default is `NULL`.
#'
#' @return A data frame with merged intra-plate, inter-plate, and inter-run CV values.
#'
#' @examples
#' cv_data <- aggregate_cv(
#'   df = yourdata,
#'   npx = "NPX",
#'   threshold = "lod_in_npx",
#'   comp_var = "run_id",
#'   filter_value = "SAMPLE_CONTROL",
#'   additionnal_var = c("Product", "Batch")
#' )
#'
#' @export
aggregate_cv <- function(df, npx, threshold, comp_var = "run_id", 
                         filter_col = "sample_type", filter_value = "SAMPLE_CONTROL", 
                         additionnal_var = NULL) {
  
  # Ensure additional variables are handled properly
  grouping_vars <- c(additionnal_var, "plate_id", "olink_id")
  grouping_vars <- grouping_vars[!is.na(grouping_vars)]  # Remove NULLs
  
  
  intracv <- calculate_intra_plate_cv(df, comp_var, npx, threshold, filter_col, filter_value, additionnal_var) %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(intra_CV = median(intra_CV, na.rm = TRUE), .groups = "drop")
  
  grouping_vars_inter <- c(additionnal_var, "olink_id")
  grouping_vars_inter <- grouping_vars_inter[!is.na(grouping_vars_inter)]  # Remove NULLs
  
  
  interplatecv <- calculate_inter_plate_cv(df, comp_var, npx, threshold, filter_col, filter_value, additionnal_var) %>%
    group_by(across(all_of(grouping_vars_inter))) %>%
    summarise(inter_plate_CV = median(inter_plate_CV, na.rm = TRUE), .groups = "drop")
  
  interruncv <- calculate_inter_run_cv(df, npx, threshold, comp_var, filter_col, filter_value, additionnal_var) %>%
    group_by(across(all_of(grouping_vars_inter))) %>%
    summarise(inter_run_CV = median(inter_run_CV, na.rm = TRUE), .groups = "drop")
  
  # Merging CV data
  cvdata <- intracv %>%
    left_join(interplatecv, by = grouping_vars_inter) %>%
    left_join(interruncv, by = grouping_vars_inter)
  
  return(cvdata)
}
