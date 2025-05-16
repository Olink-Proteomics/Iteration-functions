#' Calculate Detectability for Each Assay
#'
#' This function calculates the detectability of assays based on whether the NPX values
#' are above the limit of detection. The detectability is computed for each `olink_id`,
#' `assay`, `block`, and any additional grouping variables provided.
#'
#' @param df Data frame. The input dataset containing assay information.
#' @param sample_name Character. The name of the column that is used to identify replicates of the same sample.
#'   If not provided, replicates won't be considered. Default is `"sample_id"`.
#' @param npx Character. The name of the column representing NPX values.
#' @param background Character. The name of the column representing the limit of detection to be used.
#' @param matrix_name Character. The name of the column that is used to identify the matrix type.
#'   If not provided, all samples will be considered.
#' @param matrix Character. The name of the matrix type to be selected to calculate detectability.
#' @param additional_group_vars Character vector. Optional additional grouping variables to include in the output.
#'
#' @return A data frame summarizing the detectability for each group.
#'
#' @export
calculate_detectability <- function(df, sample_name = "sample_id", npx, background, 
                                    matrix_name = NULL, matrix = NULL, 
                                    additional_group_vars = NULL) {
  # Validate input parameters
  required_columns <- c(sample_name, npx, background)
  
  if (!all(required_columns %in% names(df))) {
    stop("One or more of the specified columns do not exist in the input dataset.")
  }
  
  if (!is.null(matrix_name) && is.null(matrix)) {
    stop("If 'matrix_name' is provided, 'matrix' must also be specified.")
  }
  
  if (!is.null(additional_group_vars)) {
    if (!all(additional_group_vars %in% names(df))) {
      stop("One or more additional grouping variables are not in the dataset.")
    }
  }
  
  
  # Define grouping variables
  base_group <- c("olink_id", "assay", "block")
  all_group_vars <- c(base_group, additional_group_vars)
  
  
  # Filter and preprocess the data
  result <- df %>%
    filter(!is.na(!!sym(npx)), assay_type == "assay", sample_type == "SAMPLE") %>%
    {
      if (!is.null(matrix_name) && !is.null(matrix)) {
        filter(., !!sym(matrix_name) == matrix)
      } else {
        .
      }
    } %>%
    select(all_of(c(sample_name, "sample_id", base_group, additional_group_vars, npx, background))) %>%
    group_by(across(all_of(c(base_group, additional_group_vars, sample_name)))) %>%
    summarise(
      above_limit = if (background == "detection") {
        sum(!!sym(background) == "above EOB", na.rm = TRUE) / n() > 0.5
      } else {
        sum(!!sym(npx) > !!sym(background), na.rm = TRUE) / n() > 0.5
      },
      .groups = 'drop'
    ) %>%
    group_by(across(all_of(all_group_vars))) %>%
    summarise(
      Detectability = mean(above_limit, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(result)
}
