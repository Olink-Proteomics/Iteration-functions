#' Calculate Detectability for Each Assay
#'
#' This function calculates the detectability of assays based on whether the NPX values
#' are above the limit of detection. The detectability is computed for each `olink_id`,
#' `assay`, and `block` group and represents the mean proportion of samples above the chosen threshold.
#'
#' @param df Data frame. The input dataset containing assay information.
#' @param sample_name Character. The name of the column that is used to identify replicates of the same sample.
#'   If not provided, replicates won't be considered. Default is `"sample_id"`.
#' @param npx Character. The name of the column representing NPX values.
#' @param background Character. The name of the column representing the limit of detection to be used.
#' @param matrix_name Character. The name of the column that is used to identify the matrix type.
#'   If not provided, all samples will be considered.
#' @param matrix Character. The name of the matrix type to be selected to calculate detectability.
#'
#' @return A data frame summarizing the detectability for each `olink_id`, `assay`, and `block`.
#'
#' @examples
#' # Calculate detectability
#' detectability_df <- calculate_detectability(
#'   df = your_data,
#'   sample_name = "sample_id",
#'   npx = "pc_normalized_npx",
#'   background = "lod_in_npx",
#'   matrix_name = "matrix_type",
#'   matrix = "Plasma"
#' )
#'
#' @export
calculate_detectability <- function(df, sample_name = "sample_id", npx, background, matrix_name = NULL, matrix = NULL) {
  # Validate input parameters
  required_columns <- c(sample_name, npx, background)
  
  if (!all(required_columns %in% names(df))) {
    stop("One or more of the specified columns do not exist in the input dataset.")
  }
  
  if (!is.null(matrix_name) && is.null(matrix)) {
    stop("If 'matrix_name' is provided, 'matrix' must also be specified.")
  }
  
  # Filter and preprocess the data
  result <- df %>%
    filter( !is.na(!!sym(npx)), assay_type == "assay", sample_type == "SAMPLE") %>%
    # Apply matrix filter if both matrix_name and matrix are provided
    {  
      if (!is.null(matrix_name) && !is.null(matrix)) {
        filter(., !!sym(matrix_name) == matrix)
      } else {
        .
      }
    } %>%
    select(sample_id, !!sym(sample_name), olink_id, assay, block, !!sym(npx),  !!sym(background)) %>%
    group_by(olink_id, assay, block, !!sym(sample_name)) %>%
    summarise(
      above_limit = if (background == "detection") {
        sum(!!sym(background) == "above EOB", na.rm = TRUE) / n() > 0.5
      } else {
        sum(!!sym(npx) > !!sym(background), na.rm = TRUE) / n() > 0.5
      },
      .groups = 'drop'
    ) %>%
    group_by(olink_id, assay, block) %>%
    summarise(
      Detectability = mean(above_limit, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(result)
}
