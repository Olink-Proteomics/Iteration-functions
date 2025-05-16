#' Calculate Detectability for different projects, eg. instruments
#'
#' This function calculates detectability for each run within a project using `calculate_detectability()`
#' and returns a data frame with the median detectability of runs for each instrument.
#'
#' @param df Data frame. The input dataset containing NPX information.
#' @param comp_var Character. The name of the column representing the project identifier.
#' @param run_var Character. The name of the column representing the run identifier.
#' @param sample_name Character. The name of the column used to identify replicates of the same sample.
#' @param npx Character. The name of the column representing NPX values.
#' @param background Character. The name of the column representing the limit of detection.
#' @param matrix_name Character. The name of the column used to identify the matrix type.
#' @param matrix Character. The name of the matrix type to filter samples.
#'
#' @return A data frame with the median detectability of runs for each project.
#'
#' @examples
#' detectability_summary <- aggregate_detectability(
#'   df = your_data,
#'   comp_var = "instrument_id",
#'   run_var = "run_id",
#'   npx = "pc_normalized_npx",
#'   background = "lod_in_npx",
#'   matrix_name = "matrix",
#'   matrix = "Plasma"
#' )
#'
#' @export
aggregate_detectability <- function(df, 
                                    comp_var, 
                                    run_var, 
                                    sample_name = "sample_id", 
                                    npx, 
                                    background, 
                                    matrix_name = NULL, 
                                    matrix = NULL) {
  # Validate required columns exist
  required_cols <- c(comp_var, run_var, npx, background)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns are missing in the dataset:", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter by matrix if provided
  if (!is.null(matrix_name) && !is.null(matrix)) {
    df <- df %>% filter(!!sym(matrix_name) == matrix)
  }
  
  
  # Initialize a list to store detectability data for each run
  detectability_list <- list()
  
  
  # Loop through unique instrument and run combinations
  for (instrument in unique(df[[comp_var]])) {
    for (run in unique(df[[run_var]][df[[comp_var]] == instrument])) {
      
      # Subset the data for the specific instrument and run
      subset_df <- df %>%
        filter(!!sym(comp_var) == instrument, !!sym(run_var) == run)
      
      
      # Calculate detectability using the correct function
      detectability <- calculate_detectability(
        df = subset_df,
        sample_name = sample_name,
        npx = npx,
        background = background,
        matrix_name = matrix_name,
        matrix = matrix
      ) %>%
        mutate( !!sym(comp_var) := instrument, !!sym(run_var):= run)
      
      
      # Store the result
      detectability_list[[paste(instrument, run, sep = "_")]] <- detectability
    }
  }
  
  
  # Combine all results into a single data frame
  detectability <- bind_rows(detectability_list)
  
  
  # Calculate median detectability for each instrument
  detectability_data <- detectability %>%
    group_by(!!sym(comp_var),olink_id, assay, block) %>%
    summarise(
      Detectability = median(Detectability, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  return(detectability_data)
}
