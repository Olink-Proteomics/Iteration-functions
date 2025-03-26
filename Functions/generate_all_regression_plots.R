#' Generate All Regression Plots for Combinations of a Variable

#'

#' This function creates regression plots for all possible combinations of unique values

#' in a specified variable (`wide_var`) and saves them to a structured folder if an output directory is provided.

#'

#' @param data A data frame containing the dataset to be used.

#' @param reg_var The dependent variable to be used for the regression.

#' @param wide_var The variable whose unique combinations will be used for comparisons.

#' @param output_dir (Optional) The base directory to save the regression plots. The function

#' will create a subdirectory named `regression_plots_<reg_var>_<wide_var>` inside the specified

#' `output_dir`. If not provided, the plots will not be saved.

#'

#' @return A list of regression plots generated for all combinations of `wide_var`.

#' The plots will also be saved to the `output_dir` if provided.

#'

#' @examples

#' # Assuming `generate_regression_plot` is defined and `ultima_diverse` is your dataset

#' output_directory <- "path/to/output/folder"

#'

#' # Generate and save all regression plots

#' all_plots <- generate_all_regression_plots(

#'   data = ultima_diverse,

#'   reg_var = "log2count",

#'   wide_var = "run_id",

#'   output_dir = output_directory

#' )

#'

#' # View the first plot

#' print(all_plots[[1]])

#'

generate_all_regression_plots <- function(data, reg_var, wide_var, output_dir = NULL) {
  
  # Get unique combinations of the wide_var
  
  unique_comparisons <- unique(data[[wide_var]])
  
  
  
  # Generate all possible combinations of the unique values
  
  comparison_pairs <- combn(unique_comparisons, 2, simplify = FALSE)
  
  
  
  # Initialize a counter for naming plots
  
  counter <- 1
  
  
  
  # Create the output directory if needed
  
  if (!is.null(output_dir)) {
    
    # Construct the subdirectory name
    
    sub_dir <- file.path(output_dir, paste0("regression_plots_", reg_var, "_", wide_var))
    
    if (!dir.exists(sub_dir)) {
      
      dir.create(sub_dir, recursive = TRUE)
      
    }
    
  }
  
  
  
  # Loop through each pair and create regression plots
  
  plot_list <- lapply(comparison_pairs, function(pair) {
    
    x_var <- pair[1]
    
    y_var <- pair[2]
    
    
    
    # Construct a unique output path for the current plot
    
    if (!is.null(output_dir)) {
      
      output_path <- file.path(sub_dir, paste0("figure_", counter, ".png"))
      
    } else {
      
      output_path <- NULL
      
    }
    
    
    
    # Generate the regression plot (and save if output_path is provided)
    
    plot <- generate_regression_plot(data, 
                            
                            x_var = x_var, 
                            
                            y_var = y_var, 
                            
                            reg_var = reg_var, 
                            
                            wide_var = wide_var, 
                            
                            output_path = output_path)
    
    
    
    counter <<- counter + 1
    
    
    
    return(plot)
    
  })
  
  
  
  return(plot_list)
  
}