generate_all_regression_plots <- function(data, reg_var, wide_var, output_dir = NULL) {
  
  # Get unique combinations of the wide_var
  unique_comparisons <- unique(data[[wide_var]])
  
  # Check if there are fewer than 2 unique values
  if (length(unique_comparisons) < 2) {
    message(paste0("Not enough unique values in '", wide_var, "' to generate comparisons. At least 2 are required."))
    return(NULL)
  }
  
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
    plot <- generate_regression_plot(
      data,
      x_var = x_var,
      y_var = y_var,
      reg_var = reg_var,
      wide_var = wide_var,
      output_path = output_path
    )
    
    counter <<- counter + 1
    
    return(plot)
  })
  
  return(plot_list)
}
