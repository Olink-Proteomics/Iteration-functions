
#' Generate a Flexible Regression Plot
#'
#' This function generates a regression plot for internal controls by selecting two unique levels of a specified 
#' `wide_var` column in the dataset. The function allows users to dynamically choose additional variables to include in the data selection.
#'
#' @param data A data frame containing the data.
#' @param uniqueID A column specifying the unique identifier for each observation. Default is `"sample_index"`.
#' @param wide_var The column name (as a string) that will be used to pivot the data into wide format. 
#'                 It should contain exactly two unique levels.
#' @param reg_var The response variable that will be used for regression analysis.
#' @param group An optional column name used for coloring the points in the plot. Default is `NULL`.
#' @param additional_vars A character vector specifying additional columns to include in the selection. Default is `NULL`.
#' @param output_path (Optional) A file path where the plot should be saved. If `NULL`, the plot will not be saved.
#' @param base_size (Optional) The base font size for the plot. Default is `12`.
#' @param file_format (Optional) The format in which to save the plot. Either `"png"` or `"pdf"`. Default is `"png"`.
#'
#' @return A `ggplot` object representing the regression plot. If the number of unique levels in `wide_var` 
#'         is not equal to 2, the function returns `NULL` and displays a warning.
#'
#' @examples
#' generate_simple_regression_plot(data = my_data, wide_var = "Instrument", reg_var = "npx", 
#'                                 additional_vars = c("block", "plate_id"))
#'
generate_simple_regression_plot <- function(data, uniqueID = "sample_index", wide_var, reg_var, group = NULL, 
                                            additional_vars = NULL, output_path = NULL, base_size = 12, 
                                            file_format = "png") {
  
  # Ensure additional_vars is a character vector
  if (!is.null(additional_vars) && !is.character(additional_vars)) {
    stop("additional_vars must be a character vector of column names.")
  }
  
  # Define the variables to select
  select_vars <- c(wide_var, uniqueID, reg_var, group, additional_vars)
  select_vars <- select_vars[!is.null(select_vars)]  # Remove NULL values
  
  # Check if all selected variables exist in the dataset
  missing_vars <- setdiff(select_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("The following variables are missing from the dataset: ", paste(missing_vars, collapse = ", "))
  }
  
  # Prepare the data by pivoting to wide format
  temp <- data %>% 
    dplyr::select(all_of(select_vars)) %>%
    pivot_wider(names_from = !!rlang::sym(wide_var), values_from = !!rlang::sym(reg_var))
  
  # Identify unique levels in wide_var
  unique_levels <- setdiff(names(temp), c(uniqueID, group, additional_vars))
  
  # Check if there are exactly two unique levels in wide_var
  if (length(unique_levels) != 2) {
    warning(paste("Expected exactly 2 unique levels in", wide_var, 
                  "but found", length(unique_levels), "- check your data."))
    return(NULL)
  }
  
  # Assign x_var and y_var based on unique levels
  x_var <- unique_levels[1]
  y_var <- unique_levels[2]
  
  # Filter out rows with NA values in x_var or y_var
  temp <- temp %>% filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]))
  
  # Create the regression plot
  regression_plot <- temp %>% 
    ggplot(aes_string(y = y_var, x = x_var, color = group)) +
    geom_point(size = 1, alpha = 0.5, na.rm = TRUE) +
    geom_smooth(aes(color = NULL), se = TRUE, method = "lm", formula = y ~ x,
                na.rm = TRUE, level = 0.95, color = "blue") +
    geom_abline(size = 1) +
    theme_tufte(base_size = base_size, base_family = "serif", ticks = TRUE) +
    theme(legend.title = element_blank()) +
    olink_color_discrete() +  
    labs(caption = (temp %>% corr_test(.data[[x_var]], .data[[y_var]]))$expression[[1]]) +
    ggtitle(eq(temp[[x_var]], temp[[y_var]]))
  
  # Save the plot if output_path is provided
  if (!is.null(output_path)) {
    if (file_format == "png") {
      ggsave(output_path, plot = regression_plot, width = 10, height = 10, bg = "white", dpi = 300)
    } else if (file_format == "pdf") {
      ggsave(output_path, plot = regression_plot, width = 16, height = 16, device = cairo_pdf)
    } else {
      stop("Invalid file format. Choose 'png' or 'pdf'.")
    }
  } else {
    warning("No output path provided. Plot will not be saved.")
  }
  
  return(regression_plot)
}
