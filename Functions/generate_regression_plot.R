
#' generate_regression_plot: Create and optionally save a regression scatter plot
#'
#' This function creates a regression scatter plot between two variables using a specified dataset. 
#' It allows customization of plot aesthetics such as base font size and file format, and includes 
#' an option to save the plot to a file.
#'
#' @param data A data frame containing the variables for the plot.
#' @param x_var A string indicating the name of the variable to be used on the x-axis.
#' @param y_var A string indicating the name of the variable to be used on the y-axis.
#' @param reg_var A string specifying the column containing the regression values.
#' @param wide_var A string specifying the column to pivot the data to wide format.
#' @param output_path A string specifying the file path to save the plot. If NULL, the plot is not saved.
#' @param base_size A numeric value for the base font size in the plot theme (default = 30).
#' @param file_format A string indicating the format of the saved file ("png" or "pdf"). Default is "png".
#'
#' @return A ggplot2 plot object containing the regression scatter plot.
#' @export
#'
#' @examples
#' # Example usage without saving
#' generate_regression_plot(
#'   data = illumina_diverse,
#'   x_var = "Z0008",
#'   y_var = "Z0013",
#'   reg_var = "log2count",
#'   wide_var = "run_id",
#'   base_size = 25
#' )
#'
#' # Example usage with saving as PDF
#' generate_regression_plot(
#'   data = both,
#'   x_var="Ultima",  reg_var = "log2count", wide_var="Instrument", output_path = "Results/Ultima/exp.png", base_size=30,file_format="png",
#'   y_var= "Illumina",
#'   reg_var = "log2count",
#'   wide_var = "Instrument",
#'   output_path = "Results/regression_plot2.pdf",
#'   base_size = 16,
#'   file_format = "pdf"
#' )
generate_regression_plot <- function(data, x_var, y_var, reg_var, wide_var, output_path = NULL, 
                            base_size = 12, file_format = "png") {
  
  # Prepare the data by pivoting to wide format
  temp <- data %>%
    dplyr::select(
      !!rlang::sym(wide_var), 
      block, 
      sample_index, 
      sample_type,
      plate_id, 
      !!rlang::sym(reg_var), 
      olink_id,
    ) %>%
    pivot_wider(names_from = !!rlang::sym(wide_var), values_from = !!rlang::sym(reg_var)) %>%
    filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]))
  
  # Create the regression plot
  regression_plot <- temp %>% 
    ggplot(aes_string(y = y_var, x = x_var, color = "sample_type")) +
    geom_point(size = 1, alpha = 0.5, na.rm = TRUE) +
    geom_smooth(aes(color = NULL), se = TRUE, method = "lm", formula = y ~ x,
                na.rm = TRUE, level = 0.95, color = "Black") +
    theme_tufte(base_size = base_size, base_family = "serif", ticks = TRUE) +
    theme(legend.title = element_blank()) +
   olink_color_discrete() +  
    labs(caption = (temp %>% corr_test(.data[[x_var]], .data[[y_var]]))$expression[[1]]) +
    ggtitle(eq(temp[[x_var]], temp[[y_var]]))
  
  # Show warning if no output path is provided
  if (is.null(output_path)) {
    warning("No output path provided. Plot will not be saved.")
  } else {
    # Save the plot based on the file format
    if (file_format == "png") {
      ggsave(output_path, plot = regression_plot, width = 10, height = 10, bg = "white", dpi = 300)
    } else if (file_format == "pdf") {
      ggsave(output_path, plot = regression_plot, width = 16, height = 16, device = cairo_pdf)
    } else {
      stop("Invalid file format. Choose 'png' or 'pdf'.")
    }
  }
  
  # Return the plot object for further use
  return(regression_plot)
}
