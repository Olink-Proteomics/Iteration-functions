#' Generate and Save a Density Plot
#'
#' This function generates a density plot based on user-defined variables for x-axis, grouping (color and fill),
#' and faceting, with customizable base size for the theme. Optionally, it saves the plot to a specified file.
#'
#' @param data A data frame containing the input data.
#' @param x_var A string specifying the column for the x-axis (e.g., "log2count").
#' @param comp_var A string specifying the column for grouping by both color and fill (e.g., "run_id").
#' @param facet_var A string specifying the column for faceting (default: "block").
#' @param base_size An integer specifying the base font size for the theme (default: 12).
#' @param output_path A string specifying the file path to save the plot (optional).
#' @param file_format A string specifying the file format to save the plot ("png" or "pdf", default: "pdf").
#'
#' @return A ggplot object.
#'
#' @examples
#' # Example usage
#' generate_density_plot(
#'   data = your_data,
#'   x_var = "log2count",
#'   comp_var = "run_id",
#'   facet_var = "block",
#'   base_size = 12,
#'   output_path = "plot.png",
#'   file_format = "png"
#' )
generate_density_plot <- function(data, x_var, comp_var, facet_var="block", base_size = 12, 
                                  output_path = NULL, file_format = "pdf") {
  # Validate inputs
  if (!(x_var %in% colnames(data))) {
    stop(paste("The x-axis variable", x_var, "does not exist in the data frame."))
  }
  if (!(comp_var %in% colnames(data))) {
    stop(paste("The grouping variable", comp_var, "does not exist in the data frame."))
  }
  if (!(facet_var %in% colnames(data))) {
    stop(paste("The facet variable", facet_var, "does not exist in the data frame."))
  }
  if (!is.null(output_path) && !(file_format %in% c("png", "pdf"))) {
    stop("Invalid file format. Choose 'png' or 'pdf'.")
  }
  
  # Create the plot
  plot <- ggplot(data, aes(x = .data[[x_var]], color = .data[[comp_var]], fill = .data[[comp_var]])) +
    geom_density(alpha = 0.5, position = "identity") +
    facet_wrap(as.formula(paste("~", facet_var))) +
    theme_tufte(base_size = base_size, base_family = "serif", ticks = TRUE) +
    olink_color_discrete() +
    olink_fill_discrete()
  
  # Show warning if no output path is provided
  if (is.null(output_path)) {
    warning("No output path provided. Plot will not be saved.")
  } else {
    # Save the plot based on the file format
    if (file_format == "png") {
      ggsave(output_path, plot = plot, width = 16, height = 16, bg = "white", dpi = 300)
    } else if (file_format == "pdf") {
      ggsave(output_path, plot = plot, width = 16, height = 16, device = cairo_pdf)
    }
  }
  
  # Return the plot object for further use
  return(plot)
}
