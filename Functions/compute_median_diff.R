#' Compute Median Difference of a Numerical Variable Between Two Groups
#'
#' This function calculates the median difference of a specified numerical variable 
#' between two groups defined by a comparison variable in a given dataset.
#'
#' @param data A data frame containing the comparison variable, `olink_id`, and the numerical variable.
#' @param comp_var A string specifying the name of the categorical variable used for grouping (e.g., "Product").
#' @param num_var A string specifying the name of the numerical variable to be analyzed (e.g., "intra_CV").
#'
#' @return A data frame with:
#' \describe{
#'   \item{Difference_Formula}{A string explicitly stating the subtraction performed, e.g., "ProductA - ProductB".}
#'   \item{Median_difference}{The median of (Category_1 - Category_2) for `num_var`.}
#' }
#'
#' @examples
#' # Example usage:
#' compute_median_diff(cv_data, "Product", "intra_CV")
#'
#' @import dplyr tidyr
#' @export
compute_median_diff <- function(data, comp_var, num_var) {
  # Get unique category names dynamically
  unique_categories <- unique(data[[comp_var]])
  
  if (length(unique_categories) != 2) {
    stop("The comparison variable must have exactly two unique categories.")
  }
  
  cat1 <- unique_categories[1]
  cat2 <- unique_categories[2]
  
  
  result <- data %>%
    group_by(!!sym(comp_var), olink_id) %>%
    summarise(agg_value = mean(!!sym(num_var), na.rm = TRUE), .groups = "drop") %>% # This in case we have more than one value for each comp var and olink id.
    pivot_wider(names_from = !!sym(comp_var), values_from = agg_value) %>%
    group_by(olink_id) %>%
    summarise(diff_value = .data[[cat1]] - .data[[cat2]], .groups = "drop") %>%
    summarise(
      Difference_Formula = paste(cat1, "-", cat2),
      Median_difference = median(diff_value, na.rm = TRUE)
    )
  
  return(result)
}
