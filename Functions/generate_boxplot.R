#' Generate a Boxplot with Optional Faceting and Grouping

#'

#' This function generates a boxplot from the provided dataset, with options to facet by a variable, 

#' group by a variable, and customize axis variables. It includes flexibility for aesthetic customization 

#' and filtering.

#'

#' @param df Data frame. The input dataset.

#' @param y_var Character. The variable to use for the y-axis (required).

#' @param group_var Character. The variable to group by for the boxplot.

#' @param facet_var Character. The variable to facet by. Default is `"block"`.

#' @param facet_by Logical. Whether to facet the plot by the `facet_var`. Default is `TRUE`.

#' @param sample_type_filter Character. A value to filter the `sample_type` column. 

#' @param x_var Character. The variable to use for the x-axis. Default is `"sample_id"`.

#' @param pattern_var Character. The variable to use for the pattern aesthetic. Default is `"plate_id"`.

#' @param axis_text_size Numeric. Font size for the axis text. Default is `10`.

#' @param title_text_size Numeric. Font size for the plot title. Default is `14`.

#' @param legend_text_size Numeric. Font size for the legend text. Default is `12`.

#'

#' @return A ggplot object representing the generated boxplot.

#'

#' @examples

#' # Generate a boxplot with default settings

#' plot <- generate_boxplot(

#'   df = your_data,

#'   y_var = "extnpx"

#' )

#' print(plot)

#'

#' # Generate a boxplot grouped by `run_id` without faceting

#' plot <- generate_boxplot(

#'   df = your_data,

#'   y_var = "NPX",

#'   group_var = "run_id",

#'   facet_by = FALSE

#' )

#' print(plot)

#'

#' # Generate a boxplot with custom x and facet variables

#' plot <- generate_boxplot(

#'   df = your_data,

#'   y_var = "some_variable",

#'   x_var = "another_variable",

#'   facet_var = "some_facet_var",

#'   facet_by = TRUE

#' )

#' print(plot)

#'

#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{mutate}}

#' @export

generate_boxplot <- function(
    
  df, 
  
  y_var,                  
  
  group_var, 
  
  facet_var = "block",    
  
  facet_by = TRUE,       
  
  sample_type_filter , 
  
  x_var = "sample_id",    
  
  pattern_var = "plate_id", 
  
  axis_text_size = 8,    
  
  title_text_size = 14,  
  
  legend_text_size = 12   
  
) {
  
  # Filter and preprocess the data
  
  df_filtered <- df %>%
    
    filter(
      
      sample_type == sample_type_filter,
      
      !is.na(NPX)
      
    ) %>%
    
    mutate(!!sym(pattern_var) := factor(!!sym(pattern_var)) ) %>%
    
    distinct(!!sym(x_var), !!sym(group_var), !!sym(y_var), !!sym(facet_var), !!sym(pattern_var)) %>%
    
    group_by(!!sym(group_var))
  
  
  

  p <- ggplot(df_filtered, aes(x =interaction(!!sym(x_var), plate_id), y = !!sym(y_var))) +
    
    geom_boxplot_pattern(
      
      aes(
        fill = !!sym(group_var), 
        
        pattern = !!sym(pattern_var), 
        
        pattern_fill = !!sym(pattern_var)
        
      ),
      
      outlier.colour = "red",
      
      outlier.shape = NA,
      
      outlier.size = 3,
      
      notch = FALSE,
      
      na.rm = TRUE
      
    ) +
    

    theme(
      
      axis.line = element_line(colour = "black"),
      
      axis.text.x = element_blank(),
      
      axis.text.y = element_text(size = axis_text_size),
      
      plot.title = element_text(hjust = 0.5, size = title_text_size),
      
      legend.title = element_blank(),
      
      legend.text = element_text(size = legend_text_size), legend.position = "bottom"
      
    ) +
    
    scale_pattern_manual(values = c("none", "circle")) +
    
    olink_fill_discrete() +
    
    olink_color_discrete()
  
  
  
  # Optionally add faceting
  
  if (facet_by) {
    
    p <- p + facet_wrap(as.formula(paste(". ~", facet_var)), scales = "free")
    
  }
  
  
  
  return(p)
  
}