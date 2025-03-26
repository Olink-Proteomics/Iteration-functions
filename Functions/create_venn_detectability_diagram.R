library(dplyr)
library(ggVennDiagram)


# Function to generate Venn diagrams for detectability overlap between instruments
create_venn_detectability_diagram <- function(df, detectability_col, thresholds, comp_var,
                                              olink_id_col="olink_id")
{
  # Ensure detectability column is numeric
  df <- df %>%
    mutate(!!detectability_col := as.numeric(.data[[detectability_col]]))
  
  # List to store Venn diagram objects
  venn_plots <- list()
  
  # Loop over each threshold to generate a Venn diagram
  for (thresh in thresholds) {
    
    # Filter olink_ids that are detectable in at least `thresh`% of samples per instrument
    detectable_sets <- df %>%
            filter(Detectability >= thresh) %>%
      group_by(!!sym(comp_var)) %>%
      summarise(olink_ids = list(unique(.data[[olink_id_col]])), .groups = "drop")
    
    # Convert to named list for Venn diagram
    venn_data <- setNames(detectable_sets$olink_ids, detectable_sets[[comp_var]])
    
    # Generate Venn diagram
    venn_plot <- ggVennDiagram(venn_data) +
      ggtitle(paste("Assays Overlap ≥", thresh *100, "% Detectability"))
    
    # Store plot in list
    venn_plots[[paste0("threshold_", thresh)]] <- venn_plot
  }
  
  return(venn_plots)
}
