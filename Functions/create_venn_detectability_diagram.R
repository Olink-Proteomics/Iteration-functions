library(dplyr)
library(ggVennDiagram)
library(patchwork)


create_venn_detectability_diagram <- function(df, detectability_col, thresholds, comp_var,
                                              olink_id_col = "olink_id", block_col = "block",
                                              by_block = FALSE, combine_blocks = FALSE,
                                              figure_title = "Venn Diagrams by block") {
  
  df <- df %>%
    mutate(!!detectability_col := as.numeric(.data[[detectability_col]]))
  
  venn_plots <- list()
  
  if (by_block) {
    blocks <- unique(df[[block_col]])
    
    for (block in blocks) {
      df_block <- df %>% filter(.data[[block_col]] == !!block)
      
      for (thresh in thresholds) {
        condition <- if (thresh == 0) {
          df_block[[detectability_col]] > 0
        } else {
          df_block[[detectability_col]] >= thresh
        }
        
        detectable_sets <- df_block %>%
          filter(condition) %>%
          group_by(!!sym(comp_var)) %>%
          summarise(olink_ids = list(unique(.data[[olink_id_col]])), .groups = "drop")
        
        if (nrow(detectable_sets) < 2) next
        
        venn_data <- setNames(detectable_sets$olink_ids, detectable_sets[[comp_var]])
        n_sets <- length(venn_data)
        
        plot_title <- paste0("Block: ", block, ", Threshold: ", if (thresh == 0) "> 0" else paste0("≥ ", thresh))
        
        if (n_sets > 3) {
          venn_plot <- ggVennDiagram(venn_data, label_alpha = 0) +
            scale_fill_gradient(low = "lightblue", high = "darkblue") +
            theme_void() +
            ggtitle(plot_title) +
            theme(
              plot.title = element_text(size = 14, face = "bold"),
              text = element_text(size = 10),
              legend.position = "none"
            )
        } else {
          venn_plot <- ggVennDiagram(venn_data) +
            ggtitle(plot_title)
        }
        
        plot_key <- paste0("block_", block, "_threshold_", thresh)
        venn_plots[[plot_key]] <- venn_plot
      }
    }
    
    if (combine_blocks) {
      combined_plot <- wrap_plots(venn_plots) +
        plot_annotation(title = figure_title)
      return(combined_plot)
    }
    
  } else {
    for (thresh in thresholds) {
      condition <- if (thresh == 0) {
        df[[detectability_col]] > 0
      } else {
        df[[detectability_col]] >= thresh
      }
      
      detectable_sets <- df %>%
        filter(condition) %>%
        group_by(!!sym(comp_var)) %>%
        summarise(olink_ids = list(unique(.data[[olink_id_col]])), .groups = "drop")
      
      if (nrow(detectable_sets) < 2) next
      
      venn_data <- setNames(detectable_sets$olink_ids, detectable_sets[[comp_var]])
      n_sets <- length(venn_data)
      
      plot_title <- paste("Detectability", if (thresh == 0) "> 0" else paste0("≥ ", thresh * 100, "%"))
      
      if (n_sets > 3) {
        venn_plot <- ggVennDiagram(venn_data, label_alpha = 0) +
          scale_fill_gradient(low = "lightblue", high = "darkblue") +
          theme_void() +
          ggtitle(plot_title) +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            text = element_text(size = 10),
            legend.position = "none"
          )
      } else {
        venn_plot <- ggVennDiagram(venn_data) +
          ggtitle(plot_title)
      }
      
      venn_plots[[paste0("threshold_", thresh)]] <- venn_plot
    }
  }
  
  return(venn_plots)
}
