#' Summarize Detectability Across Runs with Plot Option
#'
#' This function calculates detectability for specified runs (or defaults to all unique values in the specified `run_var` column),
#' merges the results, calculates the median and maximum detectability across runs for each row, and summarizes the average detectability per block.
#' It also supports generating detectability plots. This function does not compare runs.
#'
#' @param df Data frame. The input dataset containing assay information.
#' @param run_var Character. The name of the column representing the run identifier.
#' @param run_ids Character vector. A list of values in the `run_var` column to calculate detectability for. If not provided, all unique values are used.
#' @param sample_name Character. The name of the column that is used to identify replicates of the same sample.
#'   If not provided, replicates won't be considered. Default is `"sample_id"`.
#' @param npx Character. The name of the column representing NPX values.
#' @param background Character. The name of the column representing the limit of detection.
#' @param matrix_name Character. The name of the column that is used to identify the matrix type.
#'   If not provided, all samples will be considered.
#' @param matrix Character. The name of the matrix type to be selected to calculate detectability.
#' @param caption Character. A caption to add to the output table.
#' @param format Character. The output table format, either `"latex"` or `"html"`. Default is `"latex"`.
#' @param fontsize Numeric. Font size for the table text. Default is `12` (only affects LaTeX tables).
#' @param plot Logical. Whether to generate a detectability plot. Default is `TRUE`.
#' @param facet_var Character. The variable to facet the plot by (default is `"block"`).
#'
#' @return A list with three elements:
#' \item{`table`}{The formatted table summarizing average detectability per block.}
#' \item{`data`}{The merged data frame with `median_detectability` and `max_detectability` columns.}
#' \item{`plot`}{The ggplot object for the detectability plot (if `plot = TRUE`).}
#'
#' @examples
#' # Summarize detectability for all runs with a plot
#' result <- summarize_detectability_across_runs(
#'   df = your_data,
#'   run_var = "run_id",
#'   npx = "pc_normalized_npx",
#'   background = "lod_in_npx",
#'   matrix_name = "matrix_name",
#'   matrix = "Plasma",
#'   caption = "Average detectability per block",
#'   format = "latex",
#'   plot = TRUE
#' )
#'
#' @export
summarize_detectability_across_runs <- function(df, 
                                    run_var, 
                                    run_ids = NULL, 
                                    sample_name = "sample_id", 
                                    npx, 
                                    background, 
                                    matrix_name = NULL, 
                                    matrix = NULL, 
                                    caption = "Detectability Summary", 
                                    format = "latex", 
                                    fontsize = 12,
                                    plot = TRUE,
                                    facet_var = "block") {
  # Validate format
  if (!format %in% c("latex", "html")) {
    stop("Invalid format. Use 'latex' or 'html'.")
  }
  
  # Validate that run_var exists
  if (!run_var %in% names(df)) {
    stop("The specified run_var column does not exist in the dataset.")
  }
  
  # Default to all unique values in the run_var column if run_ids are not provided
  if (is.null(run_ids)) {
    run_ids <- unique(df[[run_var]])
  }
  
  # Validate that run_ids exist in the dataset
  if (!all(run_ids %in% unique(df[[run_var]]))) {
    stop("One or more specified run_ids do not exist in the dataset.")
  }
  
  # Initialize an empty list to store detectability data for each run
  detectability_list <- list()
  
  # Calculate detectability for each run
  for (run_id in run_ids) {
    detectability <- calculate_detectability(
      df = filter(df, !!sym(run_var) == !!run_id),
      sample_name = sample_name,
      npx = npx,
      background = background,
      matrix_name = matrix_name,
      matrix = matrix
    ) %>%
      rename(!!run_id := Detectability) # Rename "Detectability" column to the run_id
    detectability_list[[run_id]] <- detectability
  }
  
  # Merge all detectability data
  merged_data <- reduce(detectability_list, left_join, by = c("olink_id", "assay", "block"))
  
  # Add median and max detectability columns
  merged_data <- merged_data %>%
    rowwise() %>%
    mutate(
      median_detectability = median(c_across(all_of(run_ids)), na.rm = TRUE),
      max_detectability = max(c_across(all_of(run_ids)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Summarize average detectability per block
  summary_data <- merged_data %>%
    group_by(block) %>%
    summarise(across(all_of(run_ids), ~ round(mean(.x, na.rm = TRUE), 2), .names = "{.col}"), .groups = "drop")
  
  # Generate a formatted table using kable
  table <- summary_data %>%
    kbl(
      position = "!h",
      format = format,
      caption = caption,
      col.names = c("Block", run_ids)
    )
  
  # Add font size for LaTeX format
  if (format == "latex") {
    table <- table %>% kable_styling(font_size = fontsize, full_width = FALSE)
  } else if (format == "html") {
    table <- table %>% kable_classic(full_width = FALSE)
  }
  
  # Generate the detectability plot if requested
  detect_plot <- NULL
  if (plot) {
    detect_plot <- merged_data %>%
      pivot_longer(cols = all_of(run_ids), names_to = "Run_id", values_to = "detectFreq") %>%
      ggplot(aes(x = detectFreq, fill = Run_id)) +
      geom_histogram(
        colour = "black",
        lwd = 0.75,
        linetype = 1,
        position = "dodge"
      ) +
      facet_wrap(as.formula(paste("~", facet_var))) +
      theme_bw() +
      xlab("Fraction detected") +
      ylab("Number of assays") +
      theme(
        strip.text = element_text(size = 15),
        legend.position = "bottom"
      ) +
      olink_color_discrete() +
      olink_fill_discrete()
  }
  
  # Return the table, merged data, and plot
  return(list(table = table, data = merged_data, plot = detect_plot))
}
