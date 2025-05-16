#' Summarize Detectability Across Runs with Plot Option (with Optional Grouping)
#'
#' [Updated description omitted for brevity—add as needed]
#'
#' @param additional_group_vars Character vector. Optional additional grouping variables to include in the output and summary.
#'
#' @return A list with three elements: table, data, and plot (if requested).
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
                                                facet_var = "block",
                                                additional_group_vars = NULL) {
  
  
  if (!format %in% c("latex", "html")) {
    stop("Invalid format. Use 'latex' or 'html'.")
  }
  
  
  if (!run_var %in% names(df)) {
    stop("The specified run_var column does not exist in the dataset.")
  }
  
  
  if (is.null(run_ids)) {
    run_ids <- unique(df[[run_var]])
  }
  
  
  if (!all(run_ids %in% unique(df[[run_var]]))) {
    stop("One or more specified run_ids do not exist in the dataset.")
  }
  
  
  # Grouping keys for merging and analysis
  grouping_key <- c("olink_id", "assay", "block", additional_group_vars)
  
  
  # Compute detectability per run
  detectability_list <- lapply(run_ids, function(run_id) {
    calculate_detectability(
      df = filter(df, !!sym(run_var) == !!run_id),
      sample_name = sample_name,
      npx = npx,
      background = background,
      matrix_name = matrix_name,
      matrix = matrix,
      additional_group_vars = additional_group_vars
    ) %>%
      rename(!!run_id := Detectability)
  })
  
  
  names(detectability_list) <- run_ids
  
  
  # Merge detectability data
  merged_data <- reduce(detectability_list, left_join, by = grouping_key)
  
  
  merged_data <- merged_data %>%
    rowwise() %>%
    mutate(
      median_detectability = median(c_across(all_of(run_ids)), na.rm = TRUE),
      max_detectability = max(c_across(all_of(run_ids)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  # Summarize table ONLY by 'block', no extra group vars
  summary_data <- merged_data %>%
    group_by(block) %>%
    summarise(
      across(all_of(run_ids), ~ round(mean(.x, na.rm = TRUE), 2), .names = "{.col}"),
      .groups = "drop"
    )
  
  
  table <- summary_data %>%
    kbl(
      position = "!h",
      format = format,
      caption = caption,
      col.names = c("Block", run_ids)
    )
  
  
  if (format == "latex") {
    table <- table %>% kable_styling(font_size = fontsize, full_width = FALSE)
  } else {
    table <- table %>% kable_classic(full_width = FALSE)
  }
  
  
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
      facet_wrap(as.formula(paste("~", facet_var)),scales = "free") +
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
  
  
  return(list(
    table = table,
    data = merged_data,  # contains additional_group_vars
    plot = detect_plot
  ))
}
