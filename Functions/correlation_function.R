
#' Perform per assay Correlation Analysis Between Comparison Pairs
#'
#' This function calculates correlations between specified comparison pairs 
#' (e.g., runs, instruments, or other factors) and summarizes the results. 
#' It supports optional filtering by `sample_type` and `assay_type`. If 
#' `comparison_pairs` is not provided, the function automatically generates all 
#' pairwise combinations of the unique values in the `comparison_column`.
#'
#' @param data A data frame containing the input data for correlation analysis. 
#'   The data frame should include columns for the `comparison_column`, `value_column`, 
#'   and any other relevant metadata such as `sample_type`, `assay_type`, and `plate_id`.
#' @param comparison_pairs Optional. A list of named pairs specifying the comparisons 
#'   (e.g., runs or instruments) to be analyzed. Each pair should be a list 
#'   with elements `comp1` and `comp2` representing the names of the comparison factors. 
#'   If `NULL`, the function will generate all pairwise combinations of the unique values 
#'   in the `comparison_column`.
#' @param value_column A string specifying the column name in the data frame 
#'   that contains the values for correlation (e.g., `"NPX"`).
#' @param comparison_column A string specifying the column name that identifies 
#'   the comparison factors (e.g., `"run_id"` or `"instrument"`).
#' @param filter_sample_type Optional. A string specifying a value of `sample_type` 
#'   to filter the data. Use `NULL` to skip filtering by `sample_type`.
#' @param filter_assay_type Optional. A string specifying a value of `assay_type` 
#'   to filter the data. Use `NULL` to skip filtering by `assay_type`.
#'
#' @return A list with two components:
#' \item{results_list}{A list of data frames, each containing the correlation results 
#'   for one comparison pair, including `estimate` (correlation coefficient), 
#'   `p_value` (p-value), `Adj.pvalue` (adjusted p-value), and significance thresholds.}
#' \item{summary}{A data frame summarizing the correlations for each comparison pair 
#'   by block, including the minimum, first quartile, mean, median, third quartile, 
#'   and maximum correlation estimates.}
#'
#' @details The function handles cases where insufficient observations are available 
#'   for correlation by skipping those pairs or blocks. The correlation results 
#'   are adjusted for multiple testing using the Benjamini-Hochberg method.
#'
#' @examples
#' # Example with run pairs and filters
#' run_pairs <- list(
#'   list(comp1 = "RunA", comp2 = "RunB"),
#'   list(comp1 = "RunC", comp2 = "RunD")
#' )
#' result <- correlation_analysis(
#'   data = my_data,
#'   comparison_pairs = run_pairs,
#'   value_column = "NPX",
#'   comparison_column = "run_id",
#'   filter_sample_type = "SAMPLE",
#'   filter_assay_type = "assay"
#' )
#'
#' # Example with automatic generation of comparison pairs
#' result <- correlation_analysis(
#'   data = my_data,
#'   value_column = "NPX",
#'   comparison_column = "instrument"
#' )
#'
#' @export

correlation_function <- function(data, 
                                 comparison_pairs = NULL, 
                                 value_column, 
                                 comparison_column, 
                                 filter_sample_type = NULL, 
                                 filter_assay_type = NULL) {
  cor_results_list <- list()
  
  # Generate all combinations of the comparison column if `comparison_pairs` is NULL
  if (is.null(comparison_pairs)) {
    unique_comparisons <- unique(data[[comparison_column]])
    comparison_pairs <- combn(unique_comparisons, 2, simplify = FALSE)
    comparison_pairs <- lapply(comparison_pairs, function(x) list(comp1 = x[1], comp2 = x[2]))
  }
  
  # Optionally filter data by `sample_type` and `assay_type`
  if (!is.null(filter_sample_type)) {
    data <- data %>% filter(sample_type == filter_sample_type)
  }
  if (!is.null(filter_assay_type)) {
    data <- data %>% filter(assay_type == filter_assay_type)
  }
  
  # Loop through each pair of comparison factors
  for (pair in comparison_pairs) {
    comp1 <- pair$comp1
    comp2 <- pair$comp2
    pair_name <- paste0(comp1, "_vs_", comp2)
    
    all_counts_cor <- data %>%
      mutate(plate_id = substr(plate_id, 1, 7)) %>%
      select(run_id = !!sym(comparison_column), 
             sample_index, 
             assay, 
             olink_id, 
             plate_id, 
             block, 
             !!sym(value_column)) %>%
      group_by(sample_index, assay, olink_id, plate_id, block) %>%
      spread(run_id, !!sym(value_column)) %>%
      ungroup()
    
    cor_results <- data.frame(
      olink_id = character(),
      Plate = character(),
      block = character(),
      estimate = numeric(),
      p_value = numeric()
    )
    
    for (a in unique(all_counts_cor$olink_id)) {
      olink_id_data <- all_counts_cor %>% filter(olink_id == a)
      
      for (p in olink_id_data %>% distinct(plate_id) %>% pull(plate_id)) {
        block_data <- olink_id_data %>% filter(plate_id == p)
        
        # Filter for finite values in both comparison columns
        block_data <- block_data %>%
          filter(is.finite(!!sym(comp1)) & is.finite(!!sym(comp2)))
        
        # Ensure there are at least two finite observations
        if (nrow(block_data) > 1) {
          test <- tryCatch({
            cor.test(block_data[[comp1]], block_data[[comp2]])
          }, error = function(e) NULL)
          
          if (!is.null(test)) {
            cor_result_aq <- data.frame(
              olink_id = a,
              Plate = p,
              block = block_data$block[1], # Assuming one block per olink_id
              estimate = test$estimate,
              p_value = test$p.value
            )
            cor_results <- rbind(cor_results, cor_result_aq)
          }
        }
      }
    }
    
    # Adjust p-values and classify significance
    cor_results <- cor_results %>%
      mutate(Adj.pvalue = p.adjust(p_value, method = "BH")) %>%
      mutate(Threshold = ifelse(Adj.pvalue < 0.05, "Significant", "Non-Significant")) %>%
      filter(!is.na(estimate))
    
    # Store results in the list
    cor_results_list[[pair_name]] <- cor_results
  }
  
  # Combine results
  merged_cor_results <- bind_rows(cor_results_list, .id = "Comparison")
  
  cor_summary <- merged_cor_results %>%
    group_by(Comparison, block) %>%
    summarise(
      Min = min(estimate, na.rm = TRUE),
      Q1 = quantile(estimate, probs = 0.25, na.rm = TRUE),
      Mean = mean(estimate, na.rm = TRUE),
      Median = median(estimate, na.rm = TRUE),
      Q3 = quantile(estimate, probs = 0.75, na.rm = TRUE),
      Max = max(estimate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(block) %>% # Sort by block
    mutate(across(where(is.numeric), ~ round(., digits = 2)))
  
  return(list(
    results_list = cor_results_list,
    summary = cor_summary
  ))
}
