#' Score Assay Quality Based on Product Metrics with Eligibility Filtering
#'
#' This function evaluates assay performance using correlation, detectability, CV, and sample range
#' metrics, but only for rows that meet eligibility criteria. Ineligible rows return NA in all score columns.
#'
#' @param datafile A data frame containing assay metric columns.
#' @param product1 A string for the concordance comparison product (e.g. "product1").
#' @param product2 A string for the product to be scored (e.g. "product2").
#' @param oid_match A string with the name of the OID column (e.g. "ht_oid").
#' @param corr_thresh Numeric. Correlation threshold (e.g. 0.8).
#' @param cv_thresh Numeric. CV threshold (e.g. 10).
#' @param detectability_thresh Numeric. Detectability difference threshold (e.g. 0.1).
#' @param sample_range_thresh Numeric. Sample range concordance threshold (e.g. 1.6).
#' @param pass_score Numeric. Minimum score to label assay as "PASS" (e.g. 2).
#' @param output_dir String. Directory to save output file. Default is current dir.
#' @param save_output Logical. If TRUE, saves output CSV file. Default is FALSE.
#'
#' @return A data frame with scores and labels for eligible rows; NA otherwise.
#'
#' @export
score_assay <- function(datafile,
                        product1,
                        product2,
                        oid_match,
                        corr_thresh,
                        cv_thresh,
                        detectability_thresh,
                        sample_range_thresh,
                        pass_score,
                        output_dir = ".",
                        save_output = FALSE) {
  
  # Build column names
  corr_col <- paste0("corr_npx_", product2)
  detect_col <- paste0("diff_detect_", product2)
  intraCV_col <- paste0("diff_intraCV_", product2)
  interCV_col <- paste0("diff_interCV_", product2)
  sample_range_col <- paste0("diff_sample_range_concordance_", product2)
  concordance_col <- paste0("Sample_range_concordance_", product1)
  detectability_concord_col <- paste0("detectability_concordance_", product2)
  
  # Determine eligibility with NA handling
  datafile <- datafile %>%
    mutate(is_eligible = ifelse(
      !is.na(.data[[oid_match]]) &
        !is.na(.data[[detectability_concord_col]]) &
        .data[[detectability_concord_col]] > 0,
      TRUE, FALSE
    ))
  
  # Score columns using conditional logic
  datafile <- datafile %>%
    mutate(
      corr_score = ifelse(
        is_eligible & (
          (!is.na(.data[[corr_col]]) & .data[[corr_col]] >= corr_thresh) |
            (!is.na(.data[[corr_col]]) & .data[[corr_col]] < corr_thresh &
               !is.na(.data[[concordance_col]]) & .data[[concordance_col]] < sample_range_thresh) |
            (is.na(.data[[corr_col]]) & !is.na(.data[[concordance_col]]) & .data[[concordance_col]] < sample_range_thresh)
        ), 1, ifelse(is_eligible, 0, NA)
      ),
      
      detect_score = ifelse(
        is_eligible & (
          (!is.na(.data[[detect_col]]) & .data[[detect_col]] >= -detectability_thresh) |
            (!is.na(.data[[detect_col]]) & .data[[detect_col]] < -detectability_thresh &
               !is.na(.data[[corr_col]]) & .data[[corr_col]] > corr_thresh)
        ), 1, ifelse(is_eligible, 0, NA)
      ),
      
      cv_score = ifelse(
        is_eligible & (
          (!is.na(.data[[intraCV_col]]) | !is.na(.data[[interCV_col]])) &
            (
              pmax(.data[[intraCV_col]], .data[[interCV_col]], na.rm = TRUE) <= cv_thresh |
                (
                  pmax(.data[[intraCV_col]], .data[[interCV_col]], na.rm = TRUE) > cv_thresh &
                    !is.na(.data[[corr_col]]) & .data[[corr_col]] > corr_thresh
                )
            )
        ), 1, ifelse(is_eligible, 0, NA)
      ),
      
      sample_range_score = ifelse(
        is_eligible & (
          (!is.na(.data[[sample_range_col]]) & .data[[sample_range_col]] >= -detectability_thresh) |
            (!is.na(.data[[sample_range_col]]) & .data[[sample_range_col]] < -detectability_thresh &
               !is.na(.data[[corr_col]]) & .data[[corr_col]] > corr_thresh)
        ), 1, ifelse(is_eligible, 0, NA)
      )
    ) %>%
    mutate(
      overall_score = rowSums(select(., corr_score, detect_score, sample_range_score, cv_score), na.rm = FALSE),
      Assay_iteration_label = ifelse(is_eligible,
                                     ifelse(overall_score >= pass_score, "PASS", "FAIL"),
                                     NA)
    ) %>%
    select(-is_eligible)
  
  
  # Save output file if needed
  if (save_output) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    filename <- normalizePath(file.path(output_dir, paste0("Assay_spreadsheet_", product1, "_iteration.csv")), mustWork = FALSE)
    write.csv2(datafile, file = filename, row.names = FALSE)
    message(paste("File saved to:", filename))
  }
  
  
  return(datafile)
}
