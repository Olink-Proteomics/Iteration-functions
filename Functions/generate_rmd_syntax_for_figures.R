#' Generate RMarkdown Syntax for Figures in a Folder
#'
#' This function generates RMarkdown-compatible syntax for all `.png` files in a specified folder.
#' Each figure is assigned a unique label, dynamically generated using a specified prefix and 
#' a sequential number (e.g., `countreg1`, `countreg2`, etc.). The syntax is designed for direct 
#' inclusion in an RMarkdown document.
#'
#' @param folder_path Character. The path to the folder containing the `.png` files.
#' @param label_prefix Character. A prefix for the figure labels (default is "countreg").
#' Each figure label in the RMarkdown output will be of the form `fig:<label_prefix><number>`.
#' @param width Character. The width of the figures in the RMarkdown output (default is "80%").
#' Specify as a percentage or a fixed width (e.g., "50%", "600px").
#'
#' @return A character vector where each element is a line of RMarkdown syntax for a figure.
#' The syntax includes the figure path, label, and width information. For example:
#' `![Run-to-Run Count regression analysis. \\label{fig:countreg1}](folder_path/figure_1.png){width=80%}`
#'
#' @examples
#' # Generate RMarkdown syntax for `.png` files in a folder
#' folder <- "Results/Maximus/NovaseqX/regression_plots_log2count_run_id"
#' rmd_syntax <- generate_rmd_syntax_for_figures(folder, label_prefix = "countreg", width = "80%")
#'
#' # Print the RMarkdown syntax for direct inclusion
#' cat(rmd_syntax, sep = "\n")
#'
#' @seealso \code{\link[base]{list.files}}, \code{\link[base]{paste0}}, \code{\link[base]{cat}}
#'
#' @export
generate_rmd_syntax_for_figures <- function(folder_path, label_prefix = "countreg", width = "80%") {
  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    stop("The specified folder does not exist.")
  }
  
  # List all PNG files in the folder
  files <- list.files(folder_path, pattern = "\\.png$", full.names = FALSE)
  
  # Sort files to ensure consistent order
  files <- sort(files)
  
  # Generate RMarkdown syntax with unique labels for each file
  rmd_syntax <- sapply(seq_along(files), function(i) {
    file <- files[i]
    paste0("![Run-to-Run Count regression analysis. \\label{fig:", label_prefix, i, "}](", 
           file.path(folder_path, file), 
           "){width=", width, "}")
  })
  
  return(rmd_syntax)
}

  