
#' This function compares the variance of each assay NPX within quartiles across different runs. 
#' Returns variance test results for each assay and a summary table
#'
#' @param comp_var The run variable to use.
#' @param df input NPX data frame
#' @return A list containing:
#'   - `var_results`: A dataframe with quartiles and p-values.
#'   - `table`: A summary table categorizing significance.
#'
#' @import dplyr
#' @import tidyr
#' @import stats
#' 
variance_function <- function(df,comp_var) {
  
  quartiles <- df %>%
    filter(!is.na(NPX), assay_type == "assay", sample_type =="SAMPLE") %>%
    group_by(!!sym(comp_var), olink_id) %>%
    summarise(Quantile = quantile(NPX, na.rm = TRUE),
              Probs = paste("Q",seq(0,1,0.25), sep = "_"), .groups = "drop") %>% 
    pivot_wider(names_from = Probs, values_from = Quantile) %>% ungroup()
  
  
  dfq <- df %>% 
    filter(!is.na(NPX)  & assay_type=="assay") %>% 
    left_join(quartiles, by = c(comp_var,"olink_id"))%>% 
    mutate(Q_Group = ifelse( NPX < Q_0.25, "Q1",
                             ifelse(NPX < Q_0.5, "Q2",
                                    ifelse(NPX < Q_0.75, "Q3", "Q4")))) %>% 
    select(-c(Q_0, Q_0.25, Q_0.5, Q_0.75, Q_1)) 
  
  
  var_results <- data.frame(
    olink_id = as.character(),
    block = as.character(),
    Q_Group = as.character(),
    estimate = as.numeric(),
    p_value = as.numeric()
  )
  
  for (id in unique(dfq$olink_id)) {
    olink_data <- dfq %>% filter(olink_id == id)
    for (q in c("Q1", "Q2", "Q3", "Q4")) {
      group_data <- olink_data %>% filter(Q_Group == q)
      test <-var.test(group_data$NPX ~ group_data[[comp_var]])
      var_results <- rbind(var_results, data.frame(
        olink_id = id,
        block = unique(group_data$block),
        Q_Group = q,
        estimate = test$estimate,
        p_value = test$p.value
      ))
    }
  }
  
  var_results <- var_results %>% 
    mutate(Adj.pvalue = p.adjust(p_value, method = "BH")) %>% 
    mutate(Threshold = ifelse(Adj.pvalue < 0.05, "Significant", "Non-Significant"))  %>%
    filter(!is.na(estimate))
  
  summary <- table(var_results$Q_Group, var_results$Threshold)
  
  return(list(var_results = var_results, summary = summary))
}
