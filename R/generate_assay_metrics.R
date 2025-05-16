#Script to generate assay metrics
# HS, 202020205
######################
#
# Source required libraries and functions
sapply(list.files("../functions", pattern = "\\.R$", full.names = TRUE), source)


# Load configuration
config <- yaml::read_yaml("../config/config_product.yml")


# Extract configuration elements
input_data <- config$input_data
lysate_data <-config$input_lysate
parameters <- config$parameters
plot_settings <- config$plot_settings
eval_cfg <- config$assay_evaluation_criteria



# Construct the output directory
product_name <- parameters$Product
output_dir <- file.path("Results", product_name, "Assay_metrics")


# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Inform the user where the figures will be saved
message("Data will be saved in: ", output_dir)

outliers<-c() # to be added to the config

# Load the input data

yourdata <- read_rds(input_data) %>% mutate(NPX= !!rlang::sym(parameters$npx_var)) %>% # some olink_analyse functions need NPX
  filter(sample_qc=="PASS", !(sample_id %in% outliers))  


#Detectability

##Maximus detectability based on all samples
# Get unique matrix types

matrix_types <- unique((yourdata %>% filter(sample_type=="SAMPLE"))[[as.character(sym(parameters$matrix_column))]] )

detectability_list <-NULL
# Loop over each matrix 
detectability_list <- map(matrix_types, function(mat) {
  
  # Filter for current matrix and fixed product
  df_filtered <- yourdata %>%
    filter(!!sym(parameters$matrix_column) == mat, !!sym(parameters$run_var) == parameters$Product)
  
  if (nrow(df_filtered) == 0) return(NULL)
  
  
  # Run detectability
  detectability_data <- aggregate_detectability(
    df = df_filtered,
    sample_name = "sample_name",
    comp_var = parameters$run_var,
    run_var = "run_id",
    npx = parameters$npx_var,
    background = parameters$Background
  )
  
  
  # Rename Detectability column to include matrix name only
  new_col_name <- paste0("detectability_", mat, "_", parameters$Product)
  detectability_data <- detectability_data %>%
    rename(!!new_col_name := Detectability) %>% select(-parameters$run_var)
  
  
  return(detectability_data)
})

# Remove NULL entries from the list
detectability_list_clean <- compact(detectability_list)


# Proceed only if there's at least one data frame
if (length(detectability_list_clean) > 0) {
  Detectability_all <- reduce(detectability_list_clean, full_join, by = c("olink_id", "assay", "block"))
} else {
  Detectability_all <- NULL
}



## Plasma detectability concordance samples for both products
concordance <- filter(yourdata,sample_label=="concordance")
products <- unique(concordance[[parameters$run_var]])

detectability_list <-NULL

# Loop over each product
Detectability_list <- map(products, function(prod) {
  
  df_filtered <- concordance %>%
    filter(!!sym(parameters$run_var) == prod)
  
  if (nrow(df_filtered) == 0) return(NULL)
  
  
  # Run detectability function
  detectability_data <- aggregate_detectability(
    df = df_filtered,
    sample_name = "sample_name",
    comp_var = parameters$run_var,
    run_var = "run_id",
    npx = parameters$npx_var,
    background = parameters$Background,
  ) %>% select(-assay)
  
  
  # Rename the Detectability column to include just the product
  new_col_name <- paste0("detectability_concordance_", prod)
  detectability_data <- detectability_data %>%
    rename(!!new_col_name := Detectability) %>% select(-parameters$run_var)
  
  
  return(detectability_data)
})

Detectability_concordance <- reduce(Detectability_list, 
                                    full_join, by= c("olink_id", "block"))  %>%
  mutate(diff_detect_EHT=detectability_concordance_Maximus - detectability_concordance_EHT)

#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""#


#calculate CV, based on Sample controls

cv_data<-aggregate_cv(yourdata, "NPX", parameters$Background, comp_var="run_id", filter_value =  "SAMPLE_CONTROL", additionnal_var=parameters$run_var) %>% 
  group_by(!!sym(parameters$run_var),olink_id) %>% summarise(intra_CV =mean(intra_CV, na.rm = TRUE),
                                                            inter_plate_CV  =mean(inter_plate_CV , na.rm = TRUE),
                                                            inter_run_CV =mean(inter_run_CV, na.rm = TRUE)) 

SC_cv_data <- cv_data %>% rename(intra_cv_CS=intra_CV,inter_plate_cv_CS= inter_plate_CV, inter_run_cv_CS=inter_run_CV ) %>% 
  pivot_wider(names_from = parameters$run_var, values_from = intra_cv_CS :inter_run_cv_CS) %>% 
  mutate(diff_intraCV_EHT= intra_cv_CS_Maximus -intra_cv_CS_EHT,
         diff_interCV_EHT=inter_plate_cv_CS_Maximus-inter_plate_cv_CS_EHT)


#CV based on healthy samples
cv_data<-aggregate_cv(yourdata, "NPX", parameters$Background, comp_var="run_id", filter_col = "sample_info", 
                      filter_value =  "Healthy pool",additionnal_var=parameters$run_var) %>% 
group_by(!!sym(parameters$run_var),olink_id) %>% summarise(intra_CV =mean(intra_CV, na.rm = TRUE),
                                                           inter_plate_CV  =mean(inter_plate_CV , na.rm = TRUE),
                                                           inter_run_CV =mean(inter_run_CV, na.rm = TRUE)) 

healthypool_cv_data <- cv_data %>% rename(intra_cv_concordance_healthy_pool=intra_CV,inter_plate_cv_concordance_healthy_pool= inter_plate_CV, inter_run_cv_concordance_healthy_pool=inter_run_CV ) %>% 
  pivot_wider(names_from = parameters$run_var, values_from = intra_cv_concordance_healthy_pool :inter_run_cv_concordance_healthy_pool) 



#calculate CV, based on Lysate replicates


#CV based on tissue lysate replicates
lysate_data <- read_rds(lysate_data) %>% mutate(NPX= !!rlang::sym(parameters$npx_var)) %>% # some olink_analyze functions need NPX
  filter(sample_qc=="PASS", !(sample_id %in% outliers))  

cv_lysate_tissue<-aggregate_cv(lysate_data,
                               "NPX", parameters$Background,
                               comp_var="run_id", filter_col = "sample_label", 
                               filter_value =  "Tissue_Lysate_replicates",
                               additionnal_var= parameters$run_var) %>% 
   rename(intra_cv_lysate_tissue=intra_CV,inter_plate_cv_lysate_tissue= inter_plate_CV, inter_run_cv_lysate_tissue=inter_run_CV ) %>% 
  pivot_wider(names_from = parameters$run_var, values_from = intra_cv_lysate_tissue :inter_run_cv_lysate_tissue) %>% 
  select(-plate_id)


#CV based on cell lysate replicates



cv_lysate_cell<-aggregate_cv(lysate_data, "NPX",
                             parameters$Background,
                             comp_var="run_id", filter_col = "sample_label",
                             filter_value =  "Cell_Lysate_replicates", 
                             additionnal_var=parameters$run_var)%>% 
  rename(intra_cv_lysate_cell=intra_CV,inter_plate_cv_lysate_cell= inter_plate_CV, inter_run_cv_lysate_cell=inter_run_CV ) %>% 
  pivot_wider(names_from = parameters$run_var, values_from = intra_cv_lysate_cell :inter_run_cv_lysate_cell) %>% 
  select(-plate_id)



lysate_cv_data <- cv_lysate_tissue %>% left_join(cv_lysate_cell, by=c("olink_id"))

#
#
#


#Calculate NPX correlations
#between products


Concordance_count_npx_above_background<- yourdata %>% filter(if (parameters$Background == "detection") {
  get(parameters$Background) == "above EOB"  
} else {
  NPX > get(parameters$Background) }) %>% filter(sample_label=="concordance", assay_type=="assay") %>% 
  group_by(!!sym(parameters$run_var), olink_id, sample_name,sample_type,assay,assay_type,OlinkID, Panel) %>% 
  summarise(log2count=median(log2count, na.rm =TRUE), NPX=median(NPX, na.rm =TRUE), .groups = "drop") 

NPX_cor <- correlation_function(
  data = Concordance_count_npx_above_background %>% mutate(plate_id="all_plates",block ="all_block", sample_index=sample_name),
  value_column = "NPX",
  comparison_column = parameters$run_var,
  filter_sample_type = "SAMPLE",
  filter_assay_type = "assay"
)

corr_product<-NPX_cor$results_list[[1]] %>% mutate(corr_npx_EHT=round(estimate, 2)) %>% 
                                        select(olink_id,corr_npx_EHT) %>%  rownames_to_column(var = "rowname") %>% select(-rowname)

# 
# 
# # between replicate runs for maximus
# 
# result <- correlation_function(
#   data = filter(yourdata, matrix == "Plasma", !!sym(parameters$run_var) == parameters$Product , if (parameters$Background == "detection") {
#     get(parameters$Background) == "above EOB"  
#   } else {
#     NPX > get(parameters$Background) }),
#   value_column = "NPX",
#   comparison_column = "run_id",
#   filter_sample_type = "SAMPLE",
#   filter_assay_type = "assay",
# )
# 
# 
# 
# estimate_df_long <- purrr::imap_dfr(result$results_list, ~{
#   .x %>%
#     select(olink_id, Plate, estimate) %>%
#     mutate(run_comparison = .y)
# }) %>% group_by(run_comparison, olink_id) %>% 
#   summarise(corr_NPX=mean(estimate, na.rm=T), .groups = "drop")
# 

# across_run_corr<- estimate_df_long %>% 
#   mutate(col_name=paste0("corr_NPX_", run_comparison,"_", parameters$Product)) %>% 
#   select(olink_id,corr_NPX,col_name) %>% 
#   pivot_wider(names_from = col_name,values_from =  corr_NPX)







#
#Sample range
#in concordance

Sample_range_concordance<-yourdata %>% filter(sample_label=="concordance", assay_type=="assay") %>%

  group_by(!!sym(parameters$run_var), run_id, olink_id) %>%
  summarise(Sample_range_concordance=max(NPX,na.rm=T) -min(NPX,na.rm = T), .groups = "drop") %>%
group_by(!!sym(parameters$run_var), olink_id) %>%
  summarise(Sample_range_concordance=median(Sample_range_concordance, na.rm=TRUE), .groups = "drop") %>% 
  mutate(col_name=paste0("Sample_range_concordance","_", !!sym(parameters$run_var))) %>% 
  select(olink_id,Sample_range_concordance ,col_name) %>% 
  pivot_wider(names_from = col_name, values_from = Sample_range_concordance ) %>% 
  mutate(diff_sample_range_concordance_EHT=(Sample_range_concordance_Maximus -Sample_range_concordance_EHT)/Sample_range_concordance_EHT)




#Sample range in Tissue lysate samples

Sample_range_lysate_tissue<-lysate_data %>% filter(sample_label=="Tissue_Lysate_replicates") %>%

  group_by(!!sym(parameters$run_var), run_id, olink_id) %>%
  summarise(Sample_range_lysate_tissue=max(NPX,na.rm=T) -min(NPX,na.rm = T), .groups = "drop") %>%
  group_by(!!sym(parameters$run_var), olink_id) %>%
  summarise(Sample_range_lysate_tissue=median(Sample_range_lysate_tissue, na.rm=TRUE), .groups = "drop") %>%
  mutate(col_name=paste0("Sample_range_lysate_tissue","_", !!sym(parameters$run_var))) %>%
  select(olink_id,Sample_range_lysate_tissue ,col_name) %>%
  pivot_wider(names_from = col_name, values_from = Sample_range_lysate_tissue )


#Sample range in cell lysate samples

Sample_range_lysate_cell<-lysate_data %>% filter(sample_label=="Cell_Lysate_replicates") %>%
  
  group_by(!!sym(parameters$run_var), run_id, olink_id) %>%
  summarise(Sample_range_lysate_cell=max(NPX,na.rm=T) -min(NPX,na.rm = T), .groups = "drop") %>%
  group_by(!!sym(parameters$run_var), olink_id) %>%
  summarise(Sample_range_lysate_cell=median(Sample_range_lysate_cell, na.rm=TRUE), .groups = "drop") %>%
  mutate(col_name=paste0("Sample_range_lysate_cell","_", !!sym(parameters$run_var))) %>%
  select(olink_id,Sample_range_lysate_cell ,col_name) %>%
  pivot_wider(names_from = col_name, values_from = Sample_range_lysate_cell )

Sample_range_lysate <- Sample_range_lysate_tissue %>% left_join(Sample_range_lysate_cell, by="olink_id")


#Signal to noise

med_NC<-yourdata %>% filter(assay_type=="assay", sample_type=="NEGATIVE_CONTROL",!!sym(parameters$run_var) == parameters$Product ) %>%
  
  group_by(olink_id) %>% summarise(Median=median(NPX, na.rm=T)) %>% ungroup()


SN<-yourdata %>% filter(sample_label=="concordance",!!sym(parameters$run_var) == parameters$Product) %>%
  filter(assay_type=="assay") %>% 
  group_by(olink_id) %>% summarise(Median_Sample=median(NPX, na.rm=T)) %>% left_join(med_NC, by="olink_id") %>% 
  mutate(col_name=paste0("Median_SN","_", parameters$Product), SN=Median_Sample -Median ) %>% select(col_name,olink_id,SN) %>% 
  pivot_wider(names_from = col_name, values_from = SN)

#################################################################################
df<-NULL
#General info you want to keep
assay_info <- yourdata %>% filter(assay_type=="assay") %>%  distinct(olink_id, ht_oid,block)

detect<-Detectability_all %>% left_join(Detectability_concordance, by=c("olink_id",  "block")) %>% 
  select(-block)

df<-reduce(list(assay_info, detect, SC_cv_data, healthypool_cv_data,lysate_cv_data, Sample_range_concordance,Sample_range_lysate,SN, #across_run_corr
                corr_product),  full_join, by ="olink_id")


#Write scores


df_final<-score_assay(
  datafile = df,
  product1 = eval_cfg$product1,
  product2 = eval_cfg$product2,
  oid_match = eval_cfg$oid_match,
  corr_thresh = eval_cfg$corr_thresh,
  cv_thresh = eval_cfg$cv_thresh,
  detectability_thresh = eval_cfg$detectability_thresh,
  sample_range_thresh = eval_cfg$sample_range_thresh,
  pass_score = eval_cfg$pass_score,
  save_output = F,
  output_dir = output_dir
)

