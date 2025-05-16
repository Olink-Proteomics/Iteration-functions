# HS, 202020205
######################
#
# Source required libraries and functions
source("mylib.R")
sapply(list.files("Functions", pattern = "\\.R$", full.names = TRUE), source)


# Load configuration
config <- yaml::read_yaml("config_product.yml")


# Extract configuration elements
input_data <- config$input_data
parameters <- config$parameters
plot_settings <- config$plot_settings


# Dynamically construct the output directory
product_name <- parameters$Product
output_dir <- file.path("Results", product_name, "Product_comparison")


# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Inform the user where the figures will be saved
message("Figures will be saved in: ", output_dir)

outliers<-c() # to be added to the config

# Load the input data
#yourdata <- readRDS(input_data)
yourdata <- arrow::read_parquet(input_data) %>% mutate(NPX= !!rlang::sym(parameters$npx_var)) %>% # some olink_analyse functions need NPX
  filter(sample_qc=="PASS", !(sample_id %in% outliers))                                           

#calculate CV, detectability for each run_id

cv_data<-aggregate_cv(yourdata, "NPX", parameters$Background, comp_var="run_id", filter_value =  "SAMPLE_CONTROL", additionnal_var=parameters$run_var) %>% 
  group_by(!!sym(parameters$run_var),plate_id, olink_id)


detectability_data<-aggregate_detectability(
    df = yourdata,
    sample_name = "sample_name",
    comp_var = parameters$run_var,
    run_var = "run_id",
    npx = parameters$npx_var,
    background = parameters$Background,
    matrix_name = "matrix",
    matrix = "Plasma"
  )

Concordance_count_npx<-yourdata %>% filter(sample_label=="concondorance_1", assay_type=="assay") %>% 
  group_by(!!sym(parameters$run_var), olink_id, sample_name,assay,OlinkID,sample_type,assay_type) %>% 
  summarise(log2count=median(log2count, na.rm =TRUE), NPX=median(NPX, na.rm =TRUE), .groups = "drop") 

Concordance_count_npx_above_background<- yourdata %>% filter(if (parameters$Background == "detection") {
  get(parameters$Background) == "above EOB"  
} else {
  NPX > get(parameters$Background) }) %>% filter(sample_label=="concondorance_1", assay_type=="assay") %>% 
  group_by(!!sym(parameters$run_var), olink_id, sample_name,sample_type,assay,assay_type,OlinkID, Panel) %>% 
  summarise(log2count=median(log2count, na.rm =TRUE), NPX=median(NPX, na.rm =TRUE), .groups = "drop") 

write_rds(Concordance_count_npx_above_background, file = "Concordance_count_npx_above_background.rds")

write_rds(Concordance_count_npx, file = "Concordance_count_npx.rds")

#Sample range

# 
# Sample_range<-yourdata %>% filter(sample_label=="concondorance_1") %>% 
#   
#   group_by(!!sym(parameters$run_var), run_id, olink_id,assay) %>% 
#   summarise(Sample_range_concordance=max(NPX,na.rm=T) -min(NPX,na.rm = T), .groups = "drop") %>% 
# group_by(parameters$run_var, run_id, olink_id,assay) %>% 
#   summarise(Sample_range_concordance=median(Sample_range_concordance, na.rm=TRUE), .groups = "drop")




# PCA Plot
PCA_plot <- Concordance_count_npx %>% mutate(SampleID=paste0(sample_name, Product)) %>% 
  filter(sample_type == "SAMPLE") %>%
  olink_pca_plot(
    col = parameters$run_var,
    label_outliers = TRUE,
    outlierDefX = 3.5,
    outlierDefY = 3.5
  )


Plot <- PCA_plot[[1]] + theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)


ggsave(
  file.path(output_dir, paste0("PCA_plot.", plot_settings$format)),
  plot = Plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)

rm(PCA_plot, Plot)

# QC Plot
qc_plot <- olink_qc_plot(
  Concordance_count_npx %>% mutate(SampleID=paste0(sample_name, Product), Panel=Product) %>%
    filter(sample_type == "SAMPLE"),
  color_g = !!rlang::sym(parameters$run_var)
) + theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)


ggsave(
  file.path(output_dir, paste0("qc_plot.", plot_settings$format)),
  plot = qc_plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)

rm(qc_plot)


distplot<- Concordance_count_npx %>% mutate(SampleID=paste0(sample_name, Product),Panel=Product) %>% 
  filter(sample_type=="SAMPLE")%>% 
  olink_dist_plot(color_g = !!rlang::sym(parameters$run_var))+theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)+
  geom_boxplot(outlier.colour = "WHITE")+theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle=0,hjust=1))+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


ggsave(
  file.path(output_dir, paste0("distplot.", plot_settings$format)),
  plot = distplot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)
rm(distplot)


# Regression Plots

generate_simple_regression_plot(Concordance_count_npx, uniqueID = "sample_name", reg_var = "log2count", wide_var = parameters$run_var, additional_vars = "olink_id",
                                output_path = file.path(output_dir, paste0("log2count_regression.", plot_settings$format)))
generate_simple_regression_plot(Concordance_count_npx, uniqueID = "sample_name", reg_var = "NPX", wide_var = parameters$run_var, additional_vars = "olink_id",
                                output_path = file.path(output_dir, paste0("NPX_regression.", plot_settings$format)))


#Correlation plot

NPX_cor <- correlation_function(
  data = Concordance_count_npx_above_background %>% mutate(plate_id="all_plates",block ="all_block", sample_index=sample_name),
  value_column = "NPX",
  comparison_column = parameters$run_var,
  filter_sample_type = "SAMPLE",
  filter_assay_type = "assay"
)

corr_plot<- ggplot(NPX_cor$results_list[[1]], aes(x=estimate, y= after_stat(count/sum(count))))+
  geom_histogram(binwidth = 0.01, fill="steelblue", color="black", alpha=0.7, boundary=1)+
  labs(title = "NPX correlation distribution",
        x="Correlation estimate",
        y="Proportion")+
  theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)


ggsave(
  file.path(output_dir, paste0("corrplot.", plot_settings$format)),
  plot = corr_plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)
rm(corr_plot)
## Detectability Venn diagram


venn_plots<-create_venn_detectability_diagram(
  df = detectability_data,
  detectability_col = "Detectability",
  thresholds = c(0.1, 0.5), # Detectability thresholds (10% and 50%)
  comp_var = parameters$run_var)


# Print both Venn diagrams
print(venn_plots[["threshold_0.1"]])  # Venn for 10% detectability
print(venn_plots[["threshold_0.5"]])  # Venn for 50% detectability

combined_plot <- venn_plots[["threshold_0.1"]] + venn_plots[["threshold_0.5"]] +plot_layout(ncol = 2)

ggsave(
  file.path(output_dir, paste0("Venn_detect.", plot_settings$format)),
  plot = combined_plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)
rm(combined_plot)
#CV distribution, violin per product

df<-cv_data %>%select(Product,olink_id,intra_CV, plate_id) %>% filter(!is.na(intra_CV)) %>% 
  group_by(Product,olink_id) %>% summarise(intra_CV = mean(intra_CV, na.rm = T, .groups ="drop"))
overlapping_assays <- df %>% 
  group_by(olink_id) %>%
  summarise(n_products = n_distinct(Product)) %>%
  filter(n_products > 1) %>%
  pull(olink_id)


# Step 2: Add Overlap Information
df <- df %>%
  mutate(overlap_status = ifelse(olink_id %in% overlapping_assays, "Overlapping", "Unique"))


violin<-ggplot(df, aes(x = reorder(Product, intra_CV, FUN = median), y = intra_CV, fill = overlap_status)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +  # Scaled violin
  geom_boxplot(width = 0.3, fill = "white", outlier.shape = NA, notch = F) +  # Boxplot with notch
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0), alpha = 0.3, size = 0.5) +  # Improved jitter
  
  
  scale_fill_manual(values = c("Overlapping" = "red", "Unique" = "turquoise")) +  # Highlight overlapping assays
  scale_y_continuous(limits = c(0, quantile(df$intra_CV, 0.99))) +  # Remove extreme outliers
  theme_minimal() +
  labs(title = "Distribution of Intra-Plate CV by Product",
       x = "Product (Sorted by Median CV)",
       y = "Intra-Plate CV",
       fill = "Assay Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


ggsave(
  file.path(output_dir, paste0("violin_intra.", plot_settings$format)),
  plot = violin,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)


#CV violin graphs

df<-cv_data %>%select(Product,olink_id,inter_plate_CV, plate_id) %>% filter(!is.na(inter_plate_CV)) %>% 
  group_by(Product,olink_id) %>% summarise(inter_plate_CV = mean(inter_plate_CV, na.rm = T, .groups ="drop"))
overlapping_assays <- df %>% 
  group_by(olink_id) %>%
  summarise(n_products = n_distinct(Product)) %>%
  filter(n_products > 1) %>%
  pull(olink_id)


df <- df %>%
  mutate(overlap_status = ifelse(olink_id %in% overlapping_assays, "Overlapping", "Unique"))


violin<-ggplot(df, aes(x = reorder(Product, inter_plate_CV, FUN = median), y = inter_plate_CV, fill = overlap_status)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +  # Scaled violin
  geom_boxplot(width = 0.3, fill = "white", outlier.shape = NA, notch = F) +  # Boxplot with notch
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0), alpha = 0.3, size = 0.5) +  # Improved jitter
  
  
  scale_fill_manual(values = c("Overlapping" = "red", "Unique" = "turquoise")) +  # Highlight overlapping assays
  scale_y_continuous(limits = c(0, quantile(df$inter_plate_CV, 0.99))) +  # Remove extreme outliers
  theme_minimal() +
  labs(title = "Distribution of Inter-Plate CV by Product",
       x = "Product (Sorted by Median CV)",
       y = "Inter-Plate CV",
       fill = "Assay Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


ggsave(
  file.path(output_dir, paste0("violin_inter.", plot_settings$format)),
  plot = violin,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)