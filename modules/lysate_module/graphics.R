# HS, 202020205
######################
#
# Source required libraries and functions

sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)


# Load configuration
config <- yaml::read_yaml("modules/lysate_module/config.yml")


# Extract configuration elements
input_data <- config$input_data
parameters <- config$parameters
plot_settings <- config$plot_settings


# Dynamically construct the output directory
product_name <- parameters$Product
output_dir <- file.path("output", product_name, "Lysate_module")


# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Inform the user where the figures will be saved
message("Figures will be saved in: ", output_dir)

outliers<-c() # to be added to the config

# Load the input data
#yourdata <- readRDS(input_data)
yourdata <- read_rds(input_data) %>% mutate(NPX= !!rlang::sym(parameters$npx_var)) %>% # some olink_analyse functions need NPX
  filter(sample_qc=="PASS", !(sample_id %in% outliers))                                           

#Intra CV distribution, violin plot 

cv_data<-aggregate_cv(yourdata, "NPX", parameters$Background, comp_var="run_id", filter_col = "sample_label", filter_value =  "Tissue_Lysate_replicates", additionnal_var=c(parameters$run_var,"assay_label") )


df<-cv_data %>%select(Product,olink_id,assay_label, intra_CV, plate_id) %>% filter(!is.na(intra_CV)) %>% 
  group_by(Product,olink_id,assay_label) %>% summarise(intra_CV = mean(intra_CV, na.rm = T, .groups ="drop"))



violin<-ggplot(df, aes(x = reorder(Product, intra_CV, FUN = median), y = intra_CV, fill = assay_label)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +  # Scaled violin
  geom_boxplot(width = 0.3, fill = "white", outlier.shape = NA, notch = F) +  # Boxplot with notch
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0), alpha = 0.3, size = 0.5) +  # Improved jitter
  
  scale_fill_manual(values = c( "red", "turquoise", "blue")) +  # Highlight overlapping assays
  scale_y_continuous(limits = c(0, quantile(df$intra_CV, 0.99))) +  # Remove extreme outliers
  theme_minimal() +
  labs(title = "Distribution of Intra-Plate CV by Product",
       x = "Product (Sorted by Median CV)",
       y = "Intra-Plate CV",
       fill = "Assay Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


ggsave(
  file.path(output_dir, paste0("violin_intra_tissue.", plot_settings$format)),
  plot = violin,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)



cv_data<-aggregate_cv(yourdata, "NPX", parameters$Background, comp_var="run_id", filter_col = "sample_label", filter_value =  "Cell_Lysate_replicates", additionnal_var=c(parameters$run_var,"assay_label") )


df<-cv_data %>%select(Product,olink_id,assay_label, intra_CV, plate_id) %>% filter(!is.na(intra_CV)) %>% 
  group_by(Product,olink_id,assay_label) %>% summarise(intra_CV = mean(intra_CV, na.rm = T, .groups ="drop"))



violin<-ggplot(df, aes(x = reorder(Product, intra_CV, FUN = median), y = intra_CV, fill = assay_label)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +  # Scaled violin
  geom_boxplot(width = 0.3, fill = "white", outlier.shape = NA, notch = F) +  # Boxplot with notch
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0), alpha = 0.3, size = 0.5) +  # Improved jitter
  
  scale_fill_manual(values = c( "red", "turquoise", "blue")) +  # Highlight overlapping assays
  scale_y_continuous(limits = c(0, quantile(df$intra_CV, 0.99))) +  # Remove extreme outliers
  theme_minimal() +
  labs(title = "Distribution of Intra-Plate CV by Product",
       x = "Product (Sorted by Median CV)",
       y = "Intra-Plate CV",
       fill = "Assay Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


ggsave(
  file.path(output_dir, paste0("violin_intra_cell.", plot_settings$format)),
  plot = violin,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)




# Detectability Venn diagram


detectability_data<-aggregate_detectability(
  df = yourdata,
  sample_name = "sample_name",
  comp_var = parameters$run_var,
  run_var = "run_id",
  npx = parameters$npx_var,
  background = parameters$Background,
)

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