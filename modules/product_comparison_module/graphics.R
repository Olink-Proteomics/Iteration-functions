# HS, 202020205
######################
#
# Source required libraries and functions
sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)


# Load configuration
config <- yaml::read_yaml("modules/product_comparison_module/config.yml")


# Extract configuration elements
input_data <- config$input_data
parameters <- config$parameters
plot_settings <- config$plot_settings


# Dynamically construct the output directory
product_name <- parameters$Product
output_dir <- file.path("output", product_name, "Product_comparison")


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

#calculate CV, detectability for each run_id

cv_data<-aggregate_cv(yourdata, "NPX", parameters$Background, comp_var="run_id", filter_value =  "SAMPLE_CONTROL", additionnal_var=c(parameters$run_var,"assay_label") )

detectability_data<-aggregate_detectability(
    df = filter(yourdata,sample_qc=="PASS", !(sample_id%in% outliers), sample_label=="concordance"),
    sample_name = "sample_name",
    comp_var = parameters$run_var,
    run_var = "run_id",
    npx = parameters$npx_var,
    background = parameters$Background,
  )

Concordance_count_npx<-yourdata %>% filter(sample_label=="concordance", assay_type=="assay") %>% 
  group_by(!!sym(parameters$run_var), olink_id, sample_name,assay,OlinkID,sample_type,block,assay_type) %>% 
  summarise(log2count=median(log2count, na.rm =TRUE), NPX=median(NPX, na.rm =TRUE), .groups = "drop") 

Concordance_count_npx_above_background<- yourdata %>% filter(if (parameters$Background == "detection") {
  get(parameters$Background) == "above EOB"  
} else {
  NPX > get(parameters$Background) }) %>% filter(sample_label=="concordance", assay_type=="assay") %>% 
  group_by(!!sym(parameters$run_var), olink_id, sample_name,sample_type,assay,block,assay_type,OlinkID, Panel) %>% 
  summarise(log2count=median(log2count, na.rm =TRUE), NPX=median(NPX, na.rm =TRUE), .groups = "drop") 
# 
 write_rds(Concordance_count_npx_above_background, file = "modules/product_comparison_module/Concordance_count_npx_above_background.rds")
# 
 write_rds(Concordance_count_npx, file = "modules/product_comparison_module/Concordance_count_npx.rds")
 

##Count distribution
 
  # Density Plots
 generate_density_plot(
   filter(yourdata, sample_label=="concordance"), 
   "log2count", 
   parameters$run_var, 
   facet_var = "block", 
   base_size = plot_settings$fontsize, 
   output_path = file.path(output_dir, paste0("count_density.", plot_settings$format)),
   file_format = plot_settings$format
 )
 
 
 generate_density_plot(
   filter(yourdata, sample_label=="concordance"), 
   "NPX", 
   parameters$run_var, 
   facet_var = "block", 
   base_size = plot_settings$fontsize, 
   output_path = file.path(output_dir, paste0("NPX_density.", plot_settings$format)),
   file_format = plot_settings$format
 )
 


distplot<- Concordance_count_npx %>% mutate(SampleID=paste0(sample_name, Product),Panel=Product) %>% filter(!is.na(olink_id)) %>% 
  filter(sample_type=="SAMPLE")%>% 
  olink_dist_plot(color_g = !!rlang::sym(parameters$run_var))+
  theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)+
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

generate_simple_regression_plot(Concordance_count_npx %>% filter(!is.na(olink_id)) , uniqueID = "sample_name", reg_var = "log2count", wide_var = parameters$run_var, additional_vars = "olink_id",
                               group = "block", output_path = file.path(output_dir, paste0("log2count_regression.", plot_settings$format)))
generate_simple_regression_plot(Concordance_count_npx%>% filter(!is.na(olink_id)) , uniqueID = "sample_name", reg_var = "NPX", wide_var = parameters$run_var, additional_vars = "olink_id",
                                group = "block",  output_path = file.path(output_dir, paste0("NPX_regression.", plot_settings$format)))


#Correlation plot

NPX_cor <- correlation_function(
  data = Concordance_count_npx_above_background %>% mutate(plate_id="all_plates",block ="all_block", sample_index=sample_name),
  value_column = "NPX",
  comparison_column = parameters$run_var,
  filter_sample_type = "SAMPLE",
  filter_assay_type = "assay"
)

corr_plot_above<- ggplot(NPX_cor$results_list[[1]], aes(x=estimate, y= after_stat(count/sum(count))))+
  geom_histogram(binwidth = 0.01, fill="steelblue", color="black", alpha=0.7, boundary=1)+
  labs(title = "NPX correlation distribution,only data above background included",
        x="Correlation estimate",
        y="Proportion")+
  theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)


ggsave(
  file.path(output_dir, paste0("corrplot_above.", plot_settings$format)),
  plot = corr_plot_above,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)
rm(corr_plot_above)

################
NPX_cor <- correlation_function(
  data = Concordance_count_npx %>% mutate(plate_id="all_plates",block ="all_block", sample_index=sample_name),
  value_column = "NPX",
  comparison_column = parameters$run_var,
  filter_sample_type = "SAMPLE",
  filter_assay_type = "assay"
)

corr_plot<- ggplot(NPX_cor$results_list[[1]], aes(x=estimate, y= after_stat(count/sum(count))))+
  geom_histogram(binwidth = 0.01, fill="steelblue", color="black", alpha=0.7, boundary=1)+
  labs(title = "NPX correlation distribution,all data included",
       x="Correlation estimate",
       y="Proportion")+
  theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)


ggsave(
  file.path(output_dir, paste0("corrplot_all.", plot_settings$format)),
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
  file.path(output_dir, paste0("violin_intra.", plot_settings$format)),
  plot = violin,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)


#CV violin graphs

df<-cv_data %>%select(Product,olink_id, assay_label, inter_plate_CV, plate_id) %>% filter(!is.na(inter_plate_CV)) %>% 
  group_by(Product,olink_id,assay_label) %>% summarise(inter_plate_CV = mean(inter_plate_CV, na.rm = T, .groups ="drop")) 


violin<-ggplot(df, aes(x = reorder(Product, inter_plate_CV, FUN = median), y = inter_plate_CV, fill = assay_label)) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +  # Scaled violin
  geom_boxplot(width = 0.3, fill = "white", outlier.shape = NA, notch = F) +  # Boxplot with notch
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0), alpha = 0.3, size = 0.5) +  # Improved jitter
  
  
  scale_fill_manual(values = c( "red", "turquoise", "blue")) +  # Highlight overlapping assays
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