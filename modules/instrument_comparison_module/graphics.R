# HS, 202020205
######################
#
# Source required libraries and functions
sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)


# Load configuration
config <- yaml::read_yaml("modules/instrument_comparison_module/config.yml")


# Extract configuration elements
input_data <- config$input_data
parameters <- config$parameters
plot_settings <- config$plot_settings


# Dynamically construct the output directory
product_name <- parameters$Product
output_dir <- file.path("output", product_name, "Instrument_comparison")


# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Inform the user where the figures will be saved
message("Figures will be saved in: ", output_dir)


# Load the input data
yourdata <- readRDS(input_data)


# PCA Plot
PCA_plot <- yourdata %>%
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


# QC Plot
qc_plot <- olink_qc_plot(
  yourdata %>% filter(sample_type == "SAMPLE"),
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



distplot<-yourdata%>% 
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




# Boxplots for Control Types
control_types <- list(
  SAMPLE_CONTROL = "SAMPLE_CONTROL",
  NEGATIVE_CONTROL = "NEGATIVE_CONTROL",
  PLATE_CONTROL = "PLATE_CONTROL"
)


for (control_name in names(control_types)) {
  distplot <- generate_boxplot(
    yourdata,
    y_var = "NPX",
    group_var = parameters$run_var,
    sample_type_filter = control_types[[control_name]]
  )
  
  ggsave(
    file.path(output_dir, paste0("distplot_", control_name, ".", plot_settings$format)),
    plot = distplot,
    width = plot_settings$width,
    height = plot_settings$height,
    bg = "white",
    dpi = plot_settings$dpi
  )
}


# Density Plots
generate_density_plot(
  yourdata, 
  "log2count", 
  parameters$run_var, 
  facet_var = "block", 
  base_size = plot_settings$fontsize, 
  output_path = file.path(output_dir, paste0("count_density.", plot_settings$format)),
  file_format = plot_settings$format
)


generate_density_plot(
  yourdata, 
  "NPX", 
  parameters$run_var, 
  facet_var = "block", 
  base_size = plot_settings$fontsize, 
  output_path = file.path(output_dir, paste0("NPX_density.", plot_settings$format)),
  file_format = plot_settings$format
)

# Regression Plots

generate_all_regression_plots(yourdata, "log2count", parameters$run_var, output_dir = output_dir)

generate_all_regression_plots(yourdata, "NPX", parameters$run_var, output_dir = output_dir)

generate_simple_regression_plot(filter(yourdata,assay_type !="assay"), uniqueID = "sample_index",additional_vars = c("plate_id", "block"),group = "assay_type", reg_var = "log2count", wide_var = parameters$run_var, output_path = file.path(output_dir, paste0("Internal_control_regression.", plot_settings$format)))



# To do: create a loop to do this for all matrices

# Detectability
results <- summarize_detectability_across_runs(
  df =  filter(yourdata,sample_qc=="PASS"),
  run_var = parameters$run_var,
  npx = parameters$npx_var,
  background = parameters$Background,
  matrix_name = parameters$matrix_column,
  matrix = "Plasma",
  sample_name = "sample_name",
  caption = "Average detectability per block",
  format = "latex",
  plot = TRUE
)


ggsave(
  file.path(output_dir, paste0("detectplot.", plot_settings$format)),
  plot = results$plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)


#Generate CV regression plots

cv_data <-  calculate_intra_plate_cv(yourdata, comp_var = parameters$run_var, npx = parameters$npx_var, threshold = parameters$Background, filter_col = "sample_type", filter_value = "SAMPLE_CONTROL") 

generate_simple_regression_plot(cv_data,reg_var = "intra_CV", wide_var = parameters$run_var, group = "block", uniqueID = "olink_id", additional_vars =  c("block", "plate_id"),
                                output_path = file.path(output_dir, paste0("intra_CV_regression_SC.", plot_settings$format)))



cv_data <-  calculate_inter_plate_cv(yourdata, comp_var = parameters$run_var, npx = parameters$npx_var, threshold = parameters$Background) 

generate_simple_regression_plot(cv_data,reg_var = "inter_plate_CV", wide_var = parameters$run_var, group = "block", uniqueID = "olink_id",additional_vars = "block",
                                output_path = file.path(output_dir, paste0("inter_plate_CV_regression_SC.", plot_settings$format)))


#Generate CV histogram plots

intracv<- generate_cv_summary(
  df = filter(yourdata,sample_qc=="PASS"),
  comp_var = parameters$run_var,
  npx = parameters$npx_var,
  threshold = parameters$Background,
  group_vars = c(parameters$run_var, "plate_id", "block"),
  pivot_var = parameters$run_var,
  value_col = "intra_CV",
  caption = "Average of intra plate CV per block, plate, and run", 
  format = "latex",
  plot = TRUE
)

ggsave(
  file.path(output_dir, paste0("intracv_regression.", plot_settings$format)),
  plot = intracv$plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)


intercv<- generate_cv_summary(
  df = filter(yourdata,sample_qc=="PASS"),
  comp_var = parameters$run_var,
  npx = parameters$npx_var,
  threshold = parameters$Background,
  group_vars = c(parameters$run_var, "plate_id", "block"),
  pivot_var = parameters$run_var,
  value_col = "inter_plate_CV",
  caption = "Average of intra plate CV per block, plate, and run", 
  format = "latex",
  plot = TRUE
)

ggsave(
  file.path(output_dir, paste0("interplatecv_regression.", plot_settings$format)),
  plot = intercv$plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)



# Calculate detectability for each instrument

run_ids <- unique(yourdata[[parameters$run_var]])
detectability_list <- list()


for (run_id in run_ids) {
  detectability <- calculate_detectability(
    df = filter(yourdata, !!sym(parameters$run_var) == !!run_id),
    sample_name = "sample_name",
    npx = parameters$npx_var,
    background = parameters$Background,
    matrix_name = parameters$matrix_column,
    matrix = "Plasma"
  ) %>%
    mutate(comp_var= run_id) 
  detectability_list[[run_id]] <- detectability
}
merged_data <- reduce(detectability_list, rbind)

generate_simple_regression_plot(merged_data,reg_var = "Detectability", wide_var = "comp_var", group = "block", uniqueID = "olink_id",additional_vars = "block",
                                output_path = file.path(output_dir, paste0("Detectability_plasma_regression_SC.", plot_settings$format)))




