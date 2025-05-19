# # HS, 202020205
# ######################
# #
# # Source required libraries and functions
sapply(list.files("../functions", pattern = "\\.R$", full.names = TRUE), source)
# 
# 
# # Load configuration
 #config <- yaml::read_yaml("modules/product_performance_module/config.yml")


# Extract configuration elements
input_data <- config$input_data
parameters <- config$parameters
plot_settings <- config$plot_settings


# Dynamically construct the output directory
product_name <- parameters$Product
instrument_name <- parameters$Instrument
output_dir <- file.path("output", product_name, instrument_name)


# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Inform the user where the figures will be saved
message("Figures will be saved in: ", output_dir)


# Load the input data
yourdata <- readRDS(input_data) %>% mutate(NPX= !!rlang::sym(parameters$npx_var))


# PCA Plot
PCA_plot <- yourdata %>% mutate(Panel=sample_subtype) %>% 
  filter(sample_type == "SAMPLE") %>%
  olink_pca_plot(
    col = parameters$run_var,
    byPanel = TRUE,
    label_outliers = TRUE,
    outlierDefX = 3.5,
    outlierDefY = 3.5
  )

combined_plot<-wrap_plots(PCA_plot,ncol = 2)



Plot <- combined_plot + theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)


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
  yourdata %>% filter(sample_type == "SAMPLE") %>%  mutate(Panel=sample_subtype),
  color_g = !!rlang::sym(parameters$run_var)
) +
  facet_wrap(. ~ Panel, ncol = 2, scales = "free")+
  theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)


ggsave(
  file.path(output_dir, paste0("qc_plot.", plot_settings$format)),
  plot = qc_plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)

outliers<-c()

distplot<-yourdata%>% filter(plate_id=="Plate1") %>% 
  filter(sample_type=="SAMPLE")%>% 
  olink_dist_plot(color_g = sample_subtype)+theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)+
  geom_boxplot(outlier.colour = "WHITE")+theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle=0,hjust=1))+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


ggsave(
  file.path(output_dir, paste0("distplot_plate1.", plot_settings$format)),
  plot = distplot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)


distplot<-yourdata%>% filter(plate_id=="Plate2") %>% 
  filter(sample_type=="SAMPLE")%>% 
  olink_dist_plot(color_g = sample_subtype)+theme_tufte(base_size = plot_settings$fontsize, base_family = "serif", ticks = TRUE)+
  geom_boxplot(outlier.colour = "WHITE")+theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle=0,hjust=1))+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())


ggsave(
  file.path(output_dir, paste0("distplot_plate2.", plot_settings$format)),
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

generate_all_regression_plots(yourdata %>% filter(sample_qc=="PASS", !(sample_id%in% outliers), sample_type=="SAMPLE"), 
                              "log2count", parameters$run_var, output_dir = output_dir)

generate_all_regression_plots(yourdata %>% filter(sample_qc=="PASS", !(sample_id%in% outliers),sample_type=="SAMPLE"),
                              "NPX", parameters$run_var, output_dir = output_dir)




# Detectability in concordance
results<-summarize_detectability_across_runs(
  df =  filter(yourdata,sample_qc=="PASS", !(sample_id%in% outliers), sample_label=="concordance"),
  run_var = parameters$run_var,
  npx = parameters$npx_var,
  background = parameters$Background,
  sample_name = "sample_name",
  caption = "Average detectability per block",
  format = "latex",
  plot = TRUE
)

ggsave(
  file.path(output_dir, paste0("detectplot_concordance.", plot_settings$format)),
  plot = results$plot,
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)

## Assays detected in at least one sample across all replicates across matrices

matrix_types <- unique((yourdata %>% filter(sample_type=="SAMPLE"))[[as.character(sym(parameters$matrix_column))]] )

process_matrix <- function(matrix_name) {
  
  results <- summarize_detectability_across_runs(
    df = filter(yourdata, sample_qc == "PASS", !sample_id %in% outliers),
    run_var = parameters$run_var,
    npx = parameters$npx_var,
    background = parameters$Background,
    matrix_name = parameters$matrix_column,
    matrix = matrix_name,
    caption = paste0("Average detectability per block, ", gsub("_", "\\\\_", matrix_name)),
    format = "latex",
    additional_group_vars = "assay_label"
  )
  return(results$data)
}

all_results <- map_dfr(matrix_types, function(mat) {
  df <- process_matrix(mat)
  df$matrix_type <- mat  # add identifier column
  return(df)
})

Vennplot<-create_venn_detectability_diagram(
  df = all_results ,
  detectability_col = "max_detectability",
  thresholds = c(0), # Detectability thresholds (10% and 50%)
  comp_var = "matrix_type")


ggsave(
  file.path(output_dir, paste0("Venn_detect_across_matrix.", plot_settings$format)),
  plot = Vennplot[["threshold_0"]],
  width = plot_settings$width,
  height = plot_settings$height,
  bg = "white",
  dpi = plot_settings$dpi
)

