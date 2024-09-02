doc <- "

This script is used to create figure 3 of feature selection.

Usage:
  fig_feature_selection.R [options]

Options:
 --csv=CSV              Path to read in the feature selection result
 --output=OUTPUT        Path to write out computational time
 --width=WIDTH          Width of the graph [default: 7]
 --height=height        Height of the graph [default: 7]
"

# Load libraries
suppressPackageStartupMessages(library(dplyr))
#library(purrr)
library(ggplot2)
library(stringr)
suppressPackageStartupMessages(library(ComplexHeatmap))
library(tidyr)
#library(grid)
#library(ggplotify)
#library(corrplot)
# Parse cli
opt <- docopt::docopt(doc)

# Convenient vars
input_path <- opt$csv
output_path <- opt$output
width <- as.numeric(opt$width)
height <- as.numeric(opt$height)
# convert weights > ranks > spearman corr >
# heatmap > stratify by sim and real > stratify by tuned params

# Custom function
wrangle_feat_selection <- function(df) {
  df %>%
  # Need to fix error for mogonet appending view in front of feature
  mutate(
    feature = case_when(
      str_detect(method, "mogonet") ~ paste(view, feature, sep="_"),
      TRUE ~ feature
    ),
    # Rename rgcca to sgcca
    method = case_when(
      str_detect(method, "rgcca") ~ "sgcca",
      TRUE ~ method
    )
  ) %>%
    # SGCCA has slight problem in missing a feature in tcga-brca and tcga-kipan???
    # So need to drop this
    filter(! (
      (feature == "RNAseq_HiSeq_Gene_level_GAGE1" & dataset_name == "tcga-brca") |
        (feature == "RNAseq_HiSeq_Gene_level_C8orf71" & dataset_name == "tcga-kipan")
    )
    ) %>%
    mutate(
      is_simulated = case_when(
        str_detect(dataset_name, "sim") ~ "simulated",
        TRUE ~ "real"
      )
    )
}


#input_path <- "data/all_feature_selection_results.csv"

# First load in data and wrangle it
feat_result_df <- read.csv(input_path) %>%
  as_tibble() %>%
  wrangle_feat_selection()

# Some intermediate dataframes here for using later
# Get rankings first
ranking_df <- feat_result_df %>%
  group_by(method, dataset_name, view) %>%
  # So the coef with rank number smaller means better
  # ie. rank 1 (highest) > rank2 > ... rank 10 > ... rank n
  mutate(ranking = rank(desc(abs(coef)))) %>%
  ungroup() %>%
  select(-coef)

wide_ranking_df <- ranking_df %>%
  pivot_wider(names_from = method, values_from=ranking)

# Then calculate correlation matrix of real data now
spearson_cor_real_mat <- wide_ranking_df %>%
  filter(is_simulated == "real") %>%
  select_if(is.numeric) %>%
  select(order(colnames(.))) %>%
  as.matrix() %>%
  cor(method="spearman")

# This function plots heatmap for visualizing real data feature
# selection ranking and taken the spearson correlation
plot_real_heatmap <- function(cor_mat, method_palette="Paired") {

  methods <- rownames(cor_mat)
  # Assign the colors
  # For the method to use default Paired
  method_colors <- RColorBrewer::brewer.pal(n=length(methods), method_palette)
  names(method_colors) <- methods

  # Row wise
  col_ha <- HeatmapAnnotation(
    Method = methods,
    col = list(
      Method = method_colors
    ),
    show_annotation_name = F
  )
  # Custom color
  # Create the color mapping function
  col_fun <- circlize::colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
  #col_fun <- RColorBrewer::brewer.pal(n=11, "RdYlBu")
  # Plot the heatmap
  real_ht <- Heatmap(
    cor_mat,
    col = col_fun,
    name = "Spearman Correlation",
    show_row_names = T, show_column_names = T,
    #top_annotation = col_ha,
    #right_annotation = row_ha
  )

  real_data_heatmap_plot <- grid.grabExpr(draw(real_ht, merge_legends = TRUE))
  return(real_data_heatmap_plot)


}

# Then plot it
real_data_heatmap_plot <- plot_real_heatmap(spearson_cor_real_mat)
# And save to disk
ggsave(output_path, plot = real_data_heatmap_plot,
       width = width, height = height)
message("Saved image of ", width, " x ", height, " to ", output_path)
# ===================================================================

#plot_real_corr <- function(cor_mat, cor_method = "color") {
  # Then to plot it
  # corrplot(
  #   cor_mat, order="hclust",
  #   method=cor_method,
  #   #    type="lower",
  #   tl.col = "black",
  #   tl.srt = 45,
  #   col = COL2('RdYlBu')
  # )

  # See https://stackoverflow.com/questions/53734543/converting-corrplot-output-to-grob
  # save correlation matrix colors to a vector, then make coloured matrix grob transparent
  #real_corr_plot <- grid.grabExpr(draw(corr_grob))
  #return(real_corr_plot)
#}

