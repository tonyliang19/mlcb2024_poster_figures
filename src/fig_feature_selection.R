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
# Parse cli
opt <- docopt::docopt(doc)

# Convenient vars
input_path <- opt$csv
output_path <- opt$output

# convert weights > ranks > spearman corr >
# heatmap > stratify by sim and real > stratify by tuned params
input_path <- "data/all_feature_selection_results.csv"
df <- read.csv(input_path) %>%
  as_tibble() %>%
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


# Get rankings first
ranking_df <- df %>%
  group_by(method, dataset_name, view) %>%
  # So the coef with rank number smaller means better
  # ie. rank 1 (highest) > rank2 > ... rank 10 > ... rank n
  mutate(ranking = rank(desc(abs(coef)))) %>%
  ungroup() %>%
  select(-coef)

wide_ranking_df <- ranking_df %>%
  pivot_wider(names_from = method, values_from=ranking)

# Then calculate different correlation matrix by real or simulated data
wide_real_df <- wide_ranking_df %>%
  filter(is_simulated == "real")

wide_sim_df <- wide_ranking_df %>%
  filter(is_simulated == "simulated")

# Then compute the
spearson_cor_mat <- wide_real_df %>%
  select_if(is.numeric) %>%
  as.matrix() %>%
  cor(method="spearman")



method_palette <- "Paired"
methods <- sort(rownames(spearson_cor_mat))
# Assign the colors
# For the method to use default Paired
method_colors <- RColorBrewer::brewer.pal(n=length(methods), method_palette)
names(method_colors) <- methods




# Then annotations of the heatmap to use
# Column wise


# Row wise
col_ha <- HeatmapAnnotation(
  Method = methods,
  col = list(
    Method = method_colors
  ),
  show_annotation_name = F
)

# Plot the heatmap
real_ht <- Heatmap(
  spearson_cor_mat,
  name = "Spearman Correlation",
  show_row_names = T, show_column_names = T,
  #top_annotation = col_ha,
  #right_annotation = row_ha
)

real_data_heatmap_plot <- grid.grabExpr(draw(real_ht, merge_legends = TRUE))


ggsave(output_path, plot = real_data_heatmap_plot)
