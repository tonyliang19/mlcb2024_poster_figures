doc <- "

This script is used to create figure 1 of performance evaluation

Usage:
  fig_performance_evaluation.R [options]

Options:
 --csv=CSV              File to load the csv
 --real_out=REAL_OUT    Path to write out performance auc of real datasets
 --sim_out=SIM_OUT      Path to write out performance auc of sim datasets
 --width=WIDTH          Width of the graph [default: 7]
 --height=height        Height of the graph [default: 7]
 --device=DEVICE        Device to print out [default: png]
 --dpi=DPI              Dots per inch [default: 300]
"

# Parse doc
opt <- docopt::docopt(doc)

# Load libraries
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(tibble)
library(here)
library(stringr)
library(tidyr)
suppressPackageStartupMessages(library(ComplexHeatmap))
# Load the variables from cli
input_path <- here(opt$csv)
real_output_path <- here(opt$real_out)
sim_output_path  <- here(opt$sim_out)
# Plot params
method_palette <- "Paired"
dataset_palette <- "Pastel1"
text_size <- 12
width <- as.numeric(opt$width)
height <- as.numeric(opt$height)
device <- opt$device
dpi <- as.numeric(opt$dpi)



# Verbose message
message("\nRendering figure of performance evaluation of classification")
message("\nInput path: ", input_path)

# Custom functions
wrangle_data <- function(df) {
  clean_df <- df %>%
    rename(method = method_name) %>%
    # Given there's same result for rgcca and sgcca
    # going to drop those of rgcca and retain sgcca only.
    filter(method != "rgcca") %>%
    # Add identifier to tell which one real or simulated
    group_by(method, dataset) %>%
    summarise(
      across(
        .cols=c(auc, f1_score, accuracy, balanced_accuracy, precision, recall),
        .fns=list(mean = mean, sd = sd)),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    # Then rank the performance of each method within each dataset
    # I.e. For Dataset L, could be m1, m3 , m5, m2 , m6 from best to worst (left to right)
    group_by(dataset) %>%
    # TODO:  ~~Sort in desceding order, and rank them, i.e. 1st equals top performing~~
    #mutate(ranking = rank(desc(auc_mean))) %>%
    mutate(ranking = rank(auc_mean)) %>%
    ungroup() %>%
    mutate(
      is_simulated = case_when(
        str_detect(dataset, "sim") ~ "yes",
        TRUE ~ "no"
      )
    ) %>%
    # Rename method names
    mutate(
      method = case_when(
        str_detect(method, "mofa") ~ "mofa + glmnet",
        str_detect(method, "sgcca") ~ "sgcca + lda",
        TRUE ~ method
      )
    ) %>%
    select(
      method, dataset, ranking,
      auc_mean, auc_sd, f1_score_mean, f1_score_sd, is_simulated
    ) %>%
    arrange(ranking)
  return(clean_df)
}

# ==================================================
# First load data and clean it for plotting

input_path <- "data/metrics.csv"

clean_df <- read.csv(input_path) %>%
  as_tibble() %>%
  wrangle_data()



# For the real data uses heatmap showing mean auc and rank them
fig1_real_df <- clean_df %>%
  filter(is_simulated == "no")

# This function has the details of making heatmap to ilustrate
# ranking of methods along the datasets
# I.e. Given method m1, m2, m3, m4 and dataset d1, d2,d3
# The rank in d1 could be m1 = 1, m2 = 4, m3 = 2, m4 = 3, where m2 is the best in d1
# The rank in d2 could be m1 = 3, m2 = 1, m3 = 2, m4 = 4, where m4 is the best in d2
plot_fig1_real <- function(
    fig1_real_df, fontsize=12, method_palette="Paired", dataset_palette="Pastel1",
    heatmap_title = "Mean AUC (5-fold CV) ranking in real datasets") {
  # Then now the figure for mean auc ranking in real data
  auc_matrix <- fig1_real_df %>%
    select(method, dataset, ranking) %>%
    pivot_wider(names_from = dataset, values_from = ranking) %>%
    arrange(method) %>%
    select(order(colnames(.))) %>%
    tibble::column_to_rownames(var="method") %>%
    as.matrix()


  methods <- rownames(auc_matrix)
  datasets <- colnames(auc_matrix)

  # Assign the colors
  # For the method to use default Paired
  method_colors <- RColorBrewer::brewer.pal(n=length(methods), method_palette)
  names(method_colors) <- methods
  # For the dataset to use default Pastel1
  dataset_colors <- RColorBrewer::brewer.pal(n=256, dataset_palette) %>% tail(length(datasets))
  names(dataset_colors) <- datasets

  # Then annotations of the heatmap to use

  # Column wise
  col_ha <- HeatmapAnnotation(
    Method = methods,
    Dataset = datasets,
    col = list(
      Dataset = dataset_colors,
      Method = method_colors
    ),
    show_annotation_name = FALSE
  )

  # Row wise
  row_ha <- rowAnnotation(
    Dataset = datasets,
    Method = methods,
    col = list(
      Method = method_colors,
      Dataset = dataset_colors
    ),
    show_legend = F,
    show_annotation_name = FALSE
  )


  # Plot the heatmap
  col_fun <- viridis::cividis(256)
  #col_fun <- viridis::mako(256)
  #col_fun <- viridis::rocket(256)
  #col_fun <- circlize::colorRamp2()
  ht <- Heatmap(
    auc_matrix,
    col = col_fun,
    border = F,
    column_title = heatmap_title,
    column_title_gp = gpar(fontsize=fontsize, fontface="bold"),
    row_title = NULL,
    cluster_rows = F,
    column_dend_reorder = T,
    show_parent_dend_line = F,
    row_labels = datasets,
    column_dend_height = unit(2.5, "cm"),
    show_row_names = F, show_column_names = F,
    # Assign legend
    heatmap_legend_param = list(
      title = "Ranking",
      legend_direction = "horizontal",
      at = seq(1, 6, 1)
    ),
    top_annotation = col_ha,
    right_annotation = row_ha
  )

  ht
  heatmap_p <- grid.grabExpr(
    #draw(ht, heatmap_legend_side="bottom", annotation_legend_side="right",
    #     legend_grouping = "original")
    draw(ht, merge_legends = TRUE,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom")
  )
  return(heatmap_p)
}

# Draw the heatmap

heatmap_fig_real <- plot_fig1_real(
  fig1_real_df = fig1_real_df,
  fontsize = text_size,
  method_palette = method_palette,
  dataset_palette = dataset_palette,
  heatmap_title = NULL
)

# Save the fig1 heatmap of real data to disk
ggsave(real_output_path, plot=heatmap_fig_real,
       width = width, height = height, device=device, dpi=dpi,
       create.dir = TRUE)
message("Saved image of ", width, " x ", height, " to ", real_output_path)


# =================
# For sim data, boxplot stratify by params
fig1_sim_df <- clean_df %>%
  filter(is_simulated == "yes")

# This function has details of showing boxplot facet by correlation and
# effect combination in intersim strategy simulated data
plot_fig1_sim <- function(
    fig1_sim_df, text_size=12,
    x_lab = "Method", y_lab="Mean Auc Score from 5-fold CV",
    method_palette="Paired") {

  # Wrangle it further
  sim_df <- fig1_sim_df %>%
    # Extract the params from dataset name, although there should be NAs,
    # since different strategy might not share the same param used
    # like mvn has latp and j , while intersim has H and e
    mutate(
      strategy = str_extract(dataset, "(?<=_strategy-)[^_]+"),
      n = as.numeric(str_extract(dataset, "(?<=_n-)[0-9]+")),
      p = as.numeric(str_extract(dataset, "(?<=_p-)[0-9]+")),
      sigma = str_extract(dataset, "(?<=_sigma-)[^_]+"),
      j = as.numeric(str_extract(dataset, "(?<=_j-)[0-9]+")),
      latp = as.numeric(str_extract(dataset, "(?<=_latp-)[0-9]+")),
      sy = as.numeric(str_extract(dataset, "(?<=_sy-)[0-9]+")),
      sp = as.numeric(str_extract(dataset, "(?<=_sp-)[0-9]+")),
      ustd = as.numeric(str_extract(dataset, "(?<=_ustd-)[0-9]+")),
      fctstr = as.numeric(str_extract(dataset, "(?<=_fctstr-)[0-9]+")),
      H = as.numeric(str_extract(dataset, "(?<=_H-)[0-9]+")),
      effect = as.numeric(str_extract(dataset, "(?<=_effect-)[0-9.]+")),
      e = as.numeric(str_extract(dataset, "(?<=_e-)[0-9]+")),
      corr = as.numeric(str_extract(dataset, "(?<=_corr-)[0-9.]+"))
    ) %>%
    # For simplicity now, only use intersim ones
    filter(strategy == "intersim") %>%
    # Then only retain relevant cols
    select(method, dataset, ranking ,
           auc_mean, auc_sd, f1_score_mean, f1_score_sd,
           effect, corr)

  # Labeller for facetting
  effect_labels <- paste0("Effect = ", sim_df$effect |> unique())
  names(effect_labels) <- sim_df$effect |> unique()
  corr_labels <- paste0("Cor = ", sim_df$corr |> unique())
  names(corr_labels) <- sim_df$corr |> unique()
  # Then the plotting
  p <- sim_df %>%
    ggplot(aes(x=method, y=auc_mean, fill=method)) +
    stat_boxplot(geom ='errorbar', width=0.25) +
    geom_boxplot()+
    scale_fill_brewer(palette = method_palette) +
    facet_grid(
      effect~corr,
      labeller = labeller(effect = effect_labels, corr = corr_labels),
      scales = "free"
    ) +
    theme_bw() +
    # And change Grid label names
    theme(
      # Rotate text of methods names
      #axis.text.x = element_text(angle = 45, hjust=1),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        size = text_size
      ),
      axis.title = element_text(size = text_size + 2),
      axis.ticks.x = element_blank(),
      strip.text.x = element_text(
        size = text_size, color = "red", face = "bold.italic"
      ),
      # TODO: Maybe consider changing color of the grid ribbon color?
      #strip.background.x = element_rect(fill="blue"),
      strip.text.y = element_text(
        size = text_size, color = "red", face = "bold.italic"
      ),
      legend.title = element_text(size = text_size + 2),
      legend.text = element_text(size = text_size),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    # Lastly the labels
    labs(x=x_lab, fill=x_lab, y = y_lab)
  return(p)
}

sim_facet_boxplot <- plot_fig1_sim(fig1_sim_df = fig1_sim_df)

sim_facet_boxplot
# Save the fig1 boxplot of sim data to disk
ggsave(sim_output_path, plot=sim_facet_boxplot,
       width = width, height = height, device=device,
       dpi = dpi, create.dir = TRUE)
message("Saved image of ", width, " x ", height, " to ", sim_output_path)
