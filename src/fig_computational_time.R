doc <- "

This script is used to create figure 2 of computational time.

Usage:
  fig_computational_time.R [options]

Options:
 --metadata=METADATA    Path to write out performance auc of real datasets
 --trace=TRACE          Path to write out performance auc of sim datasets
 --output=OUTPUT        Path to write out computational time
 --width=WIDTH          Width of the graph [default: 7]
 --height=height        Height of the graph [default: 7]
 --device=DEVICE        Device to print out [default: png]
 --dpi=DPI              Dots per inch [default: 300]
"

# Load libraries
suppressPackageStartupMessages(library(dplyr))
library(purrr)
library(ggplot2)
library(stringr)
library(tidyr)
# Parse cli
opt <- docopt::docopt(doc)

# Convenient vars
output_path <- opt$output
metadata_path <- opt$metadata
trace_path <- opt$trace

# Plotting params
width <- as.numeric(opt$width)
height <- as.numeric(opt$height)
device <- opt$device
dpi <- as.numeric(opt$dpi)
text_size <- 12
method_palette <- "Paired"


# Custom functions
toSeconds <- function(x) {
  parts <- unlist(strsplit(x, " "))
  hours <- minutes <- seconds <- 0
  for (part in parts) {
    if (grepl("h", part)) {
      hours <- as.numeric(gsub("h", "", part))
    } else if (grepl("m", part)) {
      minutes <- as.numeric(gsub("m", "", part))
    } else if (grepl("s", part)) {
      seconds <- as.numeric(gsub("s", "", part))
    }

    # TODO: need to handle miliseconds as well?
  }
  return(hours * 3600 + minutes * 60 + seconds)
}


# Function to wrangle metadata
wrangle_metadata <- function(metadata_df) {
  metadata_df %>%
    rename(dataset = dataset_name) %>%
    mutate(subject_dimensions_list = str_split(subject_dimensions, ",")) %>%
    mutate(
      sample_size = map_dbl(
        subject_dimensions_list, ~ case_when(
          all(.x == .x[1]) ~ as.numeric(.x[1]),
          TRUE ~ NA
        )
      )
    ) %>%
    # Then remove these old columns after getting sample size
    select(-subject_dimensions_list, -subject_dimensions) %>%
    mutate(total_number_feature = str_split(feat_dimensions, ",") %>%
             map_dbl(~ sum(as.numeric(.x)))) %>%
    mutate(dataset_dim = sample_size * total_number_feature) %>%
    select(dataset, omics_names, sample_size,
           dataset_dim, is_simulated, positive_prop)
}

# ========================
# First handle the metadata
#metadata_path <- "data/parsed_metadata.csv"
metadata_df <- read.csv(metadata_path) %>%
  as_tibble() %>%
  wrangle_metadata()


# ================================
# Function to wrangle trace
wrangle_trace <- function(
    trace_df,
    workflow_prefix="NFCORE_MESSI_BENCHMARK:MESSI_BENCHMARK:",
    time_col="duration") {

  trace_df_pre <- trace_df %>%
    select(process, tag, realtime, duration) %>%
    mutate(
      process = str_replace(process, workflow_prefix, "") |> tolower()
    ) %>%
    # Need to fix the cooperative learning name for regex matching later
    mutate(process = str_replace_all(process, "cooperative_learning", "cooperative-learning")) %>%
    # Get the ones of select feature and cross validation only
    # Since there are other jobs like prepare metadata
    filter(
      str_detect(process, "^feature_selection:[^_]+_select_feature") |
        str_detect(process, "^cross_validation:")
    ) %>%
    filter(
      !str_detect(process, "downstream|merge_result_table")
    ) %>%
    # Then just replace long prefixes in front of the process
    mutate(
      process = str_replace(
        process,
        "^(feature_selection:|cross_validation:cv.*:)", "")
    ) %>%
    # Now split the <method>_<action> to more columns
    separate(process, into = c("method", "action"), sep = "_", extra = "merge") %>%
    # Now for diablo, check if tag contains null or full (which is its design)
    mutate(
      method = case_when(
        str_detect(method, "diablo") & str_detect(tag, "-(null|full)") ~ paste0(
          method, str_extract(tag, "-(null|full)")
        ),
        str_detect(method, "mofa") ~ "mofa + glmnet",
        TRUE ~ method
      ),
      dataset = str_replace(tag, "-fold.*", ""),
      tag = str_replace(tag, "-(null|full)", "") # Clean up tag column
    ) %>%
    mutate(raw_seconds = sapply(!! sym( time_col ), toSeconds) |> as.numeric())
  # ============================================================================
  # Need to creat additional diablo rows given they shared same preprocess step
  diablo_null_copy <- trace_df_pre %>%
    filter(method == "diablo") %>%
    mutate(method = "diablo-null")

  diablo_full_copy <- trace_df_pre %>%
    filter(method == "diablo") %>%
    mutate(method = "diablo-full")

  # TODO: this might not be too readable?
  sgcca_copy <- trace_df_pre %>%
    filter(method == "rgcca",
           !str_detect(tag, "rgcca")) %>%
    mutate(method = "sgcca + lda") %>%
    distinct(tag, action, .keep_all=TRUE)


  # And also need to handle those of rgcca vs sgcca

  # Lastly combine these anad output it
  output_df <- trace_df_pre %>%
    # This remove the diablo "preprocess" step, and add in from our aside copy
    filter(method != "diablo", method != "rgcca") %>%
    bind_rows(diablo_null_copy, diablo_full_copy, sgcca_copy)

  return(output_df)
}


# Then hanlde the execution trace
# Then also want to wrangle the execution trace
#trace_path <- "data/execution_trace.txt"
trace_df <- readr::read_tsv(trace_path, col_types = readr::cols()) %>%
  wrangle_trace()

# ================================================================
# Now to combine the trace with the metadata
plot_df <- left_join(
  x = metadata_df,
  y = trace_df,
  by = "dataset"
  ) %>%
  # Add a label column to group datasets by their sizes
  mutate(
    size_label = ifelse(
      dataset_dim < median(metadata_df$dataset_dim),
      "Small",
      "Large"
    ) %>% factor(levels=c("Small", "Large"))
  ) %>%
  select(method, dataset, dataset_dim, size_label, raw_seconds, action)

# Plotting starts here
computation_time_plot <- plot_df %>%
  ggplot(aes(x=size_label, y = raw_seconds, fill=method)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(outlier.color = "red", outlier.fill="red") +
  scale_y_log10(labels = scales::label_log()) +
  scale_fill_brewer(palette=method_palette) +
  labs(x = "Dataset Size", y = "Computation time (seconds)", fill = "Method") +
  theme_classic() +
  theme(
    # Make sizing
    axis.text = element_text(size = text_size),
    axis.title = element_text(size = text_size + 2),
    legend.title = element_text(size = text_size + 2),
    legend.text = element_text(size = text_size),
    legend.position = "bottom"
  ) +
  guides(fill=guide_legend(nrow=2))
# Lastly save it
ggsave(output_path, plot=computation_time_plot,
       width=width, height=height, device=device, dpi=dpi,
       create.dir = TRUE)
message("Saved image of ", width, " x ", height, " to ", output_path)
