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
#library(tidyr)
# Parse cli
opt <- docopt::docopt(doc)

# Convenient vars
input_path <- opt$csv
output_path <- opt$output

df <- iris

p <- df %>%
  ggplot(aes(x=Petal.Length, y=Petal.Width)) +
  geom_point() +
  theme_bw()


ggsave(output_path, p)
