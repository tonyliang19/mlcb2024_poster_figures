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
"

# Load libraries
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

# Parse cli
opt <- docopt::docopt(doc)

# Convenient vars
output_path <- opt$output

# Plotting starts here
df <- iris

p <- df %>%
  ggplot(aes(x=Petal.Length, y=Petal.Width)) +
  geom_point() +
  theme_bw()


ggsave(output_path, plot=p)
