library(dplyr)
library(stringr)
library(purrr)
# Helper to check format of table and transform it
convert_table_format <- function(table) {
  # Make sure first col is sample name
  cols <- colnames(table)
  # First col is always sample_name
  match_first_col <- cols[1] == "sample_name"
  if (!match_first_col) {
    stop("First column is not sample_name, wrong naming or missed somewhere")
  }
  # TODO: Should at least contain sample name, phat, method_name, dataset
  # TODO: allow this extra column of fold
  relevant_cols <- c("sample_name", "y",  "phat", "method_name", "dataset", "fold")
  contains_relevant_cols <- cols %in% relevant_cols
  if(!all(contains_relevant_cols)) {
    warning("\nResult table might not contain all relevant columns\n")
    # TODO: Find a better way for this?
    table <- table[, relevant_cols]
  }

  # Also check if has right column types
  bv <- c(1, 0)
  is_binary_y <- all(is.element(table$y, bv))
  if (!is_binary_y) {
    message("\nConverting y to binary output\n")
    table$y <- ifelse(table$y == "yes", 1, 0)
  }
  return(table)
}

# all_files <- list.files("merge_result_table_sklearn/", pattern=".csv")
#
# all_files |>
#   as_tibble() |>
#   mutate(
#     dataset_name = str_extract(value, "^[^-]+"),             # Extracts everything before the first hyphen
#     fold_name = str_extract(value, "fold_\\d+"),             # Extracts fold name like fold_1, fold_2
#     model = str_extract(value, "(?<=sklearn-)[^-]+")         # Extracts the model name after 'sklearn-' and before '-result'
#   )
#   # group_by(model, dataset_name) %>%
#   # summarize(n = n())


# Read and merge all files rowwise

chunk_size <- 50
all_files <- list.files("merge_result_table_sklearn/", pattern = ".csv", full.names = TRUE)

# Split the files into chunks
file_chunks <- split(all_files, ceiling(seq_along(all_files) / chunk_size))

convert_table_format <- function(table) {
  # Make sure first col is sample name
  cols <- colnames(table)
  # First col is always sample_name
  match_first_col <- cols[1] == "sample_name"
  if (!match_first_col) {
    stop("First column is not sample_name, wrong naming or missed somewhere")
  }
  # TODO: Should at least contain sample name, phat, method_name, dataset
  # TODO: allow this extra column of fold
  relevant_cols <- c("sample_name", "y",  "phat", "method_name", "dataset", "fold")
  contains_relevant_cols <- cols %in% relevant_cols
  if(!all(contains_relevant_cols)) {
    warning("\nResult table might not contain all relevant columns\n")
    # TODO: Find a better way for this?
    table <- table[, relevant_cols]
  }

  # Also check if has right column types
  bv <- c(1, 0)
  is_binary_y <- all(is.element(table$y, bv))
  if (!is_binary_y) {
    message("\nConverting y to binary output\n")
    table$y <- ifelse(table$y == "yes", 1, 0)
  }
  return(table)
}

# Process each chunk
merged_chunks <- lapply(file_chunks, function(chunk) {
  map_df(chunk, ~ {
    # Read the CSV file
    table <- read.csv(.x, header=TRUE, colClasses=c("sample_name"="character"))

    # Add custom processing here, for example:
    table <- convert_table_format(table)  # Call your format conversion function

    # You can add more processing steps if needed

    # Return the processed table
    return(table)
  })
})
# Combine all chunks together
final_merged_data <- bind_rows(merged_chunks)

final_merged_data |>
  write.csv("data/sklearn-result.csv", row.names = F)
