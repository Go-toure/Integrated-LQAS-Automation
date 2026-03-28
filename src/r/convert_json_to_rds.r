#!/usr/bin/env Rscript
# Convert JSON files to RDS format for efficient processing

suppressPackageStartupMessages({
  library(jsonlite)
  library(logger)
  library(fs)
  library(argparse)
})

# Configure logging
log_appender(appender_file("logs/convert.log"))

# Parse arguments
parser <- ArgumentParser()
parser$add_argument("--input-dir", default = "data/input", help = "Input directory with JSON files")
parser$add_argument("--output-dir", default = "data/input", help = "Output directory for RDS files")
args <- parser$parse_args()

log_info("Converting JSON to RDS in {args$input_dir}")

# Find all JSON files
json_files <- dir_ls(args$input_dir, glob = "*.json")

# Exclude metadata files (those with "metadata" in name or very small)
json_files <- json_files[!grepl("metadata", json_files, ignore.case = TRUE)]

log_info("Found {length(json_files)} JSON files to convert")

for (json_file in json_files) {
  tryCatch({
    # Read JSON
    data <- fromJSON(json_file, simplifyVector = TRUE, flatten = TRUE)
    
    # Convert to tibble
    df <- as_tibble(data)
    
    # Save as RDS
    rds_file <- file.path(args$output_dir, paste0(tools::file_path_sans_ext(basename(json_file)), ".rds"))
    saveRDS(df, rds_file)
    
    log_info("✅ Converted {basename(json_file)} to {basename(rds_file)}: {nrow(df)} rows")
    
    # Optionally remove JSON to save space (comment out if you want to keep)
    # file_delete(json_file)
    
  }, error = function(e) {
    log_error("Failed to convert {basename(json_file)}: {e$message}")
  })
}

log_info("JSON to RDS conversion complete")