#!/usr/bin/env Rscript
# ============================================================
# LQAS Data Processing Script
# GitHub Actions Compatible Version
# ============================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(qs)
  library(logger)
  library(fs)
  library(argparse)
  library(stringr)
  library(stringi)
  library(janitor)
})

# Configure logging
log_appender(appender_file("logs/r_processing.log"))
log_info("Starting LQAS R processing")

# Parse command line arguments
parser <- ArgumentParser()
parser$add_argument("--input-dir", default = "data/input", help = "Input directory for RDS files")
parser$add_argument("--output-dir", default = "data/output", help = "Output directory for CSV files")
parser$add_argument("--chunk", type = "integer", default = 1, help = "Chunk number for parallel processing")
parser$add_argument("--total-chunks", type = "integer", default = 1, help = "Total number of chunks")
parser$add_argument("--force-full", action = "store_true", help = "Force full processing")
args <- parser$parse_args()

log_info("Arguments: {paste(names(args), args, sep = '=', collapse = ', ')}")

# Create output directory
dir_create(args$output_dir, recurse = TRUE)

# ============================================================
# HELPER FUNCTIONS (from your original script)
# ============================================================

rename_repetitive_columns <- function(data) {
  pattern <- "^Count_HH\\[\\d+\\]/Count_HH/"
  new_columns <- sapply(colnames(data), function(col) {
    if (grepl(pattern, col)) gsub("Count_HH/", "", col) else col
  })
  colnames(data) <- new_columns
  return(data)
}

apply_custom_rules <- function(data, file_name) {
  if (startsWith(file_name, "3583")) {
    data <- data %>% mutate(Country = "GHA")
  }
  if (startsWith(file_name, "8834")) {
    data <- data %>% mutate(Region = District, District = District)
  }
  if (startsWith(file_name, "4351")) {
    data <- data %>% mutate(District = district)
  }
  return(data)
}

normalize_reason_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_to_lower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  
  x[x %in% c("", "na", "n_a", "n/a", "null", "none", "missing", "unknown", ".", "-", "--")] <- NA
  
  x
}

map_abs_reason <- function(x) {
  x <- normalize_reason_text(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c("farm") ~ "Farm",
    x %in% c("market") ~ "Market",
    x %in% c("school") ~ "School",
    x %in% c("in_playground", "playground") ~ "In_playground",
    x %in% c("travelled", "travel", "travelling", "traveling") ~ "Travelled",
    TRUE ~ "Other"
  )
}

map_nc_reason <- function(x) {
  x <- normalize_reason_text(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c("religious_cultural", "religious", "cultural", "religious_and_cultural") ~ "Religious_Cultural",
    x %in% c("vaccines_safety", "vaccine_safety", "safety", "vaccine_safety_concern") ~ "Vaccines_Safety",
    x %in% c("no_felt_need", "no_need", "no_need_felt", "no_perceived_need") ~ "No_felt_Need",
    x %in% c("too_many_rnd", "too_many_round", "too_many_rounds") ~ "Too_Many_Rnd",
    x %in% c("no_care_giver_consent", "no_caregiver_consent", "caregiver_refusal", "no_parental_consent") ~ "No_Care_giver_Consent",
    x %in% c("child_sick", "child_is_sick", "sick_child", "childsick") ~ "Child-Sick",
    x %in% c("covid_19", "covid19", "covid") ~ "Covid_19",
    x %in% c("poliofree", "polio_free", "poliofree_area") ~ "PolioFree",
    x %in% c("nopvconcern", "nopv_concern", "n_opv_concern") ~ "nOPVConcern",
    TRUE ~ "Others"
  )
}

make_reason_slug <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "") %>%
    stringr::str_to_lower()
}

build_reason_wide <- function(data, reason_cols, names_prefix) {
  if (length(reason_cols) == 0) {
    return(
      data %>%
        dplyr::distinct(Country, Region, District, Response, roundNumber)
    )
  }
  
  data %>%
    dplyr::select(Country, Region, District, Response, roundNumber, dplyr::all_of(reason_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(reason_cols),
      names_to = "reason_source_col",
      values_to = "reason"
    ) %>%
    dplyr::filter(!is.na(reason)) %>%
    dplyr::count(Country, Region, District, Response, roundNumber, reason, name = "n") %>%
    dplyr::mutate(reason = make_reason_slug(reason)) %>%
    tidyr::pivot_wider(
      names_from = reason,
      values_from = n,
      values_fill = 0,
      names_prefix = names_prefix
    )
}

# ============================================================
# MAIN PROCESSING FUNCTION
# ============================================================

process_single_file <- function(file_path, output_dir) {
  file_name <- basename(file_path)
  log_info("Processing file: {file_name}")
  
  # Read the RDS file
  tryCatch({
    data <- qs::qread(file_path)
  }, error = function(e) {
    log_error("Failed to read {file_name}: {e$message}")
    return(NULL)
  })
  
  if (is.null(data) || nrow(data) == 0) {
    log_warn("Empty data in {file_name}")
    return(NULL)
  }
  
  log_info("  Initial data: {nrow(data)} rows, {ncol(data)} columns")
  
  # Apply transformations
  data <- rename_repetitive_columns(data)
  data <- apply_custom_rules(data, file_name)
  
  # Select relevant columns
  selected_columns <- c(
    "Response", "roundNumber", "Country", "Region", "District", "Date_of_LQAS",
    "_GPS_hh_latitude", "_GPS_hh_longitude", "_GPS_hh_altitude",
    grep("^Count_HH\\[\\d+\\]/(Sex_Child|FM_Child|FM_ChildR|FM_ChildL|Reason_Not_FM|Reason_NC_NFM|Reason_ABS_NFM|Care_Giver_Informed_SIA)$",
         names(data), value = TRUE),
    "Count_HH_count", "Cluster"
  )
  selected_columns <- intersect(selected_columns, names(data))
  data <- data %>% select(all_of(selected_columns))
  
  # Standardize data
  standardize_yes_no <- function(x) {
    case_when(
      x %in% c("Yes", "YES", "yes", "Y") ~ 1,
      x %in% c("No", "NO", "no", "N") ~ 0,
      TRUE ~ NA_real_
    )
  }
  
  standardize_informed_sia <- function(x) {
    case_when(
      x %in% c("Y", "1") ~ 1,
      x %in% c("N", "0") ~ 0,
      TRUE ~ NA_real_
    )
  }
  
  data <- data %>%
    mutate(
      Country = str_squish(toupper(Country)),
      Region = str_squish(toupper(Region)),
      District = str_squish(toupper(District))
    ) %>%
    mutate(
      across(matches("^Count_HH\\[\\d+\\]/Sex_Child$"),
             ~ ifelse(. == "F", 1, ifelse(. == "M", 0, .))),
      across(matches("^Count_HH\\[\\d+\\]/FM_Child$"), standardize_yes_no),
      across(matches("^Count_HH\\[\\d+\\]/FM_ChildR$"), standardize_yes_no),
      across(matches("^Count_HH\\[\\d+\\]/FM_ChildL$"), standardize_yes_no),
      across(matches("^Count_HH\\[\\d+\\]/Care_Giver_Informed_SIA$"), standardize_informed_sia),
      across(matches("^Count_HH\\[\\d+\\]/Reason_ABS_NFM$"), map_abs_reason),
      across(matches("^Count_HH\\[\\d+\\]/Reason_NC_NFM$"), map_nc_reason)
    )
  
  # Save output
  output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(file_name), ".csv"))
  write_csv(data, output_file)
  
  log_info("  ✅ Saved {nrow(data)} rows to {output_file}")
  
  return(data)
}

# ============================================================
# MAIN EXECUTION
# ============================================================

main <- function() {
  # Get all RDS files
  rds_files <- dir_ls(args$input_dir, glob = "*.rds")
  
  if (length(rds_files) == 0) {
    log_warn("No RDS files found in {args$input_dir}")
    return(invisible(NULL))
  }
  
  log_info("Found {length(rds_files)} files to process")
  
  # Split into chunks for parallel processing
  chunks <- split(rds_files, cut(seq_along(rds_files), args$total_chunks, labels = FALSE))
  
  if (args$chunk <= length(chunks)) {
    current_chunk <- chunks[[args$chunk]]
    log_info("Processing chunk {args$chunk}/{args$total_chunks}: {length(current_chunk)} files")
    
    # Process each file
    for (file_path in current_chunk) {
      process_single_file(file_path, args$output_dir)
    }
  } else {
    log_info("Chunk {args$chunk} has no files to process")
  }
  
  log_info("Chunk {args$chunk} complete")
}

# Run the script
if (interactive()) {
  main()
} else {
  main()
}