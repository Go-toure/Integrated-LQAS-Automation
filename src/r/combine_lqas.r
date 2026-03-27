#!/usr/bin/env Rscript
# ============================================================
# Combine LQAS Files Script
# GitHub Actions Compatible Version
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
  library(logger)
  library(fs)
  library(argparse)
  library(stringr)
  library(stringi)
  library(janitor)
})

# Configure logging
log_appender(appender_file("logs/combine.log"))
log_info("Starting LQAS combine process")

# Parse arguments
parser <- ArgumentParser()
parser$add_argument("--input-dir", default = "data/output", 
                    help = "Input directory with CSV files")
parser$add_argument("--output-file", default = "data/final_lqas_data.csv", 
                    help = "Output file path")
parser$add_argument("--start-date", default = "2019-10-01", 
                    help = "Minimum start date filter")
args <- parser$parse_args()

log_info("Arguments: input-dir={args$input_dir}, output-file={args$output_file}")

# ============================================================
# READ ALL FINAL LQAS LEVEL-1 FILES
# ============================================================

list_of_files <- list.files(
  args$input_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
) %>%
  .[!str_detect(., "QA_unmapped_reasons")] %>%
  .[!str_detect(basename(.), "^processed_files_log\\.csv$")]

log_info("Found {length(list_of_files)} CSV files to combine")

if (length(list_of_files) == 0) {
  log_warn("No CSV files found in {args$input_dir}")
  quit(status = 0)
}

# ============================================================
# READ + COMBINE
# ============================================================

A <- lapply(list_of_files, function(x) {
  tryCatch({
    readr::read_csv(x, show_col_types = FALSE)
  }, error = function(e) {
    log_warn("Failed to read {x}: {e$message}")
    return(NULL)
  })
}) %>%
  bind_rows() %>%
  filter(!is.na(response), response != "NA")

log_info("Combined {nrow(A)} rows from {length(list_of_files)} files")

# ============================================================
# FINAL HARMONIZED AFRO LQAS DATASET
# ============================================================

LQAS_fin <- A %>%
  mutate(
    start_date = as_date(start_date),
    end_date   = as_date(end_date),
    year       = year(start_date)
  ) %>%
  select(
    country,
    province,
    district,
    response,
    vaccine.type,
    roundNumber,
    numbercluster,
    round_start_date = any_of("round_start_date"),
    start_date,
    end_date,
    year,
    
    male_sampled,
    female_sampled,
    total_sampled,
    male_vaccinated,
    female_vaccinated,
    total_vaccinated,
    total_missed,
    
    status,
    performance,
    
    r_non_compliance        = any_of("r_Non_Compliance"),
    r_house_not_visited     = any_of("r_House_not_visited"),
    r_childabsent,
    r_child_was_asleep      = any_of("r_Child_was_asleep"),
    r_child_is_a_visitor    = any_of("r_Child_is_a_visitor"),
    r_vaccinated_but_not_FM = any_of("r_Vaccinated_but_not_FM"),
    r_childnotborn          = any_of("r_childnotborn"),
    r_security              = any_of("r_security"),
    other_r,
    
    prct_care_giver_informed_SIA = any_of("percent_care_Giver_Informed_SIA"),
    prct_non_compliance          = any_of("prct_r_Non_Compliance"),
    prct_house_not_visited       = any_of("prct_r_House_not_visited"),
    prct_childabsent             = any_of("prct_r_childabsent"),
    prct_child_was_asleep        = any_of("prct_r_Child_was_asleep"),
    prct_child_is_a_visitor      = any_of("prct_r_Child_is_a_visitor"),
    prct_vaccinated_but_not_FM   = any_of("prct_r_Vaccinated_but_not_FM"),
    prct_security                = any_of("prct_r_security"),
    prct_childnotborn            = any_of("prct_r_childnotborn"),
    prct_other_r                 = any_of("prct_other_r"),
    
    starts_with("abs_reason_"),
    starts_with("nc_reason_"),
    
    abs_reason_total = any_of("abs_reason_total"),
    nc_reason_total  = any_of("nc_reason_total"),
    abs_reason_check = any_of("abs_reason_check"),
    nc_reason_check  = any_of("nc_reason_check")
  ) %>%
  filter(
    start_date > as_date(args$start_date),
    year > 2019,
    district != "NA"
  ) %>%
  mutate(
    response = case_when(
      response == "NIE-2024_nOPV2" ~ "NIE-2024-nOPV2",
      TRUE ~ response
    ),
    # Fix end_date years if needed
    end_date = case_when(
      year(end_date) == 2028 ~ update(end_date, year = 2023),
      year(end_date) == 2025 ~ update(end_date, year = 2024),
      TRUE ~ end_date
    )
  ) %>%
  arrange(start_date)

log_info("Final dataset: {nrow(LQAS_fin)} rows, {ncol(LQAS_fin)} columns")

# ============================================================
# WRITE FINAL COMBINED AFRO LQAS DATA
# ============================================================

# Create output directory if needed
dir_create(dirname(args$output_file))

readr::write_csv(LQAS_fin, args$output_file)
log_info("✅ Saved combined data to {args$output_file}")

# Print summary
summary_stats <- LQAS_fin %>%
  summarise(
    total_records = n(),
    total_countries = n_distinct(country),
    total_districts = n_distinct(district),
    date_range = paste(min(start_date), "to", max(start_date)),
    total_sampled = sum(total_sampled, na.rm = TRUE),
    total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
    coverage = round(total_vaccinated / total_sampled * 100, 1)
  )

log_info("Summary statistics:")
log_info("  Records: {summary_stats$total_records}")
log_info("  Countries: {summary_stats$total_countries}")
log_info("  Districts: {summary_stats$total_districts}")
log_info("  Date range: {summary_stats$date_range}")
log_info("  Total sampled: {format(summary_stats$total_sampled, big.mark = ',')}")
log_info("  Total vaccinated: {format(summary_stats$total_vaccinated, big.mark = ',')}")
log_info("  Overall coverage: {summary_stats$coverage}%")