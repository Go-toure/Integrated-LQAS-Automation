#!/usr/bin/env Rscript
# ============================================================
# Special Case: Process Nigeria LQAS CSV (Pre-existing File)
# GitHub Actions Compatible Version
# Input: data/input/Nigeria_LQAS_int_oct_2025.csv
# Output: data/output/nigeria_processed.csv
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(readr)
  library(logger)
  library(fs)
  library(argparse)
})

# Configure logging
log_appender(appender_file("logs/special_nigeria.log"))
log_info("=" %>% paste(rep("=", 60), collapse = ""))
log_info("Starting Special Case Nigeria CSV Processing")
log_info("Note: This is a pre-existing file in the repository")
log_info("=" %>% paste(rep("=", 60), collapse = ""))

# Parse arguments
parser <- ArgumentParser()
parser$add_argument("--input-file", default = "data/input/Nigeria_LQAS_int_oct_2025.csv", 
                    help = "Input CSV file path")
parser$add_argument("--lookup-file", default = "data/lookup/lqas_lookup.xlsx", 
                    help = "Lookup Excel file path")
parser$add_argument("--output-file", default = "data/output/nigeria_processed.csv", 
                    help = "Output CSV file path")
args <- parser$parse_args()

log_info("Arguments:")
log_info("  input-file: {args$input_file}")
log_info("  lookup-file: {args$lookup_file}")
log_info("  output-file: {args$output_file}")

# ============================================================
# CHECK IF FILE EXISTS
# ============================================================

if (!file.exists(args$input_file)) {
  log_warn("Nigeria CSV file not found: {args$input_file}")
  log_warn("This is expected if the file hasn't been added to the repository yet")
  log_info("Skipping processing of Nigeria CSV")
  quit(status = 0)  # Exit gracefully, not an error
}

log_info("✅ Nigeria CSV file found, proceeding with processing...")

# ============================================================
# HELPER FUNCTIONS
# ============================================================

clean_binary_var <- function(x) {
  x <- as.character(x)
  case_when(
    tolower(x) %in% c("yes", "y", "1") ~ 1,
    tolower(x) %in% c("no", "n", "0") ~ 0,
    TRUE ~ NA_real_
  )
}

clean_sex_var <- function(x) {
  x <- as.character(x)
  case_when(
    toupper(x) == "F" ~ 1,
    toupper(x) == "M" ~ 0,
    TRUE ~ NA_real_
  )
}

# ============================================================
# MAIN PROCESSING FUNCTION
# ============================================================

process_nigeria_csv <- function(input_file, lookup_file, output_file) {
  
  log_info("Reading input file: {input_file}")
  
  # Read the CSV file
  df <- read_csv(input_file, show_col_types = FALSE)
  log_info("Read {nrow(df)} rows from input file")
  
  # Fix invalid UTF-8
  df <- df %>%
    mutate(across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8", sub = "")))
  
  # ============================================================
  # MAP COLUMNS TO EXPECTED NAMES (Nigeria CSV specific)
  # ============================================================
  log_info("Mapping columns for Nigeria CSV structure...")
  
  # Map state/Region
  if ("state" %in% names(df)) {
    df <- df %>% rename(states = state)
  } else if ("State" %in% names(df)) {
    df <- df %>% rename(states = State)
  } else if ("REGION" %in% names(df)) {
    df <- df %>% rename(states = REGION)
  }
  
  # Map lga/District
  if ("lga" %in% names(df)) {
    df <- df %>% rename(lgas = lga)
  } else if ("LGA" %in% names(df)) {
    df <- df %>% rename(lgas = LGA)
  } else if ("district" %in% names(df)) {
    df <- df %>% rename(lgas = district)
  }
  
  # Map date/today
  if ("date" %in% names(df)) {
    df <- df %>% rename(today = date)
  } else if ("Date" %in% names(df)) {
    df <- df %>% rename(today = Date)
  } else if ("submission_time" %in% names(df)) {
    df <- df %>% rename(today = submission_time)
  }
  
  # Map cluster
  if ("cluster" %in% names(df)) {
    df <- df %>% rename(Cluster = cluster)
  } else if ("Cluster" %in% names(df)) {
    # Already named correctly
  } else {
    df$Cluster <- NA
  }
  
  # Ensure required columns exist
  if (!("states" %in% names(df))) {
    log_error("Column 'states' (or 'state') not found in input file")
    log_info("Available columns: {paste(names(df), collapse = ', ')}")
    return(NULL)
  }
  if (!("lgas" %in% names(df))) {
    log_error("Column 'lgas' (or 'lga') not found in input file")
    log_info("Available columns: {paste(names(df), collapse = ', ')}")
    return(NULL)
  }
  if (!("today" %in% names(df))) {
    log_error("Column 'today' (or 'date') not found in input file")
    log_info("Available columns: {paste(names(df), collapse = ', ')}")
    return(NULL)
  }
  
  log_info("Found required columns:")
  log_info("  Region (states): {names(df)[names(df) == 'states']}")
  log_info("  District (lgas): {names(df)[names(df) == 'lgas']}")
  log_info("  Date (today): {names(df)[names(df) == 'today']}")
  log_info("  Cluster: {ifelse('Cluster' %in% names(df), 'Found', 'Created default')}")
  
  # ============================================================
  # PROCESSING (same as 272 but adapted for CSV input)
  # ============================================================
  log_info("Applying Nigeria-specific processing...")
  
  df <- df |>
    mutate(country = "NIE") |>
    mutate(Cluster = ifelse(!is.na(Cluster), 1, NA_real_)) |>
    mutate(across(starts_with("Children_Seen_"), ~ suppressWarnings(as.numeric(.x)))) |>
    select(
      country, Region = states, District = lgas, Cluster, today,
      matches("Children_seen_h[1-9]|Children_Seen_h10"),
      matches("Sex_Child[1-9]|Sex_Child10"),
      matches("FM_Child[1-9]|FM_Child10"),
      matches("Reason_Not_FM[1-9]|Reason_Not_FM10"),
      matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10")
    )
  
  # Parse and extract date info
  df <- df |>
    mutate(
      today = parse_date_time(today, orders = c("ymd", "dmy", "mdy"), quiet = TRUE),
      year = year(today),
      month = format(today, "%b")
    )
  
  # Compute households visited
  df <- df |>
    mutate(across(matches("Children_seen_h[1-9]|Children_Seen_h10"),
                  ~ ifelse(!is.na(.), 1, 0),
                  .names = "h_{.col}")) |>
    mutate(tot_hh_visited = rowSums(across(starts_with("h_Children_Seen_")), na.rm = TRUE))
  
  # Clean binary variables
  df <- df |>
    mutate(
      across(matches("Sex_Child[1-9]|Sex_Child10"), clean_sex_var),
      across(matches("FM_Child[1-9]|FM_Child10"), clean_binary_var),
      across(matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10"), clean_binary_var)
    )
  
  # Process reasons
  reason_cols <- c("childnotborn", "childabsent", "noncompliance", "housenotvisited", "security")
  for (r in reason_cols) {
    for (i in 1:10) {
      col_in <- paste0("Reason_Not_FM", i)
      col_out <- paste0("R_", r, i)
      if (col_in %in% names(df)) {
        df[[col_out]] <- ifelse(tolower(as.character(df[[col_in]])) == r, 1, 0)
      }
    }
  }
  
  # Summary stats
  df <- df |>
    mutate(
      female_sampled = rowSums(across(matches("Sex_Child[1-9]|Sex_Child10")), na.rm = TRUE),
      male_sampled = tot_hh_visited - female_sampled,
      total_vaccinated = rowSums(across(matches("FM_Child[1-9]|FM_Child10")), na.rm = TRUE),
      missed_child = tot_hh_visited - total_vaccinated
    ) |>
    rowwise() |>
    mutate(
      female_vaccinated = sum(
        unlist(across(matches("Sex_Child[1-9]|Sex_Child10"))) == 1 &
          unlist(across(matches("FM_Child[1-9]|FM_Child10"))) == 1,
        na.rm = TRUE
      ),
      male_vaccinated = total_vaccinated - female_vaccinated
    ) |>
    ungroup()
  
  # Aggregate reasons
  df <- df |>
    mutate(
      R_House_not_visited = rowSums(across(matches("R_housenotvisited[1-9]|R_housenotvisited10")), na.rm = TRUE),
      R_childabsent = rowSums(across(matches("R_childabsent[1-9]|R_childabsent10")), na.rm = TRUE),
      R_Non_Compliance = rowSums(across(matches("R_noncompliance[1-9]|R_noncompliance10")), na.rm = TRUE),
      R_childnotborn = rowSums(across(matches("R_childnotborn[1-9]|R_childnotborn10")), na.rm = TRUE),
      R_security = rowSums(across(matches("R_security[1-9]|R_security10")), na.rm = TRUE),
      Care_Giver_Informed_SIA = rowSums(across(matches("Caregiver_Aware_h[1-9]|Caregiver_Aware_h10")), na.rm = TRUE)
    )
  
  # Round and response logic
  df <- df |>
    mutate(
      today = as_date(today),
      month = format(today, "%b"),
      roundNumber = case_when(
        str_detect(month, "janv.") ~ "Rnd1",
        str_detect(month, "févr.") ~ "Rnd2",
        str_detect(month, "mars") ~ "Rnd3",
        str_detect(month, "avr.") ~ "Rnd4",
        str_detect(month, "mai") ~ "Rnd5",
        str_detect(month, "juin") ~ "Rnd6",
        str_detect(month, "juil.") ~ "Rnd7",
        str_detect(month, "août") ~ "Rnd8",
        str_detect(month, "sept.") ~ "Rnd9",
        str_detect(month, "oct.") ~ "Rnd10",
        str_detect(month, "nov.") ~ "Rnd11",
        str_detect(month, "déc.") ~ "Rnd12",
        TRUE ~ NA_character_
      )
    )
  
  df <- df |>
    mutate(
      roundNumber = case_when(
        year == 2025 & month %in% c("juin", "juil.") ~ "Rnd2",
        year == 2025 & month %in% c("janv.", "févr.", "mars", "avr.", "mai") ~ "Rnd1",
        year == 2024 & month %in% c("févr.", "mars") ~ "Rnd1",
        year == 2024 & month %in% c("avr.", "mai", "juin", "août") ~ "Rnd2",
        year == 2024 & month %in% c("sept.", "oct.") ~ "Rnd3",
        year == 2024 & month == "nov." ~ "Rnd4",
        year == 2024 & month == "déc." ~ "Rnd5",
        year == 2023 & month %in% c("janv.", "mai") ~ "Rnd1",
        year == 2023 & month %in% c("juin", "juil.", "août") ~ "Rnd2",
        year == 2023 & month %in% c("sept.", "oct.") ~ "Rnd3",
        year == 2023 & month == "nov." ~ "Rnd4",
        year == 2023 & month == "déc." ~ "Rnd5",
        TRUE ~ roundNumber
      ),
      total_sampled = tot_hh_visited,
      vaccine.type = case_when(
        year == 2025 & month %in% c("oct.", "nov.") ~ "nOPV2",
        year == 2025 & month %in% c("juin", "juil.") ~ "nOPV2",
        year == 2025 & month %in% c("janv.", "avr.") ~ "nOPV2",
        year == 2024 ~ "nOPV2",
        year == 2023 & month == "mai" ~ "fIPV+nOPV2",
        year == 2023 & month == "juil." ~ "fIPV+nOPV2",
        year == 2023 & month %in% c("août", "oct.", "nov.", "déc.") ~ "nOPV2",
        year == 2023 & month == "sept." ~ "fIPV+nOPV2",
        year %in% 2020:2022 ~ "bOPV",
        TRUE ~ "nOPV2"
      ),
      response = case_when(
        year == 2025 & month %in% c("oct.", "nov.") ~ "NIE-2025-10-01_nOPV_sNID",
        year == 2025 & month %in% c("juin", "juil.") ~ "NIE-2025-04-01_nOPV_NIDs",
        year == 2025 & month %in% c("avr.", "mai") ~ "NIE-2025-04-01_nOPV_NIDs",
        year == 2024 ~ "NIE-2024-nOPV2",
        year == 2023 & month %in% c("mai", "juin") ~ "NIE-2023-04-02_nOPV",
        year == 2023 & month %in% c("juil.", "août", "sept.", "oct.", "nov.", "déc.") ~ "NIE-2023-07-03_nOPV",
        year == 2020 ~ "NGA-20DS-01-2020",
        year == 2021 & month %in% c("mars", "avr.") ~ "NGA-2021-013-1",
        year == 2021 & month %in% c("avr.", "mai") ~ "NGA-2021-011-1",
        year == 2021 & month == "juin" ~ "NGA-2021-016-1",
        year == 2021 & month %in% c("juil.", "août") ~ "NGA-2021-014-1",
        year == 2021 & month == "sept." ~ "NGA-2021-020-2",
        TRUE ~ "OBR_name"
      )
    )
  
  # Special case adjustments
  df <- df |>
    mutate(
      roundNumber = case_when(
        response == "NIE-2025-10-01_nOPV_sNID" & year == 2025 & month %in% c("oct.", "nov.") ~ "Rnd1",
        TRUE ~ roundNumber
      ),
      today = case_when(
        response == "NIE-2025-10-01_nOPV_sNID" & roundNumber == "Rnd1" ~ as.Date("2025-10-10"),
        TRUE ~ today
      )
    )
  
  # Aggregate to cluster level
  df <- df |>
    filter(year > 2019) |>
    group_by(country, Region, District, response, vaccine.type, roundNumber) |>
    summarise(
      start_date = min(today),
      end_date = max(today),
      year = year(start_date),
      numbercluster = sum(Cluster, na.rm = TRUE),
      male_sampled = sum(male_sampled, na.rm = TRUE),
      female_sampled = sum(female_sampled, na.rm = TRUE),
      total_sampled = sum(total_sampled, na.rm = TRUE),
      male_vaccinated = sum(male_vaccinated, na.rm = TRUE),
      female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
      total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
      missed_child = sum(missed_child, na.rm = TRUE),
      r_Non_Compliance = sum(R_Non_Compliance, na.rm = TRUE),
      r_House_not_visited = sum(R_House_not_visited, na.rm = TRUE),
      r_childabsent = sum(R_childabsent, na.rm = TRUE),
      r_security = sum(R_security, na.rm = TRUE),
      r_childnotborn = sum(R_childnotborn, na.rm = TRUE),
      Care_Giver_Informed_SIA = sum(Care_Giver_Informed_SIA, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(numbercluster >= 2) |>
    mutate(
      percent_care_Giver_Informed_SIA = ifelse(total_sampled > 0, round(Care_Giver_Informed_SIA / total_sampled * 100, 2), 0),
      total_missed = ifelse(total_sampled < 60, 60 - total_sampled + missed_child, missed_child),
      status = ifelse(total_missed <= 3, "Pass", "Fail"),
      performance = case_when(
        total_missed < 4 ~ "high",
        total_missed < 9 ~ "moderate",
        total_missed < 20 ~ "poor",
        TRUE ~ "very poor"
      ),
      tot_r = r_Non_Compliance + r_House_not_visited + r_childabsent + r_security + r_childnotborn,
      other_r = pmax(total_missed - tot_r, 0),
      prct_r_Non_Compliance = ifelse(total_missed > 0, round(r_Non_Compliance / total_missed * 100, 2), 0),
      prct_r_House_not_visited = ifelse(total_missed > 0, round(r_House_not_visited / total_missed * 100, 2), 0),
      prct_r_childabsent = ifelse(total_missed > 0, round(r_childabsent / total_missed * 100, 2), 0),
      prct_r_childnotborn = ifelse(total_missed > 0, round(r_childnotborn / total_missed * 100, 2), 0),
      prct_r_security = ifelse(total_missed > 0, round(r_security / total_missed * 100, 2), 0),
      prct_other_r = ifelse(total_missed > 0, round(other_r / total_missed * 100, 2), 0)
    ) |>
    mutate(
      start_date = case_when(
        response == "OBR_name" & year(start_date) == 2025 & month(start_date) == 2 & roundNumber == "Rnd1" ~ as_date("2025-01-20"),
        response == "OBR_name" & year(start_date) == 2025 & month(start_date) == 3 & roundNumber == "Rnd1" ~ as_date("2025-01-20"),
        response == "NIE-2025-04-01_nOPV_NIDs" & year(start_date) == 2025 & month(start_date) == 5 & roundNumber == "Rnd1" ~ as_date("2025-04-26"),
        response == "NIE-2025-04-01_nOPV_NIDs" & year(start_date) == 2025 & month(start_date) == 7 & roundNumber == "Rnd2" ~ as_date("2025-06-14"),
        TRUE ~ start_date
      )
    )
  
  # ============================================================
  # JOIN WITH LOOKUP TABLE
  # ============================================================
  log_info("Joining with lookup table...")
  
  if (file.exists(lookup_file)) {
    prep_data <- read_excel(lookup_file) |>
      filter(Country == "NIGERIA") |>
      mutate(
        `Round Number` = case_when(
          `Round Number` == "Round 0" ~ "Rnd0",
          `Round Number` == "Round 1" ~ "Rnd1",
          `Round Number` == "Round 2" ~ "Rnd2",
          `Round Number` == "Round 3" ~ "Rnd3",
          `Round Number` == "Round 4" ~ "Rnd4",
          `Round Number` == "Round 5" ~ "Rnd5",
          `Round Number` == "Round 6" ~ "Rnd6",
          TRUE ~ `Round Number`
        )
      )
    
    prep_data <- prep_data |>
      rename(
        response = `OBR Name`,
        vaccine.type = Vaccines,
        roundNumber = `Round Number`
      ) |>
      mutate(
        round_start_date = as_date(`Round Start Date`),
        start_date = round_start_date + 4,
        end_date = as_date(start_date) + 1
      ) |>
      select(response, vaccine.type, roundNumber, round_start_date, start_date, end_date)
    
    lookup_table <- as_tibble(prep_data) |>
      mutate(
        start_date = as_date(start_date),
        end_date = as_date(end_date),
        round_start_date = as_date(round_start_date)
      )
    
    # Clean join keys
    df <- df %>%
      mutate(
        response = trimws(as.character(response)),
        vaccine.type = trimws(as.character(vaccine.type)),
        roundNumber = trimws(as.character(roundNumber))
      )
    lookup_table <- lookup_table %>%
      mutate(
        response = trimws(as.character(response)),
        vaccine.type = trimws(as.character(vaccine.type)),
        roundNumber = trimws(as.character(roundNumber))
      )
    
    df <- df |>
      left_join(lookup_table, by = c("response", "vaccine.type", "roundNumber")) |>
      mutate(
        start_date = coalesce(start_date.y, as_date(start_date.x)),
        end_date = as_date(start_date) + 1,
        round_start_date = coalesce(round_start_date, start_date - days(4))
      ) |>
      select(-start_date.x, -start_date.y, -end_date.x, -end_date.y) |>
      filter(!is.na(District))
  }
  
  # Special district rules
  districts_special <- c("YUSUFARI", "GURI", "BIRINIWA", "KIRI KASAMA", "NGURU", "MACHINA", "KARASUWA", "BARDE")
  province_special <- c("Adamawa", "Bauchi", "Borno", "Jigawa", "Kano", "Yobe")
  
  df <- df |>
    mutate(vaccine.type = case_when(
      District %in% districts_special & response == "NIE-2025-04-01_nOPV_NIDs" ~ "nOPV2 & bOPV",
      Region %in% province_special & response == "NIE-2025-10-01_nOPV_sNID" ~ "nOPV2 & bOPV",
      TRUE ~ vaccine.type
    )) |>
    select(
      country, province = Region, district = District, response, vaccine.type,
      roundNumber, numbercluster, round_start_date, start_date, end_date, year,
      male_sampled, female_sampled, total_sampled,
      male_vaccinated, female_vaccinated, total_vaccinated, missed_child,
      r_Non_Compliance, r_House_not_visited, r_childabsent, r_security, r_childnotborn,
      Care_Giver_Informed_SIA, percent_care_Giver_Informed_SIA, total_missed, status, performance,
      tot_r, other_r, prct_r_Non_Compliance, prct_r_House_not_visited, prct_r_childabsent,
      prct_r_childnotborn, prct_r_security, prct_other_r
    )
  
  # Write output
  log_info("Writing output to: {output_file}")
  dir_create(dirname(output_file))
  write_csv(df, output_file)
  
  log_info("✅ Successfully processed Nigeria CSV: {nrow(df)} rows")
  
  return(df)
}

# ============================================================
# EXECUTION
# ============================================================

result <- process_nigeria_csv(args$input_file, args$lookup_file, args$output_file)

if (is.null(result)) {
  log_error("Processing failed")
  quit(status = 1)
} else {
  log_info("Processing completed successfully")
  quit(status = 0)
}