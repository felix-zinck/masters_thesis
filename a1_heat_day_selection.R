# =====================================================================
# Workflow 1 – Heat day selection
# =====================================================================

# Load libraries
library(dplyr)
library(readr)

# Paths to DWD Tempelhof observations (Station ID 00433)
hist_path   <- "/home/fix/Documents/sustainable_development/master_thesis/data/meteorological_data/produkt_klima_tag_19480101_20241231_00433.txt"
recent_path <- "/home/fix/Documents/sustainable_development/master_thesis/data/meteorological_data/produkt_klima_tag_20240616_20251217_00433.txt"

# Load historical and recent data
hist <- read_delim(hist_path, delim = ";", na = c("-999","-9999"), show_col_types = FALSE)
recent <- read_delim(recent_path, delim = ";", na = c("-999","-9999"), show_col_types = FALSE)

# Merge and remove duplicates
all_data <- bind_rows(hist, recent) %>%
  distinct(STATIONS_ID, MESS_DATUM, .keep_all = TRUE)

# Convert MESS_DATUM to Date
all_data <- all_data %>%
  mutate(date = as.Date(as.character(MESS_DATUM), format = "%Y%m%d"))

# Clean column names
colnames(all_data) <- trimws(colnames(all_data))

# Convert TXK (Tmax) to numeric
all_data$TXK <- as.numeric(all_data$TXK)

# Convert TXK (stored in tenths) to °C
all_data <- all_data %>%
  mutate(Tmax = ifelse(!is.na(TXK) & TXK > 100, TXK / 10, TXK))

# Filter for hot days (>= 30°C) between 2018-01-01 and 2025-12-31
heat_days <- all_data %>%
  filter(date >= as.Date("2018-01-01") &
         date <= as.Date("2025-12-31") &
         !is.na(Tmax) & Tmax >= 30 ) %>% select(date, Tmax)

# Export CSV
interim_results_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/interim_results" 
dir.create(dirname(interim_results_path), showWarnings = FALSE, recursive = TRUE)
write_csv(heat_days, file.path(interim_results_path, "01_tempelhof_heat_days_2018_2025.csv"))
