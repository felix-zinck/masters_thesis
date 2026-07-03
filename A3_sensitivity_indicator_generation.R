# =====================================================================
# Workflow 3 – Sensitivity Layer Generation 
# =====================================================================

# -----------------------------
# 0. Load required packages
# -----------------------------
library(sf)
library(dplyr)
library(readr)
library(corrplot)

# -----------------------------
# 1. Load Berlin boundary and transform to EPSG:3035 (complies with Census 2022 
# CSV grid data)
# -----------------------------
berlin <- st_read(
  "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_borders/berlin.shp"
  ) %>% st_union() %>% st_transform(3035)

# -----------------------------
# 2. Import Zensus 2022 100 m grid geometry and extract grid cells fully within
# Berlin boundary
# -----------------------------

# Load an read grid file
grid_file  <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_grid_geometry/zensus_grid_geometry.gpkg"
grid_layer <- "Zensus2022_100mGitter"
grid <- st_read(grid_file, layer = grid_layer)

# Keep only grid ID, inhabitants, and average age attribute
# grid_id: used to merge with Zensus 2022 CSV files later
# population: used to check number of grid cells with few inhabitants
# avg_age: used to check attribute consistency with Zensus CSV files later
grid <- grid[, c("GITTER_ID_100m", "Einwohner", "Durchschnittsalter")]

# Rename attributes
names(grid)[names(grid) == "GITTER_ID_100m"]   <- "grid_id"
names(grid)[names(grid) == "Einwohner"]        <- "population"
names(grid)[names(grid) == "Durchschnittsalter"] <- "avg_age"

# Transform to EPSG:3035
grid <- st_transform(grid, 3035)

# Confirm reprojection and CRS consistency with Berlin boundary
st_crs(grid)
st_crs(berlin)

# Extract grid cells that are fully inside Berlin boundary
berlin_grid <- grid[st_within(grid, berlin, sparse = FALSE), ]

# Check number of grid cells inside Berlin (40280)
nrow(berlin_grid)

# Check min number of inhabitants (3)
min(berlin_grid$population, na.rm = TRUE)

# Save clipped Berlin grid
output_grid_berlin <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_100m_grid.gpkg"
dir.create(dirname(output_grid_berlin), showWarnings = FALSE)
st_write(berlin_grid, output_grid_berlin, delete_layer = TRUE)

# Clean up workspace
rm(list = ls())

# -----------------------------
# 3. Add Zensus 2022 attributes from CSV files based on grid_id to Berlin grid 
# -----------------------------

# Load and read Berlin grid
berlin_grid_file  <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_100m_grid.gpkg"
berlin_grid <- st_read(berlin_grid_file)

# -----------------------------
# 3.1 Load and preprocess Zensus 2022 attribute CSV files
# -----------------------------

# Folder where Zensus CSVs with attributes are stored
data_folder <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/indicator_data"

# List all CSV files in folder
csv_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Function to read, standardize, and clean all CSVs
zensus_list <- lapply(csv_files, function(file) {
  
  # Read CSV
  df <- read_delim(file, delim = ";", show_col_types = FALSE)
  
  # Standardize grid ID column name
  id_col <- grep("^GITTER_ID", names(df), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(id_col)) {
    names(df)[names(df) == id_col] <- "grid_id"
  }
  
  # Remove coordinate columns if present
  df <- df %>%
    select(-any_of(c("x_mp_100m", "y_mp_100m")))
  
  # Convert all non-ID columns to numeric
  df <- df %>%
    mutate(across(
      -grid_id,
      ~ suppressWarnings(as.numeric(as.character(.)))
    ))
  
  return(df)})

# Name list elements using file names
names(zensus_list) <- tools::file_path_sans_ext(basename(csv_files))

# Check loaded datasets
names(zensus_list)

# Function to check that CSVs are loaded and attribute columns converted correctly
summarize_zensus <- function(zensus_list) {
  
  lapply(names(zensus_list), function(name) {
    
    df <- zensus_list[[name]]
    
    # Basic structure checks
    has_id        <- "grid_id" %in% names(df)
    n_numeric     <- sum(sapply(df, is.numeric))
    n_character   <- sum(sapply(df, is.character))
    id_unique     <- if (has_id) n_distinct(df$grid_id) == nrow(df) else NA
    
    tibble(
      Dataset          = name,
      Rows             = nrow(df),
      Columns          = ncol(df),
      Numeric_Columns  = n_numeric,
      Character_Cols   = n_character,
      ID_Present       = has_id,
      ID_Unique        = id_unique,
      Total_NA         = sum(is.na(df)),
      Column_Types    = paste(sapply(df, class), collapse = ", ")
    )
    
  }) %>% bind_rows()}

# Run summary and produce summary table
zensus_summary <- summarize_zensus(zensus_list)
zensus_summary

# Save summary table
interim_results_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/interim_results"
write_csv(zensus_summary,
          file.path(interim_results_path, "02_table_zensus_csv_summary.csv"))

# -------------------------------------------------
# 3.2 Add Zensus attributes to Berlin grid
# -------------------------------------------------

# Merge all attribute tables into one
all_attributes <- Reduce(function(x, y) {
  full_join(x, y, by = "grid_id")
}, zensus_list)

# Check for duplicate grid IDs after merging
if (nrow(all_attributes) != n_distinct(all_attributes$grid_id)) {
  warning("Duplicate grid_id values detected in merged attribute table.")
}

# Add attributes to Berlin grid by "grid_id"
berlin_grid_atr <- berlin_grid %>%
  left_join(all_attributes, by = "grid_id")

# -------------------------------------------------
# 3.3 Define attribute columns needed for indicator calculation and to confirm 
# that join by "grid_id" worked correctly
# -------------------------------------------------
keep_cols <- c(
  
  # Grid attributes
  "grid_id", "population", "avg_age", 
  "geom", # Geometry column
  
  # Zensus attributes
  "Insgesamt_Bevoelkerung.x", "Durchschnittsalter", "a65undaelter", "Unter3",
  "a3bis5", "durchschnFlaechejeBew", "durchschnMieteQM",
  "Ausland_Sonstige.x", # Country of birth
  "Insgesamt_Haushalte","1_Person",
  "Ausland_Sonstige.y", # State membership
  "Insgesamt_Familie", "Vater_mind_1Kind_unter18", "Mutter_mind_1Kind_unter18"
)

# Subset the grid to heat sensibility attributes
berlin_grid_sens_atr <- berlin_grid_atr %>% select(all_of(keep_cols))

# Rename attribute columns
berlin_grid_sens_atr <- berlin_grid_sens_atr %>%
  rename(
    population_check         = Insgesamt_Bevoelkerung.x,
    avg_age_check            = Durchschnittsalter,
    elderly_65plus           = a65undaelter,
    children_under3          = Unter3,
    children_3to5            = a3bis5,
    avg_living_space_m2      = durchschnFlaechejeBew,
    avg_rent_eur_m2          = durchschnMieteQM,
    foreign_born             = Ausland_Sonstige.x,   # Country of birth
    non_citizens             = Ausland_Sonstige.y,   # State membership
    total_households         = Insgesamt_Haushalte,
    single_person_households = `1_Person`,
    total_families           = Insgesamt_Familie,
    single_father_families   = Vater_mind_1Kind_unter18,
    single_mother_families   = Mutter_mind_1Kind_unter18
  )

# Rescale average population age from Zensus CSV data
berlin_grid_sens_atr <- berlin_grid_sens_atr %>%
  mutate(avg_age_check = round(avg_age_check / 100, 2))

# Check if join by grid_id worked correctly by comparing number of inhabitants 
# and average population age from grid and zensus datasets (returns 0 for both
# sum checks and summary outputs are identical)
sum(berlin_grid_sens_atr$population != berlin_grid_sens_atr$population_check, na.rm = TRUE)
sum(abs(berlin_grid_sens_atr$avg_age - berlin_grid_sens_atr$avg_age_check) > 0.0001, na.rm = TRUE)
summary(berlin_grid_sens_atr$population)
summary(berlin_grid_sens_atr$population_check)
summary(berlin_grid_sens_atr$avg_age)
summary(berlin_grid_sens_atr$avg_age_check)

# Drop redundant columns after join check
berlin_grid_sens_atr <- berlin_grid_sens_atr %>%
  select(-c(population_check, avg_age, avg_age_check))

# Save Berlin 100m grid with Zensus heat sensitivity attributes
# Output path
output_grid_sens_atr <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_sens_atr.gpkg"
dir.create(dirname(output_grid_sens_atr), showWarnings = FALSE)
st_write(berlin_grid_sens_atr, output_grid_sens_atr, delete_layer = TRUE)

# -----------------------------
# 4. Indicator calculation and normalization
# -----------------------------

# Load and read data
input_grid <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_sens_atr.gpkg"
berlin_grid_sens_atr <- st_read(input_grid)

# Indicator calculation 
berlin_grid_indicators_raw <- berlin_grid_sens_atr %>%
  mutate(
    # Demographic
    share_elderly_65plus = if_else(population > 0,
                                   elderly_65plus / population,
                                   NA_real_),
    
    share_children_under6 = if_else(population > 0,
                                    (children_under3 + children_3to5) / population,
                                    NA_real_),
    
    # Social structure
    share_single_households = if_else(total_households > 0,
                                      single_person_households / total_households,
                                      NA_real_),
    
    share_single_parent_families = if_else(total_families > 0,
                                           (single_father_families +
                                              single_mother_families) / total_families,
                                           NA_real_),
    
    # Migration / legal status
    share_foreign_born = if_else(population > 0,
                                 foreign_born / population,
                                 NA_real_),
    
    share_non_citizens = if_else(population > 0,
                                 non_citizens / population,
                                 NA_real_),
    
    # Housing stress (inverse indicators)
    inv_avg_rent = if_else(!is.na(avg_rent_eur_m2) & avg_rent_eur_m2 > 0,
                           1 / avg_rent_eur_m2,
                           NA_real_),
    
    inv_avg_living_space = if_else(!is.na(avg_living_space_m2) & avg_living_space_m2 > 0,
                                   1 / avg_living_space_m2,
                                   NA_real_))

# Check indicator calculation
summary(select(berlin_grid_indicators_raw, 
               starts_with("share_"), starts_with("inv_")))

# -----------------------------
# Check share indicators outside logical bounds (<0 or >1)
# -----------------------------

# Strip geometry
indicator_df <- st_set_geometry(berlin_grid_indicators_raw, NULL)

# Select share indicator columns only
share_cols <- indicator_df %>% select(starts_with("share_")) %>% names()

# Define relevant denominator for each share indicator
denominator_lookup <- tibble::tibble(
  indicator = c(
    "share_elderly_65plus", "share_children_under6",
    "share_single_households","share_single_parent_families",
    "share_foreign_born","share_non_citizens"
  ),
  denominator_type = c(
    "population", "population", "households","families","population","population"
  ),
  denominator_col = c(
    "population", "population", "total_households","total_families",
    "population","population"))

# Create diagnostics table for share indicator grid cells out of logical bounds
share_ind_out_of_bounds <- purrr::map_dfr(share_cols, function(ind) {
  
  denominator_col <- denominator_lookup %>%
    filter(indicator == ind) %>% pull(denominator_col)
  
  denominator_type <- denominator_lookup %>%
    filter(indicator == ind) %>% pull(denominator_type)
  
  # Cells where the share exceeds the logical upper bound
  cells_gt1 <- indicator_df %>%
    filter(!is.na(.data[[ind]]), .data[[ind]] > 1)
  
  tibble::tibble(
    indicator = ind,
    denominator = denominator_type,
    n_cells_gt1 = sum(indicator_df[[ind]] > 1, na.rm = TRUE),
    n_cells_lt0 = sum(indicator_df[[ind]] < 0, na.rm = TRUE),
    min_denominator = if (nrow(cells_gt1) > 0) min(cells_gt1[[denominator_col]], na.rm = TRUE) else NA_real_,
    median_denominator = if (nrow(cells_gt1) > 0) round(median(cells_gt1[[denominator_col]], na.rm = TRUE), 2) else NA_real_,
    max_denominator = if (nrow(cells_gt1) > 0) max(cells_gt1[[denominator_col]], na.rm = TRUE) else NA_real_
  )})

# View and save table
share_ind_out_of_bounds
appendix_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/appendix"
dir.create(appendix_path, showWarnings = FALSE, recursive = TRUE)
write_csv(share_ind_out_of_bounds,
  file.path(appendix_path, "appendix_table_share_indicators_out_of_bounds.csv"))

# -----------------------------
# Cap share indicators to logical bounds
# -----------------------------
berlin_grid_indicators_cap <- berlin_grid_indicators_raw %>%
  mutate(across(all_of(share_cols),~ pmin(pmax(., 0), 1)))

# Check cap (min value = 0; max value = 1)
summary(berlin_grid_indicators_cap %>% st_set_geometry(NULL) %>%
    select(all_of(share_cols)))

# Save Berlin 100m grid containing capped heat sensitivity indicators
output_grid_indicators_cap <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_indicators_cap.gpkg"
dir.create(dirname(output_grid_indicators_cap), showWarnings = FALSE)
st_write(berlin_grid_indicators_cap, output_grid_indicators_cap, delete_layer = TRUE)

# Clean up workspace
rm(list = ls())

# -----------------------------
# 5. Normalize indicators using min-max normalization
# -----------------------------

# Load and read grid data
input_grid <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_indicators_cap.gpkg"
berlin_grid_indicators <- st_read(input_grid)

# Min-Max normalization function      
minmax_norm <- function(x) {
  if (all(is.na(x))) return(x) # If all values are NA, leave indicator unchanged
  
  rng <- range(x, na.rm = TRUE) # Returns min (rng[1]) and max (rng[2]) value of indicator
  if (rng[1] == rng[2]) return(rep(NA_real_, length(x))) # Return NA if indicator
  # has nor spatial variation
  
  (x - rng[1]) / (rng[2] - rng[1]) # Min-Max normalization formula
}

# Apply min-max function to indicators stored in berlin_grid_indicators and add
# normailzed indicators to the grid

# List of indicator columns
indicator_cols <- berlin_grid_indicators %>%
  st_set_geometry(NULL) %>%
  select(starts_with("share_"), starts_with("inv_")) %>% names()

# Normalize indicators and add to grid
berlin_grid_indicators_norm <- berlin_grid_indicators %>%
  mutate(across(all_of(indicator_cols), minmax_norm,.names = "{.col}_norm"))

# Check if normalization worked for each indicator:
# Identify normalized indicator columns
norm_cols <- names(berlin_grid_indicators_norm)[grepl("_norm$", names(berlin_grid_indicators_norm))]

# Corresponding original columns (remove _norm suffix)
orig_cols <- sub("_norm$", "", norm_cols)

# Create a check table
check_norm <- sapply(seq_along(norm_cols), function(i) {
  orig <- berlin_grid_indicators_norm[[orig_cols[i]]]
  norm <- berlin_grid_indicators_norm[[norm_cols[i]]]
  
  c(orig_min = min(orig, na.rm = TRUE),
    orig_max = max(orig, na.rm = TRUE),
    norm_min = min(norm, na.rm = TRUE),
    norm_max = max(norm, na.rm = TRUE))
})

# Transpose for readability
check_norm <- t(check_norm)
rownames(check_norm) <- norm_cols
check_norm

# Save Berlin 100m grid indicators and normalized indicators
output_grid_indicators_norm <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_indicators_norm.gpkg"
dir.create(dirname(output_grid_indicators_norm), showWarnings = FALSE)
st_write(berlin_grid_indicators_norm, output_grid_indicators_norm, delete_layer = TRUE)

# -----------------------------
# 6. Spearman: Correlation matrix between indicators to remove possible double counts
# -----------------------------

# Load and read data
input_grid <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_indicators_norm.gpkg"
berlin_grid_indicators_norm <- st_read(input_grid)

# Remove geometry
indicator_df <- st_set_geometry(berlin_grid_indicators_norm, NULL)

# Select normalized indicators
norm_indicator_cols <- indicator_df %>% select(ends_with("_norm")) %>% names()

norm_indicators <- indicator_df %>% select(all_of(norm_indicator_cols))

# Compute correlation matrix
cor_matrix <- cor(norm_indicators, method = "spearman",
                  use = "pairwise.complete.obs")

# Define desired order
ind_order <- c(
  "share_elderly_65plus_norm", "share_children_under6_norm",
  "share_single_households_norm","share_single_parent_families_norm",
  "share_foreign_born_norm", "share_non_citizens_norm",
  "inv_avg_living_space_norm", "inv_avg_rent_norm")

# Reorder matrix
cor_matrix <- cor_matrix[ind_order, ind_order]

# Define clean labels
label_map <- c(
  share_elderly_65plus_norm = "Elderly (65+)",
  share_children_under6_norm = "Children (<6)",
  share_single_households_norm = "Single households",
  share_single_parent_families_norm = "Single parents",
  share_foreign_born_norm = "Foreign-born",
  share_non_citizens_norm = "Non-citizens",
  inv_avg_living_space_norm = "Living space (inverse)",
  inv_avg_rent_norm = "Rent (inverse)"
)

# Apply labels after ordering
colnames(cor_matrix) <- label_map[colnames(cor_matrix)]
rownames(cor_matrix) <- label_map[rownames(cor_matrix)]

# Plot correlation matrix
plot_corr_matrix <- function(cor_matrix) {
  
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xpd = NA)
  
  corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
    
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.7,
    
    addCoef.col = "black",
    number.cex = 0.6,
    
    cl.pos = "r",
    cl.length = 6,
    cl.cex = 0.7,
    
    diag = FALSE,
    addgrid.col = NA,
    mar = c(0, 0, 0, 0))
}

# Preview and export corrplot
plot_corr_matrix(cor_matrix)

png(
  filename = file.path(appendix_path, "appendix_figure_indicator_correlation_matrix.png"),
  width = 120,
  height = 100,
  units = "mm",
  res = 300
)

plot_corr_matrix(cor_matrix)

dev.off()

# Desicion which indicator to drop
# Non-citizen population indicator has more NAs and lower variance
sapply(indicator_df[, c("share_foreign_born_norm", "share_non_citizens_norm")], function(x) sum(is.na(x)))
sapply(indicator_df[, c("share_foreign_born_norm", "share_non_citizens_norm")], var, na.rm = TRUE)

# -----------------------------
# 7. Create and normalize composite heat sensitivity index using equal weighting (mean)
# -----------------------------

# Create composite index
berlin_grid_indicators_norm_comp <- berlin_grid_indicators_norm %>%
  rowwise() %>% # Treat each row independently
  mutate(composite_index = mean(c(
      share_elderly_65plus_norm, share_children_under6_norm,
      share_single_households_norm, share_single_parent_families_norm,
      share_foreign_born_norm, inv_avg_rent_norm, inv_avg_living_space_norm
    ), na.rm = TRUE) # Ensures mean is calculated only from valid values
  ) %>% ungroup() # Remove row-wise grouping

# Apply normalization
berlin_grid_indicators_norm_comp <- berlin_grid_indicators_norm_comp %>%
  mutate(composite_index_norm = minmax_norm(composite_index))

# Save final Berlin 100m grid containing full sized grid cells, indicators, 
# normalized indicators and normalized composite index

output_grid_indicators_norm_comp <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_indicators_norm_comp.gpkg"
dir.create(dirname(output_grid_indicators_norm_comp), showWarnings = FALSE)
st_write(berlin_grid_indicators_norm_comp, output_grid_indicators_norm_comp, delete_layer = TRUE)
