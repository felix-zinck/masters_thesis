# =====================================================================
# Workflow 5 – Merging sensitivity and exposure layer
# =====================================================================

# -----------------------------
# 0. Load required packages
# -----------------------------
library(terra)
library(sf)         
library(dplyr)      
library(data.table) 
library(readr)      

# -----------------------------
# 1. Load datasets
# -----------------------------
# Zensus Berlin 100m grid
census_grid_file <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_indicators_norm_comp.gpkg"
census_grid <- st_read(census_grid_file)

# Median LST raster (scenario 4y_10cc)
lst_raster_file <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/median_LST_final/median_LST_FINAL_4y_10cc.tif"
lst_raster <- rast(lst_raster_file)

# -----------------------------
# 2. Transform CRS to projected EPSG:3035 for area-consistent calculations
# -----------------------------
if (crs(lst_raster, proj = TRUE) != st_crs(census_grid)$wkt) {
  # Project raster to match Zensus grid (Lambert Azimuthal Equal Area)
  lst_raster <- terra::project(lst_raster, crs(census_grid))
}

# Convert Zensus grid to terra vector
census_vect <- vect(census_grid)

# -----------------------------
# 3. Extract median and mean LST per grid cell
# -----------------------------

lst_cell_median <- terra::extract(lst_raster, census_vect,
                                  fun = function(x, ...) median(x, na.rm = TRUE))
lst_cell_mean   <- terra::extract(lst_raster, census_vect,
                                  fun = function(x, ...) mean(x, na.rm = TRUE))

# Add to grid
census_grid$lst_median <- lst_cell_median[,2]
census_grid$lst_mean   <- lst_cell_mean[,2]

# -----------------------------
# 4. Compute area-based coverage per grid cell (weighted by fractional pixel overlap)
# -----------------------------

# 4a. Extract raster values with fractional overlap
cov_df <- terra::extract(
  lst_raster, census_vect,
  weights = TRUE,  # fraction of pixel inside grid cell
  cells = TRUE,    # include raster cell IDs
  na.rm = FALSE)   # keep NA pixels for consistency

# 4b. Compute area per pixel (m²)
pixel_area <- prod(res(lst_raster))

# 4c. Multiply by weight to get actual area of pixel inside each grid cell
cov_df$pixel_area_inside <- cov_df$weight * pixel_area

# 4d. Identify LST value column and select only valid pixels (without NA)
value_col <- names(cov_df)[2]
cov_df_valid <- cov_df[!is.na(cov_df[[value_col]]), ]

# 4e. Aggregate valid LST area per grid cell
dt <- data.table(cov_df_valid)
valid_area_dt <- dt[, .(lst_valid_area_m2 = sum(pixel_area_inside)), by = ID]

# Ensure correct ordering of IDs (robust assignment)
valid_area_dt <- valid_area_dt[order(ID)]

# 4f. Compute grid cell area
grid_area <- expanse(census_vect, unit = "m")

# 4g. Assign valid area to full vector (initialize with 0 for cells without valid pixels)
valid_area <- numeric(length(census_vect))
valid_area[valid_area_dt$ID] <- valid_area_dt$lst_valid_area_m2

# 4h. Compute coverage (%) and cap at 100 to avoid floating-point artifacts
coverage_pct <- pmin((valid_area / grid_area) * 100, 100)

# 4i. Add to Zensus grid
census_grid$grid_area_m2      <- grid_area
census_grid$lst_valid_area_m2 <- valid_area
census_grid$lst_coverage_pct  <- coverage_pct

# -----------------------------
# 5. Remove grid cells with poor LST data coverage (< 90%)
# -----------------------------

# Check number of cells with LST area coverage < 90% (37)
sum(census_grid$lst_coverage_pct < 90, na.rm = TRUE)

# Remove grid cells with poor LST data coverage
census_grid <- census_grid %>% filter(lst_coverage_pct >= 90)

# -----------------------------
# 6. Normalize aggregated LST values
# -----------------------------

# Min-Max normalization function      
minmax_norm <- function(x) {
  if (all(is.na(x))) return(x) # Leave as-is if all values are NA
  
  rng <- range(x, na.rm = TRUE) # min and max
  if (rng[1] == rng[2]) return(rep(NA_real_, length(x))) # No variation => NA
  
  (x - rng[1]) / (rng[2] - rng[1])} # Min-Max normalization formula

# Apply normalization to median and mean LST
census_grid$lst_n_median <- minmax_norm(census_grid$lst_median)
census_grid$lst_n_mean   <- minmax_norm(census_grid$lst_mean)

# -----------------------------
# 7. Calculate and normalize Exposure-Sensitivity Indices
# -----------------------------

# Calculate ESI for individual heat sensitivity indicator 
sei_norm_cols <- c(
  "share_elderly_65plus_norm", "share_children_under6_norm",
  "share_single_households_norm", "share_single_parent_families_norm",
  "share_foreign_born_norm", "inv_avg_rent_norm",
  "inv_avg_living_space_norm")

for (sei in sei_norm_cols) {
  esi_name <- paste0(sei, "_esi")
  census_grid[[esi_name]] <- census_grid[[sei]] + census_grid$lst_n_median
  census_grid[[paste0(esi_name, "_norm")]] <- minmax_norm(census_grid[[esi_name]])
}

# Calculate and normalize general ESI
census_grid$esi_comb_raw <- census_grid$composite_index_norm + census_grid$lst_n_median
census_grid$esi_n_comb <- minmax_norm(census_grid$esi_comb_raw)

# -----------------------------
# 8. Rename final census grid for clarity
# -----------------------------
census_grid_final <- census_grid %>%
  rename(
    
    # Socio-economic heat sensitivity indicators (raw)
    sei_r_elderly         = share_elderly_65plus,
    sei_r_children        = share_children_under6,
    sei_r_single_hh       = share_single_households,
    sei_r_single_parent   = share_single_parent_families,
    sei_r_foreign_born    = share_foreign_born,
    sei_r_non_citizen     = share_non_citizens,
    sei_r_rent            = inv_avg_rent,
    sei_r_space           = inv_avg_living_space,
    
    # Socio-economic heat sensitivity indicators (normalized)
    sei_n_elderly         = share_elderly_65plus_norm,
    sei_n_children        = share_children_under6_norm,
    sei_n_single_hh       = share_single_households_norm,
    sei_n_single_parent   = share_single_parent_families_norm,
    sei_n_foreign_born    = share_foreign_born_norm,
    sei_n_non_citizen     = share_non_citizens_norm,
    sei_n_rent            = inv_avg_rent_norm,
    sei_n_space           = inv_avg_living_space_norm,
    
    # Composite Sensitivity Index
    csi_r                = composite_index,
    csi_n                = composite_index_norm,
    
    # Exposure-Sensitivity Indices (raw)
    esi_r_elderly         = share_elderly_65plus_norm_esi,
    esi_r_children        = share_children_under6_norm_esi,
    esi_r_single_hh       = share_single_households_norm_esi,
    esi_r_single_parent   = share_single_parent_families_norm_esi,
    esi_r_foreign_born    = share_foreign_born_norm_esi,
    esi_r_rent            = inv_avg_rent_norm_esi,
    esi_r_space           = inv_avg_living_space_norm_esi,
    
    # Exposure-Sensitivity Indices (normalized)
    esi_n_elderly         = share_elderly_65plus_norm_esi_norm,
    esi_n_children        = share_children_under6_norm_esi_norm,
    esi_n_single_hh       = share_single_households_norm_esi_norm,
    esi_n_single_parent   = share_single_parent_families_norm_esi_norm,
    esi_n_foreign_born    = share_foreign_born_norm_esi_norm,
    esi_n_rent            = inv_avg_rent_norm_esi_norm,
    esi_n_space           = inv_avg_living_space_norm_esi_norm,
    
    # Composite ESI
    cesi_r                = esi_comb_raw,
    cesi_n                = esi_n_comb
  )

# Reorder
census_grid_final <- census_grid_final %>%
  select(
    # 1. Grid and population
    grid_id, population, 
    
    # 2. Demographic / socio-economic attributes
    elderly_65plus, children_under3, children_3to5, 
    avg_living_space_m2, avg_rent_eur_m2, foreign_born,
    total_households, single_person_households,
    non_citizens, total_families,
    single_father_families, single_mother_families,
    
    # 3. Heat Sensitivity Indicators (raw)
    sei_r_elderly, sei_r_children, sei_r_single_hh, sei_r_single_parent,
    sei_r_foreign_born, sei_r_non_citizen, sei_r_rent, sei_r_space,
    
    # 4. Heat Sensitivity Indicators (normalized)
    sei_n_elderly, sei_n_children, sei_n_single_hh, sei_n_single_parent,
    sei_n_foreign_born, sei_n_non_citizen, sei_n_rent, sei_n_space,
    
    # 5. Composite Heat Sensitivity Index
    csi_r, csi_n,
    
    # 6. Land Surface Temperature
    lst_median, lst_mean, grid_area_m2, lst_valid_area_m2, lst_coverage_pct,
    lst_n_median, lst_n_mean,
    
    # 7. Exposure-Sensitivity Indices (raw & normalized)
    esi_r_elderly, esi_n_elderly,
    esi_r_children, esi_n_children,
    esi_r_single_hh, esi_n_single_hh,
    esi_r_single_parent, esi_n_single_parent,
    esi_r_foreign_born, esi_n_foreign_born,
    esi_r_rent, esi_n_rent,
    esi_r_space, esi_n_space,
    
    # 8. Composite Exposure-Sensitivity Index
    cesi_r, cesi_n,
    
    # 9. Geometry
    geom)

# -----------------------------
# 9. Save final Berlin 100m grid
# -----------------------------
output_grid_final <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_final.gpkg"
dir.create(dirname(output_grid_final), showWarnings = FALSE)
st_write(census_grid_final, output_grid_final, delete_layer = TRUE)