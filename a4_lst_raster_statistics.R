# =====================================================================
# Workflow 4 – LST Scenario Evaluation
# =====================================================================

# -----------------------------
# 0. Load required packages
# -----------------------------
library(terra)
library(sf)
library(dplyr)
library(readr)

# -----------------------------
# 1. Define paths
# -----------------------------
lst_final_path <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/median_LST_final"
obs_final_path <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/observation_count_final"

# -----------------------------
# 2. Load finalized rasters
# -----------------------------
lst_files <- list.files(lst_final_path, pattern = "\\.tif$", full.names = TRUE)
obs_files <- list.files(obs_final_path, pattern = "\\.tif$", full.names = TRUE)

# -----------------------------
# 3. Load census grid for Berlin created in Workflow 3
# -----------------------------
grid_file <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_indicators_norm_comp.gpkg"
zensus_grid <- st_read(grid_file)

# Dissolve grid to single polygon
grid_outline <- st_union(zensus_grid)

# Convert to terra vector and project to raster CRS
outline_vect <- vect(grid_outline)
outline_vect <- project(outline_vect, crs(rast(lst_files[1])))

# -----------------------------
# 4. Function to compute raster statistics 
# -----------------------------
compute_raster_stats <- function(lst_raster, obs_raster, scenario_name, outline_vect) {
  
  # Mask rasters to Berlin outline
  lst_masked <- mask(lst_raster, outline_vect)
  obs_masked <- mask(obs_raster, outline_vect)
  
  # Extract values
  lst_vals <- values(lst_masked)
  obs_vals <- values(obs_masked)
  
  # Pixels inside Berlin grid (based on observation availability)
  inside_idx <- !is.na(obs_vals)
  lst_inside <- lst_vals[inside_idx]
  obs_inside <- obs_vals[inside_idx]
  
  # Valid values
  lst_valid <- lst_inside[!is.na(lst_inside)]
  obs_valid <- obs_inside[!is.na(obs_inside)]
  
  total_pixels <- length(obs_inside)
  
  # NA percentage (LST)
  NA_pixels <- sum(is.na(lst_inside))
  NA_percent <- round(100 * NA_pixels / total_pixels, 2)
  
  # Observation stats
  median_obs <- round(median(obs_valid, na.rm = TRUE), 2)
  coverage_5 <- round(100 * sum(obs_inside >= 5, na.rm = TRUE) / length(obs_inside), 1)
  coverage_6 <- round(100 * sum(obs_inside >= 6, na.rm = TRUE) / length(obs_inside), 1)
  
  # LST stats
  median_lst <- round(median(lst_valid, na.rm = TRUE), 2)
  iqr_lst    <- round(IQR(lst_valid, na.rm = TRUE), 2)

  # Compact output
  data.frame(
    Scenario     = scenario_name,
    Median_LST   = median_lst,
    IQR_LST      = iqr_lst,
    Median_Obs   = median_obs,
    Coverage_5   = coverage_5,
    Coverage_6   = coverage_6,
    NA_percent   = NA_percent
  )
}

# -----------------------------
# 5. Compute statistics for all scenarios
# -----------------------------
comparison_stats_df <- Map(
  function(lst_path, obs_path, scenario_name) {
    
    lst_raster <- rast(lst_path)
    obs_raster <- rast(obs_path)
    
    compute_raster_stats(lst_raster, obs_raster, scenario_name, outline_vect)
  },
  lst_files,
  obs_files,
  gsub("median_LST_FINAL_|\\.tif", "", basename(lst_files))
) %>% bind_rows()

# Preview
print(comparison_stats_df)

# -----------------------------
# 6. Save results
# -----------------------------
main_results_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/main_results"
dir.create(main_results_path, showWarnings = FALSE, recursive = TRUE)
write_csv(comparison_stats_df,
          file.path(main_results_path, "5_1_table_lst_raster_statistics.csv"))

# -----------------------------
# 8. Spatial agreement with selected scenario (4y_10cc)
# -----------------------------

# Define selected scenario
selected_scenario <- "4y_10cc"

# Identify selected raster
selected_idx <- which(gsub("median_LST_FINAL_|\\.tif", "", basename(lst_files)) == selected_scenario)

# Load and mask selected raster
selected_raster <- rast(lst_files[selected_idx])
selected_masked <- mask(selected_raster, outline_vect)
selected_vals <- values(selected_masked)

# Compute correlation of each scenario against selected scenario
correlation_df <- Map(
  function(lst_path, scenario_name) {
    
    lst_raster <- rast(lst_path)
    lst_masked <- mask(lst_raster, outline_vect)
    lst_vals <- values(lst_masked)
    
    valid_idx <- !is.na(selected_vals) & !is.na(lst_vals)
    
    data.frame(
      Scenario = scenario_name,
      Pearson_r = round(cor(selected_vals[valid_idx], lst_vals[valid_idx], method = "pearson"), 4),
      Compared_Pixels = sum(valid_idx)
    )
  },
  lst_files,
  gsub("median_LST_FINAL_|\\.tif", "", basename(lst_files))
) %>% bind_rows()

# Preview
print(correlation_df)

# Save correlation results
interim_results_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/interim_results"
write_csv(correlation_df,
          file.path(appendix_path, "appendix_table_lst_spatial_agreement_to_4y_10cc.csv"))