# =====================================================================
# Workflow 2 – Median LST Raster Generation 
# =====================================================================

# -----------------------------
# 0. Load required packages
# -----------------------------
library(reticulate)
library(rgee)
library(terra)
library(sf)
library(lwgeom)
library(readr)
library(dplyr)
library(lubridate)

# -----------------------------
# 1. Initialize Google Earth Engine (GEE)
# -----------------------------
# ee_Authenticate()  # Run once for GEE setup or when credentials are expired

ee_Initialize(
  user    = "felice.zinck@gmail.com", project = "thesisproject-482109")

# -----------------------------
# 2. Extract Berlin city boundary from Zensus 2022 administrative borders,  
#    transform to Coordinate Reference System (CRS) EPSG:4326 (complies with 
#    GEE), and convert to GEE geometry
# -----------------------------

# File paths
admin_shapefile <- "C:/Users/felix/Documents/master_thesis/data/geodata/zensus_administrative_borders/VG250_GEM.shp"
output_folder <- "C:/Users/felix/Documents/master_thesis/data/geodata/zensus_berlin_borders"
output_city     <- file.path(output_folder, "berlin.shp")
dir.create(output_folder, showWarnings = FALSE)

# Read administrative boundaries
admin_borders_germany <- st_read(admin_shapefile)

# Extract Berlin polygon (GEN = attribute for geographic name)
berlin <- admin_borders_germany[admin_borders_germany$GEN == "Berlin",]

# Keep only city name attribute to reduce computational size
# 'drop = FALSE' ensures the object remains sf data frame with geometry
berlin <- berlin[, "GEN", drop = FALSE]

# Rename attribute 'GEN' to 'City'
names(berlin)[names(berlin) == "GEN"] <- "City"

# Repair potential invalid geometries and transform to EPSG:4326
berlin <- st_make_valid(berlin) %>% st_transform(4326)

# Check reprojection
st_crs(berlin)

# Save Berlin boundary
st_write(berlin, output_city, delete_layer = TRUE)

# Clean up workspace
rm(admin_borders_germany)

# Create GEE object
berlin_wgs84_ee <- sf_as_ee(berlin)

# -----------------------------
# 3. Load hot day dataset (all days with Tmax ≥ 30°C from 2018-2025)
# -----------------------------
hot_days_df <- read.csv(
  "C:/Users/felix/Documents/master_thesis/results/interim_results/01_tempelhof_heat_days_2018_2025.csv",
  stringsAsFactors = FALSE)

# Convert date column
hot_days_df$date <- as.Date(hot_days_df$date)

if (any(is.na(hot_days_df$date))) {
  stop("Date conversion failed. Check CSV format.")}

# -----------------------------
# 4. Load Landsat 8 & 9 Collection 2 Level-2
# -----------------------------
ls8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")
ls9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")
landsat <- ls8$merge(ls9)

# -----------------------------
# 5. Cloud masking function (pixel-level)
# -----------------------------
maskClouds <- function(image) {
  
  qa <- image$select("QA_PIXEL")
  
  mask <- qa$bitwiseAnd(2)$eq(0)$     # bit 1 = dilated cloud
    And(qa$bitwiseAnd(4)$eq(0))$      # bit 2 = cirrus
    And(qa$bitwiseAnd(8)$eq(0))$      # bit 3 = cloud
    And(qa$bitwiseAnd(16)$eq(0))      # bit 4 = cloud shadow
  
  image$updateMask(mask)}

# -----------------------------
# 6. LST conversion function (K to °C)
# -----------------------------
convert_to_celsius <- function(image) {
  image$select("ST_B10")$
    multiply(0.00341802)$
    add(149.0)$
    subtract(273.15)$
    rename("LST_C")$
    copyProperties(image, image$propertyNames())}

# -----------------------------
# 7. Output directories
# -----------------------------
out_path <- "C:/Users/felix/Documents/master_thesis/geodata/"

dir.create(file.path(out_path, "median_LST_raw"),
           showWarnings = FALSE, recursive = TRUE)

dir.create(file.path(out_path, "observation_count_raw"),
           showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 8. Increase timeout for raster downloads
# -----------------------------
options(timeout = 600)

# -----------------------------
# 9. Main raster generation function
# -----------------------------
generate_LST_rasters <- function(n_years, cloud_threshold) {
  
  for (ny in n_years) {
    for (cc in cloud_threshold) {
      
      cat("Processing:", ny, "years |", cc, "% cloud\n")
      
      # Calendar-year based selection
      max_year   <- lubridate::year(max(hot_days_df$date))
      start_year <- max_year - ny + 1
      
      hot_days_scenario <- hot_days_df %>%
        dplyr::filter(
          lubridate::year(date) >= start_year &
            lubridate::year(date) <= max_year
        )
      
      if (nrow(hot_days_scenario) == 0) {
        cat("No hot days in selected period.\n\n")
        next
      }
      
      # GEE requires character format YYYY-MM-DD
      hot_dates_gee <- as.character(hot_days_scenario$date)
      
      # Filter Landsat collection
      landsat_hot <- landsat$
        filterBounds(berlin_wgs84_ee)$
        filter(ee$Filter$lt("CLOUD_COVER", cc))$
        map(maskClouds)$
        filter(ee$Filter$inList("DATE_ACQUIRED", hot_dates_gee))
      
      n_scenes <- landsat_hot$size()$getInfo()
      
      if (n_scenes == 0) {
        cat("No Landsat scenes available.\n\n")
        next
      }
      
      cat("Number of Landsat scenes:", n_scenes, "\n")
      
      # Convert to Celsius
      landsat_hot_C <- landsat_hot$map(convert_to_celsius)
      
      # Median LST raster
      lst_median <- landsat_hot_C$median()
      
      lst_file <- file.path(
        out_path,
        "median_LST_raw",
        paste0("median_LST_raw_", ny, "y_", cc, "cc.tif")
      )
      
      download.file(
        lst_median$getDownloadURL(list(
          scale  = 30,
          region = berlin_wgs84_ee,
          format = "GeoTIFF"
        )),
        destfile = lst_file,
        mode     = "wb",
        method   = "libcurl"
      )
      
      # Observation count raster
      obs_raster <- landsat_hot_C$
        map(function(img) {
          img$select("LST_C")$
            mask()$
            rename("obs")
        })$
        sum()
      
      obs_file <- file.path(
        out_path,
        "observation_count_raw",
        paste0("observation_count_raw_", ny, "y_", cc, "cc.tif")
      )
      
      download.file(
        obs_raster$getDownloadURL(list(
          scale  = 30,
          region = berlin_wgs84_ee,
          format = "GeoTIFF"
        )),
        destfile = obs_file,
        mode     = "wb",
        method   = "libcurl"
      )
      
      cat("Finished:", ny, "years |", cc, "% cloud\n\n")}}}

# -----------------------------
# 10. Generate median LST and observation count raster 
# -----------------------------
generate_LST_rasters(
  n_years = c(1, 2, 3, 4, 5, 6, 7, 8),
  cloud_threshold = c(10, 15, 20, 25, 30))

# =====================================================================
# Raster Finalization
# =====================================================================

# -----------------------------
# 11. Create final output directories
# -----------------------------
lst_final_path <- file.path(out_path, "median_LST_final")
obs_final_path <- file.path(out_path, "observation_count_final")

dir.create(lst_final_path, showWarnings = FALSE, recursive = TRUE)
dir.create(obs_final_path, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 12. Load Berlin boundary as terra vector
# -----------------------------
berlin_vect <- vect(berlin)

# -----------------------------
# 13. Function to validate & finalize rasters
# -----------------------------
validate_LST_obs <- function(lst_raster, obs_raster, berlin_vect, scenario_name) {
  
  # Logical masks
  lst_zero <- lst_raster == 0
  obs_zero <- obs_raster == 0
  
  # Count LST = 0 pixels where observations > 0
  mismatch_raster <- lst_zero & !obs_zero
  zero_mismatch <- sum(values(mismatch_raster), na.rm = TRUE)
  if (zero_mismatch > 0) {
    warning(paste0(
      "Scenario ", scenario_name, ": ", zero_mismatch,
      " pixels have LST≈0 but observations > 0."
    ))
  }
  
  # Set LST = NA where observation count = 0
  lst_raster[lst_zero & obs_zero] <- NA
  
  # Ensure CRS match locally
  berlin_vect_proj <- berlin_vect
  if (crs(lst_raster) != crs(berlin_vect_proj)) {
    berlin_vect_proj <- project(berlin_vect_proj, crs(lst_raster))
  }
  
  # Clip and mask rasters to Berlin
  lst_raster <- crop(lst_raster, berlin_vect_proj)
  lst_raster <- mask(lst_raster, berlin_vect_proj)
  obs_raster <- crop(obs_raster, berlin_vect_proj)
  obs_raster <- mask(obs_raster, berlin_vect_proj)
  
  return(list(lst_raster = lst_raster, obs_raster = obs_raster))}

# -----------------------------
# 14. Finalize and save all generated rasters
# -----------------------------
lst_files <- list.files(file.path(out_path, "median_LST_raw"),
                        pattern = "\\.tif$", full.names = TRUE)

if(length(lst_files) == 0) stop("No LST files found!")

for(lst_file in lst_files) {
  
  # Extract scenario name from LST filename
  scenario_name <- gsub("median_LST_raw_|\\.tif", "", basename(lst_file))
  
  # Build corresponding observation count filename
  obs_file <- file.path(
    out_path,
    "observation_count_raw",
    paste0("observation_count_raw_", scenario_name, ".tif")
  )
  
  # Check existence
  if(!file.exists(obs_file)) {
    warning(paste0("Observation raster not found for scenario: ", scenario_name))
    next
  }
  
  # Read rasters
  lst_raster <- rast(lst_file)
  obs_raster <- rast(obs_file)
  
  # Validate & clip
  validated <- validate_LST_obs(lst_raster, obs_raster, berlin_vect, scenario_name)
  
  # Save final rasters
  writeRaster(
    validated$lst_raster,
    file.path(lst_final_path, paste0("median_LST_FINAL_", scenario_name, ".tif")),
    overwrite = TRUE
  )
  
  writeRaster(
    validated$obs_raster,
    file.path(obs_final_path, paste0("observation_count_FINAL_", scenario_name, ".tif")),
    overwrite = TRUE
  )
  
  cat("Finalized and saved:", scenario_name, "\n")
}

# =====================================================================
# 15. Landsat Acquisition-Time Check
# =====================================================================

# Define broadest scenario (8 years, 30% cloud cover)
ny_check <- 8
cc_check <- 30

# Select hot-days for the time check scenario and convert to character 
# (required by GEE)
hot_dates_gee_check <- hot_days_df %>%
  dplyr::filter(
    lubridate::year(date) >= lubridate::year(max(date)) - ny_check + 1,
    lubridate::year(date) <= lubridate::year(max(date))
  ) %>%
  dplyr::pull(date) %>% as.character()

# Recreate the filtered Landsat image collection for check scenario and 
# extract scene center times from image metadata
landsat_hot_check <- landsat$
  filterBounds(berlin_wgs84_ee)$
  filter(ee$Filter$lt("CLOUD_COVER", cc_check))$
  map(maskClouds)$
  filter(ee$Filter$inList("DATE_ACQUIRED", hot_dates_gee_check))

scene_times_utc <- unlist(
  landsat_hot_check$aggregate_array("SCENE_CENTER_TIME")$getInfo())

# Create time check data frame
landsat_time_summary <- data.frame(
  scenario = paste0(ny_check, "y_", cc_check, "cc"),
  n_scenes = landsat_hot_check$size()$getInfo(),
  min_time_utc = min(scene_times_utc),
  max_time_utc = max(scene_times_utc),
  min_time_cest = format(
    as.POSIXct(min(scene_times_utc), format = "%H:%M:%OSZ", tz = "UTC") + lubridate::hours(2),
    "%H:%M:%S"
  ),
  max_time_cest = format(
    as.POSIXct(max(scene_times_utc), format = "%H:%M:%OSZ", tz = "UTC") + lubridate::hours(2),
    "%H:%M:%S"))

#Preview and save table
print(landsat_time_summary)
interim_results_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/interim_results"
dir.create(main_results_path, showWarnings = FALSE, recursive = TRUE)
write_csv(landsat_time_summary,
          file.path(interim_results_path, "table_landsat_aquisition_times.csv"))