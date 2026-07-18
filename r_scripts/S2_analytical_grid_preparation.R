# =====================================================================
# Script 2 – Analytical Grid Preparation
# =====================================================================

# -----------------------------
# 0. Load required packages
# -----------------------------
library(sf)
library(dplyr)

# -----------------------------
# 1. Load Berlin boundary and transform to EPSG:3035
#    (matches the CRS of the Zensus 2022 grid data)
# -----------------------------

# Path to Berlin administrative boundary shapefile
# (can be extracted from the Germany-wide administrative boundary dataset)
berlin_file <- "path/to/berlin.shp"

berlin <- st_read(
  berlin_file,
  quiet = TRUE
) %>%
  st_union() %>%
  st_transform(3035)

# -----------------------------
# 2. Import Zensus 2022 100 m grid geometry and extract grid cells fully within
#    the Berlin boundary
# -----------------------------

# Path to Germany-wide Zensus 2022 grid geometry
grid_file <- "path/to/zensus_grid_geometry.gpkg"
grid_layer <- "Zensus2022_100mGitter"

# Load grid file
grid <- st_read(
  grid_file,
  layer = grid_layer,
  quiet = TRUE
)

# Keep only grid ID, inhabitants, and average age attributes
# grid_id: used to merge with Zensus 2022 CSV files later
# population: used to check the number of grid cells with few inhabitants
# avg_age: used to check attribute consistency with Zensus CSV files later
grid <- grid[, c(
  "GITTER_ID_100m",
  "Einwohner",
  "Durchschnittsalter"
)]

# Rename attributes
names(grid)[names(grid) == "GITTER_ID_100m"] <- "grid_id"
names(grid)[names(grid) == "Einwohner"] <- "population"
names(grid)[names(grid) == "Durchschnittsalter"] <- "avg_age"

# Transform to EPSG:3035
grid <- st_transform(grid, 3035)

# Confirm reprojection and CRS consistency with Berlin boundary
st_crs(grid)
st_crs(berlin)

# Extract grid cells that are fully inside the Berlin boundary
berlin_grid <- grid[
  st_within(grid, berlin, sparse = FALSE),
]

# Check number of grid cells inside Berlin (40,280)
nrow(berlin_grid)

# Check minimum number of inhabitants (3)
min(berlin_grid$population, na.rm = TRUE)

# Save prepared Berlin grid
output_grid_berlin <- file.path(
  "study_data",
  "berlin_100m_grid.gpkg"
)

dir.create(
  dirname(output_grid_berlin),
  showWarnings = FALSE,
  recursive = TRUE
)

st_write(
  berlin_grid,
  output_grid_berlin,
  delete_layer = TRUE,
  quiet = TRUE
)