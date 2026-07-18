# Master's Thesis

## Mapping Heat-Related Inequality in Berlin: Linking Socio-Economic Sensitivity and Urban Heat Exposure

Hey there :)

This repository contains the RStudio code and the main datasets that I have used in my Master's thesis. The thesis investigates the spatial relationship between socio-economic heat sensitivity and heat exposure in Berlin using open-source spatial data.

Socio-economic data was derived from the latest German census (Zensus 2022) using the highest spatial resolution available: 100 × 100 m. Landsat 8/9 Collection 2 Level-2 images used to create median land surface temperature rasters were accessed via Google Earth Engine.

The R workflow scripts are provided in the r_scripts folder and are numbered according to the order in which they were applied.

The repository documents the complete analytical workflow used in this study. The R workflow scripts 1–3 and 5 are included for transparency and documentation of the data-preparation process but are not required to reproduce the reported results. The analyses can be reproduced using the datasets supplied in the study_data folder together with Workflows 4, 6, and 7.

Script 1 documents the extraction of dates for hot days in Berlin based on observations from the Berlin-Tempelhof weather station. 

Script 2 documents the preparation of the analytical grid using the Germany-wide Zensus 2022 grid geometry. This source dataset is not included because of its large file size.

The scripts 3 and 5 are provided to document the generation and evaluation of candidate land surface temperature rasters, rather than to enable a direct reproduction. Script 3 requires an individual Google Earth Engine account, local authentication through rgee::ee_Authenticate(), and a Google Cloud project enabled for Earth Engine. The produced candidate land surface temperature and observation-count rasters required for workflow 5 are not included in the repository because of their combined file size.

To reproduce the results reported in the thesis, open 'master_thesis.Rproj' in RStudio, install the required R packages, and run the scripts 4, 6, and 7 in numerical order from the project root. All file paths are specified relative to the project directory.

The required input datasets are provided in the 'study_data' folder. These include the selected final land surface temperature raster, the prepared Berlin 100 m grid, the Berlin study area boundary, and the Zensus 2022 CSV files used in the analysis. The CSV files included in this repository were spatially restricted to the Berlin study area to reduce file size. The original nationwide datasets are available from the official Zensus 2022 data portal.

Feel free to use or adapt the code or workflow if you are interested in similar topics or if it might help you in your own work.

All the best,  
Felix
