# Master's Thesis

## Mapping Heat-Related Inequality in Berlin: Linking Socio-Economic Sensitivity and Urban Heat Exposure

Hey there :)

This repository contains the RStudio code and the datasets that I have used in my Master's thesis. The thesis investigates the spatial relationship between socio-economic heat sensitivity and heat exposure in Berlin using open-source spatial data.

Socio-economic data was derived from the latest German census (Zensus 2022) using the highest spatial resolution available: 100 × 100 m. Landsat 8/9 Collection 2 Level-2 images used to create median land surface temperature rasters were accessed via Google Earth Engine.

The R-workflow scripts are provided in the folder 'r_scripts' and are numbered according to the order in which they were applied. 

Script 1 uses daily observations from the Berlin Tempelhof weather station to identify hot days in Berlin. The corresponding data are provided in the 'supplementary_data' folder.

Script 2 documents the preparation of the analytical grid using the Germany-wide Zensus 2022 grid geometry. This source dataset is not included in the repository because of its large file size. The shapefile used to extract Berlin's administrative boundary is available in the 'supplementary_data' folder. The resulting prepared Berlin grid is provided in the 'study_data' folder.

Scripts 3 and 5 document the generation and evaluation of candidate LST rasters. Script 3 requires an individual Google Earth Engine account, local authentication via 'rgee::ee_Authenticate()', and a Google Cloud project enabled for Earth Engine. It is included to document the LST-generation process rather than to enable direct reproduction. The candidate LST and observation-count rasters are not included because of their combined file size; Script 5 therefore serves only to document their evaluation.

To reproduce the results reported in this thesis, use the data provided in the 'study_data' folder and run Scripts 4, 6, and 7 in numerical order. The supplied data include the selected final LST raster, the prepared Berlin 100 m grid, and the Zensus 2022 CSV files required for the analysis.

Feel free to use or adapt the code or workflow if you are interested in similar topics or if it might help you in your own work.

All the best,  
Felix
