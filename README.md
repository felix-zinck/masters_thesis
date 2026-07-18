# Master's Thesis

## Mapping Heat-Related Inequality in Berlin: Linking Socio-Economic Sensitivity and Urban Heat Exposure

Hey there :)

This repository contains the RStudio code and the datasets that I have used in my Master's thesis. The thesis investigates the spatial relationship between socio-economic heat sensitivity and heat exposure in Berlin using open-source spatial data.

Socio-economic data was derived from the latest German census (Zensus 2022) using the highest spatial resolution available: 100 × 100 m. Landsat 8/9 Collection 2 Level-2 images used to create median land surface temperature rasters were accessed via Google Earth Engine.

The R workflow scripts are provided in the r_scripts folder and are numbered according to the order in which they were applied.

The repository documents the complete analytical workflow used in this study. Workflows 1–3 and 5 are included for transparency and documentation of the data-preparation process but are not required to reproduce the reported results. The analyses can be reproduced using the datasets supplied in the study_data folder together with Workflows 4, 6, and 7.

Workflow 1 uses daily observations from the Berlin-Tempelhof weather station to identify hot days in Berlin. The original meteorological input files are not included in the repository.

Workflow 2 documents the preparation of the analytical grid using the Germany-wide Zensus 2022 grid geometry. This source dataset is not included because of its large file size. The shapefile used to extract Berlin’s administrative boundary is provided in the supplementary_data folder, while the resulting prepared Berlin grid is included in the study_data folder.

Workflows 3 and 5 document the generation and evaluation of candidate land surface temperature rasters. Workflow 3 requires an individual Google Earth Engine account, local authentication through rgee::ee_Authenticate(), and a Google Cloud project enabled for Earth Engine. It is therefore included to document the raster-generation process rather than to enable direct reproduction. The candidate land surface temperature and observation-count rasters are not included because of their combined file size; Workflow 5 consequently serves only to document their evaluation.

To reproduce the results reported in the thesis, use the data provided in the study_data folder and run Workflows 4, 6, and 7 in numerical order. The supplied datasets include the selected final land surface temperature raster, the prepared Berlin 100 m grid, and the Zensus 2022 CSV files required for the analysis.

Feel free to use or adapt the code or workflow if you are interested in similar topics or if it might help you in your own work.

All the best,  
Felix
