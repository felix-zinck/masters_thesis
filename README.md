# Master's Thesis

## Mapping Heat-Related Inequality in Berlin: Linking Socio-Economic Sensitivity and Urban Heat Exposure

Hey there :)

This repository contains the RStudio code and the datasets that I have used in my Master's thesis. The thesis investigates the spatial relationship between socio-economic heat sensitivity and heat exposure in Berlin using open-source spatial data.

Socio-economic data was derived from the latest German census (Zensus 2022) using the highest spatial resolution available: 100 × 100 m. Landsat 8/9 Collection 2 Level-2 images used to create median land surface temperature rasters were accessed via Google Earth Engine.

Feel free to use or adapt this code if you are interested in similar topics or if it might help you in your own work.

Regarding the repository's contents:

The R-workflow scripts are provided in the folder 'r_scripts' and are numbered according to the order in which they were applied. 

Script 1 uses daily observations from the Berlin Tempelhof weather station to derive dates for hot days in Berlin. The corresponding datasets are stored in the folder 'meteorological_data'. 

Script 2 documents how the analytical grid for the study was created by utilizing the Germany-wide census grid dataset, which is not provided seperately here due to its large file size. The shapefile used to extract the administartive boundaries for Berlin is stored in the folder 'supplementary_data'. 

The scripts 3 and 5 document how the candidate LST rasters were generated and evaluated. Since script 2 requires an individual Google Earth Engine account, local authentication via rgee::ee_Authenticate(), and a Google Cloud project enabled for Earth Engine is provided to document the generation of the LST rasters, rather than to enable a direct reproduction. Because of the combined file size of all generated rasters, they are not included in this repository. 

To reproduce the results reported in this thesis, run the scrips 4, 6, and 7 and use the data provided in the folder 'study_data'. The data includes the selected LST scenario and the used census CSV files. 

All the best,  
Felix
