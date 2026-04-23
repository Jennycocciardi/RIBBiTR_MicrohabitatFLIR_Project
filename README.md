# How do microclimates available within and across RIBBiTR sites affect disease prevalence and intensity? 

## Introduction
### Project information:
RIBBiTR FLIR photos are being sorted into usable images as they come in from the field.
We are interested in seeing how thermal microhabitats used by frogs relate to disease prevalence at field sites. 

FLIR photos have been copied into the *'Ohmer Lab --> Undergraduate Research --> AKang folder'*
from the Ecophysiology google drive and are up-to-date as of April 2025.

We will relate environmental data to mins, maxs, means of temperatures extracted from thermal images.
For now, we will use the temperature and humidity data recorded at the time of the thermal image survey.
In the future, we we can extract more data from the RIBBiTR database.

### Information about folders and files within this repository:
- *scripts*: folder containing all scripts mentioned below
- *data*: folder containing data used and produced as part of this project
- *outputs*: folder containing all figures and tables produced through analyses
- *RIBBiTR_FLIR_Analysis.Rproj*: RStudio project file. This repository is structured as an RStudio Project. Opening the .Rproj file ensures that all scripts run with correct relative paths.

## Step 1: FLIR photo image processing and data extraction

This step is to extract temperatures from each photo.

**1a. Run the `setup_FLIR.R` script** to setup our environment. 

```source("scripts/setup_FLIR.R")```

This script installs and loads required packages and automatically runs the `utils_FLIR.R` script. The `utils_FLIR.R` script loads functions needed to extract and analyze temperatures from photos. This is needed because the R package `ThermStats` has not been kept up to date and so we will write similiar functions to do what the package was doing before...but without using outdated libraries that `ThermStats` is dependant on.

*Please note*: Thermstats and these functions require the external software ExifTool. Information and istallation instructions for ExifTool can be found here: http://www.sno.phy.queensu.ca/~phil/exiftool/install.html

**1b. Extract pixel data and compute temperature statistics using the `extract_temp_FLIR.r` script**
The `extract_temp_FLIR.r` script uses the custom functions loaded from the `utils_FLIR.R` script to extract temperature data from each FLIR photo and compute relevant summary statistics. It then merges this data to the processed metadata for each study system and filters out FLIR photos that were determined to be not usable. 

This script produces one datasheet per study system which is exported to the 'data' folder and should be used for further processesing and data analysis:

-`PA_flir-temp-data.csv` 

These will be given to the RIBBiTR database manager for uploading to the RIBBiTR database.

## Other/extra code
- *RIBBiTR_FLIR_Analysis.R*: code used to append necessary metadata together to create one dataset we can use for PA study system
- *rename_reorg_images.R*: script for PA study system to append year to end of image ID to create unique image IDs


