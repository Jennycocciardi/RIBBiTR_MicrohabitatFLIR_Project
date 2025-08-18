#Author(s): Jenny Cocciardi
#Date: April 24, 2025
#Purpose: Script to setup analysis for RIBBiTR FLIR Project

#Overall AIM of project: 
# How do microclimates available within and across sites affect disease prevalence and intensity? 

#RIBBiTR FLIR photos are being sorted into usable images as they come in from the field
#We are interested in seeing how thermal microclimates used by frogs relate to disease
# prevalence at field sites. 

#FLIR photos have been copied into the Ohmer Lab --> Undergraduate Research --> AKang folder
#from the Ecophysiology google drive and are up-to-date as of April 2025

#We will relate survey environmental data to mins, maxs, means of thermal images
#For now, we will use the temperature and humidity data recorded at the time of the
#thermal image survey
#In the future, we we can extract more data from the RIBBiTR database.



#1. load packages and functions for project
    # setup.R is a script that loads necessary packages and links to utils script for functions
source("scripts/setup_FLIR.R") 

#2. load metadata
#     Anna and Sadie have compiled a metadata spreadsheet for each study system.