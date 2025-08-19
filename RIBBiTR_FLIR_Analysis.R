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



#1. load packages and functions for project----
    # setup.R is a script that loads necessary packages and links to utils script for functions
source("scripts/setup_FLIR.R") 


#2. load metadata----

#PA
Pa_metadata_file_path <- "/Users/jennycocciardi/Library/CloudStorage/GoogleDrive-jenny.cocciardi@gmail.com/My Drive/Ecophysiology/Thermal Images_FLIR photos/Pennsylvania/PA_FLIRMetadata_2023_2024.xlsx"
sheet_names <- excel_sheets(Pa_metadata_file_path)

#Identify the survey data sheet in the PA_metadata and clean
survey_data <- read_excel(Pa_metadata_file_path, sheet = "7_penn_thermalimaging") %>%
  janitor::clean_names() %>%
  select(`record_id`, `latitude`, `longitude`, observer, date, site,
         survey_time, air_temperature_c, humidity)

#ID other sheets in the PA metadata and clean
sheet_info <- tibble::tibble(
  sheet_name = c("7_penn_thermalimaging_whole_sit",
                 "7_penn_thermalimaging_transect_",
                 "7_penn_thermalimaging_microhabi"),
  photo_type = c("whole_site", "transect", "microhabitat"))


#Clean each sheet, filter, join, rename, label, and stack on top of one another
PA_FLIR_data <- pmap_dfr(sheet_info, function(sheet_name, photo_type) {
  data <- read_excel(path = Pa_metadata_file_path, sheet = sheet_name) %>%
    janitor::clean_names()
  
  # Select + rename based on photo_type
  # Select + rename based on photo_type
  if (photo_type == "whole_site") {
    data_clean <- data %>%
      select(record_id, latitude, longitude, time_wholesite,
             distance_from_object_m_wholesite,
             thermal_photo_id_wholesite) %>%
      rename(FLIR_ID = thermal_photo_id_wholesite)
    
  } else if (photo_type == "transect") {
    data_clean <- data %>%
      select(record_id, latitude, longitude, meter_marker,
             distance_from_object_m_transect,
             thermal_photo_id_transect) %>%
      rename(FLIR_ID = thermal_photo_id_transect)
    
  } else if (photo_type == "microhabitat") {
    data_clean <- data %>%
      select(record_id, latitude, longitude, marker_number,
             microhabitat_type,
             distance_from_object_m_microhabitat,
             thermal_photo_id_microhabitat) %>%
      rename(FLIR_ID = thermal_photo_id_microhabitat)
  }
  
  #join with metadata and photo_type
  data_clean %>%
    left_join(survey_data, by = "record_id") %>%
    mutate(photo_type = photo_type)
})

#join FLIR_ID column with date to get unqie FLIR images
#convert date column to Date format
PA_FLIR_data <- PA_FLIR_data %>%
  mutate(date = as.Date(date))

PA_FLIR_data <- PA_FLIR_data %>%
  mutate(FLIR_ID_Date = paste0(FLIR_ID, "_", format(date, "%Y-%m"))) %>%
  mutate(FLIR_ID_Year = paste0(FLIR_ID, "_", format(date, "%Y")))


#3. Append year to FLIR Image to match datasheet ID----
# We will use the photos directly from the Ecophysiology folder for this

library(fs)
library(stringr)

# Set paths
source_dir <- "/Users/jennycocciardi/Library/CloudStorage/GoogleDrive-jenny.cocciardi@gmail.com/My Drive/Ecophysiology/Thermal Images_FLIR photos/Pennsylvania" 
                #folder containing photos
target_dir <- "~/Google Drive/My Drive/Ohmer Lab/Undergraduate research projects/AKang_FLIR_Microhabitat_Project/FLIR_Images/Pennsylvania/photos_renamed"       
                #where renamed images god

# List all subfolders, excluding repeat downloads
exclude_folder <- "/Users/jennycocciardi/Library/CloudStorage/GoogleDrive-jenny.cocciardi@gmail.com/My Drive/Ecophysiology/Thermal Images_FLIR photos/Pennsylvania/REPEAT_DownloadedImages_4Jun2025"
folders <- dir_ls(source_dir, type = "directory")
folders <- folders[folders != exclude_folder]        

# Loop through folders
for (folder in folders) {
  folder_name <- path_file(folder)
  year <- str_extract(folder_name, "\\d{4}")
  
  images <- dir_ls(folder, regexp = "\\.(jpg|jpeg|png|tif)$", recurse = FALSE)
  
  for (img in images) {
    img_name <- path_file(img)
    id <- tools::file_path_sans_ext(img_name)
    new_name <- paste0(id, "_", year, ".", path_ext(img_name))
    
    file_copy(img, path(target_dir, new_name), overwrite = TRUE)
  }
}


