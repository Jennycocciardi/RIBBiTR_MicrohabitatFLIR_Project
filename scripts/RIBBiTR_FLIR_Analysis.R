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
Pa_metadata_file_path <- "/Users/jennycocciardi/Library/CloudStorage/GoogleDrive-jenny.cocciardi@gmail.com/My Drive/Ecophysiology/Thermal Images_FLIR photos/Pennsylvania/PA_FLIRMetadata_2023_2025.xlsx"
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
      select(record_id, latitude, longitude, time,
             distance_from_object_m,
             thermal_photo_id_wholesite) %>%
      rename(FLIR_ID = thermal_photo_id_wholesite)
    
  } else if (photo_type == "transect") {
    data_clean <- data %>%
      select(record_id, latitude, longitude, meter_marker,
             distance_from_object_m, time, direction,
             thermal_photo_id_transect) %>%
      rename(FLIR_ID = thermal_photo_id_transect)
    
  } else if (photo_type == "microhabitat") {
    data_clean <- data %>%
      select(record_id, latitude, longitude, marker_number,
             microhabitat_type,
             distance_from_object_m, time,
             thermal_photo_id_microhabitat) %>%
      rename(FLIR_ID = thermal_photo_id_microhabitat)
  }
  
  #join with metadata and photo_type
  data_clean %>%
    left_join(survey_data, by = "record_id") %>%
    mutate(photo_type = photo_type)
})

#join FLIR_ID column with date to get unique FLIR images
#convert date column to Date format
PA_FLIR_data <- PA_FLIR_data %>%
  mutate(date = as.Date(date))

PA_FLIR_data <- PA_FLIR_data %>%
  mutate(FLIR_ID_Date = paste0(FLIR_ID, "_", format(date, "%Y-%m"))) %>%
  mutate(FLIR_ID_Year = paste0(FLIR_ID, "_", format(date, "%Y")))

#add in determined_usage column from PA_FLIR_AK datasheet based on FLIR_ID_Year attribute
#read in our metadata sheet
PA_data_AK <- read_excel("~/Google Drive/My Drive/Ohmer Lab/Undergraduate research projects/AKang_FLIR_Microhabitat_Project/FLIR_Images/Pennsylvania/Pennsylvania_metadata (1).xlsx")

#our date column is in 2 different formats...
library(lubridate)

PA_data_AK <- PA_data_AK %>%
  mutate(surveydate = case_when(
    grepl("^\\d{5,}\\.0$", surveydate) ~ as.character(as.Date(as.numeric(surveydate), origin = "1899-12-30")),
    TRUE ~ as.character(as.Date(surveydate))
  )) %>%
  mutate(surveydate = as.Date(surveydate))

PA_data_AK <- PA_data_AK %>%
  mutate(FLIRIMAGEID = str_pad(FLIRIMAGEID, width = 4, pad = "0"),
         FLIR_ID_Date = paste0("FLIR",FLIRIMAGEID, "_", format(surveydate, "%Y-%m")))

# Fill in the determined usage column with dates from PA datasheet
# 1. Find non-duplicated FLIR_IDs in PA_FLIR_data
non_duplicated_FLIRs <- PA_FLIR_data %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  distinct(FLIR_ID, year_month) %>%
  count(FLIR_ID) %>%
  filter(n == 1) %>%
  pull(FLIR_ID)

# 2. Create FLIR ID column to match
PA_data_AK <- PA_data_AK %>%
  mutate(FLIR_ID = paste0("FLIR", str_pad(FLIRIMAGEID, 4, pad = "0")))

# 3. Create a lookup table of FLIR_ID to date (only for non-duplicates)
date_lookup <- PA_FLIR_data %>%
  filter(FLIR_ID %in% non_duplicated_FLIRs) %>%
  select(FLIR_ID, date)

# 4. Join and conditionally update surveydate in PA_data_AK
PA_data_AK_updated <- PA_data_AK %>%
  left_join(date_lookup, by = "FLIR_ID") %>%
  mutate(
    surveydate = if_else(is.na(surveydate), date, surveydate)) %>%
  select(-date)  # Drop the temporary date column

# Now re-do FLIR_ID_Date column
PA_data_AK_updated <- PA_data_AK_updated %>%
  mutate(FLIRIMAGEID = str_pad(FLIRIMAGEID, width = 4, pad = "0"),
         FLIR_ID_Date = paste0("FLIR",FLIRIMAGEID, "_", format(surveydate, "%Y-%m"))) %>%
  mutate(FLIRIMAGEID = str_pad(FLIRIMAGEID, width = 4, pad = "0"),
         FLIR_ID_Year = paste0("FLIR",FLIRIMAGEID, "_", format(surveydate, "%Y")))


# join our determined usage to the metadata sheet

PA_FLIR_data_updated <- PA_FLIR_data %>%
  left_join(
    PA_data_AK_updated %>% select(FLIR_ID_Year, 'determined usage', water),
    by = "FLIR_ID_Year"
  )

#reorganize columns
PA_FLIR_data_updated <- PA_FLIR_data_updated %>%
  rename(determined_usage = `determined usage`) %>%
  select(FLIR_ID_Year, FLIR_ID, site, date, time, survey_time, photo_type, determined_usage, water,
    air_temperature_c, humidity, microhabitat_type, distance_from_object_m, meter_marker,
    marker_number, direction, everything())

write.csv(PA_FLIR_data_updated, "outputs/PA_data_20-08-2025.csv", row.names = FALSE)

PA_FLIR_data %>%
  count(FLIR_ID_Year) %>%
  filter(n > 1) 

PA_data_AK_updated %>%
  count(FLIR_ID_Year) %>%
  filter(n > 1)


#check photos in our new 'photos_renamed' folder and compare to the datasheet to see what we are missing

target_dir <- "~/Google Drive/My Drive/Ohmer Lab/Undergraduate research projects/AKang_FLIR_Microhabitat_Project/FLIR_Images/Pennsylvania/photos_renamed"       

# List all filenames (without extensions)
image_files <- dir_ls(target_dir, regexp = "\\.(jpg|jpeg|png|tif)$", recurse = FALSE)
image_names <- file_path_sans_ext(path_file(image_files))  # e.g., FLIR0012_2024

# Compare to FLIR_ID_Year column
missing_images <- PA_FLIR_data %>%
  filter(!(FLIR_ID_Year %in% image_names))

# View or export missing entries
print(missing_images)
write.csv(missing_images, "outputs/missing_FLIR_images.csv", row.names = FALSE)

