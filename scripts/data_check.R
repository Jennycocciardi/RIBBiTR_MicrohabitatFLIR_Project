#Author(s): Jenny Cocciardi
#Date: April 24, 2025
#Purpose: check metadata and images to ID missing information

library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(stringr)


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
  mutate(FLIR_ID_Date = paste0(FLIR_ID, "_", format(date, "%Y-%m")))


#Now let's check the photos that we have!

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


#Compare missing FLIR_ID_Date columsn between datasheets to see which photos we are missing
missing_FLIR_rows <- PA_FLIR_data %>%
  anti_join(PA_data_AK, by = "FLIR_ID_Date") %>%
  select(FLIR_ID_Date, photo_type, date, site)

print(missing_FLIR_rows)


#IDs that we have...
matched_FLIR_rows <- PA_FLIR_data %>%
  semi_join(PA_data_AK, by = "FLIR_ID_Date") %>%
  select(FLIR_ID_Date, photo_type, date)

print(matched_FLIR_rows)


# Count number of unique year-months per FLIR_ID
duplicate_FLIRs <- PA_FLIR_data %>%
  mutate(year_month = format(date, "%Y")) %>%
  distinct(FLIR_ID, year_month) %>%
  count(FLIR_ID) %>%
  filter(n > 1)

print(duplicate_FLIRs)

#lets use that info to fill out our metadata to compare....

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

missing_FLIR_rows <- PA_FLIR_data %>%
  anti_join(PA_data_AK_updated, by = "FLIR_ID_Date") %>%
  select(FLIR_ID_Date, photo_type, date, site)

print(missing_FLIR_rows)
write.csv(missing_FLIR_rows, "/outputs/missing_FLIR_rows.csv", row.names = FALSE)

