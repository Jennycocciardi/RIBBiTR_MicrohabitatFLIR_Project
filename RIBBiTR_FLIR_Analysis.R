# Script with Anna Kang to extract data from RIBBiTR FLIR photos and analyze with disease data

#   Author(s): Jenny Cocciardi
#   Date: April 24, 2025

#   Overall AIM of project: How do microclimates available within and across sites affect disease prevalence and intensity? 

# RIBBiTR FLIR photos are being sorted into usable images as they come in from the field
# We are interested in seeing how thermal microclimates used by frogs relate to disease
#   prevalence at field sites. 

# FLIR photos have been copied into the Ohmer Lab --> Undergraduate Research --> AKang folder
#   from the Ecophysiology google drive and are up-to-date as of April 2025

# We will relate survey environmental data to mins, maxs, means of thermal images
# For now, we will use the temperature and humidity data recorded at the time of the
#   thermal image survey
#   In the future, we we can extract more data from the RIBBiTR database.


# Anna and Sadie have compiled a metadata spreadsheet for each study system.

# 1. load packages and functions for project----
    # setup.R is a script that loads necessary packages and links to utils script for functions
source("scripts/setup_FLIR.R") 



# 2. Set path to where we are keeping the FLIR images----
    # for this example, we will use PA
img_path <- "../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good"



# 3. Extract raw thermal data and metadata----
flir_raw <- batch_extract_custom(in_dir = img_path, 
                                 exiftool_path = exiftool_path)

#check raw data
length(flir_raw$raw_dat)       # number of valid thermal images
head(names(flir_raw$raw_dat))  # preview image names
str(flir_raw$camera_params)    # preview camera calibration constants
str(flir_raw$metadata)         # preview environmental metadata



# 4. Convert raw data to temp data----
# use batch_convert function to convert extracted data to temperature
flir_temp <- batch_convert(
  raw_dat = flir_raw$raw_dat,
  metadata = flir_raw$metadata,
  camera_params = flir_raw$camera_params,
  E = 0.98
)

# Check output
length(flir_temp)
image(flir_temp[[1]], main = names(flir_temp)[1])  # plot example



# 5. Compute temp and summary stats----
flir_stats <- map2(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y)
)

# Summary temperature stats
flir_pstats <- map2_dfr(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y, return_vals = "pstats")$pstats
)

# Hot/cold patch summary
flir_patches <- map2_dfr(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y)$patches %>%
    mutate(id = .y, .before = 1)
)

# Join summary and patch stats on image ID
flir_summary_combined <- left_join(flir_pstats, flir_patches, by = "id")

# View result
glimpse(flir_summary_combined)



# 6. Ex plots with one image----
img_name <- "FLIR1131.jpg"
plot_patches(df = flir_temp[[img_name]],
             patches = TRUE,
             outline_size = 0.5,
             hatching = TRUE)

plot_patches(flir_temp[[img_name]])


# 7. Connect metadata----

library(dplyr)
library(lubridate)
library(ggplot2)

meta_sheet <- Pennsylvania_metadata

# add in id column to match id to temp data
meta_sheet <- meta_sheet %>%
  mutate(id = paste0("FLIR", FLIRIMAGEID, ".jpg"))


# Convert SurveyDate to Date format and extract season
meta_sheet <- meta_sheet %>%
  mutate(
    SurveyDate = as.Date(SurveyDate),
    month = month(SurveyDate)
  ) %>%
  mutate(
    Season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    Hour = as.numeric(substr(StartTime, 1, 2)),
    TimeOfDay = case_when(
      Hour >= 6 & Hour < 18 ~ "Day",
      TRUE ~ "Night"
    )
  )

flir_summary_full <- flir_summary_combined %>%
  left_join(meta_sheet, by = "id") %>%
  mutate(
    # Clean up if needed
    Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")),
    TimeOfDay = factor(TimeOfDay, levels = c("Day", "Night"))
  )

seasonal_summary <- flir_summary_full %>%
  group_by(Site, Season) %>%
  summarise(
    n_images = n(),
    mean_median_temp = mean(median, na.rm = TRUE),
    mean_hot_pixels = mean(hot_pixels, na.rm = TRUE),
    mean_cold_pixels = mean(cold_pixels, na.rm = TRUE),
    mean_SHDI = mean(SHDI, na.rm = TRUE),
    .groups = "drop"
  )

# Filter out rows where Site or Season is NA
flir_summary_full_clean <- flir_summary_combined %>%
  left_join(meta_sheet, by = "id") %>%
  filter(!is.na(Site), !is.na(Season))  %>%
  filter(median > 0)


ggplot(flir_summary_full_clean, aes(x = Site, y = median, fill = Site)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  labs(
    title = "Distribution of Median Temperatures Across Sites",
    x = "Site",
    y = "Median Temperature (°C)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2", guide = "none")  # No legend for fill


ggplot(flir_summary_full_clean, aes(x = Site, y = median, fill = Season)) +
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = Season),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1, alpha = 0.6) +
  labs(
    title = "Median Temperature Distribution by Site and Season",
    x = "Site",
    y = "Median Temperature (°C)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

meta_sheet %>%
  summarize(
    start_date = min(SurveyDate, na.rm = TRUE),
    end_date   = max(SurveyDate, na.rm = TRUE),
    years      = paste0(sort(unique(year(SurveyDate))), collapse = ", "),
    months     = paste0(sort(unique(month(SurveyDate, label = TRUE))), collapse = ", ")
  )


# 8. Compare hobodata to flir data----

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)


# 1. Prepare logger data (PA only)
pa_logger_data <- data %>%
  filter(StudyArea == "PA") %>%
  mutate(
    DateTime_clean = str_remove(DateTime, " [A-Z]+$"),
    DateTime = ymd_hms(DateTime_clean, tz = "America/New_York"),
    LoggerDate = as.Date(DateTime),
    LoggerTime = format(DateTime, format = "%H:%M:%S"),
    Site = recode(Site,
                  "Admin" = "Admin Pond",
                  "Phelps" = "Phelps Pond",
                  "RV" = "RV Pond",
                  "Tuttle" = "Tuttle Pond",
                  "TW" = "Tyron-Weber",
                  "Vorisek" = "Vorisek Pond",
                  "Wood" = "Wood Lab Pond")
  ) %>%
  filter(!is.na(Temperature))

# 2. Prepare FLIR data
flir_summary_full_clean <- flir_summary_full %>%
  filter(!is.na(median)) %>%
  mutate(
    SurveyDate = as.Date(SurveyDate),
    StartTime_clean = ifelse(
      is.na(StartTime) | StartTime == "" | StartTime == "NA",
      "00:00:00",  # Replace NA or bad StartTimes with midnight
      StartTime
    ),
    SurveyDateTime_str = paste(SurveyDate, StartTime_clean),
    SurveyDateTime = ymd_hms(SurveyDateTime_str, tz = "America/New_York"),
    Site = recode(Site, "Tryon-Weber" = "Tyron-Weber") # Fix typo
  )


# 3. Create a datetime column for FLIR
flir_summary_full_clean <- flir_summary_full %>%
  filter(!is.na(median)) %>%
  mutate(
    SurveyDate = as.Date(SurveyDate),
    StartTime_clean = ifelse(
      is.na(StartTime) | StartTime == "" | StartTime == "NA",
      "00:00:00",   # Replace missing StartTimes with midnight
      StartTime
    ),
    SurveyDateTime = ymd_hms(paste(SurveyDate, StartTime_clean), tz = "America/New_York"),
    Site = recode(Site, "Tryon-Weber" = "Tyron-Weber") # Fix typo!
  )

install.packages("fuzzyjoin")


# 4. Join logger data to flir data by site + **near time**
joined_near <- fuzzyjoin::difference_inner_join(
  pa_logger_data,
  flir_summary_full_clean,
  by = c("Site" = "Site", "DateTime" = "SurveyDateTime"),
  max_dist = as.difftime(3, units = "hours")
)

# 5. Summarize logger data per FLIR photo
logger_summary_near <- joined_near %>%
  group_by(id, Site, SurveyDate) %>%
  summarise(Logger_Mean_Temp = mean(Temperature, na.rm = TRUE), .groups = "drop")

# 6. Rebuild FLIR summary
flir_summary_simple <- flir_summary_full_clean %>%
  select(id, Site, SurveyDate, FLIR_Median_Temp = median)

# 7. Merge logger + FLIR summary
comparison_data <- left_join(logger_summary_near, flir_summary_simple, by = c("id", "Site", "SurveyDate"))

# 8. Pivot longer
comparison_long <- comparison_data %>%
  pivot_longer(cols = c(Logger_Mean_Temp, FLIR_Median_Temp),
               names_to = "Source",
               values_to = "Temperature_C") %>%
  filter(!is.na(Temperature_C))  # Remove missing temps

# 9. Violin plot----
ggplot(comparison_long, aes(x = Site, y = Temperature_C, fill = Source)) +
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 1, alpha = 0.7) +
  theme_minimal() +
  labs(x = "Site", y = "Temperature (°C)", fill = "Measurement Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

