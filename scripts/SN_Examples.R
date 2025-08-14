# Script for SN - FLIR image examples
# Author: Jenny Cocciardi
# Date: 9 Jun, 2025


# 1. load packages and functions for project----
# setup.R is a script that loads necessary packages and links to utils script for functions
source("scripts/setup_FLIR.R") 



# 2. Set path to where we are keeping the FLIR images----

# Anna has organized SN into 2 folders - one for 2023, one for 2024
img_path <- "../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good"
img_path_2 <- "../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)"


# 3. Extract raw thermal data and metadata----
flir_raw <- batch_extract_custom(in_dir = img_path, 
                                 exiftool_path = exiftool_path)
flir_raw_2 <- batch_extract_custom(in_dir = img_path_2, 
                                 exiftool_path = exiftool_path)

#check raw data
length(flir_raw$raw_dat)       # number of valid thermal images
head(names(flir_raw$raw_dat))  # preview image names
str(flir_raw$camera_params)    # preview camera calibration constants
str(flir_raw$metadata)         # preview environmental metadata

length(flir_raw_2$raw_dat)       # number of valid thermal images
head(names(flir_raw_2$raw_dat))  # preview image names
str(flir_raw_2$camera_params)    # preview camera calibration constants
str(flir_raw_2$metadata)         # preview environmental metadata


# 4. Convert raw data to temp data----
# use batch_convert function to convert extracted data to temperature
flir_temp <- batch_convert(
  raw_dat = flir_raw$raw_dat,
  metadata = flir_raw$metadata,
  camera_params = flir_raw$camera_params,
  E = 0.98
)

flir_temp_2 <- batch_convert(
  raw_dat = flir_raw_2$raw_dat,
  metadata = flir_raw_2$metadata,
  camera_params = flir_raw_2$camera_params,
  E = 0.98
)

# Check output
length(flir_temp)
image(flir_temp[[1]], main = names(flir_temp)[1])  # plot example

length(flir_temp_2)
image(flir_temp_2[[1]], main = names(flir_temp_2)[1])  # plot example


# 5. Compute temp and summary stats----
flir_stats <- map2(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y)
)

flir_stats_2 <- map2(
  .x = flir_temp_2,
  .y = names(flir_temp_2),
  .f = ~ get_stats(img = .x, id = .y)
)

# Summary temperature stats
flir_pstats <- map2_dfr(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y, return_vals = "pstats")$pstats
)

flir_pstats_2 <- map2_dfr(
  .x = flir_temp_2,
  .y = names(flir_temp_2),
  .f = ~ get_stats(img = .x, id = .y, return_vals = "pstats")$pstats
)

# Hot/cold patch summary
flir_patches <- map2_dfr(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y)$patches %>%
    mutate(id = .y, .before = 1)
)

flir_patches_2 <- map2_dfr(
  .x = flir_temp_2,
  .y = names(flir_temp_2),
  .f = ~ get_stats(img = .x, id = .y)$patches %>%
    mutate(id = .y, .before = 1)
)

# Join summary and patch stats on image ID
flir_summary_combined <- left_join(flir_pstats, flir_patches, by = "id")
flir_summary_combined_2 <- left_join(flir_pstats_2, flir_patches_2, by = "id")


# View result
glimpse(flir_summary_combined)
glimpse(flir_summary_combined_2)

# Combine 2 datasets for 2023 and 2024
flir_summary_all <- bind_rows(flir_summary_combined, flir_summary_combined_2)
nrow(flir_summary_all)
glimpse(flir_summary_all)


# 6. Ex plots with one image----
img_name <- "FLIR2095.jpg"
plot_patches(df = flir_temp[[img_name]],
             patches = TRUE,
             outline_size = 0.5,
             hatching = TRUE)

plot_patches(flir_temp[[img_name]])


# 7. Connect metadata----

library(dplyr)
library(lubridate)
library(ggplot2)

meta_sheet <- Sierra_Nevada_metadata

# add in id column to match id to temp data
meta_sheet <- meta_sheet %>%
  mutate(id = paste0("FLIR", FLIRIMAGEID, ".jpg"))


# Convert SurveyDate to Date format and extract year
meta_sheet <- meta_sheet %>%
  mutate(
    SurveyDate = as.Date(SurveyDate),
    Year = year(SurveyDate),
    Hour = as.numeric(substr(StartTime, 1, 2)),
    TimeOfDay = case_when(
      Hour >= 6 & Hour < 18 ~ "Day",
      TRUE ~ "Night"
    )
  )

# Merge and inspect missing joins
flir_summary_full <- flir_summary_combined %>%
  left_join(meta_sheet, by = "id") %>%
  mutate(
    TimeOfDay = factor(TimeOfDay, levels = c("Day", "Night"))
  )

# Clean dataset: remove rows with missing site or year and filter by median
flir_summary_full_clean <- flir_summary_full %>%
  filter(!is.na(Site), !is.na(Year)) %>%
  filter(median > 0)

# Group summary by site and year
yearly_summary <- flir_summary_full_clean %>%
  group_by(Site, Year) %>%
  summarise(
    n_images = n(),
    mean_median_temp = mean(median, na.rm = TRUE),
    mean_hot_pixels = mean(hot_pixels, na.rm = TRUE),
    mean_cold_pixels = mean(cold_pixels, na.rm = TRUE),
    mean_SHDI = mean(SHDI, na.rm = TRUE),
    .groups = "drop"
  )

# Violin plot
SNbyyear <- ggplot(flir_summary_full_clean, aes(x = Site, y = median)) +
  geom_violin(aes(fill = factor(Year)), trim = TRUE) +
  geom_jitter(aes(color = factor(Year)), width = 0.2, size = 1, alpha = 0.5) +
  labs(
    title = "Distribution of Median Temperatures Across Sites",
    x = "Site",
    y = "Median Temperature (°C)",
    fill = "Year",
    color = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=9),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")


pdf("output/SN_example_by_year.pdf", width=6, height=4)
plot(SNbyyear)
dev.off()

unique(flir_summary_full_clean$Site)
flir_summary_full_clean <- flir_summary_full_clean %>%
  mutate(Site = recode(Site,
                       "goddard" = "Goddard",
                       "sleet" = "Sleet Lake",
                       "unicorn" = "Unicorn",
                       "conness" = "Conness",
                       "marmot" = "Marmot",
                       "54188" = "Trapdoor",
                       "72808" = "Unicorn"
  ))


high_temp_images <- flir_summary_full_clean %>%
  filter(median > 30) %>%
  select(FLIRIMAGEID, id, Site, SurveyDate, median)

print(high_temp_images)

cold_temp_images <- flir_summary_full_clean %>%
  filter(median < 5) %>%
  select(FLIRIMAGEID, id, Site, SurveyDate, median)

print(cold_temp_images)




meta_sheet %>%
  filter(Site %in% c("50183", "72996", "10109", "70459", "50837", "50731")) %>%
  select(Site, id) %>%
  mutate(match_in_summary = id %in% flir_summary_combined$id)
setdiff(meta_sheet$id, flir_summary_combined$id)
meta_sheet %>%
  count(Site, name = "n_images") %>%
  arrange(n_images)


good <- batch_extract_custom("../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)/FLIR3459.jpg")
bad  <- batch_extract_custom("../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)/FLIR2859.jpg")

View(good$metadata)
View(bad$metadata)


file.exists("../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)/FLIR3459.jpg")
library(fs)
dir_ls(path = "../FLIR_Images", recurse = TRUE, regexp = "FLIR3459.jpg")
results <- batch_extract_debug("../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)", 
                               exiftool_path = "/usr/local/bin/exiftool")

meta <- exiftoolr::exif_read("../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)/FLIR2859.jpg")
meta[c("PlanckR1", "PlanckB", "PlanckF", "PlanckO", "PlanckR2")]



results <- batch_extract_custom_debug("../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)", 
                                      exiftool_path = "/usr/local/bin/")
str(results)

`%||%` <- function(a, b) if (!is.null(a)) a else b

batch_extract_custom_debug <- function(in_dir, exiftool_path, write_results = FALSE) {
  files <- list.files(in_dir, pattern = "\\.jpg$", full.names = TRUE)
  
  raw_dat_list <- list()
  camera_params_list <- list()
  metadata_list <- list()
  
  for (file in files) {
    message("Processing: ", file)
    
    # Step 1: Read thermal matrix
    raw_matrix <- tryCatch({
      Thermimage::readflirJPG(file, exiftoolpath = exiftool_path)
    }, error = function(e) {
      warning("Failed to read thermal matrix for ", basename(file), ": ", e$message)
      return(NULL)
    })
    
    if (is.null(raw_matrix) || !is.matrix(raw_matrix)) {
      next
    }
    
    # Step 2: Read metadata
    meta <- tryCatch({
      exiftoolr::exif_read(file)
    }, error = function(e) {
      warning("Failed to read metadata for ", basename(file), ": ", e$message)
      return(NULL)
    })
    
    if (is.null(meta)) next
    
    # Step 3: Store results
    raw_dat_list[[basename(file)]] <- raw_matrix
    
    camera_params_list[[basename(file)]] <- data.frame(
      file = basename(file),
      PlanckR1 = as.numeric(meta$PlanckR1 %||% NA),
      PlanckB  = as.numeric(meta$PlanckB  %||% NA),
      PlanckF  = as.numeric(meta$PlanckF  %||% NA),
      PlanckO  = as.numeric(meta$PlanckO  %||% NA),
      PlanckR2 = as.numeric(meta$PlanckR2 %||% NA)
    )
    
    metadata_list[[basename(file)]] <- data.frame(
      file = basename(file),
      photo_no = tools::file_path_sans_ext(basename(file)),
      atm_temp = as.numeric(meta$AtmosphericTemperature %||% NA),
      rel_humidity = as.numeric(meta$RelativeHumidity %||% NA)
    )
  }
  
  return(list(
    raw_dat = raw_dat_list,
    camera_params = dplyr::bind_rows(camera_params_list),
    metadata = dplyr::bind_rows(metadata_list)
  ))
}

# List all files attempted
all_files <- list.files("../FLIR_Images/Sierra Nevada/Photos_Sierra_Nevada/Good/FLIR_2024 (All Good)", pattern = "\\.jpg$", full.names = TRUE)

# Get just the file names (without paths)
file_names <- basename(all_files)

# Determine which files were successfully processed
success_raw <- names(results$raw_dat)
success_meta <- results$metadata$file
success_params <- results$camera_params$file

# Build summary table
summary_df <- data.frame(
  file = file_names,
  raw_matrix = file_names %in% success_raw,
  metadata = file_names %in% success_meta,
  camera_params = file_names %in% success_params
)

# Flag status
summary_df$status <- dplyr::case_when(
  !summary_df$raw_matrix ~ "❌ failed thermal matrix",
  !summary_df$metadata ~ "❌ failed metadata",
  !summary_df$camera_params ~ "❌ failed camera params",
  TRUE ~ "✅ success"
)

# View result
summary_df <- dplyr::arrange(summary_df, desc(status != "✅ success"))
ace_tools.display_dataframe_to_user("FLIR Image Processing Summary", summary_df)












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

