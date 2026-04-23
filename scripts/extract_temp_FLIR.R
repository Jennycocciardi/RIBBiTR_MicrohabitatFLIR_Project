# Code to extract temperatures from FLIR images
# Author: Jenny Cocciardi
# 22 Apr, 2026
#________________

# Pennsylvania

# 1. Set path to where we are keeping FLIR images

# PA FLIR images have been copied to:
# *'Ohmer Lab --> Undergraduate Research --> AKang_FLIR_Microhabitat --> 
#       FLIR Images --> Pennsylvania --> photos_renamed'*
# The year that they each image was taken has been appended to the Image ID in this folder. 
#     This is because there were duplicate IDs across different seasons, so this insures a 
#     unique image ID. This was done using the *rename_reorg_images.R* script.

# check wd() - if you have opened up the files using the R project, then the
#   path names will all be correct
getwd()

img_path <- "../FLIR_Images/Pennsylvania/photos_renamed"


#____
# 2. Extract FLIR image pixel temperature data using the batch_extract function from utils script
#     **NOTE: depending on how many photos there are to extract data from, it can take up to 1 hr or so
flir_raw <- batch_extract_custom(in_dir = img_path, 
                                 exiftool_path = exiftool_path)

#check the raw data that is produced
length(flir_raw$raw_dat)       # number of valid thermal images
head(names(flir_raw$raw_dat))  # preview image names
str(flir_raw$camera_params)    # preview camera calibration constants
str(flir_raw$metadata)         # preview environmental metadata


#____
# 3. Convert our raw data to temp data using emissivity, relative humidity, and temp
#     this is important because FLIR camera stores infrared radiation data, not temp data
#     so we need to convert to temo data
# **NOTE: here, we are using the relative humidity and temp data stored in the camera alongside
#   each FLIR image. If field teams inputted this into the camera, then this is okay.
#   However, it is most likely that field teams did not do this and so we will need to add the
#   environmental data recorded during the survey to best convert temps

# use batch_convert function to convert extracted data to temperature
flir_temp <- batch_convert(
  raw_dat = flir_raw$raw_dat,
  metadata = flir_raw$metadata,
  camera_params = flir_raw$camera_params,
  E = 0.98)

# check output of batch_convert
length(flir_temp)
image(flir_temp[[1]], main = names(flir_temp)[1])  # plot example


#____
# 4. Compute temp and summary stats - this creates a large list with every output
flir_stats <- map2(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y))

# summary temperature stats - this creates a more defined table with useful data
flir_pstats <- map2_dfr(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y,
                   return_vals = "pstats",
                   sum_stats = c("mean", "median", "min", "max", "sd", 
                                 "perc_5", "perc_95", "SHDI"))$pstats)

# hot/cold patch summary
flir_patches <- map2_dfr(
  .x = flir_temp,
  .y = names(flir_temp),
  .f = ~ get_stats(img = .x, id = .y)$patches %>%
    mutate(id = .y, .before = 1))
# **NOTE: right now, our patch summary is based on the 95th and 5th percentile,
#       meaning patch counts are based on the most extreme pixels in each image.
#       We will most likely want to change this function to include something more
#       biologically meaningful, like a set temperature for each species and if we are
#       interested in looking at how many cold and hot patches - then including a 
#       min patch size and then modifying the function to include this count.

# join summary and patch stats on image ID
flir_summary_combined <- left_join(flir_pstats, flir_patches, by = "id")
glimpse(flir_summary_combined)


#____
# 5. example plot of one image
img_name <- "FLIR4821_2025.jpg"
plot_patches(df = flir_temp[[img_name]],
             patches = TRUE,
             outline_size = 0.5,
             hatching = TRUE)

plot_patches(flir_temp[[img_name]])


#____
# 6. join metadata temp data

# PA metadata sheet was copied over into the R project 'data' folder on April 22 2026 as a .csv.
#     if metadata sheet is updated after 4/22/26, will need to recopy to data folder in this R project drive.
meta_sheet <- read.csv("data/PA_metadata_04-22-26.csv")

# add in id column to match id to temp data - for PA, this is just adding in the .jpg text
meta_sheet <- meta_sheet %>%
  mutate(id = paste0(FLIR_ID_Year, ".jpg"))

# convert date of the survey to date format and check structure of data
str(meta_sheet)
meta_sheet <- meta_sheet %>%
  mutate(date = as.Date(date,format = "%m/%d/%y"),
    year = year(date))

# merge metadata with temp stats
flir_summary_full <- meta_sheet %>%
  left_join(flir_summary_combined, by = "id")


#____
# 7. clean dataset: remove photos determined to be not usable
nrow(flir_summary_full)
colSums(is.na(flir_summary_full))
flir_summary_full %>%
  count(determined_usage)
flir_summary_full <- flir_summary_full %>%
  mutate(determined_usage = ifelse(determined_usage == "good ", "good", determined_usage))

flir_summary_full_clean <- flir_summary_full %>%
  filter(determined_usage == "good")
nrow(flir_summary_full_clean)


#____
# 8. export to 'data' folder for further processing and analysis
write.csv(flir_summary_full_clean, "data/PA_flir-temp-data.csv", row.names = FALSE)



#________________

# Sierra Nevada

# 1. Set path to where we are keeping FLIR images


