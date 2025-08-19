#Author(s): Jenny Cocciardi
#Date: Aug 19, 2025
#Purpose: Script to rename and organize FLIR photo images (.jpgs)


# Append year to FLIR Image to match datasheet ID----
  # We will use the photos directly from the Ecophysiology folder for this
  # ONLY NEED TO DO THIS ONCE

library(fs)
library(stringr)

# Set paths
source_dir <- "/Users/jennycocciardi/Library/CloudStorage/GoogleDrive-jenny.cocciardi@gmail.com/My Drive/Ecophysiology/Thermal Images_FLIR photos/Pennsylvania" 
#folder containing photos
target_dir <- "~/Google Drive/My Drive/Ohmer Lab/Undergraduate research projects/AKang_FLIR_Microhabitat_Project/FLIR_Images/Pennsylvania/photos_renamed"       
#where renamed images go

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