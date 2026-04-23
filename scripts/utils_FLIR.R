# functions for analyzing FLIR images
# ThermStats has not been kept up to date, so we will write similiar functions to do
#   what the package was doing before...but without using outdated libraries that 
#   ThermStats is dependant on

# ---- Packages Required ----
library(Thermimage)
library(exiftoolr)
library(dplyr)
library(stringr)
library(ggplot2)

# batch_extract function
#   extracts metadata from FLIR images in one folder, using exirflir
exiftool_path <- "/opt/homebrew/bin/"

batch_extract_custom <- function(in_dir, exiftool_path, write_results = FALSE) {
  files <- list.files(in_dir, pattern = "\\.jpg$", full.names = TRUE)
  
  raw_dat_list <- list()
  camera_params_list <- list()
  metadata_list <- list()
  
  for (file in files) {
    try({
      message("Processing: ", file)
      
      # Try reading the thermal matrix
      raw_matrix <- Thermimage::readflirJPG(file, exiftoolpath = exiftool_path)
      if (!is.matrix(raw_matrix)) {
        warning("Skipping ", basename(file), " — not a thermal matrix")
        next
      }
      
      # Try reading metadata using exiftoolr
      meta <- exiftoolr::exif_read(file)
      
      raw_dat_list[[basename(file)]] <- raw_matrix
      
      # Extract calibration constants
      camera_params_list[[basename(file)]] <- data.frame(
        PlanckR1 = as.numeric(meta$PlanckR1),
        PlanckB  = as.numeric(meta$PlanckB),
        PlanckF  = as.numeric(meta$PlanckF),
        PlanckO  = as.numeric(meta$PlanckO),
        PlanckR2 = as.numeric(meta$PlanckR2)
      )
      
      # Extract other relevant metadata
      metadata_list[[basename(file)]] <- data.frame(
        photo_no = tools::file_path_sans_ext(basename(file)),
        atm_temp = as.numeric(meta$AtmosphericTemperature),
        rel_humidity = as.numeric(meta$RelativeHumidity)
      )
    }, silent = TRUE)
  }
  
  return(list(
    raw_dat = raw_dat_list,
    camera_params = do.call(rbind, camera_params_list),
    metadata = do.call(rbind, metadata_list)
  ))
}



# batch_convert function
#   converts raw data to temperature using parameters like emissivity, temp, etc.
batch_convert <- function(raw_dat, metadata, camera_params, E = 0.98, OD = 1, write_results = FALSE) {
  
  # Match metadata to image names
  img_names <- names(raw_dat)
  photo_index <- match(gsub("\\.jpg$", "", img_names), metadata$photo_no)
  
  # Initialize list to store converted temperature matrices
  converted_list <- list()
  
  for (i in seq_along(img_names)) {
    name <- img_names[i]
    raw <- raw_dat[[i]]
    
    # Safely retrieve metadata and camera params
    meta_row <- metadata[photo_index[i], , drop = FALSE]
    param_row <- camera_params[name, , drop = FALSE]
    
    # Skip if metadata or params missing
    if (nrow(meta_row) == 0 || nrow(param_row) == 0 || anyNA(meta_row) || anyNA(param_row)) {
      warning(paste("Skipping image", name, "- missing metadata or camera parameters"))
      print("meta_row:"); print(meta_row)
      print("param_row:"); print(param_row)
      next
    }
    
    # Convert using Thermimage::raw2temp with correct metadata column names
    temp_C <- Thermimage::raw2temp(
      raw = raw,
      E = E,
      OD = OD,
      RTemp = meta_row$atm_temp,
      ATemp = meta_row$atm_temp,
      RH = meta_row$rel_humidity,
      PR1 = param_row$PlanckR1,
      PB = param_row$PlanckB,
      PF = param_row$PlanckF,
      PO = param_row$PlanckO,
      PR2 = param_row$PlanckR2
    )
    
    # Save results
    converted_list[[name]] <- temp_C
    
    if (write_results) {
      filename <- paste0("converted_", name, ".csv")
      write.csv(temp_C, file = filename, row.names = FALSE)
    }
  }
  
  return(converted_list)
}



# function to calculate per image thermal images and identify hot and cold spots
get_stats <- function(img, id,
                      calc_connectivity = FALSE,   # Placeholder if you want to add later
                      patches = TRUE,
                      img_proj = NULL,             # Not used in this version
                      img_extent = NULL,           # Not used in this version
                      return_vals = c("df", "pstats", "patches"),
                      sum_stats = c("mean", "median", "min", "max", "sd", "perc_5", "perc_95", "SHDI")) {
  
  # Flatten temperature matrix and remove NA
  flat_vals <- as.vector(img)
  flat_vals <- flat_vals[!is.na(flat_vals)]
  
  # Initialize list for summary stats
  stats <- list()
  
  if ("mean" %in% sum_stats)
    stats$mean <- mean(flat_vals, na.rm = TRUE)
  
  if ("median" %in% sum_stats)
    stats$median <- median(flat_vals, na.rm = TRUE)
  
  if ("min" %in% sum_stats)
    stats$min <- min(flat_vals, na.rm = TRUE)
  
  if ("max" %in% sum_stats)
    stats$max <- max(flat_vals, na.rm = TRUE)
  
  if ("sd" %in% sum_stats)
    stats$sd <- sd(flat_vals, na.rm = TRUE)
  
  if ("perc_5" %in% sum_stats)
    stats$perc_5 <- as.numeric(quantile(flat_vals, 0.05, na.rm = TRUE))
  
  if ("perc_95" %in% sum_stats)
    stats$perc_95 <- as.numeric(quantile(flat_vals, 0.95, na.rm = TRUE))
  
  if ("SHDI" %in% sum_stats) {
    rounded <- round(flat_vals, 0)
    freqs <- table(rounded) / length(rounded)
    stats$SHDI <- -sum(freqs * log(freqs))
  }
  
  stat_df <- data.frame(id = id, as.data.frame(stats))
  
  # Identify hot and cold "patches" based on quantile thresholds
  patch_df <- NULL
  if (patches) {
    hot_thresh <- quantile(flat_vals, 0.95)
    cold_thresh <- quantile(flat_vals, 0.05)
    
    patch_df <- data.frame(
      hot_pixels = sum(img > hot_thresh, na.rm = TRUE),
      cold_pixels = sum(img < cold_thresh, na.rm = TRUE),
      hot_mean = mean(img[img > hot_thresh], na.rm = TRUE),
      cold_mean = mean(img[img < cold_thresh], na.rm = TRUE)
    )
  }
  
  # Assemble outputs
  out <- list()
  if ("df" %in% return_vals) out$df <- img
  if ("pstats" %in% return_vals) out$pstats <- stat_df
  if ("patches" %in% return_vals && !is.null(patch_df)) out$patches <- patch_df
  
  return(out)
}


# function to plot patches
plot_patches <- function(df, patches = TRUE, outline_size = 0.5,
                         hatching = FALSE, hatch_density = c(0.75, 0.75),
                         plot_distribution = TRUE) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Convert matrix to long-format data frame
  df_long <- as.data.frame(df)
  df_long$y <- nrow(df):1
  df_long <- pivot_longer(df_long, -y, names_to = "x", values_to = "temp")
  df_long$x <- as.integer(gsub("V", "", df_long$x))
  
  # Define quantile thresholds
  hot_thresh <- quantile(df_long$temp, 0.95, na.rm = TRUE)
  cold_thresh <- quantile(df_long$temp, 0.05, na.rm = TRUE)
  
  # Classify patch type
  df_long <- df_long %>%
    mutate(category = case_when(
      temp >= hot_thresh ~ "hot",
      temp <= cold_thresh ~ "cold",
      TRUE ~ NA_character_
    ))
  
  # Base plot
  p <- ggplot(df_long, aes(x = x, y = y, fill = temp)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno", name = "Temp (°C)") +
    coord_fixed() +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  
  # Overlay patch outlines with a legend
  if (patches) {
    p <- p + geom_tile(data = df_long %>% filter(!is.na(category)),
                       aes(color = category),
                       fill = NA,
                       linewidth = outline_size) +
      scale_color_manual(name = "Patches",
                         values = c("hot" = "red", "cold" = "blue"),
                         na.translate = FALSE)
  }
  
  print(p)
  
  # Optional histogram plot
  if (plot_distribution) {
    hist_plot <- ggplot(df_long, aes(x = temp)) +
      geom_histogram(bins = 50, fill = "gray70", color = "black") +
      geom_vline(xintercept = c(cold_thresh, hot_thresh),
                 color = c("blue", "red"),
                 linetype = "dashed") +
      labs(x = "Temperature (°C)", y = "Frequency", title = "Temperature Distribution") +
      theme_minimal()
    print(hist_plot)
  }
}



