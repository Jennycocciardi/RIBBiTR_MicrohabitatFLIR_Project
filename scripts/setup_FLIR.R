# setup.R

# ---- Install required packages ----
required_packages <- c("Thermimage", "exiftoolr", "dplyr", "stringr", "purrr", "tidyr",
                       "readxl", "fs", "lubridate", "ggplot2")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# ---- Load packages ----
library(Thermimage)
library(exiftoolr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(readxl)
library(fs)
library(tools)
library(lubridate)
library(ggplot2)

# ---- Source custom functions ----
source("scripts/utils_FLIR.R")

# ---- Set default options (if needed) ----
options(stringsAsFactors = FALSE)

