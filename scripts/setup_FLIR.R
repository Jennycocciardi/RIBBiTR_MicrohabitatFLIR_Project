# setup.R

# ---- Install required packages ----
required_packages <- c("Thermimage", "exiftoolr", "dplyr", "stringr", "purrr")

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

# ---- Source custom functions ----
source("scripts/utils_FLIR.R")

# ---- Set default options (if needed) ----
options(stringsAsFactors = FALSE)

