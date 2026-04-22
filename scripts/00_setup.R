# ------------------------------
# 00_setup.R
# ------------------------------

# Load environment variables
data_path    <- Sys.getenv("BRFSS_DATA")
project_path <- Sys.getenv("BRFSS_PROJECT")

# Define standardized paths
raw_data_path <- file.path(data_path, "LLCP2024.XPT")
clean_data_path <- file.path(project_path, "data", "brfss_clean.csv")
output_path <- file.path(project_path, "output")

# Create directories
dir.create(file.path(output_path, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_path, "figures"), recursive = TRUE, showWarnings = FALSE)

# Libraries
library(haven)
library(dplyr)
library(survey)
library(ggplot2)
library(viridis)
library(scales)
library(broom)
library(tidyverse)
library(ggeffects)
library(pROC)

# Source custom functions
source(file.path(project_path, "R/functions/set_ref_weighted.R"))
source(file.path(project_path, "R/functions/roc_auc.R"))
source(file.path(project_path, "R/functions/calibration_plot.R"))