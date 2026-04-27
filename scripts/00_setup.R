# ------------------------------
# 00_setup.R
# ------------------------------

data_path    <- Sys.getenv("BRFSS_DATA")
project_path <- Sys.getenv("BRFSS_PROJECT")

if (data_path == "") stop("BRFSS_DATA not set")
if (project_path == "") stop("BRFSS_PROJECT not set")

# Paths
raw_data_path <- file.path(data_path, "LLCP2024.XPT")

data_dir    <- file.path(project_path, "data")
output_dir  <- file.path(project_path, "output")

clean_data_path <- file.path(data_dir, "brfss_clean.csv")
model_path      <- file.path(output_dir, "models")
table_path      <- file.path(output_dir, "tables")
figure_path     <- file.path(output_dir, "figures")

# Create dirs
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_path, recursive = TRUE, showWarnings = FALSE)
dir.create(table_path, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)

# Libraries
library(tidyverse)
library(haven)
library(survey)
library(ggeffects)
library(pROC)
library(broom)
library(viridis)
library(scales)

# Functions
#functions_path <- file.path(project_path, "scripts", "utils")

# source(file.path(functions_path, "set_ref_weighted.R"))
# source(file.path(functions_path, "roc_auc.R"))
# source(file.path(functions_path, "calibration_plot.R"))


