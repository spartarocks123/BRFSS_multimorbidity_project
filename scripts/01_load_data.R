#------------------------------
# 01_load_data.R
# ------------------------------

source("scripts/00_setup.R")

brfss <- read_xpt(raw_data_path)
