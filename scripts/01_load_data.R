

# ------------------------------
# Load Data Huh
# ------------------------------
source("scripts/00_setup.R")

brfss <- read_xpt(raw_data_path)

# Clean names (removes leading underscores)
names(brfss) <- gsub("^_", "", names(brfss))

