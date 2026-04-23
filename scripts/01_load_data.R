

# ------------------------------
# Load Data
# ------------------------------
brfss <- read_xpt(raw_data_path)

# Clean names (removes leading underscores)
names(brfss) <- gsub("^_", "", names(brfss))
