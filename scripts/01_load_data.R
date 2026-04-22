

# ------------------------------
# Load Data
# ------------------------------
brfss <- read_xpt("/Users/moh/Desktop/LLCP2024.XPT")

# Clean names (removes leading underscores)
names(brfss) <- gsub("^_", "", names(brfss))
