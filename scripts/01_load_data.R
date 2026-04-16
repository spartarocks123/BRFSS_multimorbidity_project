# ------------------------------
# Libraries
# ------------------------------
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

# ------------------------------
# Load Data
# ------------------------------
brfss <- read_xpt("/Users/moh/Desktop/LLCP2024.XPT ")

# Clean names (removes leading underscores)
names(brfss) <- gsub("^_", "", names(brfss))
