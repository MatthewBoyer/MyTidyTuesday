#####################################################################################################

# Tidy Tuesday - Week 4 - 14 January 2023: Alone Data

#####################################################################################################

# Load data cleaning packages
library(tidytuesdayR)
library(tidyverse)

# Import Alone data sets
tuesdata <- tidytuesdayR::tt_load(2023, week = 4)

srvlst <- tuesdata$survivalists
loadout <- tuesdata$loadouts
epsd <- tuesdata$episodes
seas <- tuesdata$seasons

# Plot average age of winner