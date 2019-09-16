# Data Wrangling in R
# 3.2 Importing CSV Files into R

# First, I need to load the tidyverse
library(tidyverse)

# Now let's try loading in the CSV file without any special settings
inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv')

# And take a look at what we loaded in
glimpse(inspections)

# That actually looks pretty good.  I can see that R made some choices about variable names
# and I'd like to replace them with my own names.  First, I'll create a vector of my names.

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

# I could simply rename the variables in the tibble that I already created, but I want to 
# show you how to supply variable names to read_csv

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names)

# That changed the variable names, but it also imported the variable names that were in the
# file as the first data row.  So let me try this again and tell read_csv to skip the first line.

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)
