# Data Wrangling in R
# 5.5 Manipulating Strings in R with stringr
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)

# And take a look at the unique regions
unique(regional_inspections$Region)
unique(inspections$Region)

# We need to load stringr separately
library(stringr)

# Let's handle the uppercase/lowercase issues by converting everything to uppercase
regional_inspections$Region <- str_to_upper(regional_inspections$Region)
inspections$Region <- str_to_upper(inspections$Region)

# What were the results of that?
unique(regional_inspections$Region)
unique(inspections$Region)

# Let's take care of a few misspellings of Chicago
regional_inspections$Region <- str_replace(regional_inspections$Region,'CCHICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHCICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHICAGOCHICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHCHICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHICAGOI, IL', 'CHICAGO, IL')

# Let's take care of a few misspellings of Chicago
inspections$Region <- str_replace(inspections$Region,'CCHICAGO', 'CHICAGO')
inspections$Region <- str_replace(inspections$Region,'CHCICAGO', 'CHICAGO')
inspections$Region <- str_replace(inspections$Region,'CHICAGOCHICAGO', 'CHICAGO')
inspections$Region <- str_replace(inspections$Region,'CHCHICAGO', 'CHICAGO')
inspections$Region <- str_replace(inspections$Region,'CHICAGOI', 'CHICAGO')


# And see what's left
unique(regional_inspections$Region)

# There are some "CHICAGO, NA" values that we can clearly correct to "CHICAGO, IL"
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHICAGO, NA', 'CHICAGO, IL')

# But we don't know what to do with "NA, IL", "NA, NA", or "INACTIVE, IL"
# so let's set those to missing values
regional_inspections$Region <- str_replace(regional_inspections$Region,'NA, ', NA)

# Whoops... we'll need to use str_detect to find those
NA_regions <- which(str_detect(regional_inspections$Region, "NA, "))
inactive_regions <- which(str_detect(regional_inspections$Region, "INACTIVE, IL"))

# And then set them to NA 
regional_inspections$Region[NA_regions] <- NA
regional_inspections$Region[inactive_regions] <- NA

# How did we do?
unique(regional_inspections$Region)
