# Data Wrangling in R
# 5.2 Missing and Special Values in R
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

# inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
#                         col_names=names, skip=1)
# 
# inspections <- read_csv("Data/inspections.csv", 
#                         col_names=names, skip=1)

# Look at a summary of the data
summary(inpatient)

# Which inspections have NA values for license?

is.na(inspections$License)
which(is.na(inspections$License))

nolicense<-which(is.na(inspections$License))
inspections[nolicense,]

# Create two new tibbles, one for licensed facilities and one for unlicensed facilities
unlicensed <- inspections[nolicense,]
licensed <- inspections[-nolicense,]

# What happens if I divide a number by 0

badmath <- c(1,2,3,4/0,0/0,NA)
badmath
is.na(badmath)
is.nan(badmath)
is.infinite(badmath)
is.finite(badmath)

