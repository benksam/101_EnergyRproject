# Data Wrangling in R
# 5.4 Combining Columns with unite()
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

colnames(inspections) = names

glimpse(inspections)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ")

# Let's look at the data
glimpse(regional_inspections)

# Whoops. I didn't want to DELETE the City and State columns.  Let's try again.
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)
glimpse(regional_inspections)

# And take a look at the unique regions
unique(regional_inspections$Region)
