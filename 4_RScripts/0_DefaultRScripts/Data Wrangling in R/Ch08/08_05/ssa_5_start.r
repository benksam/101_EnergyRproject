# Data Wrangling in R
# Social Security Disability Case Study

# Load the tidyverse
library(tidyverse)
library(lubridate)
library(stringr)

# Read in the coal dataset
ssa <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv")

# Take a look at how this was imported
glimpse(ssa)

# Make the dataset long
ssa_long <- gather(ssa, month, applications, -Fiscal_Year)

# And what do we get?
print(ssa_long, n=20)

# Split the month and application type
ssa_long <- separate(ssa_long, month, c("month", "application_method"), sep="_")

# What does that look like?
print(ssa_long, n=20)

# What values do we have for months?
unique(ssa_long$month)

# Convert month to standard abbreviations
ssa_long$month <- substr(ssa_long$month,1,3)

# What values do we now have for months and years?
unique(ssa_long$month)
unique(ssa_long$Fiscal_Year)

# Convert Fiscal_Year from alphanumeric strings to actual years
ssa_long$Fiscal_Year <- str_replace(ssa_long$Fiscal_Year, "FY", "20")

# What values do we now have for months?
unique(ssa_long$Fiscal_Year)

# Build a date string using the first day of the month
paste('01', ssa_long$month, ssa_long$Fiscal_Year)

ssa_long$Date <- dmy(paste("01", ssa_long$month, ssa$Fiscal_Year))

# What do those look like?
unique(ssa_long$Date)
