# Data Wrangling in R
# Social Security Disability Case Study

# Load the tidyverse
library(tidyverse)
library(lubridate)
library(stringr)

# Read in the coal dataset
ssa <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv")
ssa <- read_csv("Data/ssadisability.csv")
View(ssa)


# Take a look at how this was imported
glimpse(ssa)


