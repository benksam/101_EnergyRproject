# Data Wrangling in R
# 3.5 Importing Delimited Files into R

# Accidental deaths associated with drug overdoses in Connecticut from 2012 to 2015
# Reading data from a carat-delimited file

# Load the tidyverse
library(tidyverse)

# Use the read_delim function to look at the file 
deaths <- read_delim(file='http://594442.youcanlearnit.net/drugdeaths.txt', delim='^')

glimpse(deaths)
