# Data Wrangling in R
# Austin Water Quality Case Study

# Load in the libraries that we'll need
library(tidyverse)
library(stringr)
library(lubridate)

# Read in the dataset
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')

water <- read_csv("Data/austinwater_short.csv")
# water <- read_csv("Data/austinwater.csv")
View(water)

# Let's take a look at what we have
glimpse(water)
