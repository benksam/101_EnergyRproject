# Data Wrangling in R
# 3.7 Importing Excel Files into R

# School breakfast program data
# Reading data stored in Excel

# Load the tidyverse
library(tidyverse)

# The readxl package isn't part of the core tidyverse so we need to install and
# load it separately
install.packages("readxl")
library(readxl)

# Try just reading the file without other arguments
breakfast <- read_excel('breakfast.xls')
glimpse(breakfast)

# Try skipping three lines

breakfast <- read_excel('breakfast.xls', skip=3)
glimpse(breakfast)

# It looks like we need to skip five lines, which will remove the column names
# So lets create a vector with column names
names <- c("Year", "FreeStudents", "ReducedStudents", "PaidStudents", "TotalStudents", 
           "MealsServed", "PercentFree")

# And then try reading the file again
breakfast <- read_excel('breakfast.xls',skip=5, col_names = names)
glimpse(breakfast)

# I'll do a little quick manipulation of this tibble. 
# First, convert the numbers of students and meals to real values
breakfast$FreeStudents = breakfast$FreeStudents * 1000000
breakfast$ReducedStudents = breakfast$ReducedStudents * 1000000
breakfast$PaidStudents = breakfast$PaidStudents * 1000000
breakfast$TotalStudents = breakfast$TotalStudents * 1000000
breakfast$MealsServed = breakfast$MealsServed * 1000000

# And show the percentage as a decimal
breakfast$PercentFree = breakfast$PercentFree / 100
glimpse(breakfast)