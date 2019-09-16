# Data Wrangling in R
# 3.6 Importing Fixed Width Files into R

# Chicago employee salaries
# Reading in data from a fixed width file

# First we need to create a list of the column lengths
lengths <- c(32,50,24,NA)

# And a list of column names
names <- c("Name", "Title", "Department", "Salary")

# And then create the widths list using the fwf_widths function.
# Setting the last value to NA allows the last column to be "ragged" - variable length
widths <- fwf_widths(length, names)

# Then we read the data into a tibble by using the read_fwf function with the 
# filename and column widths as arguments.
employees <- read_fwf('http://594442.youcanlearnit.net/chicagoemployees.txt', widths)

# And let's take a look at what we got
glimpse(employees)

# The last correction we need to make is skipping the first line of the file so that we don't read
# the variable names as an observation
employees <- read_fwf('http://594442.youcanlearnit.net/chicagoemployees.txt', widths, skip=1)
glimpse(employees)
