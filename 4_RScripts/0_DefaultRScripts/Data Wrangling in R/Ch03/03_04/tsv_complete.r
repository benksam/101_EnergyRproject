# Data Wrangling in R
# 3.4 Importing TSV Files into R

# Read in TSV containing Medicare data
# This file is comparing the charges for the 100 most common inpatient services and 30 common outpatient services.  Providers determine what they will charge for items and services provided to patients and these charges are the amount the providers bills for an item or service.

# First, we need to load the tidyverse
library(tidyverse)

# Next, let's just try reading in the file and see what it looks like
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv')

glimpse(inpatient)

# First issue that we need to correct is the variable names.  

names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", 
           "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")

inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names)

# Now the names are the first observation!
# Two ways I can deal with this, I can just delete the first row or I can fix... Let's fix

inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1)

# Let's specify our data types, reading only discharges as an integer for now

types = 'cccccccciccc'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1, col_types = types)
glimpse(inpatient)

# Now let's deal with the currency amounts
# Currency is floating point, so we might be tempted to try doubles

types = 'cccccccciddd'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1, col_types = types)
glimpse(inpatient)

# But that doesn't work quite right, so we can use numeric instead
# It finds the first number in the field.  Good for getting rid of $, % etc.

types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1, col_types = types)

glimpse(inpatient)
