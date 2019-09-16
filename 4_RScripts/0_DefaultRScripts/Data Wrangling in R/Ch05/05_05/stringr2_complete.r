# Data Wrangling in R
# 5.5 Manipulating Strings in R with stringr
# Part 2

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Separate at the fourth position
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)

# And take a look at the data now
glimpse(inpatient_separateP)

# Load stringr
library(stringr)

# Trim the DRGcode field
inpatient_separateP$DRGcode <- str_trim(inpatient_separateP$DRGcode)
glimpse(inpatient_separateP)

# The DRGdescription field has a hyphen in front so we need to do something different
inpatient_separate$DRGdescription <- str_sub(inpatient_separate$DRGdescription, 3)
glimpse(inpatient_separate)
