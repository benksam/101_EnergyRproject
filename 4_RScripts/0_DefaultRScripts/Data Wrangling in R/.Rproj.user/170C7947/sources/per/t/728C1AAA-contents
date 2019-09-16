# Data Wrangling in R
# 5.3 Breaking Apart Columns With Separate
#

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Take a look at the diagnosis-related group unique values
unique(inpatient$DRG)

# inpatient_separate = NULL

# Let's try separating this on the hyphen
inpatient_separateS <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),'-')

head(inpatient_separateS)

glimpse(inpatient_separateS)

# LONGER dataset
# What's going on with those warning rows?  Let's look at row 45894
inpatient$DRG[45894]

# Let's separate with character position instead
inpatient_separateP <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)

# And take a look at the data now
glimpse(inpatient_separateP)
