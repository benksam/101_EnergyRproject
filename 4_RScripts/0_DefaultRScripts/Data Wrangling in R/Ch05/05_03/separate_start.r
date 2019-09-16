# Data Wrangling in R
# 5.3 Breaking Apart Columns With Separate
#

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", 
           "AverageCharges", "AverageTotalPayments", "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, 
                      skip=1, col_types = types)

