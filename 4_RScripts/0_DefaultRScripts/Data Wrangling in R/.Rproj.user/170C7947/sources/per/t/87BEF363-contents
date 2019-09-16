# Data Wrangling in R
# 5.1 Detecting Outliers
# Data="inpatient" TOO BIG!

# Load the tidyverse
library(tidyverse)
library(ggplot2)

# Read in the Medicare payments dataset
names <- c("DRG", "ProviderID", "Name", "Address", "City", 
           "State", "ZIP", "Region", "Discharges", 
           "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")

types = 'ccccccccinnn'

inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

inpatient <- read_csv("Data/inpatient_short.csv",
                      col_types = types) # skip=1, col_names = names, 

colnames(inpatient) = names

# Let's look at a histogram
ggplot(data=inpatient) + 
  geom_histogram(mapping=aes(x=AverageCharges))

# What if we change the limit of the y axis
ggplot(data=inpatient) + 
  geom_histogram(mapping=aes(x=AverageCharges)) +
  coord_cartesian(ylim=c(0,25))

# We could also view this with a single boxplot
ggplot(data=inpatient) + 
  geom_boxplot(mapping=aes("charges",AverageCharges))

# Or we can use a series of boxplots broken out by state
ggplot(data=inpatient) + 
  geom_boxplot(mapping=aes(State,AverageCharges))

# Plenty to investigate here, but let's dig in to those over $500,000
highCharges <- filter(inpatient, AverageCharges>50000)

lowCharges <- filter(inpatient, AverageCharges<50000)

unique(highCharges$DRG) # DATA over 100000 raws shortened! PBM!!!
unique(lowCharges$DRG)


# Or we can use a series of boxplots broken out by state
ggplot(data=lowCharges) + 
  geom_boxplot(mapping=aes(State,AverageCharges))


ggplot(data=highCharges) + 
  geom_point(mapping=aes(State,AverageCharges)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
