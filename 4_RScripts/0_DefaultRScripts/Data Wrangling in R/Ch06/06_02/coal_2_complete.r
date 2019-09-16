# Data Wrangling in R
# Coal Consumption Case Study

# Load the tidyverse
library(tidyverse)

# Read in the coal dataset
coal <- read_csv("Data/coal.csv", skip=2)
View(coal)

glimpse(coal)

# Skip the first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# Rename the first column as region
colnames(coal)[1] <- "region"
summary(coal)

str(coal)
glimpse(coal)


