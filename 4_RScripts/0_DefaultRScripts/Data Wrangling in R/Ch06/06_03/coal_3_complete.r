# Data Wrangling in R
# Coal Consumption Case Study

# Load the tidyverse
library(tidyverse)

# Read in the coal dataset
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)

# Skip the first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# Rename the first column as region
colnames(coal)[1] <- "region"
summary(coal)

# Convert from a wide dataset to a long dataset using gather
coal_long <- gather(coal, 'year', 'coal_consumption', -region)
glimpse(coal_long)

# Convert years to integers
coal_long$year <- as.integer(coal_long$year)
summary(coal_long)

# Convert coal consumption to numeric
coal_long$coal_consumption <- as.numeric(coal_long$coal_consumption)
summary(coal_long)

