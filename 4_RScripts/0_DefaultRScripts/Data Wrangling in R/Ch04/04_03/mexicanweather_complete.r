# Data Wrangling in R
# 4.3 Making Long Datasets Wide with Spread

# Load the tidyverse
library(tidyverse)

# Read the dataset
weather <- read_csv("http://594442.youcanlearnit.net//mexicanweather.csv")
weather <- read_csv("Data/weather.csv")

# Let's look at what we have
View(weather)
colnames(weather)

# And use spread() to make it wider
# weather.wide <- spread(weather, element, value)

weather.long <- gather(weather, 
                       days, 
                       value,-c("station","year","month","element"))
View(weather.long)

weather.wide <- spread(weather.long, element, value)

# Where are we now?
View(weather.wide)

