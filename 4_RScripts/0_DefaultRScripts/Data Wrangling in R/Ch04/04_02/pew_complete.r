# Data Wrangling in R
# 4.2 Making Wide Datasets Long with gather()
#

# Load the tidyverse
library(tidyverse)

# Read in the Pew dataset
pew <- read_csv("http://594442.youcanlearnit.net/pew.csv")

# Let's take a look at what we have
pew
View(pew)

# This looks to be a gathering problem.  Our dataset is wide and we want it to be long.
# The gather function can take care of that for us
pew.long <- gather(pew, income, freq, -religion)

# And what did we get?
View(pew.long)

