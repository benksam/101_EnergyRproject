# Data Wrangling in R
# Austin Water Quality Case Study

# Load in the libraries that we'll need
library(tidyverse)
library(stringr)
library(lubridate)

# Read in the dataset
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')

# Let's take a look at what we have
glimpse(water)

# First, let's get rid of a lot of columns that we don't need
# I'm going to do that by building a new tibble with just
# siteName, siteType, parameter, result and unit

water <- tibble('siteName'=water$SITE_NAME,
                'siteType'=water$SITE_TYPE,
                'sampleTime'=water$SAMPLE_DATE,
                'parameterType'=water$PARAM_TYPE,
                'parameter'=water$PARAMETER,
                'result'=water$RESULT,
                'unit'=water$UNIT)

glimpse(water)

# Now let's start finding the rows that we need.
# First, we need pH.  I might start by trying to look at all unique parameter names

unique(water$parameter)

# but that's way too long... what if we try searching for names that contain PH?

unique(water[which(str_detect(water$parameter,'PH')),]$parameter)

# still a mess... let's backtrack and look at parameter types

unique(water$parameterType)

# OK, what if I filter this down to look only at parameter types of Alkalinity/Hardness/pH
# and Conventionals

filtered_water <- subset(water,(parameterType=='Alkalinity/Hardness/pH') |
                                  parameterType=='Conventionals')

# Notice that this is much smaller in size.  let's check what parameters we have now

unique(filtered_water$parameter)

# I want only two of these, (discuss PH and temp choices), so let's filter those

filtered_water <- subset(filtered_water, ((parameter=='PH') |
                                            (parameter=='WATER TEMPERATURE')))

glimpse(filtered_water)
