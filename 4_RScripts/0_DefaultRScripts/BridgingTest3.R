
###########################
## LOOPING in FC Vs .shp ##
###########################
# Data IMPORT Part!!!

library(arcgisbinding)

arc.check_product()

#####################



# Simple READING file by file
# test_df <- arc.open(path = 'C:/LPAShpData/ne_10m_admin_0_countries.shp')
# test_df <- arc.open(path = 'C:/LPAShpData/ne_10m_admin_0_countries.shp')

# test_df <- arc.open(path = 'C:/000_LPA/Data/test.gdb/countries')
# class(test_df)
# str(test_df)
# 
# test_select_df <- arc.select(object = test_df)
# class(test_select_df)
# 
# test_select_df <- arc.select(object = test_df)
# ldf1_select_df <- arc.select(object = ldf[[1]])


# setwd("C:/LPAShpData/")
# , fields = test_df[1:2]c('FID','SOVEREIGNT','POP_EST')


?list.files


# C:\arcgis\ArcTutor\Spatial Analyst\Stowe.gdb


# BATCH READING fall files at once!
# Preparation for looping over List of FC! 

# IMPORT list from PYTHON output (FC from .gdb)
# Output from File: "ListDataBASIC_GdbFCDataSets"
lpython = c("nuts2")
  
# ('ca_cities', 'ca_outline', 'O3_Sep06_3pm')

# ('destination', 'rec_sites', 'roads', 'schools')

list = paste("C:/DM_10.6/europe/data/nuts2.gdb/",lpython,sep="")
list

# C:/arcgis/ArcTutor/Spatial Analyst/Stowe.gdb/
# C:/arcgis/ArcTutor/Geostatistical Analyst/ca_ozone.gdb/
# C:\DM_10.6\europe\data\nuts2.gdb


# Looping for arc.open
ldf <- list() # creates a list
for (k in 1:length(list)){
  ldf[[k]] <- arc.open(path = list[k])
}
ldf[[1]]
str(ldf[[1]])   


# Looping for arc.select
sel <- list()
for (k in 1:length(ldf)){
  sel[[k]] <- arc.select(object = ldf[[k]])
}
class(sel[[1]])
str(sel[[1]])


# ldf[[1]]@path
# Looping for paths for column fields
path <- list()
for (k in 1:length(ldf)){
  path[[k]] <- ldf[[k]]@path
}
path[[1]]
path

# List of Shape files for Field Check!!!
list

# colnames(sel[[1]])
# Looping for Field Names
col <- list()
for (k in 1:length(sel)){
  col[[k]] <- colnames(sel[[k]])
}
col


# List of Shape files for Field Check!!!
list


################
## NO LOOPING ##
#####################
# Data Wrangling Part!!!
library(tidyverse)
#####################


# Sel[[k]] for list[k] (.shp)

sel[[1]] %>%
  select("SITE_NAME","ELEVATION","OZONE") %>%
  View()

glimpse(sel[[1]])

data = sel[[1]] 

# %>%
# select("SITE_NAME","ELEVATION","OZONE") 

# 1st scatterplot
ggplot(data) +
  geom_point(mapping=aes(x=MALES, y=FEMALES)) # , color=SITE_NAME

# 1st scatterplot
ggplot(data) +
  geom_point(mapping=aes(x=log(AGE_0_14), y=log(AGE_60_))) # , color=SITE_NAME

# 1st histogram
ggplot(data) +
  geom_histogram(mapping=aes(x=POPULATION), boundary=0) # , , y=OZONE

# 2nd histogram
ggplot(data, mapping=aes(x=POP_DENSIT)) +
  geom_histogram(bins=30, boundary=0) # , , y=

# 1st boxplot
ggplot(data) +
  geom_boxplot(mapping=aes(x=PP_CATEGOR, y=POPULATION))

# 2nd boxplot
ggplot(data) +
  geom_boxplot(mapping=aes(x=PP_CATEGOR, y=GDP)) # , , y=

# sel = test_select_df %>%
#   select(SOVEREIGNT,POP_EST)
# 
# sel = test_select_df %>%
#   select('SOVEREIGNT','POP_EST') %>%
#   arrange(-desc(SOVEREIGNT)) %>%
#   mutate(world = sum(as.numeric(POP_EST))) %>%
#   mutate(pct = 100*POP_EST/world) 
# 
# class(sel)

library("PerformanceAnalytics")

x11()
chart.Correlation(data[,5:9], histogram=TRUE, pch=19)

# col=data$PP_CATEGOR


################
## NO LOOPING ##
#####################
# Data Analysing Part!!!

library(sp)
#####################

test_spdf = arc.data2sp(sel[[2]])

# test2_spdf = arc.data2sp(sel2)

arcgis_df <- arc.sp2data(test_spdf)
# arcgis2_df <- arc.sp2data(test2_spdf)

class(arcgis_df)

arc.write('C:/000_LPA/Data/test.gdb/Y_NEWTEST',arcgis_df, 
          shape_info = arc.shapeinfo(ldf[[2]]))

# arc.write('C:/000_LPA/Data/test.gdb/test2',arcgis2_df, 
#           shape_info = arc.shapeinfo(test_df))

