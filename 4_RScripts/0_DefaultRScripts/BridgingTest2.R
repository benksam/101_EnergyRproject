

################
## LOOPING ##
#####################
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

# BATCH READING fall files at once!
# Preparation for looping over List of shp! 
list = list.files(path = "C:/LPAShpData", pattern = ".[shp]")
list

shp = which(grepl('.shp$', list))

list_shp = list[shp]
list_shp

list = paste("C:/LPAShpData/",list_shp,sep="")
list


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

sel[[1]]


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
  select('SOVEREIGNT','POP_EST') %>%
  View()


glimpse(test_select_df)

sel = test_select_df %>%
  select(SOVEREIGNT,POP_EST)

sel = test_select_df %>%
  select('SOVEREIGNT','POP_EST') %>%
  arrange(-desc(SOVEREIGNT)) %>%
  mutate(world = sum(as.numeric(POP_EST))) %>%
  mutate(pct = 100*POP_EST/world) 

class(sel)


data = sel[[1]] 

# %>%
# select("SITE_NAME","ELEVATION","OZONE") 

# 1st scatterplot
ggplot(data) +
  geom_point(mapping=aes(x=log(POP_EST), y=POP_RANK)) # , color=SITE_NAME

# 1st scatterplot
ggplot(data) +
  geom_point(mapping=aes(x=GDP_MD_EST, y=POP_EST)) # , color=SITE_NAME

# 1st histogram
ggplot(data) +
  geom_histogram(mapping=aes(x=POP_EST), boundary=0) # , , y=OZONE

# 2nd histogram
ggplot(data, mapping=aes(x=GDP_MD_EST)) +
  geom_histogram(bins=30, boundary=0) # , , y=

# 1st boxplot
ggplot(data) +
  geom_boxplot(mapping=aes(x=INCOME_GRP, y=GDP_MD_EST))

# 2nd boxplot
ggplot(data) +
  geom_boxplot(mapping=aes(x=INCOME_GRP, y=POP_EST)) # , , y=



library("PerformanceAnalytics")

x11()
chart.Correlation(data[,37:39], histogram=TRUE, pch=19, col=data$INCOME_GRP)

# col=data$PP_CATEGOR



################
## NO LOOPING ##
#####################
# Data Analysing Part!!!

library(sp)
#####################

test_spdf = arc.data2sp(sel[[1]])

# test2_spdf = arc.data2sp(sel2)

arcgis_df <- arc.sp2data(test_spdf)
# arcgis2_df <- arc.sp2data(test2_spdf)

class(arcgis_df)

arc.write('C:/000_LPA/Data/test.gdb/X_NEWTEST',arcgis_df, 
          shape_info = arc.shapeinfo(ldf[[1]]))

# arc.write('C:/000_LPA/Data/test.gdb/test2',arcgis2_df, 
#           shape_info = arc.shapeinfo(test_df))

