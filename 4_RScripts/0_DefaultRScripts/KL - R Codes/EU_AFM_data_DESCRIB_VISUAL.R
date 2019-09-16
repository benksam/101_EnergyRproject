


library(FactoMineR)
library(ggplot2)
library("factoextra")
library("dplyr")
library(ggrepel)

library(reshape2)

library("PerformanceAnalytics")
library(psych)
library(lattice)
library(tabplot)

# Missing values map
library(Amelia)

# load the library for density by attributes
library(caret)

##############################
# DATA MANIPULATION DONE!


AFM<- read.csv("FULL_AFM_NEAt.csv", header = T, row.names = 1)
str(AFM)
names(AFM)

desc<- read.csv("FULL_AFM_NEAt.csv", header = T)
str(desc)

summary(desc)

# create a missing map
missmap(desc[,1:10], col=c("black", "grey"), legend=FALSE)

# Description
describe(desc[,1:10]) 
describeBy(desc[,1:10],desc$Area) 

x11()
chart.Correlation(desc[,3:10], histogram=TRUE, pch=19, col=desc$Area)



# density plots for each attribute by class value
x <- desc[,3:4]
y <- desc[,2] # EU/DK
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


# Histogram functions
# load the data
data(iris)
View(iris)
# create histograms for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  hist(iris[,i], main=names(iris)[i])
}


# load libraries
library(lattice)
# load dataset
data(iris)
# create a panel of simpler density plots by attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  plot(density(iris[,i]), main=names(iris)[i])
}


# load dataset
data(iris)
# Create separate boxplots for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(iris[,i], main=names(iris)[i])
}


# load the library
library(caret)
# load the data
data(iris)
# density plots for each attribute by class value
x <- iris[,1:4]
y <- iris[,5]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)



chart.Correlation(iris[,1:4], histogram=TRUE, pch=19, col=iris$Species)
chart.Correlation(iris[,1:4], histogram=TRUE, pch="+")
?chart.Correlation

# pair-wise scatterplots colored by class
pairs(Species~., data=iris, col=iris$Species)



# Boxplot Function!!!
names(desc)

plotCols <- c("Pre_LTU_pc_act"   ,     "Pre_LTU_pc_ne"    ,     "Pre_EMP_ED0_2"     ,    "Pre_EMP_ED3_4"    ,     "Pre_EMP_ED5_8")

# box fill: aes(fill = factor(TYP))

ggbox <- function(x) {
  title <- paste("Box plot of", x, "Area")
  ggplot(desc[,1:10], aes_string('factor(Area)', x)) +
    geom_boxplot() +
#    coord_flip() +
    ggtitle(title)
}
lapply(plotCols, ggbox)





# 
# # Tableplots
# tableplot(desc[,5:10], sortCol = 5)
# tableplot(desc[,1:10], select = c(2,3,4), sortCol = 2)
# tableplot(desc[,1:10], select = c(2,3,4), subset = Area == "EU", sortCol = 2)
# 
# 
# 
# # diagnostic
# library(ggfortify)
# 
# res <- lm(desc[,7] ~ desc[,8], data = desc[,1:10])  
# mp <- autoplot(res, ncol = 4)
# mp




