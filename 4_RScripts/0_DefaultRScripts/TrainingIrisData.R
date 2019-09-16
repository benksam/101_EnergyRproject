

# Histogram functions
# load the data
data(iris)
View(iris)

anyDuplicated(iris)
which(duplicated(iris))


# create histograms for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  hist(iris[,i], main=names(iris)[i])
}


# # load libraries
# library(lattice)

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


# # load the library
# library(caret)

# # load the data
# data(iris)
# # density plots for each attribute by class value
# x <- iris[,1:4]
# y <- iris[,5]
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# featurePlot(x=x, y=y, plot="density", scales=scales)


chart.Correlation(iris[,1:4], histogram=TRUE, pch=19, col=iris$Species)
chart.Correlation(iris[,1:4], histogram=TRUE, pch="+")

?chart.Correlation

# pair-wise scatterplots colored by class
pairs(Species~., data=iris, col=iris$Species)


plotCols <- names(iris)[1:4]

# box fill: aes(fill = factor(TYP))

ggbox <- function(x) {
  title <- paste("Box plot of", x, "Area")
  ggplot(iris, aes_string('factor(Species)', x)) +
    geom_boxplot() +
    #    coord_flip() +
    ggtitle(title)
}

lapply(plotCols, ggbox)


# Combinations of size 2 for scatterplot (or correlation)!
list = combn(plotCols, 2, simplify = FALSE)
lapply(list, '[[', 2)
list[[1]][1]

# Function for looping scatter plots NOT working

for (i in 1:length(list)) {
  # print(list[[i]][1])
  # print(list[[i]][2])

  ggplot(iris, aes(list[[i]][1], list[[i]][2])) +
    geom_point(size = 3.5) + # aes(color = factor(Species), 
    # geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
    # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
    theme_bw() + theme(legend.position = "none") +
    scale_color_manual(values = c("red", "darkblue","green"))   + 
    # geom_text_repel(aes(label = Emp_Age_Prop_Pr_Y[,1], color = factor(Emp_Age_Prop_Pr_Y[,10])),size = 3.5) +
    xlab("Besk?ftigelsesratio") +
    ylab("Produktivitetsv?kstraten per besk?ftiget (pct.)") +
    ggtitle("Samlet besk?ftigelsesrate i alderen 15_24") + 
    theme(plot.title = element_text(hjust = 0.5))
}


# Combination function
# Loop with pairs
# GRID from gridExtra



a = ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = factor(Species), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue","green"))   + 
  # geom_text_repel(aes(label = Emp_Age_Prop_Pr_Y[,1], color = factor(Emp_Age_Prop_Pr_Y[,10])),size = 3.5) +
  xlab("Besk?ftigelsesratio") +
  ylab("Produktivitetsv?kstraten per besk?ftiget (pct.)") +
  ggtitle("Samlet besk?ftigelsesrate i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

b = ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point(aes(color = factor(Species), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue","green"))   + 
  # geom_text_repel(aes(label = Emp_Age_Prop_Pr_Y[,1], color = factor(Emp_Age_Prop_Pr_Y[,10])),size = 3.5) +
  xlab("Besk?ftigelsesratio") +
  ylab("Produktivitetsv?kstraten per besk?ftiget (pct.)") +
  ggtitle("Samlet besk?ftigelsesrate i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

require(gridExtra)

x11()
grid.arrange(a, b,  ncol= 2)
