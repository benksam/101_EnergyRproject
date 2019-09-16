
# UNIQUEMENT ACP - Complete Cases (No NA) et MISSMDA (50 pays importants)

library(readxl)
library(readr)
library(tidyverse)
library(PerformanceAnalytics)
library(FactoMineR)
# Missing values map
library(Amelia)


##########################################################################
# Working with FULL data (110 FULL Observations - Complete Cases) from Energy.R
##########################################################################

Energy_full = read.csv("3_TidyData/Energy_full.csv",
                       header=TRUE,sep=",", dec=".", 
                       row.names = 1)  # ROW 1 NAME as 1st column
str(Energy_full)
tail(Energy_full)
View(Energy_full)

colnames(Energy_full)
rownames(Energy_full) <- Energy_full$'Country Code'
  
Energy_full <- Energy_full[,c(1:4,6,8:9,21:23,10:17,18:20)]
colnames(Energy_full)

Energy_full <- Energy_full[,c(1:7,9:21)]
colnames(Energy_full)
rownames(Energy_full) <- Energy_full$'Country.Code'

str(Energy_full[,c(3:20)])

#######################
##### AFM and CAH  # (Hierarchical Classification with AFM not tried yet!!!)
#######################


dev.off()

res <- MFA(Energy_full[,c(3:20)], group=c(1,1,2,3,8,3), type=c(rep("n",2),rep("s",4)),
           ncp=4, name.group=c("Region","IncomeGroup","Desc","Income","Energy","Emmissions"),
           num.group.sup=c(1:3))

# R?sum? des r?sultats
summary(res)


######
# ...
######


##################
# Cluster Analysis
##################

# res.hcpc <- HCPC(res, graph=T)
x11()
res.hcpc <- HCPC(res, kk = Inf, graph=T) # , ncp = 4

names(res.hcpc )

x11()
plot(res.hcpc, axes=1:2)
x11()
plot(res.hcpc, axes=3:4)

HCPC(res)   # Results for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **Results for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res$eig
head(round(res$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

# catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**


# catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)


res.hcpc
res.hcpc$call
res.hcpc$desc.var  # Description des classes de HCPC**  / res.hcpc$desc.var$test.chi2 
res.hcpc$desc.var$category
res.hcpc$desc.var$quanti.var
res.hcpc$desc.var$quanti 

res.hcpc$desc.axe    # Description des classes par Facteurs Principaux**


x11()
plot(res.hcpc, axes=c(1,2), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(1,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes") # rect = F, 

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)


# Clust = res.hcpc$call$X$clust
# Energy_fullMFA = Energy_full
# Energy_fullMFA$Cluster = Clust
# 
# str(Energy_fullMFA)
# colnames(Energy_fullMFA)[1] <- "Country Code"

# Energy_fullMFA$Country.Code



##############################################
### Copying Clusters for MAP Vizualisation
##############################################

Energy_fullMFA = Energy_full

dataClust = as.data.frame(res.hcpc$call$X)
rownames(dataClust)

dataClust = dataClust %>% 
  mutate(Code = rownames(dataClust)) %>% 
  arrange(Code)

# sort(res.hcpc$call$X)
# order(test[,1])

dataClust$Code == Energy_fullMFA$'Country.Code' 

Clust = dataClust$clust
Energy_fullMFA$Cluster = Clust

View(Energy_fullMFA)

str(Energy_fullMFA)

colnames(Energy_fullMFA)[1] <- "Country Code"
# colnames(Energy_fullMFA)[1] <- "Country Code"

# Energy_fullMFA$Country.Code




###########################
## LOOPING in FC Vs .shp ##
###########################
# Data IMPORT Part!!!

library(arcgisbinding)
arc.check_product()

#####################

lpython = c("CO2_Emissions_WB")

# ('ca_cities', 'ca_outline', 'O3_Sep06_3pm')

# ('destination', 'rec_sites', 'roads', 'schools')

list = paste("C:/101_EnergyRproject/0_Gdb/Input.gdb/",lpython,sep="")
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

head((sel[[1]]))

CO2 = sel[[1]]

geo <- as.data.frame(CO2) %>%
  select("GMI_CNTRY", "Year_2013") %>%
  arrange(GMI_CNTRY) %>% 
  mutate('Country Code' = GMI_CNTRY) 



View(geo)

glimpse(geo)

geodata = geo %>% 
  left_join(Energy_fullMFA, by = "Country Code")

glimpse(geodata)

View(geodata)

CO2 = CO2 %>% 
  arrange(GMI_CNTRY)

View(CO2)

CO2$GMI_CNTRY == geodata$GMI_CNTRY

which(!(is.na(geodata$'Cluster')))

# clu = which(CO2$GMI_CNTRY %in% geodata$GMI_CNTRY)
clu = which(!(is.na(geodata$'Cluster')))
length(clu)


length(CO2$GMI_CNTRY)
length(geodata$GMI_CNTRY)

CO2$Cluster = NA

# CO2[clu,"Cluster"] = CO2[clu,geodata$Cluster]
CO2$Cluster = geodata$Cluster




####################
###################
##################


#####################
library(sp)
#####################

CO2_spdf = arc.data2sp(CO2)

# test2_spdf = arc.data2sp(sel2)

CO2_df <- arc.sp2data(CO2_spdf)
# arcgis2_df <- arc.sp2data(test2_spdf)

class(CO2_df)

arc.write('C:/101_EnergyRproject/0_Gdb/Output.gdb/CO2_Emissions_WB_FULL_MFA_Clust000', CO2_df, 
          shape_info = arc.shapeinfo(ldf[[1]]))


##################################################
# Working with COMPLETED data (50 or so  COMPLETED Observations from Energy.R) 
##################################################

Energy_comp = read.csv("3_TidyData/EnergyRed_Compl.csv",
                       header=TRUE,sep=",", dec=".", 
                       row.names = 1)  # ROW 1 NAME as 1st column
str(Energy_comp)
tail(Energy_comp)
View(Energy_comp)

colnames(Energy_comp)
colnames(Energy_comp)[1] <- 'Country Code'

rownames(Energy_comp) <- Energy_comp$'Country Code'

Energy_comp <- Energy_comp[,c(1:7,19,8:18)]
colnames(Energy_comp)

# Energy_comp <- Energy_comp[,c(1:7,9:21)]
# colnames(Energy_comp)
# rownames(Energy_comp) <- Energy_comp$'Country.Code'

str(Energy_comp[,c(3:19)])

?summary


# GGplots Graph - To be reviewed!!!

plotCols0 <- c("Energy.use.per.capita..kg.oil.eq.",
               "CO2.emissions..tons.per.capita.")


ggbox <- function(x) {
  title <- paste("Scatterplot af", x, "med Produktivitetn p? tv?rs af bestemte EU-regioner")
  ggplot(Energy_comp, aes(x, y=GDP.per.capita)) +
    geom_point() + 
    facet_grid(. ~ Income.Group) +
    # stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
    # stat_smooth(aes(group = 1)) +
    theme_bw()
}

lapply(plotCols0, ggbox)

# Customize upper panel
lower.panel<-function(x, y){
  points(x,y, pch=19, col=c("red", "green", "blue", "grey")[Energy_comp$Income.Group])
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(Energy_comp[,c(6,15,17)], upper.panel = NULL, 
      lower.panel = lower.panel)

levels(Energy_comp$Income.Group)
# "High income" "Low income""Lower middle income" "Upper middle income"
# "red", "green", "blue", "grey"

?pairs

summary(Energy_comp$Income.Group)

###########################
# For R Notebook
###########################
write.csv(Energy_full, "3_TidyData.Energy_full.csv")
write.csv(Energy_comp, "EnergyCompleted_Temp")

# For R Notebook
Energy_comp = read.csv("3_TidyData/EnergyCompleted_Final.csv",
                       header=TRUE,sep=",", dec=".", 
                       row.names = 1)
colnames(Energy_comp)
###


#######################
##### AFM and CAH  # (Hierarchical Classification with AFM not tried yet!!!)
#######################

res <- MFA(Energy_comp[,c(3:19)], group=c(1,1,2,2,8,3), type=c(rep("n",2),rep("s",4)),
           ncp=4, graph=T, name.group=c("Region","IncomeGroup","Desc","Income","Energy","Emmissions"),
           num.group.sup=c(1:3))


x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), repel = T)

###########################
# For R Notebook - No country Code Column
###########################
res <- MFA(Energy_comp[,c(2:18)], group=c(1,1,2,2,8,3), type=c(rep("n",2),rep("s",4)),
           ncp=4, graph=T, name.group=c("Region","IncomeGroup","Desc","Income","Energy","Emmissions"),
           num.group.sup=c(2:3))

x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), repel = T)


# R?sum? des r?sultats
summary(res)


x11()
round(res$eig,2)
barplot(res$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res$eig), sep=""))

# Description des dimensions
dimdesc(res)
dimdesc(res, c(4))



# abline(h=100/9)

# estim_ncp(Energy_full[,c(6:23)], scale = TRUE, method = "Smooth")     # suggestion de NCP
# ?estim_ncp

x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20") # ,  ylim=c(-10,10), xlim=c(-10,10))
x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20", habillage=2) 
x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20", axes = 3:4, habillage=2)

# plot.PCA(res, choix="ind", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
#          ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
# plot.PCA(res, choix="ind", invisible = "ind.sup", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
#          ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)



x11()
plot.PCA(res, choix="var", cex=0.6) # , invisible = "var" select="contrib 10") 
x11()
plot.PCA(res, choix="var",  axes = 3:4, cex=0.6) # select="contrib 10", invisible = "var", 
# 
# x11()
# plot.PCA(res, choix="var", invisible = "var",  axes = 5:6)
# x11()
# plot.PCA(res, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res)
# dimdesc(res, axes = 4:6)
# dimdesc(res, axes = 7:9)

?dimdesc
?catdes

catdes(Energy_full_df[,c(3:4,6:23)], num.var=2) # Description de la var. categorielle par autres var.**
catdes(Energy_full_df[,c(3:4,6:23)], num.var=1)


##################
# Cluster Analysis
##################

# res.hcpc <- HCPC(res, graph=T)
x11()
res.hcpc <- HCPC(res, kk = Inf, graph=T) # , ncp = 4

names(res.hcpc )

x11()
plot(res.hcpc, axes=1:2)
x11()
plot(res.hcpc, axes=3:4)

HCPC(res)   # Results for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **Results for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res$eig
head(round(res$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

# catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**


# catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)


res.hcpc
res.hcpc$call
res.hcpc$desc.var  # Description des classes de HCPC**  / res.hcpc$desc.var$test.chi2 
res.hcpc$desc.var$category
res.hcpc$desc.var$quanti.var
res.hcpc$desc.var$quanti 

res.hcpc$desc.axe    # Description des classes par Facteurs Principaux**


x11()
plot(res.hcpc, axes=c(1,2), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(1,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes") # rect = F, 

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)


##############################################
### Copying Clusters for MAP Vizualisation
##############################################

Energy_compMFA = Energy_comp

dataClust = as.data.frame(res.hcpc$call$X)
rownames(dataClust)

dataClust = dataClust %>% 
  mutate(Code = rownames(dataClust)) %>% 
  arrange(Code)

# sort(res.hcpc$call$X)
# order(test[,1])

dataClust$Code == Energy_compMFA$'Country Code' 

Clust = dataClust$clust
Energy_compMFA$Cluster = Clust

View(Energy_compMFA)

str(Energy_compMFA)
# colnames(Energy_compMFA)[1] <- "Country Code"

# Energy_fullMFA$Country.Code


###########################
## LOOPING in FC Vs .shp ##
###########################
# Data IMPORT Part!!!

library(arcgisbinding)

arc.check_product()

#####################

lpython = c("CO2_Emissions_WB")

# ('ca_cities', 'ca_outline', 'O3_Sep06_3pm')

# ('destination', 'rec_sites', 'roads', 'schools')

list = paste("C:/101_EnergyRproject/0_Gdb/Input.gdb/",lpython,sep="")
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

head((sel[[1]]))

CO2 = sel[[1]]

geo <- as.data.frame(CO2) %>%
  select("GMI_CNTRY", "Year_2013") %>%
  arrange(GMI_CNTRY) %>% 
  mutate('Country Code' = GMI_CNTRY) 

View(geo)

glimpse(geo)

geodata = geo %>% 
  left_join(Energy_compMFA, by = "Country Code")

glimpse(geodata)

View(geodata)

CO2 = CO2 %>% 
  arrange(GMI_CNTRY)

View(CO2)

CO2$GMI_CNTRY == geodata$GMI_CNTRY

which(!(is.na(geodata$'Cluster')))

# clu = which(CO2$GMI_CNTRY %in% geodata$GMI_CNTRY)
clu = which(!(is.na(geodata$'Cluster')))
length(clu)


length(CO2$GMI_CNTRY)
length(geodata$GMI_CNTRY)

CO2$Cluster = NA

# CO2[clu,"Cluster"] = CO2[clu,geodata$Cluster]
CO2$Cluster = geodata$Cluster

#####################
library(sp)
#####################

CO2_spdf = arc.data2sp(CO2)

# test2_spdf = arc.data2sp(sel2)

CO2_df <- arc.sp2data(CO2_spdf)
# arcgis2_df <- arc.sp2data(test2_spdf)

str(CO2_df)

class(CO2_df)

arc.write('C:/101_EnergyRproject/0_Gdb/Output.gdb/CO2_Emissions_WB_COMP_MFA_Clust000', CO2_df, 
          shape_info = arc.shapeinfo(ldf[[1]]))

