
# UNIQUEMENT ACP - Complete Cases (No NA) et MISSMDA (50 pays importants)

library(readxl)
library(tidyverse)
library(PerformanceAnalytics)
library(FactoMineR)
library(missMDA)
# Missing values map
library(Amelia)

?readxl

# Income, HDI, Energy and Emissions
# Try just reading the file without other arguments

Energy_gdp <- read_excel("1_RawData/PCA_Cluster_Energy_Emissions.xlsx", 
                                           sheet = 1)
View(Energy_gdp)

### Getting Income and Region labels!

write.csv(Energy_gdp[,1:4], "Regions_Income.csv")

?write.csv
          
Energy_hdi <- read_excel("1_RawData/PCA_Cluster_Energy_Emissions.xlsx", 
                         sheet = 2)

View(Energy_hdi)

Energy_nrj <- read_excel("1_RawData/PCA_Cluster_Energy_Emissions.xlsx", 
                         sheet = 3)

View(Energy_nrj)


Energy_co2 <- read_excel("1_RawData/PCA_Cluster_Energy_Emissions.xlsx", 
                         sheet = 4)

View(Energy_co2)


Energy_m1 = Energy_gdp %>% 
  left_join(Energy_nrj)

Energy_m2 = Energy_m1 %>% 
  left_join(Energy_co2)

Energy_m3 = Energy_m2 %>% 
  left_join(Energy_hdi)

glimpse(Energy_m3)

colnames(Energy_m3)

Energy = Energy_m3 %>% 
  select(-c(21:22))

colnames(Energy)

summary(Energy)

glimpse(Energy)


# create a missing map
missmap(Energy, col=c("black", "grey"), legend=FALSE, 
        x.cex = 0.5,
        y.labels)
?missmap

#####################################
# Trying to Reduce the Data Set
#####################################

HighPop = Energy %>% 
  filter(Population > 100000000)

View(HighPop)

EnergyRed = Energy %>% 
  select(-c(21,22))

glimpse(EnergyRed)

rownames(EnergyRed)

# create a missing map
x11()
missmap(EnergyRed, col=c("black", "grey"), legend=FALSE, 
        x.cex = 0.3,
        rank.order = F)

colnames(EnergyRed)
row.names(EnergyRed)
EnergyRed$`Country Name`

EnergyRed %>% 
  select(c(1:2,11,16:17)) %>% 
  View()

sel = c('Afghanistan', 'Angola', 'Albania', 'United Arab Emirates', 'Argentina', 
'Armenia', 'Australia', 'Austria', 'Azerbaijan', 
'Burundi', 'Belgium', 'Benin', 'Burkina Faso', 'Bangladesh', 'Bulgaria', 'Bahrain',  
'Bosnia and Herzegovina', 'Belarus', 'Belize', 'Bolivia (Plurinational State of)', 'Brazil',
'Botswana', 'Central African Republic', 'Canada', 'Switzerland', 'Chile', 'China', "Côte d'Ivoire", 
'Cameroon', 'Democratic Republic of the Congo', 'Congo', 'Colombia', 'Costa Rica', 'Cuba', 
'Czech Republic', 'Germany', 'Djibouti', 'Dominica', 'Denmark', 'Dominican Republic', 'Algeria', 'Ecuador', 'Egypt', 
'Eritrea', 'Spain', 'Estonia', 'Ethiopia', 'Finland', 'Fiji', 'France', 
'Gabon', 'United Kingdom of Great Britain and Northern Ireland', 'Georgia', 'Ghana', 
'Greece', 'Guatemala', 'China, Hong Kong Special Administrative Region', 'Honduras',
'Croatia', 'Haiti', 'Hungary', 'Indonesia', 'India', 'Ireland', 'Iran (Islamic Republic of)', 'Iraq', 'Iceland', 'Israel', 'Italy',
'Jamaica', 'Jordan', 'Japan', 'Kazakhstan', 'Kenya', 'Kyrgyzstan', 'Cambodia', 'Republic of Korea', 'Kuwait', 
"Lao People's Democratic Republic", 'Lebanon', 'Liberia', 'Libya', 'Sri Lanka', 'Lithuania', 'Luxembourg', 'Latvia', 'Namibia',
'Morocco', 'Madagascar', 'Mexico', 'Mali', 'Malta',
'Montenegro', 'Mongolia', 'Mozambique', 'Mauritania', 'Mauritius', 'Malawi', 'Malaysia', 'Namibia', 'Niger', 
'Nigeria', 'Nicaragua', 'Netherlands', 'Norway', 'Nepal', 'New Zealand', 'Oman', 'Pakistan', 'Panama', 'Peru', 'Philippines',
'Poland', "Democratic People's Republic of Korea", 'Portugal', 'Paraguay', 'Qatar',
'Romania', 'Russian Federation', 'Rwanda', 'Saudi Arabia', 'Sudan', 'Senegal', 'Singapore', 'Sierra Leone', 'El Salvador',
'Serbia', 'Slovakia', 'Slovenia', 'Sweden', 'Syrian Arab Republic', 'Chad', 'Togo', 'Thailand', 'Tajikistan', 'Turkmenistan',
'Tunisia', 'Turkey', 'United Republic of Tanzania', 'Uganda', 'Ukraine', 'Uruguay', 'United States of America', 'Uzbekistan', 
'Viet Nam', 'South Africa', 'Zambia')

vec = which(EnergyRed$'Country Name' %in% sel)

EnergyRed = EnergyRed[vec,]

EnergyRed %>% 
  select(c(1:2,11,16:17)) %>%
  View()

# create a missing map
x11()
missmap(EnergyRed, col=c("black", "grey"), legend=FALSE, 
        x.cex = 0.3,
        rank.order = F)

###############################
# Full - Complete cases = 110!
###############################

Full = Energy %>% 
  complete.cases(Energy) %>% 
  which(complete.cases(Energy)==TRUE) 

length(Full)

Energy_full = Energy %>% 
  filter(complete.cases(Energy))

summary(Energy_full)

colnames(Energy_full)
rownames(Energy_full)



Energy_full_df = as.data.frame(Energy_full)

### Saving FULL data!
write.csv(Energy_full, "3_TidyData.Energy_full.csv")

rownames(Energy_full_df) = Energy_full_df$`Country Code`
str(Energy_full_df[,c(4,6,8:23)])


# Total Energy correlations
x11()
chart.Correlation(Energy_full[,c(6,8:23)], histogram=TRUE, pch=19)

library(gdata)

# Top Correlations
corm <- cor(Energy_full[,c(6,8:23)])
class(corm)

corm[lower.tri(corm)] <- 0
corm[lower.tri(corm,diag=TRUE)] <- 0

cor <- as.data.frame(as.table(corm))
# cor <- as.matrix(as.table(corm))
high<-subset(cor, abs(Freq) > 0.6)
as.matrix(high[order(-high[,3]),])



head(Energy_full_df[,c(3:4,6,8:23)],1)

rownames(Energy_full_df)

#########
# PCA ###
#########


# REMOVING GDP Ranking - PCA and CAH with Kommune NUMBER - IMPOSED ncp=6 instead of Inf for CAH!!!
res <- PCA(Energy_full_df[,c(3:4,6,8:23)], quali.sup=c(1:2), 
           scale.unit=TRUE, graph = F, col.w = NULL) # ncp=6 quanti.sup=c(10:29), ind.sup = c(2, 19, 20, 27, 52, 82, 83, 92, 95), 

?PCA

x11()
round(res$eig,2)
barplot(res$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res$eig), sep=""))

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

# condes(prot, num.var=1) # Description de la var. continue par autres variables (cor/eta2)

# Cluster Analysis
# res.hcpc <- HCPC(res, graph=T)
x11()
res.hcpc <- HCPC(res, kk = Inf, graph=T) # , ncp = 4

names(res.hcpc )

x11()
plot(res.hcpc, axes=1:2)

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
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=T, rect = F, autoLab="yes")

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)


Clust = res.hcpc$call$X$clust
Energy_full_df$Cluster = Clust


##################################################################################################
######################## IMPUTATIONS before merging with regions  ##########
##################################################################################################

View(EnergyRed)

rownames(EnergyRed) = EnergyRed$'Country Code'

glimpse(EnergyRed)
class(EnergyRed)

# IMPUTING BOTH ACTIVE and PASSIVE variables - Productivity in MERGED in the end NOT IMPUTTED!!!
library(missMDA)

EnergyRed_df = as.data.frame(EnergyRed)
class(EnergyRed_df)

nb <- estim_ncpPCA(EnergyRed_df[,c(6,8:21)], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

# nbX <- estim_ncpPCA(Educ[,20:38], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
# nbX

comp <- imputePCA(EnergyRed_df[,c(6,8:21)], ncp=3, scale=TRUE) ## Compl?te le tableau -c(121,206)

# compX <- imputePCA(Educ[,20:38], ncp=2, scale=TRUE) ## Compl?te le tableau -c(121,206)


?imputePCA

dev.off()

mi <- MIPCA(EnergyRed_df[,c(6,8:21)], scale = TRUE, ncp=3) # -c(121,206)
plot(mi)

dev.off()

# mi <- MIPCA(Educ[,20:38], scale = TRUE, ncp=2) # -c(121,206)
# plot(mi)

summary(comp$completeObs)
class(comp$completeObs)


############
# Problem: 15 rows (33 obs) IMPUTED with NEGATIVE GDP, Consumptions, .... Set to 1!!!
############

data_neg = as.data.frame(comp$completeObs)


# Variables with negative values! 

# Testing different neg_alternatives
data_negx = data_neg
str(data_neg)

str(data_neg)
which(data_neg$'GDP (Mio USD)'<0)

data_neg[data_neg < 0]
data_neg[data_neg < 0] <- 1

# WORKED !!!
has.neg <- apply(data_neg, 1, function(row) any(row < 0))
class(has.neg)

# rows with negative values
which((as.vector(has.neg) == TRUE))
length(which((as.vector(has.neg) == TRUE)))


# GDP (Mio USD), GDP per capita,   Electricity Consumption per capita (kWh)
# Energy use (kg oil eq) per capita CO2 emissions (kt) CO2 emissions (tons per capita)

# 2,3,4,
# 11,12,13

#  GDP (Mio USD) - second lowest = 806.7
sort(data_neg[,2])
which(data_neg[,2] < 0)
data_neg[44,2] <- 100

# GDP per capita - second lowest = 647.900
sort(data_neg[,3])
which(data_neg[,3] < 0)
data_neg[44,3] <- 100

# Electricity Consumption per capita (kWh) - second lowest = 38.9700
sort(data_neg[,4])
which(data_neg[,4] < 0)

for (i in which(data_neg[,4] < 0)){
  data_neg[i, 4] <- 10.0000
} 

# Energy use (kg oil eq) per capita - second lowest = 150.7300
sort(data_neg[,11])
which(data_neg[,11] < 0)

for (i in which(data_neg[,11] < 0)){
  data_neg[i, 11] <- 50.0000
} 

# CO2 emissions (kt) - second lowest = 135.68
sort(data_neg[,12])
which(data_neg[,12] < 0)
data_neg[44,12] <- 50.00

# CO2 emissions (tons per capita) - second lowest = 0.040000
sort(data_neg[,13])
which(data_neg[,13] < 0)
data_neg[44,13] <- 0.010000
data_neg[122,13] <- 0.010000


Energy_Comp = as.tibble(data_neg)
summary(Energy_Comp)


EnergyRed_Comp = bind_cols(EnergyRed[,1:4],Energy_Comp)

EnergyRed_Comp_df = as.data.frame(EnergyRed_Comp)
rownames(EnergyRed_Comp_df) <- EnergyRed_Comp_df$`Country Code`
str(EnergyRed_Comp_df)
View(EnergyRed_Comp_df)

summary(EnergyRed_Comp)

# Still a few NA in categorical variables for Eritrea!
which(is.na(EnergyRed_Comp_df))

unique(EnergyRed_Comp_df$Region)
unique(EnergyRed_Comp_df$IncomeGroup)

which(is.na(EnergyRed_Comp_df$Region))
which(is.na(EnergyRed_Comp_df$IncomeGroup))

# NA = Sub-Saharan Africa, Low income
EnergyRed_Comp_df[44,]

EnergyRed_Comp_df$Region[44] = "Sub-Saharan Africa"
EnergyRed_Comp_df$IncomeGroup[44] = "Low income"

# head(comp$completeObs)
# str(comp$completeObs)
# 
# cpt_obs = as.data.frame(comp$completeObs)
# head(cpt_obs)
# str(cpt_obs)
# summary(cpt_obs)



### Saving OBS_Completed data!
write.csv(EnergyRed_Comp_df, "EnergyRed_Compl.csv")

res <- PCA(EnergyRed_Comp_df[,c(3:19)], quali.sup=c(1:2), 
           scale.unit=TRUE, graph = F, col.w = NULL) # ncp=6 quanti.sup=c(10:29), ind.sup = c(2, 19, 20, 27, 52, 82, 83, 92, 95), 

?PCA

x11()
round(res$eig,2)
barplot(res$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res$eig), sep=""))


# # Create a `date` variable
# cpt_obs$Region = rownames(cpt_obs)
# # Reset the `rownames` of your original data
# rownames(cpt_obs) = NULL

# Educ_cpt = read.csv("PCA-ALL-AGES-EDUC-GENDER_sh_COMPLETE.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1

# cptX_obs = as.data.frame(compX$completeObs)
# head(cptX_obs)
# str(cptX_obs)


# Educ_cpt = cbind(cpt_obs, cptX_obs, Educ[,39])
# str(Educ_cpt)
# summary(Educ_cpt)

# RENAME A COLUMN
# colnames(Educ_cpt)[39] <- "Geo"


# abline(h=100/9)

estim_ncp(EnergyRed_Comp_df[,c(5:19)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp

x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20") # ,  ylim=c(-10,10), xlim=c(-10,10))
x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20", habillage=2) 
x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20", axes = 2:3, habillage=2)

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

# condes(prot, num.var=1) # Description de la var. continue par autres variables (cor/eta2)

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


Clust = res.hcpc$call$X$clust
EnergyRed_Comp_df$Cluster = Clust

str(EnergyRed_Comp_df)




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

glimpse(geo)

geodata = geo %>% 
  left_join(EnergyRed_Comp_df, by = "Country Code")

CO2$Cluster = geodata$Cluster

#####################
library(sp)
#####################

CO2_spdf = arc.data2sp(CO2)

# test2_spdf = arc.data2sp(sel2)

CO2_df <- arc.sp2data(CO2_spdf)
# arcgis2_df <- arc.sp2data(test2_spdf)

class(CO2_df)

arc.write('C:/101_EnergyRproject/0_Gdb/Output.gdb/CO2_Emissions_WB_MissMDA_Clust2', CO2_df, 
          shape_info = arc.shapeinfo(ldf[[1]]))


