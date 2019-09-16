
####################################
# Initial PCA with limited Variables
####################################



## EU Regional DATA - MANY MISSING VALUES => MISSMDA done later!!!



############
# PCA + CAH without MISSING and DUPLICATES, but CORRECTED for NUTS1 and OTHER COuntry level rows - Normal PCA/CAH on REDUCED SAMPLE
# PCA + CAH with MISSING and NO DUPLICATES, but CORRECTED for NUTS1 and OTHER COuntry level rows - MISSMDA
############



###################
# ATTENTION ...
###################  
#### ANALYSE NO OMITTED REFAITE a la fin ... PAS DE NUTS1 AVEC NUTS 2!!! 90 obs a 78 obs (negligenace initialle!!!)
########################




##################
# Extraction des clusters pour density plot separees - fviz marche que sur plans 1 et 2
##################





# remove.packages("FactoMineR")
# remove.packages("ggplot2")
# remove.packages("factoextra")
# remove.packages("dplyr")
# 
# install.packages("FactoMineR", dependencies = TRUE)
# install.packages("FactoXtra", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)


# PBM with factoextra VISUALIZATION TOOL  (fviz) - gmail code!


library(FactoMineR)
library(ggplot2)
library("factoextra")
library("dplyr")

library(reshape2)
library(psych)


####################################
## PCA analysis
####################################

# Merging NEAT DATA and WEIGHTS

# dat<- read.csv("PCA_AVERAGES_Productivity_LAST.csv",header=TRUE,sep=",", dec=".")
# str(dat)
# head(dat)
# tail(dat)
# 
# # Weights per Område
# W_split<- read.csv("Weights_Split.csv",header=TRUE,sep=",", dec=".")
# str(W_split)
# head(W_split)

# merge = merge(dat, W_split, by = c("kom"), all = T)
# tail(merge)
# 
# 
# # write.csv(merge,"PCA_AVERAGES_Productivity_LAST_W.csv")



################
# RAW DATA
################


raw = read.csv("PCA_EU_data.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(raw)
tail(raw)

duplicated(raw)
raw[duplicated(raw), ]

nodup = raw[!duplicated(raw), ]
str(nodup)
tail(nodup)

summary(nodup)


# write.csv(nodup,"EU_NO_DUP.csv")


# NO OMITTED VARIABLES
nodupna = na.omit(nodup)
str(nodupna)

rownames(nodupna)

# write.csv(nodupna,"EU_NO_DUP_NA.csv")



#################################
# OLD part - DONE Smarter with NUTS2-regions (WORD/EXCEL file)
#################################
 


# nodupnaX = subset(nodupna,!(rownames(nodupna)=="DE" | rownames(nodupna)=="DK" | 
#                               rownames(nodupna)=="ES" | rownames(nodupna)=="IT" |
#                               rownames(nodupna)=="PT" | rownames(nodupna)=="SI" |
#                               rownames(nodupna)=="SK"))
# 
# str(nodupnaX)

# IMPERFECT NUTS1 detection and elimination
# BETTER elimination with Excel and Merges (%in% vector)
# which(rownames(nodupna)=="DE")
# which(rownames(nodupna)=="DK")
#       which(rownames(nodupna)=="ES")
#             which(rownames(nodupna)=="IT")
#                   which(rownames(nodupna)=="PT")
#                         which(rownames(nodupna)=="SI")
#                               which(rownames(nodupna)=="SK")
#                               which(rownames(nodupna)=="DK0")
#                               which(rownames(nodupna)=="LV0")
#                               
#                               c(2, 19, 20, 27, 52, 82, 83, 92, 95)
                              
# ########################
# ## CLUSTERS sensible to OUTLIERS (own cluster)
# #########################
# 
# # Outliers in FULL SAMPLE
# which(raw$kom==450)
# # 51
# 
# which(raw$kom==326)
# # 37
# 
# #########################
# # Outliers in ØVRIGE KOMMUNER 
# which(raw$kom==326)
# # 37
# 
# which(raw$kom==450)
# # 51
# 
# which(raw$kom==530)
# # 57
# 
# # ROW 1 NAME as identifier (Kommune number or names)
# data = read.csv("PCA_AVERAGES_Productivity_LAST_W.csv",header=TRUE,sep=",", dec=".", row.names=1) 
# str(data)
# tail(data)
# 
# 
# 
# colnames(data)
# 
# ?PCA
# 
# # which(rownames(data)=="999")
# 
# 
# dataW = read.csv("Cluster_Analysis_Kommune_Navn_NO_OUTLIERS_WEIGHTS.csv",header=TRUE,sep=",", dec=".", row.names=1) 
# str(dataW)
# tail(dataW)


colnames(nodupna)



# PCA and CAH with Kommune NUMBER - IMPOSED ncp=6 instead of Inf for CAH!!!
res <- PCA(nodupna, scale.unit=TRUE, graph = F, col.w = NULL, quanti.sup=c(10:29), ind.sup = c(2, 19, 20, 27, 52, 82, 83, 92, 95), ncp=6) # normalt excluded  37,51, 57, 

# PCA and CAH - IMPOSED ncp=6 instead of ncp=Inf for CAH!!!



# ################################
# # IMPORT with COMPLETED NAME data AND NO OUTLIERS AND WEIGHTS ADJUSTED!!!
# ################################
# 
# 
# colnames(dataW)
# 
# hoved = which(dataW$Region_navn=="Hovedstaden")
# head(hoved)
# 
# rest = which(dataW$Region_navn!="Hovedstaden") # DK included!!!
# head(rest)
# 
# 
# 
# tail(dataW[,c(2:3,4:41)])
# colnames(dataW[,c(2:3,4:41,64)])
# 
# res_red <- PCA(dataW[-c(hoved,93),c(2:3,4:41,64)], scale.unit=TRUE,graph = F, row.w=dataW[-c(hoved,93),64],
#                quali.sup=1:2, quanti.sup=c(18:40), ncp=6) # 37,51, 57, should be excluded
# 
# 
# 
# # WITH NO OUTLIERS FOR CLUSTER/RESULTS ROBUST-CHECK!
# 
# 
# ################################
# # IMPORT with COMPLETED NAME data
# ################################
# 
# 
# data_redX = read.csv("Cluster_Analysis_Kommune_Navn.csv",header=TRUE,sep=",", dec=".", row.names = 1) 
# head(data_redX)
# str(data_redX)
# 
# 
# colnames(data_redX)
# 
# tail(data_redX[,c(2:3,4:41)])
# colnames(data_redX[,c(2:3,4:41)])
# 
# res_red <- PCA(data_redX[,c(2:3,4:41)], scale.unit=TRUE,graph = F, col.w = NULL,
#                quali.sup=1:2, quanti.sup=c(18:40), ind.sup = c(hoved, 96), ncp=6) # 37,51, 57, should be excluded
# 
# Extra summary


centre<-res_red$call$centre
std<-res_red$call$ecart.type
head(rbind(centre,std))

sum_red = as.data.frame(rbind(centre,std))
head(sum_red)

write.csv(sum_red,"KL_PCA_Summary_Stat_REDUCED.csv")




#############################################################
# DONNEES brutes reduites - IMPORTANT!!! POUR INTERPRETATION PCA


data_REDUCED_scaled = scale(data[c(34:95),c(4:60)])
tail(data_REDUCED_scaled)

data_REDUCED = data[c(34:95),c(4:60)]
tail(data_REDUCED)

write.csv(data_REDUCED_scaled,"KL_PCA_Productivity_REDUCED_Scaled.csv")
write.csv(data_REDUCED,"KL_PCA_Productivity_REDUCED.csv")



######
# ACP
######



x11()
round(res$eig,2)
barplot(res$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res$eig), sep=""))
abline(h=100/9)


estim_ncp(data[34:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-10,10), xlim=c(-10,10))
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
plot.PCA(res, choix="ind", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
plot.PCA(res, choix="ind", invisible = "ind.sup", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res, choix="var" , invisible = "var")# , invisible = "var" select="contrib 10") 
x11()
plot.PCA(res, choix="var", invisible = "var",  axes = 3:4) # select="contrib 10", invisible = "var", 
x11()
plot.PCA(res, choix="var", invisible = "var",  axes = 5:6)
x11()
plot.PCA(res, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res)
dimdesc(res, axes = 4:6)
dimdesc(res, axes = 7:9)

?dimdesc
?catdes

catdes(data[1:95,],num.var=1) # Description de la var. categorielle par autres var.**

condes(prot, num.var=1) # Description de la var. continue par autres variables (cor/eta2)



tab_var<-round(cbind(res$var$contrib[,1:6],res$var$coord[,1:6],res$var$cos2[,1:6]),2)
colnames(tab_var)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))


tab_varx<-round(cbind(res$var$contrib[,7:9],res$var$coord[,7:9],res$var$cos2[,7:9]),2)
colnames(tab_varx)=c(rep("contrib",3),rep("coord",3),rep("cos2",3))


tab_var1<-tab_var[order(-tab_var[,1]),]   # axe 1
tab_var2<-tab_var[order(-tab_var[,2]),]   # axe 2
tab_var3<-tab_var[order(-tab_var[,3]),]
tab_var4<-tab_var[order(-tab_var[,4]),]
tab_var5<-tab_var[order(-tab_var[,5]),]
tab_var6<-tab_var[order(-tab_var[,6]),]

tab_varx7<-tab_varx[order(-tab_varx[,1]),]
tab_varx8<-tab_varx[order(-tab_varx[,2]),]
tab_varx9<-tab_varx[order(-tab_varx[,3]),]

tab_var1[1:10,]
tab_var2[1:10,]
tab_var3[1:10,]
tab_var4[1:10,]
tab_var5[1:10,]
tab_var6[1:10,]

tab_varx7[1:10,]
tab_varx8[1:10,]
tab_varx9[1:10,]


# important dimensions
tab_ind<-round(cbind(res$ind$contrib[,1:6],res$ind$coord[,1:6],res$ind$cos2[,1:6]),2)
colnames(tab_ind)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))

# high dimensions
tab_indx<-round(cbind(res$ind$contrib[,7:9],res$ind$coord[,7:9],res$ind$cos2[,7:9]),2)
colnames(tab_indx)=c(rep("contrib",3),rep("coord",3),rep("cos2",3))


tab_ind1<-tab_ind[order(-tab_ind[,1]),]   # axe 6
tab_ind2<-tab_ind[order(-tab_ind[,2]),]   # axe 7
tab_ind3<-tab_ind[order(-tab_ind[,3]),]
tab_ind4<-tab_ind[order(-tab_ind[,4]),]
tab_ind5<-tab_ind[order(-tab_ind[,5]),]
tab_ind6<-tab_ind[order(-tab_ind[,6]),]

tab_indx7<-tab_indx[order(-tab_indx[,1]),]
tab_indx8<-tab_indx[order(-tab_indx[,2]),]
tab_indx9<-tab_indx[order(-tab_indx[,3]),]


tab_ind1[1:10,]
tab_ind2[1:10,]
tab_ind3[1:10,]
tab_ind4[1:10,]
tab_ind5[1:10,]
tab_ind6[1:10,]

tab_indx7[1:10,]
tab_indx8[1:10,]
tab_indx9[1:10,]



# Coordonnes/Projections des individus supplementaires NON PRATIQUER!!!

head(res$ind.sup$coord[,1:9])
head(res$ind.sup$cos2[,1:9])


tab_ind.sup<-round(cbind(res$ind.sup$coord[,1:6],res$ind.sup$cos2[,1:6]),2)
colnames(tab_ind.sup)=c(rep("coord",6),rep("cos2",6))

# high dimensions
tab_ind.supx<-round(cbind(res$ind.sup$coord[,7:9],res$ind.sup$cos2[,7:9]),2)
colnames(tab_ind.supx)=c(rep("coord",3),rep("cos2",3))


tab_ind.sup1<-tab_ind.sup[order(-tab_ind.sup[,1]),]   # 7
tab_ind.sup2<-tab_ind.sup[order(-tab_ind.sup[,2]),]   # 8
tab_ind.sup3<-tab_ind.sup[order(-tab_ind.sup[,3]),] # 9
tab_ind.sup4<-tab_ind.sup[order(-tab_ind.sup[,4]),] # 10
tab_ind.sup5<-tab_ind.sup[order(-tab_ind.sup[,5]),] # 11
tab_ind.sup6<-tab_ind.sup[order(-tab_ind.sup[,6]),] # 12


tab_ind.supx7<-tab_ind.supx[order(-tab_ind.supx[,1]),]
tab_ind.supx8<-tab_ind.supx[order(-tab_ind.supx[,2]),]
tab_ind.supx9<-tab_ind.supx[order(-tab_ind.supx[,3]),]



tab_ind.sup1[1:10,] # +151, 159, 169, 210, 173, 157, 185, 230, 101, 359 / -217, 175, 259, 270
tab_ind.sup2[1:10,] # +151, 161, 153, 269, 250 / -147, 185, 201, 270
tab_ind.sup3[1:10,] # +187, 151, 210, 157 / -190, 161
tab_ind.sup4[1:10,] # +187, 173, 260, 157, 201, 250, 240 / -165, 147
tab_ind.sup5[1:10,] # +151, 101, 259, 219, 159, 165, 169 / - 190
tab_ind.sup6[1:10,] # + 185 / -190, 173, 151, 161, 223, 217, 175, 153

tab_ind.supx7[1:10,] # +185, 269 / -230, 187, 151, 169, 159, 153, 165, 223
tab_ind.supx8[1:10,] # +230, 223, 269, 187, 265, 147 / -260, 219
tab_ind.supx9[1:10,] # +153, 260, 173 / -147, 230



res$eig
res$ind
res$ind.sup
res$var
res$quanti.sup
res$quali.sup


library(PerformanceAnalytics)
x11()
chart.Correlation(nodupna[,c(1:10)], histogram=TRUE, pch=19)
chart.Correlation(data[,c(11:20)], histogram=TRUE, pch=19)
chart.Correlation(data[,c(21:29)], histogram=TRUE, pch=19)


colnames(nodupna)

x11()
# Pre Vs Crisis
chart.Correlation(data[,c(22,23,25,26,28,29,31,32,34,35,37,38,40,41)], histogram=TRUE, pch=19)

x11()
# Crisis Vs Post
chart.Correlation(data[c(34:95),c(23,24,26,27,29,30,32,33,35,36,38,39,41,42)], histogram=TRUE, pch=19)

x11()
# Pre Vs Post
chart.Correlation(data[c(34:95),c(22,24,25,27,28,30,31,33,34,36,37,39,40,42)], histogram=TRUE, pch=19)




############
# CAH
############



# res.hcpc <- HCPC(res, graph=T)
res.hcpc <- HCPC(res, kk = Inf, graph=T)
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


catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)


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
plot(res.hcpc, axes=c(1,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(1,5), choice="map", draw.tree=F, ind.names = T, centers.plot=T, rect = F, autoLab="yes")

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

?fviz_cluster


x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"))

x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), axes = c(1,3))

res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**


# EXTRA


str(res.hcpc$data.clust)

duplicated(res.hcpc$data.clust[,1:29])
res.hcpc$data.clust[duplicated(res.hcpc$data.clust), ]

nodup = res.hcpc$data.clust[!duplicated(res.hcpc$data.clust), ]
str(nodup)
tail(nodup)


write.csv(res.hcpc$call$X,"Cluster_EU.csv")


# FOR GRAPHICAL PURPOSES - DENSITIES ALONG different DIMENSIONS!!! (Visula ANOVA)

# write.csv(res.hcpc$data.clust[,c(1:2,41)],"Cluster_Øvrige.csv")
# write.csv(res.hcpc$call$X,"Cluster_AXES_Øvrige.csv")


CL_EU<- read.csv("Cluster_EU.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(CL_EU)

CL_EU$Cluster = as.factor(CL_EU$Cluster)

CL_EUx = subset(CL_EU, CL_EU$Cluster!=5) # Cluster with 1 obs
CL_EUx = subset(CL_EUx, CL_EU$Cluster!=6)  # Cluster with 1 obs


# CONDENSING - REDUCING FACTOR LEVELS AFTER SUBSETTING
# CL_EU$Cluster = factor(CL_EU$Cluster)
# levels(CL_EU$Cluster)



# Courbe de densité marginale de x (panel du haut)

xdensity <- ggplot(CL_EUx, aes(Dim.5, fill=Cluster)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#000000", "red", "#009E73",  "#0072B2", "#999999")) + # "#CC79A7", 
  theme(legend.position = "none") + theme_bw()
x11()
xdensity


# xdensity <- ggplot(CL_EU, aes(Dim.4, fill=Cluster)) + 
#   geom_density(alpha=.5) + 
#   scale_fill_manual(values = c("#000000", "red", "#009E73",  "#0072B2", "#CC79A7", "#999999")) + 
#   theme(legend.position = "none") + theme_bw()
# x11()
# xdensity

x11()
ggplot(CL_EU, aes(x=Dim.1, y=Dim.5, color=Cluster)) +  geom_text(aes(label=Region), size=4) +
  theme_bw() + stat_ellipse(type = "euclid", alpha = 1/2, aes(fill = Cluster)) + 
  scale_fill_manual(values = c("#000000", "red", "#009E73",  "#0072B2", "#999999"))



?stat_ellipse()


######################################
######################################
#### ANALYSE AVEC DONNNES MANQUANTES - MISSMDA!!! FIRST trial with Package!!!
######################################
######################################



# write.csv(nodup,"EU_NO_DUP.csv")

# Country NUTS 1 levels included initially - Taken away by _reg!!!

na = read.csv("EU_NO_DUP_reg.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(na)
tail(na)
summary(na)

na[35:45,]


library(missMDA)

nb <- estim_ncpPCA(na[,1:9], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

comp <- imputePCA(na[,1:9], ncp=4, scale=TRUE) ## Complète le tableau -c(121,206)
res.pca <- PCA(comp$completeObs) ## Effectue l'ACP

?imputePCA

dev.off()

mi <- MIPCA(na[,1:9], scale = TRUE, ncp=4) # -c(121,206)
plot(mi)



# which(rownames(na)=="PTZ")
# which(rownames(na)=="FRZ")




######
# ACP
######

x11()
round(res.pca$eig,2)
barplot(res.pca$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res.pca$eig), sep=""))
abline(h=100/9)


estim_ncp(data[34:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res.pca, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-10,10), xlim=c(-10,10))
x11()
plot.PCA(res.pca, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
x11()
plot.PCA(res.pca, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 4:5)

# plot.PCA(res.pca, choix="ind", invisible = "ind.sup", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
#          ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res.pca, choix="var" , select="contrib 10")# , invisible = "var" select="contrib 10") 
x11()
plot.PCA(res.pca, choix="var", select="contrib 10",  axes = 3:4) # select="contrib 10", invisible = "var", 
x11()
plot.PCA(res.pca, choix="var", select="contrib 10",  axes = 4:5)
x11()
plot.PCA(res.pca, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res.pca)
dimdesc(res.pca, axes = 4:5)
dimdesc(res.pca, axes = 7:9)



############
# CAH
############



# res.pca.hcpc <- HCPC(res.pca, graph=T)
res.pca.hcpc <- HCPC(res.pca, kk = Inf, graph=T)
names(res.pca.hcpc )
x11()
plot(res.pca.hcpc, axes=1:2)

HCPC(res.pca)   # res.pcaults for the Hierarchical Clustering on Principal Components**
res.pca.hcpc$call        # **res.pcaults for the Principal Component Analysis (PCA)**
res.pca.hcpc$call$t$tree # BASICS for the HCPC **

res.pca$eig
head(round(res.pca$eig[,1],3))
head(round(res.pca.hcpc$call$t$inert.gain,3))

# catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**


catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)


res.pca.hcpc
res.pca.hcpc$call
res.pca.hcpc$desc.var  # Description des classes de HCPC**  / res.pca.hcpc$desc.var$test.chi2 
res.pca.hcpc$desc.var$category
res.pca.hcpc$desc.var$quanti.var
res.pca.hcpc$desc.var$quanti 

res.pca.hcpc$desc.axe    # Description des classes par Facteurs Principaux**


x11()
plot(res.pca.hcpc, axes=c(1,2), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.pca.hcpc, axes=c(1,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.pca.hcpc, axes=c(1,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.pca.hcpc, axes=c(1,5), choice="map", draw.tree=F, ind.names = T, centers.plot=T, rect = F) # , autoLab="yes"

?plot.PCA

x11()
plot(res.pca.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.pca.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.pca.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

?fviz_cluster


x11()
fviz_cluster(res.pca.hcpc, ggtheme = theme_bw(), geom = c("point"))







################### 
################### 
#### ANALYSE NO OMITTED REFAITE ... PAS DE NUTS1 AVEC NUTS 2!!! 90 obs a 78 obs (negligenace initialle!!!)
########################
################### 





# NO OMITTED VARIABLES AND NO DUPLICATES
dk = na.omit(na)
str(dk)
head(dk)

rownames(dk)
colnames(dk)


# write.csv(dk,"DK_EU_reg.csv")


# CHECKED CLEAN FROM NUTS1


# NO AT (earlier mistake)

# PCA and CAH with Kommune NUMBER - IMPOSED ncp=6 instead of Inf for CAH!!!
res <- PCA(dk[-1,], scale.unit=TRUE, graph = F, col.w = NULL,
           quanti.sup=c(10:29), ncp=6) 
# normalt excluded  37,51, 57, 


# PCA and CAH with Kommune NAVN - IMPOSED ncp=6 instead of Inf for CAH!!!



# Extra summary

centre<-res_red$call$centre
std<-res_red$call$ecart.type
head(rbind(centre,std))

sum_red = as.data.frame(rbind(centre,std))
head(sum_red)

write.csv(sum_red,"KL_PCA_Summary_Stat_REDUCED.csv")


#############################################################
# DONNEES brutes reduites - IMPORTANT!!! POUR INTERPRETATION PCA


data_REDUCED_scaled = scale(data[c(34:95),c(4:60)])
tail(data_REDUCED_scaled)

data_REDUCED = data[c(34:95),c(4:60)]
tail(data_REDUCED)

write.csv(data_REDUCED_scaled,"KL_PCA_Productivity_REDUCED_Scaled.csv")
write.csv(data_REDUCED,"KL_PCA_Productivity_REDUCED.csv")


######
# ACP
######

x11()
round(res$eig,2)
barplot(res$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res$eig), sep=""))
abline(h=100/9)


estim_ncp(data[34:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-10,10), xlim=c(-10,10))
x11()
plot.PCA(res, choix="ind", label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
x11()
plot.PCA(res, choix="ind",  label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
plot.PCA(res, choix="ind", invisible = "ind.sup", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res, choix="var" , select="contrib 10")# , invisible = "var" select="contrib 10") 
x11()
plot.PCA(res, choix="var", select="contrib 10",  axes = 3:4) # select="contrib 10", invisible = "var", 
x11()
plot.PCA(res, choix="var", select="contrib 10",  axes = 5:6)
x11()
plot.PCA(res, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res)
dimdesc(res, axes = 4:6)
dimdesc(res, axes = 7:9)

?dimdesc
?catdes

catdes(data[1:95,],num.var=1) # Description de la var. categorielle par autres var.**

condes(prot, num.var=1) # Description de la var. continue par autres variables (cor/eta2)



tab_var<-round(cbind(res$var$contrib[,1:6],res$var$coord[,1:6],res$var$cos2[,1:6]),2)
colnames(tab_var)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))


tab_varx<-round(cbind(res$var$contrib[,7:9],res$var$coord[,7:9],res$var$cos2[,7:9]),2)
colnames(tab_varx)=c(rep("contrib",3),rep("coord",3),rep("cos2",3))


tab_var1<-tab_var[order(-tab_var[,1]),]   # axe 1
tab_var2<-tab_var[order(-tab_var[,2]),]   # axe 2
tab_var3<-tab_var[order(-tab_var[,3]),]
tab_var4<-tab_var[order(-tab_var[,4]),]
tab_var5<-tab_var[order(-tab_var[,5]),]
tab_var6<-tab_var[order(-tab_var[,6]),]

tab_varx7<-tab_varx[order(-tab_varx[,1]),]
tab_varx8<-tab_varx[order(-tab_varx[,2]),]
tab_varx9<-tab_varx[order(-tab_varx[,3]),]

tab_var1[1:10,]
tab_var2[1:10,]
tab_var3[1:10,]
tab_var4[1:10,]
tab_var5[1:10,]
tab_var6[1:10,]

tab_varx7[1:10,]
tab_varx8[1:10,]
tab_varx9[1:10,]


# important dimensions
tab_ind<-round(cbind(res$ind$contrib[,1:6],res$ind$coord[,1:6],res$ind$cos2[,1:6]),2)
colnames(tab_ind)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))

# high dimensions
tab_indx<-round(cbind(res$ind$contrib[,7:9],res$ind$coord[,7:9],res$ind$cos2[,7:9]),2)
colnames(tab_indx)=c(rep("contrib",3),rep("coord",3),rep("cos2",3))


tab_ind1<-tab_ind[order(-tab_ind[,1]),]   # axe 6
tab_ind2<-tab_ind[order(-tab_ind[,2]),]   # axe 7
tab_ind3<-tab_ind[order(-tab_ind[,3]),]
tab_ind4<-tab_ind[order(-tab_ind[,4]),]
tab_ind5<-tab_ind[order(-tab_ind[,5]),]
tab_ind6<-tab_ind[order(-tab_ind[,6]),]

tab_indx7<-tab_indx[order(-tab_indx[,1]),]
tab_indx8<-tab_indx[order(-tab_indx[,2]),]
tab_indx9<-tab_indx[order(-tab_indx[,3]),]


tab_ind1[1:10,]
tab_ind2[1:10,]
tab_ind3[1:10,]
tab_ind4[1:10,]
tab_ind5[1:10,]
tab_ind6[1:10,]

tab_indx7[1:10,]
tab_indx8[1:10,]
tab_indx9[1:10,]



# Coordonnes/Projections des individus supplementaires NON PRATIQUER!!!

head(res$ind.sup$coord[,1:9])
head(res$ind.sup$cos2[,1:9])


tab_ind.sup<-round(cbind(res$ind.sup$coord[,1:6],res$ind.sup$cos2[,1:6]),2)
colnames(tab_ind.sup)=c(rep("coord",6),rep("cos2",6))

# high dimensions
tab_ind.supx<-round(cbind(res$ind.sup$coord[,7:9],res$ind.sup$cos2[,7:9]),2)
colnames(tab_ind.supx)=c(rep("coord",3),rep("cos2",3))


tab_ind.sup1<-tab_ind.sup[order(-tab_ind.sup[,1]),]   # 7
tab_ind.sup2<-tab_ind.sup[order(-tab_ind.sup[,2]),]   # 8
tab_ind.sup3<-tab_ind.sup[order(-tab_ind.sup[,3]),] # 9
tab_ind.sup4<-tab_ind.sup[order(-tab_ind.sup[,4]),] # 10
tab_ind.sup5<-tab_ind.sup[order(-tab_ind.sup[,5]),] # 11
tab_ind.sup6<-tab_ind.sup[order(-tab_ind.sup[,6]),] # 12


tab_ind.supx7<-tab_ind.supx[order(-tab_ind.supx[,1]),]
tab_ind.supx8<-tab_ind.supx[order(-tab_ind.supx[,2]),]
tab_ind.supx9<-tab_ind.supx[order(-tab_ind.supx[,3]),]



tab_ind.sup1[1:10,] # +151, 159, 169, 210, 173, 157, 185, 230, 101, 359 / -217, 175, 259, 270
tab_ind.sup2[1:10,] # +151, 161, 153, 269, 250 / -147, 185, 201, 270
tab_ind.sup3[1:10,] # +187, 151, 210, 157 / -190, 161
tab_ind.sup4[1:10,] # +187, 173, 260, 157, 201, 250, 240 / -165, 147
tab_ind.sup5[1:10,] # +151, 101, 259, 219, 159, 165, 169 / - 190
tab_ind.sup6[1:10,] # + 185 / -190, 173, 151, 161, 223, 217, 175, 153

tab_ind.supx7[1:10,] # +185, 269 / -230, 187, 151, 169, 159, 153, 165, 223
tab_ind.supx8[1:10,] # +230, 223, 269, 187, 265, 147 / -260, 219
tab_ind.supx9[1:10,] # +153, 260, 173 / -147, 230



res$eig
res$ind
res$ind.sup
res$var
res$quanti.sup
res$quali.sup


library(PerformanceAnalytics)
x11()
chart.Correlation(nodupna[,c(1:10)], histogram=TRUE, pch=19)
chart.Correlation(data[,c(11:20)], histogram=TRUE, pch=19)
chart.Correlation(data[,c(21:29)], histogram=TRUE, pch=19)


colnames(nodupna)

x11()
# Pre Vs Crisis
chart.Correlation(data[,c(22,23,25,26,28,29,31,32,34,35,37,38,40,41)], histogram=TRUE, pch=19)

x11()
# Crisis Vs Post
chart.Correlation(data[c(34:95),c(23,24,26,27,29,30,32,33,35,36,38,39,41,42)], histogram=TRUE, pch=19)

x11()
# Pre Vs Post
chart.Correlation(data[c(34:95),c(22,24,25,27,28,30,31,33,34,36,37,39,40,42)], histogram=TRUE, pch=19)




############
# CAH
############



# res.hcpc <- HCPC(res, graph=T)
res.hcpc <- HCPC(res, kk = Inf, graph=T)
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


catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)


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
plot(res.hcpc, axes=c(1,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(4,6), choice="map", draw.tree=F, ind.names = T, centers.plot=F, rect = F, autoLab="yes")

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(5,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")

?fviz_cluster

str(dk)

x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), repel = T)

x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), axes = c(1,3))

res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**












################### 
###################################### 
# OLD PARTS
###################################### 
################### 




#############################################################
# FOKUS: HOVESTADEN
# FULL SAMPLE - HOVEDSTADEN as ONLY ACTIVE INDIVIDUALS
#############################################################

head(data)
str(data)

hoved = which(data$Area=="Hovedstaden")
hoved


data_red=data[,c(1:3,6:66)]
colnames(data_red)

# write.csv(data_red,"Cluster_Analysis_Kommune_Navn.csv")



# PCA and CAH with Kommune Navn
data_redX = read.csv("Cluster_Analysis_Kommune_Navn.csv",header=TRUE,sep=",", dec=".", row.names = 1) 
head(data_redX)
str(data_redX)


colnames(data_redX)

tail(data_redX[,c(2,4:41)])
colnames(data_redX[,c(2,4:41)])

res_hov <- PCA(data_redX[,c(2,4:41)], scale.unit=TRUE,graph = F, col.w = NULL,
               quali.sup=1, quanti.sup=c(17:39), ind.sup = c(rest,96), ncp=6)


colnames(data_red)



summary(res_hov, nbelements=13, ncp=5)


# Extra summary
# centre<-res_hov$call$centre  
# std<-res_hov$call$ecart.type
# rbind(centre,std)  


# DONNEES brutes et reduites - IMPORTANT!!! SANS DANEMARK

data_HOVEDSTAD_scaled = scale(data[c(1:33),c(4:60)])
tail(data_HOVEDSTAD_scaled)

data_HOVEDSTAD = data[c(1:33),c(4:60)]
tail(data_HOVEDSTAD)

write.csv(data_HOVEDSTAD_scaled,"KL_PCA_Productivity_HOVEDSTAD_Scaled.csv")
write.csv(data_HOVEDSTAD,"KL_PCA_Productivity_HOVEDSTAD_RAW.csv")


#########
x11()
round(res_hov$eig,2)
barplot(res_hov$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res_hov$eig), sep=""))
abline(h=100/15)

tail(data)

estim_ncp(data[c(1:33),c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

?plot.PCA

x11()
plot.PCA(res_hov, choix="ind", invisible = c("ind.sup"), habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8))
x11()
plot.PCA(res_hov, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
x11()
plot.PCA(res_hov, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
x11()
plot.PCA(res_hov, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-5,5), xlim=c(-5,5), axes = 7:8)

x11()
plot.PCA(res_hov, choix="var", select="contrib 10")
x11()
plot.PCA(res_hov, choix="var", select="contrib 10", axes = 3:4)
x11()
plot.PCA(res_hov, choix="var", select="contrib 10", axes = 5:6)
x11()
plot.PCA(res_hov, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res_hov)
dimdesc(res_hov, axes = 4:6)
dimdesc(res_hov, axes = 7:9)

?dimdesc
?catdes

catdes(data_red[-c(rest,96),c(1,3:17)],num.var=1) # Description de la var. categorielle par autres_hov var.**

condes(prot, num.var=1) # Description de la var. continue par autres_hov variables (cor/eta2)


# Important dimensions
tab_var<-round(cbind(res_hov$var$contrib[,1:6],res_hov$var$coord[,1:6],res_hov$var$cos2[,1:6]),2)
colnames(tab_var)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))


# Higher dimensions
tab_varx<-round(cbind(res_hov$var$contrib[,7:9],res_hov$var$coord[,7:9],res_hov$var$cos2[,7:9]),2)
colnames(tab_varx)=c(rep("contrib",3),rep("coord",3),rep("cos2",3))


tab_var1<-tab_var[order(-tab_var[,1]),]   # axe 1
tab_var2<-tab_var[order(-tab_var[,2]),]   # axe 2
tab_var3<-tab_var[order(-tab_var[,3]),]
tab_var4<-tab_var[order(-tab_var[,4]),]
tab_var5<-tab_var[order(-tab_var[,5]),]
tab_var6<-tab_var[order(-tab_var[,6]),]


tab_varx7<-tab_varx[order(-tab_varx[,1]),]
tab_varx8<-tab_varx[order(-tab_varx[,2]),]
tab_varx9<-tab_varx[order(-tab_varx[,3]),]


tab_var1[1:10,]
tab_var2[1:10,]
tab_var3[1:10,]
tab_var4[1:10,]
tab_var5[1:10,]
tab_var6[1:10,]

tab_varx7[1:10,]
tab_varx8[1:10,]
tab_varx9[1:10,]



tab_ind<-round(cbind(res_hov$ind$contrib[,1:6],res_hov$ind$coord[,1:6],res_hov$ind$cos2[,1:6]),2)
colnames(tab_ind)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))

# high dimensions
tab_indx<-round(cbind(res_hov$ind$contrib[,7:9],res_hov$ind$coord[,7:9],res_hov$ind$cos2[,7:9]),2)
colnames(tab_indx)=c(rep("contrib",3),rep("coord",3),rep("cos2",3))



tab_ind1<-tab_ind[order(-tab_ind[,1]),]   # 7
tab_ind2<-tab_ind[order(-tab_ind[,2]),]   # 8
tab_ind3<-tab_ind[order(-tab_ind[,3]),] # 9
tab_ind4<-tab_ind[order(-tab_ind[,4]),] # 10
tab_ind5<-tab_ind[order(-tab_ind[,5]),] # 11
tab_ind6<-tab_ind[order(-tab_ind[,6]),] # 12


tab_indx7<-tab_indx[order(-tab_indx[,1]),]
tab_indx8<-tab_indx[order(-tab_indx[,2]),]
tab_indx9<-tab_indx[order(-tab_indx[,3]),]



tab_ind1[1:10,]
tab_ind2[1:10,]
tab_ind3[1:10,]
tab_ind4[1:10,]
tab_ind5[1:10,]
tab_ind6[1:10,]

tab_indx7[1:10,]
tab_indx8[1:10,]
tab_indx9[1:10,]


# Coordonnes/Projections des individus supplementaires

head(res_hov$ind.sup$coord[,1:9])
head(res_hov$ind.sup$cos2[,1:9])


tab_ind.sup<-round(cbind(res_hov$ind.sup$coord[,1:6],res_hov$ind.sup$cos2[,1:6]),2)
colnames(tab_ind.sup)=c(rep("coord",6),rep("cos2",6))

# high dimensions
tab_ind.supx<-round(cbind(res_hov$ind.sup$coord[,7:9],res_hov$ind.sup$cos2[,7:9]),2)
colnames(tab_ind.supx)=c(rep("coord",3),rep("cos2",3))


tab_ind.sup1<-tab_ind.sup[order(tab_ind.sup[,1]),]   # 7
tab_ind.sup2<-tab_ind.sup[order(tab_ind.sup[,2]),]   # 8
tab_ind.sup3<-tab_ind.sup[order(tab_ind.sup[,3]),] # 9
tab_ind.sup4<-tab_ind.sup[order(tab_ind.sup[,4]),] # 10
tab_ind.sup5<-tab_ind.sup[order(tab_ind.sup[,5]),] # 11
tab_ind.sup6<-tab_ind.sup[order(tab_ind.sup[,6]),] # 12


tab_ind.supx7<-tab_ind.supx[order(tab_ind.supx[,1]),]
tab_ind.supx8<-tab_ind.supx[order(tab_ind.supx[,2]),]
tab_ind.supx9<-tab_ind.supx[order(tab_ind.supx[,3]),]



tab_ind.sup1[1:10,] # +530 / -440, 390, 330, 376, 510, 430, 890, 791, 340, 370
tab_ind.sup2[1:10,] # +326, 530, 760, 746 / -450, 707, 671, 607
tab_ind.sup3[1:10,] # +766, 615, 621, 746 / -730, 671, 849, 773, 440, 665
tab_ind.sup4[1:10,] # +607, 730, 671, 530, 756, 450 / -326, 727, 766, 840
tab_ind.sup5[1:10,] # + 530, 450, 607 / - 440, 326, 390, 707, 306
tab_ind.sup6[1:10,] # + 607, 530 / -326, 482, 849, 440

tab_ind.supx7[1:10,] # +336 / -450, 607, 360, 530
tab_ind.supx8[1:10,] # +730, 530, 756 / -326, 727, 450, 540, 360, 779
tab_ind.supx9[1:10,] # +450, 756, 607, 326, 671 / -730, 840, 787


res_hov$eig
res_hov$ind
res_hov$ind.sup
res_hov$var
res_hov$quanti.sup
res_hov$quali.sup



library(PerformanceAnalytics)
x11()
chart.Correlation(data[c(1:33),c(4:12)], histogram=TRUE, pch=19)
chart.Correlation(data[c(1:33),c(13:21)], histogram=TRUE, pch=19)


colnames(data)

x11()
# Pre Vs Crisis
chart.Correlation(data[c(1:33),c(22,23,25,26,28,29,31,32,34,35,37,38,40,41)], histogram=TRUE, pch=19)

x11()
# Crisis Vs Post
chart.Correlation(data[c(1:33),c(23,24,26,27,29,30,32,33,35,36,38,39,41,42)], histogram=TRUE, pch=19)

x11()
# Pre Vs Post
chart.Correlation(data[c(1:33),c(22,24,25,27,28,30,31,33,34,36,37,39,40,42)], histogram=TRUE, pch=19)



############
# CAH
############

x11()
res.hcpc <- HCPC(res_hov, nb.clust=0, kk = Inf, graph=T, consol=T)
plot(res.hcpc, axes = 5:6)

?HCPC

HCPC(res,t.levels="all")

nb.clust=0

res.hcpc <- HCPC(res_hov,graph=F,consol=T)
names(res.hcpc )

x11()
plot(res.hcpc, axes=1:2)

HCPC(res_red)   # Results for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **Results for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res$eig
head(round(res$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**

res.hcpc
res.hcpc$call
res.hcpc$desc.var  # Description des classes de HCPC**  / res.hcpc$desc.var$test.chi2 
res.hcpc$desc.var$category
res.hcpc$desc.var$quanti.var
res.hcpc$desc.var$quanti 

res.hcpc$desc.axe    # Description des classes par Facteurs Principaux**

x11()
plot(res.hcpc, axes=1:2)
x11()
plot(res.hcpc, axes=1:3)
x11()
plot(res.hcpc, axes=2:4)


res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**


x11()
fviz_cluster(res.hcpc, repel = T, ggtheme = theme_bw())

?fviz_cluster

# , xlab = "Dim3 (15.6 %)", ylab = "Dim4 (13.4 %)"

res.hcpc$call$t$res$quali.sup


# Check ASSOCIATION CLUSTER AND REGION - DIFFICILE (REMANIPULER ET MERGER les DONNEES) !!! CONFIANT en OUTPUT HCPC!!!

# res.test.chi2 <- chisq.test(data_redX[,7],data_redX[,8])  # Chisq.test Tjek!
# res.test.chi2
# res.test.chi2$stat




#################################
# FULL SAMPLE - ALL INCLUDED 
#################################



str(data_redX)
tail(data_redX)
colnames(data_redX)

# which(data_red$kom==450)
# which(data_red$kom==326)

res <- PCA(data_redX[-c(37, 51, 57, 96),c(2:41,63)], scale.unit=TRUE,graph = F, col.w = NULL, 
           quali.sup=1:2, quanti.sup=c(18:40), ncp=6)  # 37 (326),51 (450) normally excluded! ind.sup = c(96),



summary(res, nbelements=13, ncp=5)

################################################
# TAKING WEIGHTS INTO ACCOUNT FOR FULL SAMPLE!!!
################################################


dataW = read.csv("Cluster_Analysis_Kommune_Navn_NO_OUTLIERS_WEIGHTS.csv",header=TRUE,sep=",", dec=".", row.names=1) 
str(dataW)
tail(dataW)

colnames(dataW)

colnames(dataW[-c(93), c(2:41,63)])


?PCA

res <- PCA(dataW[-c(93), c(2:41,63)], scale.unit=TRUE,graph = F, row.w=dataW[-c(93),63],
           quali.sup=1:2, quanti.sup=c(18:40), ncp=6)  

# 37 (326),51 (450) normally excluded! - SANS BILLUND EXCEPTION pour WEight/ NOn WEIGHTS COMAPRAISON



# Extra summary

# centre<-res$call$centre  
# std<-res$call$ecart.type
# rbind(centre,std)  


# DONNEES brutes reduites - IMPORTANT!!!
data_scaled = scale(data[c(1:95),c(4:60)])
head(data_scaled)

write.csv(data_scaled,"KL_PCA_Productivity_Scaled.csv")

x11()
round(res$eig,2)
barplot(res$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res$eig), sep=""))
abline(h=100/15)


estim_ncp(data[1:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res, choix="ind", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8))
x11()
plot.PCA(res, choix="ind", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
x11()
plot.PCA(res, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
x11()
plot.PCA(res, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res, choix="var", select="contrib 10")
x11()
plot.PCA(res, choix="var", select="contrib 10", axes = 3:4)
x11()
plot.PCA(res, choix="var", select="contrib 10", axes = 5:6)
x11()
plot.PCA(res, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res)
dimdesc(res, axes = 4:6)

?dimdesc
?catdes

catdes(data[1:95,],num.var=3) # Description de la var. categorielle par autres var.**

condes(prot, num.var=1) # Description de la var. continue par autres variables (cor/eta2)



tab_var<-round(cbind(res$var$contrib[,1:5],res$var$coord[,1:5],res$var$cos2[,1:5]),2)
colnames(tab_var)=c(rep("contrib",5),rep("coord",5),rep("cos2",5))


tab_var1<-tab_var[order(-tab_var[,1]),]   # axe 1
tab_var2<-tab_var[order(-tab_var[,2]),]   # axe 2
tab_var3<-tab_var[order(-tab_var[,3]),]
tab_var4<-tab_var[order(-tab_var[,4]),]
tab_var5<-tab_var[order(-tab_var[,5]),]

tab_var1
tab_var2
tab_var3
tab_var4
tab_var5



tab_ind<-round(cbind(res$ind$contrib[,1:5],res$ind$coord[,1:5],res$ind$cos2[,1:5]),2)
colnames(tab_ind)=c(rep("contrib",5),rep("coord",5),rep("cos2",5))


tab_ind1<-tab_ind[order(tab_ind[,6]),]   # axe 1
tab_ind2<-tab_ind[order(-tab_ind[,7]),]   # axe 2
tab_ind3<-tab_ind[order(-tab_ind[,8]),]
tab_ind4<-tab_ind[order(-tab_ind[,9]),]
tab_ind5<-tab_ind[order(-tab_ind[,5]),]

tab_ind1[1:10,]
tab_ind2[1:10,]
tab_ind3[1:10,]
tab_ind4[1:10,]
tab_ind5[1:10,]



res$eig
res$ind
res$ind.sup
res$var
res$quanti.sup
res$quali.sup






############
# CAH
############

# kk = 50
x11()
res.hcpc <- HCPC(res, nb.clust = 0, kk = Inf, graph=T ,consol=T)
# plot(res.hcpc, axes = 5:6)

res.hcpc <- HCPC(res,graph=F,consol=T)
names(res.hcpc )

x11()
plot(res.hcpc, axes=c(1,3))

?plot.hcpc

HCPC(res_red)   # Results for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **Results for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res$eig
head(round(res$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

# catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**

catdes(data_red[-c(96),c(1:2,3:17)],num.var=1)


res.hcpc
res.hcpc$call
res.hcpc$desc.var  # Description des classes de HCPC**  / res.hcpc$desc.var$test.chi2 
res.hcpc$desc.var$category
res.hcpc$desc.var$quanti.var
res.hcpc$desc.var$quanti 

res.hcpc$desc.axe    # Description des classes par Facteurs Principaux**

?plot

head(res.hcpc$call$X)

x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = "text", repel = F)
# fviz_cluster(object = res.hcpc, data = res.hcpc$call$X, choose.vars = c(res.hcpc$call$X[,1],res.hcpc$call$X[,3]))


x11()
plot(res.hcpc, axes=c(1,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(1,2), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(1,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)


x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

?plot

?plot.HCPC


# fviz_mca_biplot(res.hcpc) +
#   theme_minimal()

# library("factoextra")
# fviz_cluster(res.hcpc, data_red[-c(96),c(1:2,3:17), frame.type = "convex")+
#   theme_minimal()



res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

str(res.hcpc$data.clust)  # Tableau des donnees + les CLUSTERS**



# EXTRA
# With WEIGHTS CLUSTERS to be compared WITHOUT WEIGHTS!!!

str(res.hcpc$data.clust)

Cluster_DK_W = res.hcpc$data.clust[,c(1,2,41,42)]
head(Cluster_DK_W)

summary(Cluster_DK_W)


colnames(Cluster_DK_W)[4] <- "clust_W"

write.csv(Cluster_DK_W,"Cluster_DK_W.csv")


CL_DK_W<- read.csv("Cluster_DK_W.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(CL_DK_W)



# FOR GRAPHICAL PURPOSES - DENSITIES ALONG different DIMENSIONS!!! (Visula ANOVA)



# write.csv(res.hcpc$data.clust[,c(1:2,41:42)],"Cluster_DK_NO_BILLUND.csv")
# write.csv(res.hcpc$call$X,"Cluster_AXES_DKrige.csv")


CL_DK<- read.csv("Cluster_DK.csv",header=TRUE,sep=",", dec=".")  # , row.names=1


# FOR WEIGHTS - NO WEIGHTS COMPARISON
CL_DK_NO_BILLUND<- read.csv("Cluster_DK_NO_BILLUND.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(CL_DK_NO_BILLUND)


# COMPARING Clusters with Weiths (5) and WITHOUT (4)

merge_DK_W = merge(CL_DK_NO_BILLUND, CL_DK_W, by = c("kommune"), all = T)
tail(merge_DK_W)
str(merge_DK_W)

summary(merge_DK_W)



res.test.chi2 <- chisq.test(merge_DK_W[,5],merge_DK_W[,9])  # Chisq.test Tjek!
res.test.chi2
res.test.chi2$stat



# Tables croisees
tabx=table(merge_DK_W[,5], merge_DK_W[,9])

prop.table(tabx,1)
prop.table(tabx,2)

addmargins(tabx)
addmargins(prop.table(tabx))


# # In compact form and listed
# mytable=table(merge_Øv_W[,4], merge_Øv_W[,8])
# 
# list(counts = mytable,
#      percent.row = prop.table(mytable,1),
#      percent.col = prop.table(mytable,2),
#      count.row = margin.table(mytable,1),
#      count.col = margin.table(mytable,2))
# addmargins(mytable)









# write.csv(res.hcpc$data.clust[,c(1:2,41)],"Cluster_DK.csv")
# write.csv(res.hcpc$call$X,"Cluster_AXES_DK.csv")


CL_DK<- read.csv("Cluster_DK.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(CL_DK)

CL_DK_dim<- read.csv("Cluster_AXES_DK.csv",header=TRUE,sep=",", dec=".") 
str(CL_DK_dim)

merge_DK = merge(CL_DK, CL_DK_dim, by = c("kom"), all = T)
tail(merge_DK)

# write.csv(merge_DK,"Cluster_DK_merged.csv")


CL_DK_m<- read.csv("Cluster_DK_merged.csv",header=TRUE,sep=",", dec=".") 
str(CL_DK_m)


# Courbe de densité marginale de x (panel du haut)

xdensity <- ggplot(CL_DK_m, aes(Dim.3, fill=Cluster)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("red", "#009E73",  "#0072B2", "#CC79A7")) + 
  theme(legend.position = "none") + theme_bw()
x11()
xdensity


# Reduced PCA / HPCH for NICE cluster plots in other dimensions than dim1 and dim2

# Plan 13
CL_DK_m_PCA13 = read.csv("Cluster_DK_merged.csv",header=TRUE,sep=",", dec=".", row.names = 1) 
str(CL_DK_m_PCA13)


res_DK13 <- PCA(CL_DK_m_PCA13[,c(4,6)], scale.unit=F,graph = F, col.w = NULL, ncp=2)


res.hcpc_DK13 <- HCPC(res_DK13, nb.clust = 4, kk = Inf, graph=T ,consol=T)


x11()
fviz_cluster(res.hcpc_DK13 , ggtheme = theme_bw(), xlab = "Dim1 (27.9 %)", ylab = "Dim3 (15.9 %)")


# x11()
# fviz_cluster(ggtheme = theme_bw(), res.hcpc, axes = c(2,4))


# Reduced PCA / HPCH for NICE cluster plots in other dimensions than dim1 and dim2

# Plan 14
CL_DK_m_PCA14 = read.csv("Cluster_DK_merged.csv",header=TRUE,sep=",", dec=".", row.names = 1) 
str(CL_DK_m_PCA14)


res_DK14 <- PCA(CL_DK_m_PCA14[,c(4,7)], scale.unit=F,graph = F, col.w = NULL, ncp=2)


res.hcpc_DK14 <- HCPC(res_DK14, nb.clust = 4, kk = Inf, graph=T ,consol=T)


x11()
fviz_cluster(res.hcpc_DK14 , ggtheme = theme_bw(), xlab = "Dim1 (27.9 %)", ylab = "Dim4 (13.5 %)")


?fviz_cluster


# CHECK ASSOCIATION CLUSTER et GEOGRAPHY - MEME ASSOCIATIONS TROUVEES!!! 
# Test ki2
ki2 = read.csv("Cluster_DK_merged.csv",header=TRUE,sep=",", dec=".", row.names = 1) 
str(ki2)


res_ki2 <- PCA(ki2, scale.unit=F, graph = F, col.w = NULL, ncp=Inf, quali.sup = 7:9)


dimdesc(res_red)
dimdesc(res_red, axes = 4:6)
dimdesc(res_red, axes = 7:9)

?dimdesc
?catdes

catdes(ki2,num.var=9) # Description de la var. categorielle par autres_red var.**

condes(prot, num.var=1) # Description de la var. continue par autres_red variables (cor/eta2)






### EXTRA NON USE EFFORTS

# For More Customised Clusters - Check K-MEANS and STAT 1 (Bruno)
x11()
# Ellipses par groupes
ggplot(CL_DK_m, aes(Dim.1, Dim.2, color = Cluster)) +
  geom_point(aes(size = 2)) + scale_alpha(guide = "none") + stat_ellipse(type = "t") +   theme_bw() 


p + stat_ellipse()
# Changer le type d'ellipses: 
# Valeurs possibles "t", "norm", "euclid"
p + stat_ellipse(type = "euclid") +   theme_bw()




# MISSING ! MAKE  A QUICK SEPARATE ANALYSIS with Outliers!
# Outliers i DK
which(raw$kom==326)
# 37
which(raw$kom==450)
# 51



# scale(temp.eu[1:23,1:16])*sqrt(22/23)
# cor(temp.eu[1:23,1:16])

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}


# Y_T
colnames(data[c(1:33),c(4:12)])

# Y_T
colnames(data[c(1:33),c(13:21)])


x11()
pairs(data[c(1:33),c(13:21)], upper.panel = panel.cor,
      panel=function(x,y){
        points(x,y)
        abline(lm(y~x), col='red')
      })

?pairs


library(gdata)

corm <- cor(data[1:33,4:60])
class(corm)

corm[lower.tri(corm)] <- 0
corm[lower.tri(corm,diag=TRUE)] <- 0

cor <- as.data.frame(as.table(corm))
high<-subset(cor, abs(Freq) > 0.8)
high[order(-high[,3]),]



# upper<-upperTriangle(corm, diag = FALSE)
# out <- which(abs(corm) > 0.70, arr.ind=TRUE)
# out[out[,1] > out[,2]]




############################################
# SUPPLEMENT
############################################

concat.data <- cbind.data.frame(temp.eu[1:23,17],res$ind$coord)
ellipse.coord <- coord.ellipse(concat.data,bary=TRUE)
plot.PCA(res, habillage=17, ellipse=ellipse.coord, cex=0.8)

# resx <- PCA(temp.eu[1:23,], scale.unit=FALSE,graph = FALSE,
#            quanti.sup=13:16, quali.sup=17, ncp=6)

# library("flashClust")


##########################################################################
#################### Section CAH  ############ Individuals = FRACTILE des REVENUS
##########################################################################


par(mfrow=c(1,1),oma=c(0,0,0,0))

res.hcpc <- HCPC(res, graph=T)
res.hcpc <- HCPC(res,graph=T,consol=F)
names(res.hcpc )

plot(res.hcpc, axes=1:2)

HCPC(res)   # Results for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **Results for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res$eig
head(round(res$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**

res.hcpc
res.hcpc$call
res.hcpc$desc.var  # Description des classes de HCPC**  / res.hcpc$desc.var$test.chi2 
res.hcpc$desc.var$category
res.hcpc$desc.var$quanti.var
res.hcpc$desc.var$quanti 

res.hcpc$desc.axe    # Description des classes par Facteurs Principaux**

res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**

# data=as.data.frame(res.hcpc$data.clust)
# dim(data)

# res.test.chi2 <- chisq.test(data[,17],data[,18])  # Chisq.test Tjek!
# res.test.chi2
# res.test.chi2$stat

# model<-lm(Annual~clust, data=data)   # ANOVA Tjek!
# summary(model)

# resx.hcpc <- HCPC(resx, graph=T, consol=T)  # NON SCALED temperatures!
# names(resx.hcpc )
# 
# resx.hcpc <- HCPC(resx,consol=F)
# plot(resx.hcpc, axes=1:2)


###################################################################

# STAT 1 supplementaire

# CAH
head(ACP)


res1=hclust(dist(temp.eu[1:23,]),"ward.D")
res2=hclust(dist(temp.eu[1:23,]))

par(mfrow=c(1,1),oma=c(0,0,0,0))

plot(res1,cex=0.5) # dendogramme
plot(res2,cex=0.5) # dendogramme
names(res1)

# Pour COMPARAISON dendogramme res1 (Complete), res2 (Ward)

lay1=cbind(c(1,1,2,2),c(1,1,2,2),c(1,1,2,2),c(1,1,2,2))
lay1
layout(lay1) 

plot(res1,cex=0.5) # dendogramme
plot(res2,cex=0.5) # dendogramme
names(res1)

par(mfrow=c(1,1),oma=c(0,0,0,0))

res1$height
tail(res1$height)
barplot(rev(res1$height)[1:15],main="diagramme des hauteurs")

# tail(res2$height)
# barplot(rev(res2$height)[1:15],main="diagramme des hauteurs")

cutree(res1,h=50)
cutree(res1,k=4)

# cutree(res2,h=rev(res2$height))  # Avec toute la liste des heights! 

par (mfrow=c(2,2))

# "WARD" method

par (mfrow=c(2,2))

for (i in 2:5) {
  plot(res$ind$coord[,1],res$ind$coord[,2],
       col=cutree(res2,h=rev(res2$height)[i]),
       main=paste("nombre de groupes=",i),
       pch=3,cex=0.5,xlab="Dim 1",ylab="Dim 2")
}

# "Complete" method
# 
# for (i in 2:5) {
#   plot(res$ind$coord[,1],res$ind$coord[,2],
#        col=cutree(res1,h=rev(res1$height)[i]),
#        main=paste("nombre de groupes=",i),
#        pch=3,cex=0.5,xlab="Dim 1",ylab="Dim 2")
# }

# Pour COMPARAISON dendogramme res1 (Complete), res2 (Ward)

# lay1=cbind(c(1,1,2,2),c(1,1,2,2),c(1,1,2,2),c(1,1,2,2))
# lay1
# layout(lay1) 

# # plotting layout
# 
# plot(res1,cex=0.5) # dendogramme
# plot(res2,cex=0.5) # dendogramme


# # Comparaison et pourcentage de correspondance, MAIS NON INDEXEE, ininterpretable!
# par(mfcol=c(2,3))
# 
# for (i in 2:4) {
#   plot(res$ind$coord[,1],res$ind$coord[,2],
#        col=cutree(res1,h=rev(res1$height)[i]),
#        main=paste("lien maximal=",i),
#        pch=3,cex=0.5,xlab="Dim 1",ylab="Dim 2")
#   
#   plot(res$ind$coord[,1],res$ind$coord[,2],
#        col=cutree(res2,h=rev(res2$height)[i]),
#        main=paste("ward=",i),
#        pch=3,cex=0.5,xlab="Dim 1",ylab="Dim 2")
#   
#   print(mean( cutree(res1,h=rev(res1$height)[i]) ==  cutree(res2,h=rev(res2$height)[i])))
# }
# 
# library(mclust)
# 
# adjustedRandIndex(cutree(res1,h=rev(res1$height)[2]), cutree(res2,h=rev(res2$height)[2]))
# adjustedRandIndex(cutree(res1,h=rev(res1$height)[3]), cutree(res2,h=rev(res2$height)[3]))
# adjustedRandIndex(cutree(res1,h=rev(res1$height)[4]), cutree(res2,h=rev(res2$height)[4]))
# 

## Clustering de variables


cor<-cor(temp.eu[1:23,1:12])
dd = as.dist((1 - cor)/2)
dd

as.matrix(dd)

par(mfrow=c(1,1))
plot(hclust(dd,method="ward")) 


###########  Supplemnt BRUNO # resultats differents car scale ! ... sinon pareil!

var1 <-temp.eu[1:23,1:12]
cha1 <-hclust(dist(t(scale(var1))),
              method="ward")
plot(cha1,xlab="",ylab="",main="Classification hiérarchique")


var0 <-temp.eu[1:23,1:12]
cha0 <-hclust(dist(t(var0)),
              method="ward")
plot(cha0,xlab="",ylab="",main="Classification hiérarchique")


lay2=cbind(c(1,2),c(1,2))
lay2
layout(lay2) # plotting layout

obj <-cor(var0, use="pairwise.complete.obs")
heatmap(obj, col=gray(seq(1,0,length=16)))


###################
# B - nuées dynamiques - kmeans
###################

# temp.eu.sel<-temp.eu[1:23,1:12]
temp.eu.sel<-temp.eu[1:23,c(1:12,17)]

library(plyr)
temp.eu.sel[,13]<-revalue(temp.eu.sel[,13], 
                          c("West"="1", "North"="2", "East"="3", "South"="4"))
as.numeric(temp.eu.sel[,13])

init=1:4
centres= temp.eu.sel[init,]
centres

n=dim(temp.eu.sel)[1]
n

indsup= n + 1:4
indsup
temp.eu.sel[indsup,]=centres

identical(centres,temp.eu.sel[init,])

row.names(temp.eu.sel)[indsup]
row.names(temp.eu.sel)[indsup]=c("a1","a2","a3","a4")
temp.eu.sel[indsup,]

# affectation d'une observation au centre le plus proche

# choixcentre=function(x){
#   res=apply(centres,1,function(c){ sum((x-c )^2 )   } )
#   which.min(res)
# }
# choixcentre(temp.eu.sel[2,])
# 

# affectation de chaque observation au centre le plus proche
# 
# groupe=apply(temp.eu.sel,1, choixcentre)
# 

# 
# # variance intra classe
# 
# 
# within=function(tab,groupe){
#   ll= lapply(tab, function(x){tapply(x,groupe, 
#                                      function(y){sum((y-mean(y))^2)} )} )
#   sum(as.data.frame(ll))
# }
# intra=within(temp.eu.sel[-indsup,],groupe[-indsup])
# 
# # 
# # itérations
# 
# library(FactoMineR)
# 
# for (i in 1:5) {
#   res.pca=PCA(cbind(temp.eu.sel,groupe),
#               ind.sup=indsup,quali.sup=8,graph=FALSE)
#   
#   plot(res.pca,axes=c(1,2),choix="ind",label="ind.sup",habillage=8
#        title=paste("intra=",round(intra,2)))
#   
#   # nouveaux centres
#   centres= as.data.frame(lapply(temp.eu.sel[-indsup,],
#                                 function(x){tapply(x,groupe[-indsup],mean)} ))
#   temp.eu.sel[indsup,]=centres
#   groupe=apply(temp.eu.sel[,],1, choixcentre)
#   
#   intra=within(temp.eu.sel[1:n,],groupe[-indsup]) 
# }
# centres


# une fonction R - le sign "moins" enleve les lignes designees!

temp.eu.sel[-indsup,]
temp.eu.sel[-(indsup),]
temp.eu.sel[indsup,]
temp.eu.sel[init,]

identical(temp.eu.sel[-(indsup),],temp.eu.sel[-indsup,])

# kmeans sur DONNEES + Centres initiaux!

library(stats)

dim(temp.eu.sel[-(indsup),])
dim(temp.eu.sel[init,])

res.kmeans=kmeans(temp.eu.sel[-(indsup),], temp.eu.sel[init,])
res.kmeans
?kmeans

names(res.kmeans)
res.kmeans$centers
res.kmeans$cluster
res.kmeans$within
res.kmeans$iter

# identicite!
sum(res.kmeans$within)
res.kmeans$tot.withinss
# within(temp.eu.sel[1:n,],res.kmeans$cluster)   # c une fonction d'en haut!!!

# NE marche pas!!!

par(mfrow=c(1,1))

res.pca=PCA(temp.eu.sel[1:23,], quali.sup=13,graph=FALSE)

plot(res.pca,axes=c(1,2),choix="ind",habillage=13, autoLab="no")
# ind.sup=indsup, title=paste("intra=",round(intra,2)) 
# ,label="ind.sup"
points(res.pca$ind$coord[,1],res.pca$ind$coord[,2],col=res.kmeans$cluster,pch=2)

# classification sur facteurs

res.km2=kmeans(res.pca$ind$coord[,1:2],res.pca$ind$coord[init,1:2])
plot(res.pca$ind$coord[,1],res.pca$ind$coord[,2],col=res.km2$cluster,
     xlab="Dim 1",ylab="Dim 2",main="classification sur facteurs")
points(res.pca$quali.sup$coord[,1],res.pca$quali.sup$coord[,2],col=1:4,pch=2)     


# Pour Aller Plus Loin - transformation de variables continues 
# en variables qualitatives

# Pour découper une variable continue en classes

vari = tea[,19]
res.hcpc = HCPC(vari, iter.max=10) max.cla=unlist(by(res.hcpc$data.clust[,1], res.hcpc$data.clust[,2], max))
breaks = c(min(vari), max.cla)
aaQuali = cut(vari, breaks, include.lowest=TRUE)
summary(aaQuali)

# Pour découper plusieurs variables continues en classes

data.cat = data
for (i in 1:ncol(data.cat)){
  vari = data.cat[,i]
  res.hcpc = HCPC(vari, nb.clust=-1, graph=FALSE)
  maxi = unlist(by(res.hcpc$data.clust[,1], res.hcpc$data.clust[,2], max))
  breaks = c(min(vari), maxi)
  aaQuali = cut(vari, breaks, include.lowest=TRUE)
  data.cat[,i] = aaQuali
}

# data = tableau de données avec les variables continues à découper en classes



####### PCA temperature et Suplement Bruno ################


library(FactoMineR)
temperature <- read.table("http://factominer.free.fr/book/temperature.csv",header=TRUE, sep=";", dec=".", row.names=1)

res <- PCA(temperature, ind.sup=24:35, quanti.sup=13:16, quali.sup=17)
plot.PCA(res, choix="ind", habillage=17)

dimdesc(res)
res$eig
res$ind
res$ind.sup
res$var
res$quanti.sup
res$quali.sup
scale(temperature[1:23,1:16])*sqrt(22/23)
cor(temperature[1:23,1:16])

concat.data <- cbind.data.frame(temperature[1:23,17],res$ind$coord)
ellipse.coord <- coord.ellipse(concat.data,bary=TRUE)
plot.PCA(res, habillage=17, ellipse=ellipse.coord, cex=0.8)


HCPC(res,t.levels="all")


###########  Supplemnt BRUNO


var <-c("age","n.enfant","scz.cons","dep.cons",
        "grav.cons","rs","ed","dr")
cha <-hclust(dist(t(scale(smp.l[,var]))),
             method="ward")
plot(cha,xlab="",ylab="",main="Classification hiérarchique")

obj <-cor(smp.l[,var], use="pairwise.complete.obs")
heatmap(obj, col=gray(seq(1,0,length=16)))


