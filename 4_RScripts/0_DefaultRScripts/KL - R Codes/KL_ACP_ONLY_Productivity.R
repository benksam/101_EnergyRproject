## CONTENU ##

# VERY FIRST PCA - 31 HOVEDSTADEN AND NO YEARLY DATA

# Produktivitet variabler (niveau, gennemsnit rater og årlige rater)
# PCA des Kommunes en un, Hovedstaden ou le reste
# Correlations des variables et histogrammes




library(FactoMineR)
library(ggplot2)
library(reshape2)
library(psych)


data_av<- read.csv("KL_3_Periods_SIDST.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_av)
head(data_av)

data_y<- read.csv("PCA_Yearly_Productivity.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y)
head(data_y)

# Merging Period AVERAGES and YEARLY growth rates
data_pdty = merge(data_av, data_y, by = c("kom"), all = T)
tail(data_pdty)

data_pdty=data_pdty[-96,]
tail(data_pdty)

summary(data_pdty)
str(data_pdty)

colnames(data_pdty)

"PCA_Yearly_Productivity.csv"
"KL_3_Periods_SIDST.csv"

# Write and Cleaning/Neating data
write.csv(data_pdty,"PCA_ALL_Productivity.csv")



####################################
## PCA analysis
####################################



data<- read.csv("PCA_ALL_Productivity_NEAT.csv",header=TRUE,sep=",", dec=".", row.names=1)
str(data)
head(data)
tail(data)


colnames(data)


?PCA

# which(rownames(data)=="999")

#################################
# FULL SAMPLE - ALL INCLUDED 
#################################



res <- PCA(data[,c(1:60)], scale.unit=TRUE,graph = F,
           quali.sup=1:3, quanti.sup=c(43:60), ind.sup = 96, ncp=Inf)

summary(res, nbelements=13, ncp=5)


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
abline(h=100/39)


estim_ncp(data[1:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8))
plot.PCA(res, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
plot.PCA(res, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
plot.PCA(res, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res, choix="var", select="contrib 10")
plot.PCA(res, choix="var", select="contrib 10", axes = 3:4)
plot.PCA(res, choix="var", select="contrib 10", axes = 5:6)
plot.PCA(res, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res)
dimdesc(res, axes = 4:8)

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


##########################################################
# FOCUS = REGIONER UDEN FOR HOVEDSTADET - 66 vs 31 - mistake: SHOULD BE 29!!!
# FULL SAMPLE - HOVEDSTADEN as SUPPLEMENTARY INDIVIDUALS
##########################################################


hoved = which(data$Area=="Hovedstaden")
hoved

head(data, 33)

str(data)

summary(data[,c(2:3)])

res_red <- PCA(data[,c(1:60)], scale.unit=TRUE,graph = F,
           quali.sup=1:3, quanti.sup=c(43:60), ind.sup = c(1:31,96), ncp=Inf)

colnames(data)

summary(res_red, nbelements=13, ncp=5)


# Extra summary

centre<-res_red$call$centre
std<-res_red$call$ecart.type
head(rbind(centre,std))

sum_red = as.data.frame(rbind(centre,std))
head(sum_red)

write.csv(sum_red,"KL_PCA_Summary_Stat_REDUCED.csv")

#############################################################
# DONNEES brutes reduites - IMPORTANT!!! SANS Hovedstaden


data_REDUCED_scaled = scale(data[c(34:95),c(4:60)])
tail(data_REDUCED_scaled)

data_REDUCED = data[c(34:95),c(4:60)]
tail(data_REDUCED)

write.csv(data_REDUCED_scaled,"KL_PCA_Productivity_REDUCED_Scaled.csv")
write.csv(data_REDUCED,"KL_PCA_Productivity_REDUCED.csv")



x11()
round(res_red$eig,2)
barplot(res_red$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res_red$eig), sep=""))
abline(h=100/39)


estim_ncp(data[34:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res_red, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8))
plot.PCA(res_red, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
plot.PCA(res_red, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
plot.PCA(res_red, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res_red, choix="var", select="contrib 10")
plot.PCA(res_red, choix="var", select="contrib 10", axes = 3:4)
plot.PCA(res_red, choix="var", select="contrib 10", axes = 5:6)
plot.PCA(res_red, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res_red)
dimdesc(res_red, axes = 4:6)
dimdesc(res_red, axes = 7:9)

?dimdesc
?catdes

catdes(data[1:95,],num.var=1) # Description de la var. categorielle par autres_red var.**

condes(prot, num.var=1) # Description de la var. continue par autres_red variables (cor/eta2)



tab_var<-round(cbind(res_red$var$contrib[,1:6],res_red$var$coord[,1:6],res_red$var$cos2[,1:6]),2)
colnames(tab_var)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))


tab_varx<-round(cbind(res_red$var$contrib[,7:9],res_red$var$coord[,7:9],res_red$var$cos2[,7:9]),2)
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
tab_ind<-round(cbind(res_red$ind$contrib[,1:6],res_red$ind$coord[,1:6],res_red$ind$cos2[,1:6]),2)
colnames(tab_ind)=c(rep("contrib",6),rep("coord",6),rep("cos2",6))

# high dimensions
tab_indx<-round(cbind(res_red$ind$contrib[,7:9],res_red$ind$coord[,7:9],res_red$ind$cos2[,7:9]),2)
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



# Coordonnes/Projections des individus supplementaires

head(res_red$ind.sup$coord[,1:9])
head(res_red$ind.sup$cos2[,1:9])


tab_ind.sup<-round(cbind(res_red$ind.sup$coord[,1:6],res_red$ind.sup$cos2[,1:6]),2)
colnames(tab_ind.sup)=c(rep("coord",6),rep("cos2",6))

# high dimensions
tab_ind.supx<-round(cbind(res_red$ind.sup$coord[,7:9],res_red$ind.sup$cos2[,7:9]),2)
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






res_red$eig
res_red$ind
res_red$ind.sup
res_red$var
res_red$quanti.sup
res_red$quali.sup


library(PerformanceAnalytics)
x11()
chart.Correlation(data[c(34:95),c(4:12)], histogram=TRUE, pch=19)
chart.Correlation(data[c(34:95),c(13:21)], histogram=TRUE, pch=19)


colnames(data)

x11()
# Pre Vs Crisis
chart.Correlation(data[c(34:95),c(22,23,25,26,28,29,31,32,34,35,37,38,40,41)], histogram=TRUE, pch=19)

x11()
# Crisis Vs Post
chart.Correlation(data[c(34:95),c(23,24,26,27,29,30,32,33,35,36,38,39,41,42)], histogram=TRUE, pch=19)

x11()
# Pre Vs Post
chart.Correlation(data[c(34:95),c(22,24,25,27,28,30,31,33,34,36,37,39,40,42)], histogram=TRUE, pch=19)




#############################################################
# FOKUS: HOVESTADEN -31 AS A MISTAKE IN DATASET
# FULL SAMPLE - HOVEDSTADEN as ONLY ACTIVE INDIVIDUALS
#############################################################


hoved = which(data$Area=="Hovedstaden")
hoved

colnames(data)

res_hov <- PCA(data[,c(1:60)], scale.unit=TRUE,graph = F,
               quali.sup=1:3, quanti.sup=c(43:60), ind.sup = c(34:96), ncp=Inf)

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



x11()
round(res_hov$eig,2)
barplot(res_hov$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res_hov$eig), sep=""))
abline(h=100/32)

tail(data)

estim_ncp(data[c(1:33),c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res_hov, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8))
plot.PCA(res_hov, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
plot.PCA(res_hov, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)
plot.PCA(res_hov, choix="ind", habillage=1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res_hov, choix="var", select="contrib 10")
plot.PCA(res_hov, choix="var", select="contrib 10", axes = 3:4)
plot.PCA(res_hov, choix="var", select="contrib 10", axes = 5:6)
plot.PCA(res_hov, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res_hov)
dimdesc(res_hov, axes = 4:6)
dimdesc(res_hov, axes = 7:9)

?dimdesc
?catdes

catdes(data[1:33,],num.var=1) # Description de la var. categorielle par autres_hov var.**

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


