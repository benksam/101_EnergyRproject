# CONTENU

# OLD SEPARATION HOVEDSTADEN, SJÆLLAND AND ØVRIGE KOMMUNER


library(FactoMineR)
library(ggplot2)
library(reshape2)


# DATA Preparation LONG AND MELTED (reshape2) done for Boxplot analysis of variables because missed earlier
# PCA Analysis from FULL DATA but Sjælland/Stor København as supplementary individuals


data<- read.csv("KL_3_Periods_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data)

# summary(data[,2])
# 
# 
# # Periods as NUM instead on Post, Pre, Crisis
# 
# data_NUM<- read.csv("KL_3_Periods_NUM_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_NUM)
# 
# 
# ?reshape
# 
# long = reshape(data = data_NUM,
#               idvar = "kom",
#               varying = colnames(data_NUM)[-c(1,2,3)],
#               sep = ".",
#               timevar = "period",
#               times = c(1,2,3),
#               direction = "long")
# 
# head(long)
# 
# 
# long$Period <- factor(long$period)
# 
# levels(long$Period)[1] <- "Pre"
# levels(long$Period)[2] <- "Crisis"
# levels(long$Period)[3] <- "Post"
# 
# head(long)
# 
# long = long[order(long$kom,long$period),]
# 
# # write.csv(long,"KL_3_Periods_LONG.csv")



# For SEPARATE ANALYSIS! DATA ORIGINALLY WIDE!

data<- read.csv("KL_3_Periods_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data)


# Subsetting WITHOUT SJÆLLAND!


levels(data$Region_navn)

Sjælland = c("Hovedstaden", "Sjælland","Danmark")

data_Rest = subset(data,!(data$Region_navn=="Sjælland" | data$Region_navn=="Danmark" | data$Region_navn=="Hovedstaden"))
head(data_Rest)
tail(data_Rest)

summary(data_Rest)


# no region id, no Denmark Total (-96) AND NO SJÆLLAND!!!
data = data_Rest[,-c(2,3)]
str(data)

summary(data)


# Pre-crisis

data_Pre = data[,c(1,grep("Pre", colnames(data)))]
str(data_Pre)

# Preparing for ggplot2 boxplots

data_long_Pre <- melt(data_Pre, id.vars=c("kom"))
head(data_long_Pre)


x11()
box_Pre <- ggplot(data_long_Pre, aes(x="kom", y = value)) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") 
box_Pre



# Post-crisis

data_Post = data[,c(1,grep("Post", colnames(data)))]
str(data_Post)

# Postparing for ggplot2 boxplots

data_long_Post <- melt(data_Post, id.vars=c("kom"))
head(data_long_Post)


x11()
box_Post <- ggplot(data_long_Post, aes(x="kom", y = value)) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") 
box_Post


# Crisis-crisis

data_Crisis = data[,c(1,grep("Crisis", colnames(data)))]
str(data_Crisis)

# Crisisparing for ggplot2 boxplots

data_long_Crisis <- melt(data_Crisis, id.vars=c("kom"))
head(data_long_Crisis)


x11()
box_Crisis <- ggplot(data_long_Crisis, aes(x="kom", y = value)) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") 
box_Crisis


# ggbox <- function(x) {
#   title <- paste("Box plot of", x, "by LOW")
#   ggplot(dat3, aes_string('factor(LOW)', x)) +
#     facet_grid(RACE ~ SMOKE, labeller = label_both, margins = TRUE) +
#     geom_boxplot() +
#     coord_flip() +
#     ggtitle(title)
# }
# lapply(plotCols0, ggbox)
# 


# FOR JOINT ANALYSIS - DATA already LONG!!!



# LONG data for BOXPLOT facets/categories (3 periods)

data_lg<- read.csv("KL_3_Periods_LONG.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_lg)

# Excluding Danmark
data_lg_DK = subset(data_lg, data_lg$kom!=999)
head(data_lg_DK)


# Excluding Danmark and Hovedstad
data_lg_Hov = subset(data_lg,!(data_lg$Region_navn=="Danmark" | data_lg$Region_navn=="Hovedstaden"))


# Excluding Danmark and Sjælland
data_lg_Rest = subset(data_lg,!(data_lg$Region_navn=="Sjælland" | data_lg$Region_navn=="Danmark" | data_lg$Region_navn=="Hovedstaden"))

head(data_lg_Rest)

 # No Hovedstad!
data_lg_Hov=data_lg_Hov[,-c(2,3)]
head(data_lg_Hov)


# No Region!
data_lg_Rest=data_lg_Rest[,-c(2,3)]
head(data_lg_Rest)


library(reshape2)

data_resh <- melt(data_lg, id.vars=c("kom","Period"))
head(data_resh)

data_resh$tid = factor(data_resh$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 

levels(data_resh$Period)

x11()
box_ALL <- ggplot(data_resh, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") 
box_ALL


# NO Hovedstad

library(reshape2)

data_resh_Hov <- melt(data_lg_Hov, id.vars=c("kom","Period"))
head(data_resh_Hov)

data_resh_Hov$tid = factor(data_resh_Hov$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 

levels(data_resh_Hov$Period)

x11()
box_Hov <- ggplot(data_resh_Hov, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") 
box_Hov





# NO SJÆLLAND

library(reshape2)

data_resh_Rest <- melt(data_lg_Rest, id.vars=c("kom","Period"))
head(data_resh_Rest)

data_resh_Rest$tid = factor(data_resh_Rest$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 

levels(data_resh_Rest$Period)

x11()
box_Rest <- ggplot(data_resh_Rest, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") 
box_Rest



# EARLYAND FIRST PCA 


?PCA

which(rownames(data_Pre)=="999")

res <- PCA(data_Pre[1:96,], scale.unit=TRUE,graph = F,
            quali.sup=1:2, ind.sup = 96, ncp=Inf)

summary(res, nbelements=13, ncp=5)


# Extra summary

# centre<-res$call$centre  
# std<-res$call$ecart.type
# rbind(centre,std)  


# DONNEES brutes reduites - IMPORTANT!!!
Pre_scale = scale(data_Pre[1:95,3:16])
head(Pre_scale)

write.csv(Pre_scale,"KL_Pre_PCA_Scaled.csv")

x11()
round(res$eig,2)
barplot(res$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res$eig), sep=""))
abline(h=100/14)


estim_ncp(data_Pre[1:95,3:16], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res, choix="ind", habillage=2, label = "ind", cex=0.8, autoLab="no", select="cos2 0.7",
         ylim=c(-8,8), xlim=c(-8,8))
plot.PCA(res, choix="ind", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
plot.PCA(res, choix="ind", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)

x11()
plot.PCA(res, choix="var", select="contrib 10")
plot.PCA(res, choix="var", select="contrib 10", axes = 3:4)
plot.PCA(res, choix="var", select="contrib 10", axes = 4:5)


dimdesc(res)
dimdesc(res, axes = 4:6)

?dimdesc
?catdes

catdes(data_Pre[1:95,],num.var=2) # Description de la var. categorielle par autres var.**

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

x11()
pairs(data_Pre[1:95,3:16], upper.panel = panel.cor,
      panel=function(x,y){
        points(x,y)
        abline(lm(y~x), col='red')
      })

?pairs


library(gdata)

corm <- cor(data_Pre[1:95,3:16])
class(corm)

corm[lower.tri(corm)] <- 0
corm[lower.tri(corm,diag=TRUE)] <- 0

cor <- as.data.frame(as.table(corm))
high<-subset(cor, abs(Freq) > 0.7)
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


