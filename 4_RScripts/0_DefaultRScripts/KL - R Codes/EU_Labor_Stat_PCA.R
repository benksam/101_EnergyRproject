

#####################################
# Separate PCA and CAH analysis 
# Filling missing DATA before merging with productivity data and keeping NUTS2 (No duplicates)
# Drawing of FIRST scatter plots in REPORT (Post, Pre and Crisis)
#####################################



########################
# LOOPS and functions FAILED with SCATTER PLOTS (ggbox-function and plotcols) !!! 
# Continuous dimension problem (WORKS well with Categories and Years for Spaghetti plots)
# NEED to SEPARATE manually and FACETTING with grid.arrange!!! WORKS well too!!!
########################



library(FactoMineR)
library(ggplot2)
library("factoextra")
library("dplyr")

library(reshape2)
library(psych)

library(missMDA)




######################################
######################################
#### ANALYSE AVEC DONNNES MANQUANTES - MISSMDA!!!
######################################
######################################




###############################################
# Separate ANALYSIS of delperiods - PCA and CAH
###############################################



#################################################################
# Post
# NUTS2 data for LABOR STATISTICS (NO PRODUCTIVITY INCLUDED)
#################################################################


Post = read.csv("PCA_Labor_Stat_Post.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(Post)
tail(Post)
summary(Post)

names(Post)


# IMPUTING BOTH ACTIVE and PASSIVE variables - Productivity in MERGED in the end NOT IMPUTTED!!!
library(missMDA)

nb <- estim_ncpPCA(Post[,1:19], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

nbX <- estim_ncpPCA(Post[,20:38], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nbX

comp <- imputePCA(Post[,1:19], ncp=4, scale=TRUE) ## Complète le tableau -c(121,206)

compX <- imputePCA(Post[,20:38], ncp=2, scale=TRUE) ## Complète le tableau -c(121,206)


?imputePCA

dev.off()

mi <- MIPCA(Post[,1:19], scale = TRUE, ncp=4) # -c(121,206)
plot(mi)

mi <- MIPCA(Post[,20:38], scale = TRUE, ncp=2) # -c(121,206)
plot(mi)



head(comp$completeObs)
str(comp$completeObs)

cpt_obs = as.data.frame(comp$completeObs)
head(cpt_obs)
str(cpt_obs)


cptX_obs = as.data.frame(compX$completeObs)
head(cptX_obs)
str(cptX_obs)


Post_cpt = cbind(cpt_obs, cptX_obs, Post[,39])
str(Post_cpt)
summary(Post_cpt)

# RENAME A COLUMN
colnames(Post_cpt)[39] <- "Geo"


# Preparing merge with PRODUCTIVITY
na = read.csv("EU_NO_DUP_reg.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(na)
tail(na)
summary(na)


# NO OMITTED VARIABLES AND NO DUPLICATES
pdty = na.omit(na)
str(pdty)

rownames(pdty)
colnames(pdty)


# Create a `date` variable
Post_cpt$Region = rownames(Post_cpt)
# Reset the `rownames` of your original data
rownames(Post_cpt) = NULL


merged = merge(Post_cpt, pdty[,1:19], by = c("Region"), all.y = T)
head(merged)
summary(merged)
str(merged)

write.csv(merged,"FULL_POST.csv")




# ALL POST data included, ALSO productivity (with Active and Passive imputed earlier)
Post = read.csv("FULL_POST.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(Post)
tail(Post)
summary(Post)
names(Post)


res.pca <- PCA(Post, scale.unit=TRUE, graph = F, col.w = NULL,
               quanti.sup=c(21:57), quali.sup = 1) ## Effectue l'ACP



# x = as.data.frame(comp$completeObs)
# head(x)

# CompObsDK = cbind(comp$completeObs, Post[,39])
# res.pca <- PCA(CompObsDK, quali.sup = 20) ## Effectue l'ACP
# which(rownames(na)=="PTZ")
# which(rownames(na)=="FRZ")



######
# ACP
######

x11()
round(res.pca$eig,2)
barplot(res.pca$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res.pca$eig), sep=""))
abline(h=100/19)


estim_ncp(data[34:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-10,10), xlim=c(-10,10))
x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 5:6)

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


head(CompObsDK)
str(CompObsDK)
catdes(CompObsDK,num.var=20) # Description de la var. categorielle par autres var.**


############
# CAH
############


x11()
# res.hcpc <- HCPC(res.pca, graph=T)
res.hcpc <- HCPC(res.pca, kk = Inf, graph=T)
names(res.hcpc )
x11()
plot(res.hcpc, axes=1:2)

HCPC(res.pca)   # res.pcaults for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **res.pcaults for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res.pca$eig
head(round(res.pca$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

# catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**
# catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)



# Which Cluster for DK?
write.csv(res.hcpc$call$X,"Preli_Cluster_Post.csv")

res.hcpc
res.hcpc$call
res.hcpc$desc.var  # Description des classes de HCPC**  / res.hcpc$desc.var$test.chi2 
res.hcpc$desc.var$category
res.hcpc$desc.var$quanti.var
res.hcpc$desc.var$quanti 

res.hcpc$desc.axe    # Description des classes par Facteurs Principaux**


res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**

x11()
plot(res.hcpc, axes=c(1,2), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(1,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="y")
x11()
plot(res.hcpc, axes=c(1,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(1,5), choice="map", draw.tree=F, ind.names = T, centers.plot=T, rect = F) # , autoLab="yes"

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="y")
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

?fviz_cluster


x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), repel = T)


names(Post)


# use THIS FOR CORRELATION VISUALIZATION

library("PerformanceAnalytics")

x11()
chart.Correlation(Post[,c(6,45)], histogram=TRUE, pch=19)

x11()
chart.Correlation(Post[,c(5,45)], histogram=TRUE, pch=19)

x11()
chart.Correlation(Post[,c(29,45)], histogram=TRUE, pch=19)


# Correlation packages and codes

# Ordering correlations
library(gdata)

corm <- cor(Post[,2:51])
class(corm)

corm[lower.tri(corm)] <- 0
corm[lower.tri(corm,diag=TRUE)] <- 0

cor <- as.data.frame(as.table(corm))
high<-subset(cor, abs(corm) > 0.5)
high[order(-high[,3]),]


# Correlation test significance
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

options(scipen=3)
cor.pvalue=cor.test.p(data[,2:14])
round(cor.pvalue,3)


# LOOPS - PAS NECESSAIRE!!! EN points!

# head(data_YL_longX_Omr1)
# 
# plotCols0 <- c("Post_EMP_ED0_2",
#                "Post_EMP_ED3_4",
#                "Av_EMP_SELF")
# 
# 
# ggbox <- function(x) {
#   title <- paste("Scatterplot af", x, "med Produktivitetn på tværs af bestemte EU-regioner")
#   ggplot(Post, aes(x, y=GVA_SAL_post, shape=Geo, color=Geo)) +
#     geom_point() + # facet_grid(Landsdel_navn ~ Område) + 
#     # stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
#     # stat_smooth(aes(group = 1)) +
#     theme_bw() 
# }
# 
# lapply(plotCols0, ggbox)
# 
# 
# require("ggrepel")
# set.seed(42)
# p + geom_text_repel(aes(label = rownames(df)),
#                     size = 3.5) 



########################
# LOOPS and function FAILED with SCATTER PLOTS!!! 
# Continuous dimension problem (WORKS with Categories and Years for Spaghetti plots)
# NEED to SEPARATE and FACETTING with grid.arrange
########################


# Scatter plots for POST
# Change point shapes and colors
a = ggplot(Post, aes(x=Post_EMP_ED0_2, y=GVA_SAL_post))  +
   theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Andel af lavt uddannede (ED0-2)") 

  
# Change point shapes and colors
b = ggplot(Post, aes(x=Post_EMP_ED3_4, y=GVA_SAL_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Andel af mellemlangt uddannede (ED3-4)") 

             

# Change point shapes and colors
c = ggplot(Post, aes(x=Post_EMP_ED5_8, y=GVA_SAL_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Andel af højt uddannede (ED5-8)") 



names(Post)


require(gridExtra)

x11()
grid.arrange(a, b, c, ncol=2, nrow=2)



# Change point shapes and colors
e = ggplot(Post, aes(x=Post_EMP_SAL, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Lønmodtagernes andel af beskæftigelsen") 

# Change point shapes and colors
f = ggplot(Post, aes(x=Post_EMP_SELF, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("De selvstændige erhvervdrivendes andel af beskæftigelsen") 



require(gridExtra)

x11()
grid.arrange(e, f, ncol=2)



###########################
# Correlation SUPPLEMENTS
###########################

# Correlation heat map! (CAH - Bruno supplement)
obj0 <-cor(Post[,2:20], use="pairwise.complete.obs")
heatmap(obj0, col=gray(seq(1,0,length=16)))

# both in one
library(psych)
corr.test(Post[,2:14], adjust = "none")

# Check range of correlations
dim(cor(Post[,2:14]))
range(cor(Post[,2:14])-diag(1,13))  # if (inverse)duplicates, PERFECT correlation!!!


# plot des correlations avec dim1 par variables
dotchart(res.pca$var$cor[,1],xlim=c(-1,1),pch=20,lab=row.names(res.pca$ind$cor)) # à droite
abline(v=0)


names(Post)


###############
# LAST
# Check for BRANCHE-EFFECT Post finanscrisen (to compare with under financecrisen)

# BRANCHE uniquement!
# Crisis-DATA exist also, BUT NOT PRE!!!
# Change point shapes and colors

l = ggplot(Post, aes(x=Post_EMP_A, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("LANDBRUG-SKOVBRUG-FISKERI") 


# Change point shapes and colors
m = ggplot(Post, aes(x=Post_EMP_B_E, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("RÅSTOFINDVINDING-FREMSTILLINGSVIRKSOMHED") 



# Change point shapes and colors
n = ggplot(Post, aes(x=GVA_SAL_post, y=Post_EMP_F))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("BYGGE-ANLÆGSVIRKSOMHED") 



# Change point shapes and colors
o = ggplot(Post, aes(x=GVA_SAL_post, y=Post_EMP_G_I))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("HANDEL-TRANSPORT-RESTAURATION") 



names(Post)


require(gridExtra)

x11()
grid.arrange(a, b, c, d, ncol=2, nrow=2)

# c is significant!!!
# d not, but interesting!


# Change point shapes and colors

p = ggplot(Post, aes(x=Post_EMP_J, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("INFORMATION-KOMMUNIKATION") 


# Change point shapes and colors
q = ggplot(Post, aes(x=Post_EMP_K, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("FINANSVIRKSOMHED-FORSIKRING") 



# Change point shapes and colors
r = ggplot(Post, aes(x=Post_EMP_L, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("FAST EJENDOM") 



# Change point shapes and colors
s = ggplot(Post, aes(x=Post_EMP_M_N, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("LIBERALE-ADMINISTRATIVE YDELSER") 

require(gridExtra)

x11()
grid.arrange(e, f, g, h, ncol=2, nrow= 2)

# ALL relatively signifikant



# Change point shapes and colors
t = ggplot(Post, aes(x=Post_EMP_O_Q, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("OFF.FORVALTNING-SOCIALSIKRING-UNDERVISNING") 



# Change point shapes and colors
u = ggplot(Post, aes(x=Post_EMP_R_U, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("KULTUR-FORLYSTELSER") 

require(gridExtra)

x11()
grid.arrange(t, u, ncol=2)

# t, u, l, m 

# ALL 7 branches

x11()
grid.arrange(l, m, t, u, ncol=2, nrow= 2)



x11()
grid.arrange(p, q, r, s, ncol=2, nrow= 2)





####################################################################
# Pre 
# NUTS2 data for LABOR STATISTICS (NO PRODUCTIVITY INCLUDED)
####################################################################


Pre = read.csv("PCA_Labor_Stat_Pre.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(Pre)
tail(Pre)
summary(Pre)

names(Pre)

# IMPUTING BOTH ACTIVE and PASSIVE variables - Productivity in MERGED in the end NOT IMPUTTED!!!
library(missMDA)

nb <- estim_ncpPCA(Pre[,1:9], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

nbX <- estim_ncpPCA(Pre[,10:28], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nbX

comp <- imputePCA(Pre[,1:9], ncp=2, scale=TRUE) ## Complète le tableau -c(121,206)

compX <- imputePCA(Pre[,10:28], ncp=2, scale=TRUE) ## Complète le tableau -c(121,206)


?imputePCA

dev.off()

mi <- MIPCA(Pre[,1:9], scale = TRUE, ncp=2) # -c(121,206)
plot(mi)

miX <- MIPCA(Pre[,10:28], scale = TRUE, ncp=2) # -c(121,206)
plot(miX)



head(comp$completeObs)
str(comp$completeObs)

cpt_obs = as.data.frame(comp$completeObs)
head(cpt_obs)
str(cpt_obs)


cptX_obs = as.data.frame(compX$completeObs)
head(cptX_obs)
str(cptX_obs)


Pre_cpt = cbind(cpt_obs, cptX_obs, Pre[,29])
str(Pre_cpt)
summary(Pre_cpt)

# RENAME A COLUMN
colnames(Pre_cpt)[29] <- "Geo"


# Preparing merge with PRODUCTIVITY
na = read.csv("EU_NO_DUP_reg.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(na)
tail(na)
summary(na)


# NO OMITTED VARIABLES AND NO DUPLICATES
pdty = na.omit(na)
str(pdty)

rownames(pdty)
colnames(pdty)


# Create a `date` variable
Pre_cpt$Region = rownames(Pre_cpt)
# Reset the `rownames` of your original data
rownames(Pre_cpt) = NULL


merged = merge(Pre_cpt, pdty[,1:19], by = c("Region"), all.y = T)
head(merged)
summary(merged)
str(merged)

write.csv(merged,"FULL_Pre.csv")




# ALL Pre data included but productivity (Active and Passive imputed earlier)
Pre = read.csv("FULL_Pre.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(Pre)
tail(Pre)
summary(Pre)
names(Pre)


res.pca <- PCA(Pre, scale.unit=TRUE, graph = F, col.w = NULL,
               quanti.sup=c(11:47), ncp = 6, quali.sup = 1) ## Effectue l'ACP



# x = as.data.frame(comp$completeObs)
# head(x)



# CompObsDK = cbind(comp$completeObs, Post[,39])
# res.pca <- PCA(CompObsDK, quali.sup = 20) ## Effectue l'ACP

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
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-10,10), xlim=c(-10,10))
x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
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
dimdesc(res.pca, axes = 4:6)
dimdesc(res.pca, axes = 7:9)


head(CompObsDK)
str(CompObsDK)
catdes(CompObsDK,num.var=20) # Description de la var. categorielle par autres var.**


############
# CAH
############



# res.hcpc <- HCPC(res.pca, graph=T)
res.hcpc <- HCPC(res.pca, kk = Inf, graph=T)
names(res.hcpc )
x11()
plot(res.hcpc, axes=1:2)

HCPC(res.pca)   # res.pcaults for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **res.pcaults for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res.pca$eig
head(round(res.pca$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

# catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**
# catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)



# Which Cluster for DK?
write.csv(res.hcpc$call$X,"Preli_Cluster_Pre.csv")

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
plot(res.hcpc, axes=c(2,5), choice="map", draw.tree=F, ind.names = T, centers.plot=T, rect = F, autoLab="y") # , autoLab="yes"

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

?fviz_cluster


x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), repel = T)


res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**



# use THIS FOR CORRELATION VISUALIZATION

library("PerformanceAnalytics")

x11()
chart.Correlation(Pre[,c(6,45)], histogram=TRUE, pch=19)

x11()
chart.Correlation(Pre[,c(5,45)], histogram=TRUE, pch=19)

x11()
chart.Correlation(Pre[,c(29,45)], histogram=TRUE, pch=19)


names(Pre)

# Correlation packages and codes

# Ordering correlations
library(gdata)

corm <- cor(Pre[,2:47])
class(corm)

corm[lower.tri(corm)] <- 0
corm[lower.tri(corm,diag=TRUE)] <- 0

cor <- as.data.frame(as.table(corm))
high<-subset(cor, abs(corm) > 0.5)
high[order(-high[,3]),]


# Correlation test significance
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

options(scipen=3)
cor.pvalue=cor.test.p(data[,2:14])
round(cor.pvalue,3)


# LOOPS - PAS NECESSAIRE!!! EN points!

# head(data_YL_longX_Omr1)
# 
# plotCols0 <- c("Pre_EMP_ED0_2",
#                "Pre_EMP_ED3_4",
#                "Av_EMP_SELF")
# 
# 
# ggbox <- function(x) {
#   title <- paste("Scatterplot af", x, "med Produktivitetn på tværs af bestemte EU-regioner")
#   ggplot(Pre, aes(x, y=GVA_SAL_Pre, shape=Geo, color=Geo)) +
#     geom_point() + # facet_grid(Landsdel_navn ~ Område) + 
#     # stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
#     # stat_smooth(aes(group = 1)) +
#     theme_bw() 
# }
# 
# lapply(plotCols0, ggbox)
# 
# require("ggrepel")
# set.seed(42)
# p + geom_text_repel(aes(label = rownames(df)),
#                     size = 3.5) 


names(Pre)


# Scatter plots for PRE - BRANCHE uniquement!
# ONLY based on AVERAGES (2001-2015) since no Pre-DATA!!!
# Change point shapes and colors

a = ggplot(Pre, aes(x=GVA_EMP_pre, y=Av_EMP_A))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af lavt uddannede (ED0-2)") 


# Change point shapes and colors
b = ggplot(Pre, aes(x=GVA_SAL_pre, y=Av_EMP_B_E))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af mellemlangt uddannede (ED3-4)") 



# Change point shapes and colors
c = ggplot(Pre, aes(x=GVA_SAL_pre, y=Av_EMP_F))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af højtuddannede (ED5-8)") 



# Change point shapes and colors
d = ggplot(Pre, aes(x=GVA_SAL_pre, y=Av_EMP_G_I))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af højtuddannede (ED5-8)") 



names(Pre)


require(gridExtra)

x11()
grid.arrange(a, b, c, d, ncol=2, nrow=2)



# Change point shapes and colors
e = ggplot(Pre, aes(x=GVA_SAL_pre, y=Av_EMP_J))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Lønmodtagernes andel af beskæftigelsen") 

# Change point shapes and colors
f = ggplot(Pre, aes(x=GVA_SAL_pre, y=Av_EMP_K))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("De selvstændige erhvervdrivendes andel af beskæftigelsen") 




g = ggplot(Pre, aes(x=GVA_SAL_pre, y=Av_EMP_L))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Lønmodtagernes andel af beskæftigelsen") 

# Change point shapes and colors
h = ggplot(Pre, aes(x=GVA_SAL_pre, y=Av_EMP_M_N))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("De selvstændige erhvervdrivendes andel af beskæftigelsen") 

require(gridExtra)

x11()
grid.arrange(e, f, g, h, ncol=2, nrow= 2)

names(Pre)


i = ggplot(Pre, aes(x=Pre_EMP_ED0_2, y=GVA_EMP_pre))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Andel af lavt uddannede (ED0-2)") 

# Change point shapes and colors
j = ggplot(Pre, aes(x=Pre_EMP_ED3_4, y=GVA_EMP_pre))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Andel af mellemlangt uddannede (ED3-4)") 


# # Change point shapes and colors
# j = ggplot(Pre, aes(x=Pre_EMP_ED5_8, y=GVA_EMP_pre))  +
#   theme(legend.position = "none") +  
#   geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
#   theme_bw() + theme(legend.position = "none") +
#   geom_point(aes(color = factor(Geo)), size = 3.5) +
#   scale_color_manual(values = c("red", "darkblue")) + 
#   geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
#   ylab("Produktivitetsvækstrate (pct.)") +
#   xlab("Andel af mellemlanguddannede (ED5-8)") 

require(gridExtra)

x11()
grid.arrange(i, j, ncol=2)


k = ggplot(Pre, aes(x=Pre_EMP_SAL, y=GVA_EMP_pre))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Lønmodtagernes andel af beskæftigelsen") 

# Change point shapes and colors
l = ggplot(Pre, aes(x=Pre_EMP_SELF, y=GVA_EMP_pre))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Pre), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("De selvstændige erhvervdrivendes andel af beskæftigelsen") 




x11()
grid.arrange(k, l, ncol=2)


# Correlation heat map! (CAH - Bruno supplement)
obj0 <-cor(Pre[,2:20], use="pairwise.complete.obs")
heatmap(obj0, col=gray(seq(1,0,length=16)))

# both in one
library(psych)
corr.test(Pre[,2:14], adjust = "none")

# Check range of correlations
dim(cor(Pre[,2:14]))
range(cor(Pre[,2:14])-diag(1,13))  # if (inverse)duplicates, PERFECT correlation!!!


# plot des correlations avec dim1 par variables
dotchart(res.pca$var$cor[,1],xlim=c(-1,1),pch=20,lab=row.names(res.pca$ind$cor)) # à droite
abline(v=0)








#####################################################################
# CRISIS
# NUTS2 data for LABOR STATISTICS (NO PRODUCTIVITY INCLUDED)
######################################################################

Crisis = read.csv("PCA_Labor_Stat_Crisis.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(Crisis)
tail(Crisis)
summary(Crisis)

names(Crisis)

# IMPUTING BOTH ACTIVE and PASSIVE variables - Productivity in MERGED in the end NOT IMPUTTED!!!
library(missMDA)

nb <- estim_ncpPCA(Crisis[,1:19], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

nbX <- estim_ncpPCA(Crisis[,20:38], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nbX

comp <- imputePCA(Crisis[,1:19], ncp=2, scale=TRUE) ## Complète le tableau -c(121,206)

compX <- imputePCA(Crisis[,20:38], ncp=2, scale=TRUE) ## Complète le tableau -c(121,206)


?imputePCA

dev.off()

mi <- MIPCA(Crisis[,1:19], scale = TRUE, ncp=2) # -c(121,206)
plot(mi)

miX <- MIPCA(Crisis[,10:38], scale = TRUE, ncp=2) # -c(121,206)
plot(miX)



head(comp$completeObs)
str(comp$completeObs)

cpt_obs = as.data.frame(comp$completeObs)
head(cpt_obs)
str(cpt_obs)


cptX_obs = as.data.frame(compX$completeObs)
head(cptX_obs)
str(cptX_obs)


Crisis_cpt = cbind(cpt_obs, cptX_obs, Crisis[,39])
str(Crisis_cpt)
summary(Crisis_cpt)

# RENAME A COLUMN
colnames(Crisis_cpt)[39] <- "Geo"
head(Crisis_cpt)


# Crisisparing merge with PRODUCTIVITY
na = read.csv("EU_NO_DUP_reg.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(na)
tail(na)
summary(na)


# NO OMITTED VARIABLES AND NO DUPLICATES
pdty = na.omit(na)
str(pdty)

rownames(pdty)
colnames(pdty)


# Create a `date` variable
Crisis_cpt$Region = rownames(Crisis_cpt)
# Reset the `rownames` of your original data
rownames(Crisis_cpt) = NULL


merged = merge(Crisis_cpt, pdty[,1:19], by = c("Region"), all.y = T)
head(merged)
summary(merged)
str(merged)

write.csv(merged,"FULL_Crisis.csv")




# ALL Crisis data included but productivity (Active and Passive imputed earlier)
Crisis = read.csv("FULL_Crisis.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column
str(Crisis)
tail(Crisis)
summary(Crisis)
names(Crisis)


res.pca <- PCA(Crisis, scale.unit=TRUE, graph = F, col.w = NULL,
               quanti.sup=c(21:57), ncp = 6, quali.sup = 1) ## Effectue l'ACP



# x = as.data.frame(comp$completeObs)
# head(x)



# CompObsDK = cbind(comp$completeObs, Post[,39])
# res.pca <- PCA(CompObsDK, quali.sup = 20) ## Effectue l'ACP

# which(rownames(na)=="PTZ")
# which(rownames(na)=="FRZ")




######
# ACP
######

x11()
round(res.pca$eig,2)
barplot(res.pca$eig[,2], main = "Eigenvalues",
        names.arg = paste("Dim",1:nrow(res.pca$eig), sep=""))
abline(h=100/19)


estim_ncp(data[34:95,c(4:42)], scale = TRUE, method = "Smooth")     # suggestion de NCP
?estim_ncp


dev.off()

x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-10,10), xlim=c(-10,10))
x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 3:4)
x11()
plot.PCA(res.pca, choix="ind", habillage = 1, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
         ylim=c(-8,8), xlim=c(-8,8), axes = 4:5)

# plot.PCA(res.pca, choix="ind", invisible = "ind.sup", habillage=2, label = "ind", cex=0.8, autoLab="no", select="contrib 20",
#          ylim=c(-8,8), xlim=c(-8,8), axes = 7:8)

x11()
plot.PCA(res.pca, choix="var" , select="contrib 10")# , invisible = "var" select="contrib 10") 
x11()
plot.PCA(res.pca, choix="var", select="contrib 10",  axes = 3:4) # select="contrib 10", invisible = "var", 
x11()
plot.PCA(res.pca, choix="var", select="contrib 10",  axes = 5:6)
x11()
plot.PCA(res.pca, choix="var", select="contrib 10", axes = 7:8)


dimdesc(res.pca)
dimdesc(res.pca, axes = 4:6)
dimdesc(res.pca, axes = 7:9)


head(CompObsDK)
str(CompObsDK)
catdes(CompObsDK,num.var=20) # Description de la var. categorielle par autres var.**


############
# CAH
############



# res.hcpc <- HCPC(res.pca, graph=T)
res.hcpc <- HCPC(res.pca, kk = Inf, graph=T)
names(res.hcpc )
x11()
plot(res.hcpc, axes=1:2)

HCPC(res.pca)   # res.pcaults for the Hierarchical Clustering on Principal Components**
res.hcpc$call        # **res.pcaults for the Principal Component Analysis (PCA)**
res.hcpc$call$t$tree # BASICS for the HCPC **

res.pca$eig
head(round(res.pca$eig[,1],3))
head(round(res.hcpc$call$t$inert.gain,3))

# catdes(temp.eu,num.var=17) # Description de la variable categorielle de PCA**
# catdes(data_red[-c(hoved,96),c(1:2,3:17)], num.var=1)



# Which Cluster for DK?
write.csv(res.hcpc$call$X,"Preli_Cluster_Crisis.csv")

res.hcpc
res.hcpc$call
res.hcpc$desc.var  # Description des classes de HCPC**  / res.hcpc$desc.var$test.chi2 
res.hcpc$desc.var$category
res.hcpc$desc.var$quanti.var
res.hcpc$desc.var$quanti 

res.hcpc$desc.axe    # Description des classes par Facteurs Principaux**


res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**


x11()
plot(res.hcpc, axes=c(1,2), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(1,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(1,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(1,5), choice="map", draw.tree=F, ind.names = T, centers.plot=T, rect = F) # , autoLab="yes"

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE, autoLab="yes")

?fviz_cluster


x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), repel = T)







names(Crisis)


# use THIS FOR CORRELATION VISUALIZATION

library("PerformanceAnalytics")

x11()
chart.Correlation(Pre[,c(6,45)], histogram=TRUE, pch=19)

x11()
chart.Correlation(Pre[,c(5,45)], histogram=TRUE, pch=19)

x11()
chart.Correlation(Pre[,c(29,45)], histogram=TRUE, pch=19)


names(Crisis)

# Correlation packages and codes

# Ordering correlations
library(gdata)

corm <- cor(Crisis[,2:51])
class(corm)

corm[lower.tri(corm)] <- 0
corm[lower.tri(corm,diag=TRUE)] <- 0

cor <- as.data.frame(as.table(corm))
high<-subset(cor, abs(corm) > 0.5)
high[order(-high[,3]),]


condes(Crisis[,2:51], num.var=41) # Description de la var. continue par autres variables (cor/eta2)



# Correlation test significance
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

options(scipen=3)
cor.pvalue=cor.test.p(data[,2:14])
round(cor.pvalue,3)


# LOOPS - PAS NECESSAIRE!!! EN points!

# head(data_YL_longX_Omr1)
# 
# plotCols0 <- c("Pre_EMP_ED0_2",
#                "Pre_EMP_ED3_4",
#                "Av_EMP_SELF")
# 
# 
# ggbox <- function(x) {
#   title <- paste("Scatterplot af", x, "med Produktivitetn på tværs af bestemte EU-regioner")
#   ggplot(Pre, aes(x, y=GVA_SAL_Pre, shape=Geo, color=Geo)) +
#     geom_point() + # facet_grid(Landsdel_navn ~ Område) + 
#     # stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
#     # stat_smooth(aes(group = 1)) +
#     theme_bw() 
# }
# 
# lapply(plotCols0, ggbox)
# 
# require("ggrepel")
# set.seed(42)
# p + geom_text_repel(aes(label = rownames(df)),
#                     size = 3.5) 


names(Crisis)


# Scatter plots for Cris - BRANCHE uniquement!
# Crisis-DATA Di exist here!!!
# Change point shapes and colors

a = ggplot(Crisis, aes(x=GVA_SAL_crisis, y=Cris_EMP_A))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af lavt uddannede (ED0-2)") 


# Change point shapes and colors
b = ggplot(Crisis, aes(x=GVA_SAL_crisis, y=Cris_EMP_B_E))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af mellemlangt uddannede (ED3-4)") 



# Change point shapes and colors
c = ggplot(Crisis, aes(x=Cris_EMP_F, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("BYGGE-ANLÆGSVIRKSOMHED") 



# Change point shapes and colors
d = ggplot(Crisis, aes(x=Cris_EMP_G_I, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("HANDEL-TRANSPORT-RESTAURATION") 



names(Crisis)


require(gridExtra)

x11()
grid.arrange(a, b, c, d, ncol=2, nrow=2)

# c is significant!!!
# d not, but interesting!


# Change point shapes and colors

e = ggplot(Crisis, aes(x=Cris_EMP_J, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("INFORMATION-KOMMUNIKATION") 


# Change point shapes and colors
f = ggplot(Crisis, aes(x=Cris_EMP_K, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("FINANSVIRKSOMHED-FORSIKRING") 



# Change point shapes and colors
g = ggplot(Crisis, aes(x=Cris_EMP_L, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("FAST EJENDOM") 



# Change point shapes and colors
h = ggplot(Crisis, aes(x=Cris_EMP_M_N, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("LIBERALE-ADMINISTRATIVE YDELSER") 

require(gridExtra)

x11()
grid.arrange(e, f, g, h, ncol=2, nrow= 2)

# ALL relatively signifikant



# Change point shapes and colors
y = ggplot(Crisis, aes(x=Cris_EMP_O_Q, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("OFF.FORVALTNING-SOCIALSIKRING-UNDERVISNING") 



# Change point shapes and colors
z = ggplot(Crisis, aes(x=Cris_EMP_R_U, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("KULTUR-FORLYSTELSER-ORGANISATIONER") 

require(gridExtra)

x11()
grid.arrange(y, z, ncol=2)



# ALL 7 branches

x11()
grid.arrange(e, f, g, h, ncol=2, nrow= 2)

x11()
grid.arrange(c, d, y, z, ncol=2, nrow= 2)



names(Crisis)


i = ggplot(Crisis, aes(x=GVA_SAL_crisis, y=Cris_EMP_ED0_2))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af lCristuddannede (ED0-2)") 

# Change point shapes and colors
j = ggplot(Crisis, aes(x=GVA_SAL_crisis, y=Cris_EMP_ED3_4))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  xlab("Produktivitetsvækstrate (pct.)") +
  ylab("Andel af mellemlanguddannede (ED3-4)") 

require(gridExtra)

x11()
grid.arrange(i, j, ncol=2)


names(Crisis)
names(Pre)
names(Post)

###################################
# No special prof status effect!!!
k = ggplot(Post, aes(x=PPS_HAB_pre, y=GVA_EMP_pre))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate før krisen (pct.)") +
  xlab("BNP per indbygger før krisen (Euro købekraftpar.)") 

# Change point shapes and colors
l = ggplot(Post, aes(x=PPS_HAB_crisis, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate under krisen (pct.)") +
  xlab("BNP per indbygger under krisen (Euro købekraftpar.)") 


x11()
grid.arrange(k, l, ncol=2)


names(Crisis)


k = ggplot(Crisis, aes(x=Cris_UNEMP, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Ledigheden i pct.") 

# Change point shapes and colors
l = ggplot(Crisis, aes(x=Cris_LTU_pc_act, y=GVA_EMP_crisis))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Crisis), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("Langtidsledigheden i pct. af arbejdsstyrken") 


x11()
grid.arrange(k, l, ncol=2)


# Correlation heat map! (CAH - Bruno supplement)
obj0 <-cor(Cris[,2:20], use="pairwise.complete.obs")
heatmap(obj0, col=gray(seq(1,0,length=16)))

# both in one
library(psych)
corr.test(Cris[,2:14], adjust = "none")

# Check range of correlations
dim(cor(Cris[,2:14]))
range(cor(Cris[,2:14])-diag(1,13))  # if (inverse)duplicates, PERFECT correlation!!!


# plot des correlations Crisec dim1 par variables
dotchart(res.pca$var$cor[,1],xlim=c(-1,1),pch=20,lab=row.names(res.pca$ind$cor)) # à droite
abline(v=0)

















############
# OLD PART

################### 
################### 
#### ANALYSE NO OMITTED REFAITE ... PAS DE NUTS1 AVEC NUTS 2!!! 90 obs a 78 obs (negligenace initialle!!!)
########################
################### 


# NO OMITTED VARIABLES AND NO DUPLICATES
dk = na.omit(na)
str(dk)

rownames(dk)
colnames(dk)


# write.csv(dk,"DK_EU_reg.csv")


# CHECKED CLEAN FROM NUTS1




# PCA and CAH with Kommune NUMBER - IMPOSED ncp=6 instead of Inf for CAH!!!
res <- PCA(dk, scale.unit=TRUE, graph = F, col.w = NULL,
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
plot(res.hcpc, axes=c(1,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(5,6), choice="map", draw.tree=F, ind.names = T, centers.plot=T, rect = F, autoLab="yes")

?plot.PCA

x11()
plot(res.hcpc, axes=c(2,3), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(2,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)
x11()
plot(res.hcpc, axes=c(3,4), choice="map", draw.tree=F, ind.names = T, centers.plot=FALSE)

?fviz_cluster

str(dk)

x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"))

x11()
fviz_cluster(res.hcpc, ggtheme = theme_bw(), geom = c("text"), axes = c(1,3))

res.hcpc$desc.ind    # Illustration des classes par Individus (Paragon/Speciaux)**

res.hcpc$data.clust  # Tableau des donnees + les CLUSTERS**



