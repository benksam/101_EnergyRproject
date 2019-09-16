# Smart commands

# Rename factor levels
library(plyr)
thex[,37]<-revalue(thex[,37], c("1"="class 1", "2"="class 2", "3"="class 3"))


# Summary
summary(gastos[1:7,1:25])
summary(t(gastos[,1:25]))

summary(gastos[1:7,1:25], maxinf)   # max levels of factors shown!


# Descriptive Summary 

library(pastecs) #  stat descriptives
stat.desc(gastos[1:7,], basic=T)
stat.desc(t(gastos[,1:25]), basic=T)


# Extra summary (Factominer)

centre<-res.pca$call$centre  
std<-res.pca$call$ecart.type  # sd de la population!
cv=std/centre
rbind(centre,std,cv)  

# or

av=apply(gastos[1:7,],2,mean)
sd=apply(gastos[1:7,],2,sd)*sqrt(6/7)
v=apply(gastos[1:7,],2,sd)*sqrt(6/7)/apply(gastos[1:7,],2,mean)
rbind(av,sd,v)



# adding/changing row names
myDF <- cbind(Row.Names = rownames(myDF), myDF)
myDF

rownames(myDF) <- NULL
myDF

myMat <- as.matrix(myDF)
names(dimnames(myMat)) <- c("Names.of.Rows", "")
myMat


# PCA


# Choosing number pc - suggestion de NCP
res$eig
estim_ncp(rhone[,-16], scale = TRUE, method = "Smooth")     
?estim_ncp

# Description de variables / pc - continu et categoriel entre elles
dimdesc(res.pca)
catdes(prot,num.var=12) # Description de la variable categorielle par autres variables**
condes(prot, num.var=1) # Description de la var. continue par autres variables (cor/eta2)


# Tableau de synthese pour variables (Contribution, Coordonnee, cos2) - IDEM pour INDIVIDUS
tab<-round(cbind(res.pca$var$contrib[,1:3],res.pca$var$coord[,1:3],res.pca$var$cos2[,1:3]),2)
colnames(tab)=c(rep("contrib",3),rep("coord",3),rep("cos2",3))

tab1<-tab[order(-tab[,1]),]   # axe 1
tab2<-tab[order(-tab[,2]),]   # axe 2
tab3<-tab[order(-tab[,3]),]

tab1
tab2
tab3

# qualite de representation des individus sur plan 1:2
cos2.ind.12<-tab[,5]+tab[,6]
sort(cos.ind.12, decreasing=T)

# qualite de representation des variables sur plan 1:2
cos2.var.12<-tab.var[,5]+tab.var[,6]
sort(cos.var.12)


summary(res.pca, npc=3)

# Calcul des valeurs moyennes des variables SCALED par category - V1 as factor!

data.scaled<-as.data.frame(scale(data[2:14]))
scaled<-cbind(data[1],data.scaled)
head(scaled)

mean.scaled1<-aggregate(scaled[,2:14], by=list(scaled$V1),mean)  # au lieu de tapply!!!
mean.scaled2<-by(scaled[,2:14],scaled$V1, FUN=colMeans)   #  liste au lie de dataframe!!!



# Smart plot with separate axes specification
par(las=2)
plot(gastos[1:7,21],type="b",axes=F,ylab="Communicationes (en Euros)",xlab="",bty="o")
axis(2)
axis(1,1:7,rownames(gastos)[1:7])
par(las=0)

# Smart plot with vector specifications
plot(res.pca, choix="var", invisible="var", axes=2:3, cex=0.7, clab.row=0.1, clab.col=0.1)
arrows(0,0,res.pca$var$coord[,2],res.pca$var$coord[,3], cex=0.5, col="red", code = 3, length = 0.1, angle = 30)
textxy(res.pca$var$coord[,2], res.pca$var$coord[,3], row.names(res.pca$var$coord),col="red")

# Smart plot with legend(factors) specifications
plot.PCA(res.pca, choix="ind", autoLab="yes",habillage=1, label="no")
legend("topright", legend=levels(data[,1]), bty="o", text.col=1:3, col=1:3, pch=19, cex=0.8)






# Correlation
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

pairs(rhone[1:39,1:15], upper.panel = panel.cor, 
      panel=function(x,y){
        points(x,y)
        abline(lm(y~x), col='red')
      })

# Correlation packages and codes

# Ordering correlations
library(gdata)

corm <- cor(prot[,1:11])
class(corm)

corm[lower.tri(corm)] <- 0
corm[lower.tri(corm,diag=TRUE)] <- 0

cor <- as.data.frame(as.table(corm))
high<-subset(cor, abs(corm) > 0.7)
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

# Correlation heat map! (CAH - Bruno supplement)
obj0 <-cor(var0, use="pairwise.complete.obs")
heatmap(obj0, col=gray(seq(1,0,length=16)))

# both in one
library(psych)
corr.test(data[,2:14], adjust = "none")

# Check range of correlations
dim(cor(data[,-1]))
range(cor(data[,-1])-diag(1,31))  # if (inverse)duplicates, PERFECT correlation!!!


# plot des correlations avec dim1 par variables
dotchart(res.pca$var$cor[,1],xlim=c(-1,1),pch=20,lab=row.names(res.pca$ind$cor)) # ? droite
abline(v=0)


# Check for DUPLICATES in row
anyDuplicated(data)
which(!duplicated(data))


# Choosing the right columns
which(colnames(rhone)=="Dt") # ONE WAY 
names(rhone)[c(3,7,8,12)]    # THE OTHER way 

cond<-data.frame(rownames(rhone),scale(rhone[,3])*sqrt(38/39))
cond[order(-cond[,2]),]


# Pour Aller Plus Loin - transformation de variables continues en variables qualitatives
# Pour d?couper une variable continue en classes

vari = tea[,19]
res.hcpc = HCPC(vari, iter.max=10) max.cla=unlist(by(res.hcpc$data.clust[,1], res.hcpc$data.clust[,2], max))
breaks = c(min(vari), max.cla)
aaQuali = cut(vari, breaks, include.lowest=TRUE)
summary(aaQuali)

# Pour d?couper plusieurs variables continues en classes

data.cat = data
for (i in 1:ncol(data.cat)){
  vari = data.cat[,i]
  res.hcpc = HCPC(vari, nb.clust=-1, graph=FALSE)
  maxi = unlist(by(res.hcpc$data.clust[,1], res.hcpc$data.clust[,2], max))
  breaks = c(min(vari), maxi)
  aaQuali = cut(vari, breaks, include.lowest=TRUE)
  data.cat[,i] = aaQuali
}


# EXTRACTION of "nec" or "pea" from rownames, fx:  1.nec or 2.pea
x <- rownames(data)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
substrRight(x, 3)  

data$typ<-as.factor(substrRight(x, 3))


# Functions apply (ATTENTION au FACTORS dans dataframe!!!)
apply(data[,2:32],2,sum)

aggregate(data[,2:32], by=list(data$type), mean)

# plot des points totaux (somme des valeurs) par type de fruit (individu)
dotchart(apply(data[,2:32],1,sum),gr=data$type)


# ANOVA - equal mean tests
y = apply(data[,-1],1,sum)
anova(lm(y ~ data$type)) # p-value = 0.07

t.test(y ~ data$type)
wilcox.test(y ~ data$type)




# Different nombres de Cluster des points sur plan 1:2
# "WARD" method
par (mfrow=c(2,2))
for (i in 2:5) {
  plot(resfr$ind$coord[,1],resfr$ind$coord[,2],
       col=cutree(resfr1,h=rev(resfr1$height)[i]),
       main=paste("nombre de groupes=",i),
       pch=3,cex=0.5,xlab="Dim 1",ylab="Dim 2")
}

# From KL RAW Data Preparation - Split by row
# Split data per category (ROWS)
KL_B_split <- split(KL_B, KL_B$dk)
new_names <- c("KL_B_B","KL_B_D","KL_B_K","KL_B_X")    

for (i in 1:length(KL_B_split)) {
  assign(new_names[i], KL_B_split[[i]])
}


str(KL_B_X)




# create a missing map

library(Amelia)
dev.off()

x11()
missmap(pdf, col=c("black", "grey"), legend=T, rank.order=F)


# Correlation matrices
library("PerformanceAnalytics")
chart.Correlation(iris[,1:4], histogram=TRUE, pch=19)
?chart.Correlation

# pair-wise scatterplots colored by class
pairs(Species~., data=iris, col=iris$Species)
