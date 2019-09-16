library(FactoMineR)

data(wine)
res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=5, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6))

# Résumé des résultats
summary(res)

# Graphe des individus et des modalités
plot(res)
plot(res, invisible="quali")
plot(res, invisible="quali", cex=0.8)
plot(res, invisible="quali", cex=0.8, partial=c("1VAU","PER1"))

# Graphe des individus avec sélection
plot(res, invisible="quali", cex=0.8, partial=c("1VAU","PER1"), select="cos2 0.4")
plot(res, ,invisible="quali", partial="all", palette=palette(c("black","transparent","transparent","blue","transparent")))

plot(res, invisible="quali", habillage=1, cex=0.8, select="cos2 0.4")
plot(res, invisible="quali", habillage="Soil", cex=0.8, select="cos2 0.4")
plot(res, invisible="quali", habillage="Soil", cex=0.8, select="contrib 5")
plot(res, invisible="quali", habillage="Soil", cex=0.8, select="contrib 5", unselect=0)
plot(res, invisible="quali", habillage="Soil", cex=0.8, select="contrib 5", unselect=1)
plot(res, invisible="quali", habillage="Soil", cex=0.8, select="contrib 5", unselect="grey70")
plot(res, invisible="quali", habillage="Soil", cex=0.8, select="contrib 5", unselect="grey70", axes=3:4)
plot(res, invisible="quali", habillage="Soil", cex=0.8, select=c("1VAU","PER1"), unselect="grey70", axes=3:4)

# Graphe des variables
plot(res, choix="var")
plot(res, choix="var", shadow=TRUE)
plot(res, choix="var", shadow=TRUE, select="contrib 5")
plot(res, choix="var", shadow=TRUE, select="contrib 5", axes=3:4)

# Graphe des axes partiels
res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
    ncp=3, name.group=c("orig","olf","vis","olfag","gust","ens"),
    num.group.sup=c(1,6))
plot(res, choix="axes")

# Description des dimensions
dimdesc(res)
