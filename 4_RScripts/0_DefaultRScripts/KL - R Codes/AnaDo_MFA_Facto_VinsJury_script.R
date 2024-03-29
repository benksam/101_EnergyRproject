# Chargement de FactoMineR
library(FactoMineR)

vins = read.table("AnaDo_JeuDonnees_VinsJury.csv", header=TRUE, sep=";", check.names=FALSE, row.names=1, fileEncoding="latin1")

# L'AFM sans groupe suppl�mentaire
res <- MFA(vins[,2:58], group=c(27,15,15), type=rep("s",3), 
           name.group=c("Expert","Etudiant","Conso"))

# L'AFM avec groupe suppl�mentaire
res <- MFA(vins, group=c(1,27,15,15,60), type=c("n",rep("s",4)), num.group.sup=c(1,5),
           name.group=c("C�page","Expert","Conso","Etudiant","Pr�f�rence"))

# On peut obtenir un r�sum� des principaux r�sultats en utilisant la fonction summary. 
summary(res)

# Nous demandons ici � avoir les r�sultats sur les 2 premi�res dimensions pour �viter d'avoir des tableaux trop grands 
# (par d�faut, la fonction retourne les r�sultats des 3 premi�res dimensions).

summary(res, ncp=2)

# Description des dimensions
dimdesc(res)

# Graphe des groupes de variables
plot(res,choix="group", title="Graphe des groupes")

# Graphe des individus
plot(res, title="Graphe des individus")

# Graphe des variables
plot(res,choix="var", invisible="quanti.sup", title="Graphe des variables actives")

selection = c(grep("passion",rownames(res$quanti.var$coord),fixed=TRUE),
    grep("Acide",rownames(res$quanti.var$coord),fixed=TRUE),
    grep("Sucree",rownames(res$quanti.var$coord),fixed=TRUE))
plot(res,choix="var",select=selection,invisible="quanti.sup")

plot(res,choix="var", invisible="quanti", habillage="none", lab.var=FALSE, 
     title="Graphe des variables suppl�mentaires")

# Graphe des points partiels
plot(res,choix="ind", partial="all", title="Graphe des points partiels")
plot(res, cex=0.8, invisible="ind", partial="all", title="Graphe des individus")

# Graphe des axes partiels
plot(res,choix="axes", title="Graphe des axes partiels")

# Coloriage des individus en fonction de leur modalit�
plot(res,choix="ind",habillage="cepage", title="Graphe des individus")
plot(res, cex=0.8, habillage=1)
