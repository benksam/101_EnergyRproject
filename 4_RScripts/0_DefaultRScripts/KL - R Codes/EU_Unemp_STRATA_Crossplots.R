

# BASED on LONG data for PIVOT TABLE / FILTER Selections and GRAPHICAL purposes 
# MANY STRATA (age group and gender)
# maybe NOT needed since WIDE can be used Variable by Variable




library(FactoMineR)
library(ggplot2)
library("factoextra")
library("dplyr")
library(ggrepel)

library(reshape2)
library(psych)

library(missMDA)




#######
# REady for Scatter plot Analysis for Educ Status!!!


Unemp_Adults_G<- read.csv("Unemp_long_Adults_Gender.csv", header = T)
str(Unemp_Adults_G)
names(Unemp_Adults_G)


# Unemp_Adults_G$Delperiod = factor(Unemp_Adults_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Unemp_Adults_G$Period)[1] <- 2
# levels(Unemp_Adults_G$Period)[2] <- 3
# levels(Unemp_Adults_G$Period)[3] <- 1


names(Unemp_Adults_G)


library(ggplot2)

dev.off()

head(Unemp_Adults_G_Pr_ED0)
str(Unemp_Adults_G_Pr_ED0)

# Subsetting LONG data for Graphical purposes
Unemp_Adults_G_Pr_M = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Pre" & Unemp_Adults_G$Gender=="M")
Unemp_Adults_G_Pr_F = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Pre" & Unemp_Adults_G$Gender=="F")
Unemp_Adults_G_Pr_T = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Pre" & Unemp_Adults_G$Gender=="T")

Unemp_Adults_G_C_M = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Crisis" & Unemp_Adults_G$Gender=="M")
Unemp_Adults_G_C_F = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Crisis" & Unemp_Adults_G$Gender=="F")
Unemp_Adults_G_C_T = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Crisis" & Unemp_Adults_G$Gender=="T")

Unemp_Adults_G_Po_M = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Post" & Unemp_Adults_G$Gender=="M")
Unemp_Adults_G_Po_F = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Post" & Unemp_Adults_G$Gender=="F")
Unemp_Adults_G_Po_T = subset(Unemp_Adults_G,Unemp_Adults_G$Period=="Post" & Unemp_Adults_G$Gender=="T")

# For Centering all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Unemp_Adults_G_Pr_M, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_Pr_M[,1], color = factor(Unemp_Adults_G_Pr_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænd i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Unemp_Adults_G_Pr_F, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_Pr_F[,1], color = factor(Unemp_Adults_G_Pr_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Unemp_Adults_G_Pr_T, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_Pr_T[,1], color = factor(Unemp_Adults_G_Pr_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Unemp_Adults_G_Pr)



d= ggplot(Unemp_Adults_G_C_M, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_C_M[,1], color = factor(Unemp_Adults_G_C_M[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mænds ledighed i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Unemp_Adults_G_C_F, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_C_F[,1], color = factor(Unemp_Adults_G_C_F[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftigett (pct.)") +
  ggtitle("Kvinders ledighed i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Unemp_Adults_G_C_T, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_C_T[,1], color = factor(Unemp_Adults_G_C_T[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet ledighed i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)



g= ggplot(Unemp_Adults_G_Po_M, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_Po_M[,1], color = factor(Unemp_Adults_G_Po_M[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mænds ledighed i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Unemp_Adults_G_Po_F, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_Po_F[,1], color = factor(Unemp_Adults_G_Po_F[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftigett (pct.)") +
  ggtitle("Kvinders ledighed i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Unemp_Adults_G_Po_T, aes(Unemp_Y20_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Adults_G_Po_T[,1], color = factor(Unemp_Adults_G_Po_T[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet ledighed i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)



# Eduction level ED0 of Young Population effect on productivity

Unemp_Young_G<- read.csv("Unemp_long_Young_Gender.csv", header = T)
str(Unemp_Young_G)
names(Unemp_Young_G)


# Unemp_Young_G$Delperiod = factor(Unemp_Young_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Unemp_Young_G$Period)[1] <- 2
# levels(Unemp_Young_G$Period)[2] <- 3
# levels(Unemp_Young_G$Period)[3] <- 1


names(Unemp_Young_G)


library(ggplot2)

dev.off()

head(Unemp_Young_G_Pr_ED0)
str(Unemp_Young_G_Pr_ED0)


# Subsetting LONG data for Graphical purposes
Unemp_Young_G_Pr_M = subset(Unemp_Young_G,Unemp_Young_G$Period=="Pre" & Unemp_Young_G$Gender=="M")
Unemp_Young_G_Pr_F = subset(Unemp_Young_G,Unemp_Young_G$Period=="Pre" & Unemp_Young_G$Gender=="F")
Unemp_Young_G_Pr_T = subset(Unemp_Young_G,Unemp_Young_G$Period=="Pre" & Unemp_Young_G$Gender=="T")

Unemp_Young_G_C_M = subset(Unemp_Young_G,Unemp_Young_G$Period=="Crisis" & Unemp_Young_G$Gender=="M")
Unemp_Young_G_C_F = subset(Unemp_Young_G,Unemp_Young_G$Period=="Crisis" & Unemp_Young_G$Gender=="F")
Unemp_Young_G_C_T = subset(Unemp_Young_G,Unemp_Young_G$Period=="Crisis" & Unemp_Young_G$Gender=="T")

Unemp_Young_G_Po_M = subset(Unemp_Young_G,Unemp_Young_G$Period=="Post" & Unemp_Young_G$Gender=="M")
Unemp_Young_G_Po_F = subset(Unemp_Young_G,Unemp_Young_G$Period=="Post" & Unemp_Young_G$Gender=="F")
Unemp_Young_G_Po_T = subset(Unemp_Young_G,Unemp_Young_G$Period=="Post" & Unemp_Young_G$Gender=="T")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Unemp_Young_G_Pr_M, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_Pr_M[,1], color = factor(Unemp_Young_G_Pr_M[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mænds ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Unemp_Young_G_Pr_F, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_Pr_F[,1], color = factor(Unemp_Young_G_Pr_F[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftigett (pct.)") +
  ggtitle("Kvinders ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Unemp_Young_G_Pr_T, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_Pr_T[,1], color = factor(Unemp_Young_G_Pr_T[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Unemp_Young_G_Pr)



d= ggplot(Unemp_Young_G_C_M, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_C_M[,1], color = factor(Unemp_Young_G_C_M[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("mænds ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Unemp_Young_G_C_F, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_C_F[,1], color = factor(Unemp_Young_G_C_F[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftigett (pct.)") +
  ggtitle("Kvinders ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Unemp_Young_G_C_T, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_C_T[,1], color = factor(Unemp_Young_G_C_T[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)



g= ggplot(Unemp_Young_G_Po_M, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_Po_M[,1], color = factor(Unemp_Young_G_Po_M[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mænds ledighed i alderen 15_24") +  
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Unemp_Young_G_Po_F, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_Po_F[,1], color = factor(Unemp_Young_G_Po_F[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftigett (pct.)") +
  ggtitle("Kvinders ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Unemp_Young_G_Po_T, aes(Unemp_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Unemp_Young_G_Po_T[,1], color = factor(Unemp_Young_G_Po_T[,10])),size = 3.5) +
  xlab("Ledighedsraten (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet ledighed i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)




#########################################
# SEPARATE COMPLEMENT
# Extraction of SIGNIFICANT CORRELATION COEFFICIENTS to SUPPORT SCATTERplots
############################################################################


# Correlation Gender - Productivity (EDUC-STRATA-file)


cor_G <- read.csv("FULL_Educ_Strata_Productivity.csv", header = T)
str(cor_G)
names(cor_G)


# # Correlation test significance
# cor.test.p <- function(x){
#   FUN <- function(x, y) cor.test(x, y)[["p.value"]]
#   z <- outer(
#     colnames(x), 
#     colnames(x), 
#     Vectorize(function(i,j) FUN(x[,i], x[,j]))
#   )
#   dimnames(z) <- list(colnames(x), colnames(x))
#   z
# }
# 
# 

# 
# options(scipen=3)
# cor.pvalue=cor.test.p(cor_G[,select.pr])
# round(cor.pvalue,3)
# 

# Pre and Post correlation coefficients
select.pr = c(42, 45, 48, 51, 54, 57, 60)
select.po = c(44, 47, 50, 53, 56, 59, 62)

options(scipen=3)
cor.pvalue=cor.test.p(cor_G[,select.po])
round(cor.pvalue,3)



library("PerformanceAnalytics")

# Pre - correlations

x11()
chart.Correlation(cor_G[,select.pr], histogram=TRUE, pch=19)

head(cor_G[,c(42, 45, 48, 51, 54, 57, 60)])

# ED0

cor.test(cor_G[,42], cor_G[,60])
cor.test(cor_G[,45], cor_G[,60])


# ED3 avec SAL since no EMP correlation

cor.test(cor_G[,48], cor_G[,63])
cor.test(cor_G[,51], cor_G[,63])


# ED5 avec GVA_SAL since no EMP correlation

cor.test(cor_G[,54], cor_G[,63])
cor.test(cor_G[,57], cor_G[,63])



library("PerformanceAnalytics")

# Post - correlations

x11()
chart.Correlation(cor_G[,select.po], histogram=TRUE, pch=19)

head(cor_G[,c(44, 47, 50, 53, 56, 59, 62)])

# ED0

cor.test(cor_G[,44], cor_G[,62])
cor.test(cor_G[,47], cor_G[,62])

# ED3

cor.test(cor_G[,50], cor_G[,62])
cor.test(cor_G[,53], cor_G[,62])


# ED5 avec GVA_SAL since no EMP correlation (YES)

cor.test(cor_G[,56], cor_G[,65])
cor.test(cor_G[,59], cor_G[,65])



# Correlation Gender - Productivity (UNEMP-STRATA-file)


cor_G <- read.csv("FULL_Unemp_Strata_Productivity.csv", header = T)
str(cor_G)
names(cor_G)

# M = c(6,8,15,17,21,23)
# F = c(9,11,18,20,21,23)
# Pre = c(6,9,15,18,21)
# Post = c(8,11,17,20,23)


library("PerformanceAnalytics")


# Pre

head(cor_G[,c(6,9,15,18,21)])

x11()
chart.Correlation(cor_G[,c(6,9,15,18,21)], histogram=TRUE, pch=19)


# Pre - Female 15_24

cor.test(cor_G[,18], cor_G[,21])




# Post


head(cor_G[,c(8,11,17,20,23)])

x11()
chart.Correlation(cor_G[,c(8,11,17,20,23)], histogram=TRUE, pch=19)


# Post - ALL gender correlated

cor.test(cor_G[,8], cor_G[,23])
cor.test(cor_G[,11], cor_G[,23])
cor.test(cor_G[,17], cor_G[,23])
cor.test(cor_G[,20], cor_G[,23])

