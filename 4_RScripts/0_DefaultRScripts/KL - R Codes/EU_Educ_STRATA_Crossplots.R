
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


Educ_Young_Prop<- read.csv("Educ_STRATA_Young_T_PROP_Long.csv", header = T)
str(Educ_Young_Prop)
names(Educ_Young_Prop)


# Educ_Young_Prop$Delperiod = factor(Educ_Young_Prop$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Young_Prop$Period)[1] <- 2
# levels(Educ_Young_Prop$Period)[2] <- 3
# levels(Educ_Young_Prop$Period)[3] <- 1


names(Educ_Young_Prop)


library(ggplot2)

dev.off()

# Subsetting LONG data for Graphical purposes
Educ_Young_Prop_Pr = subset(Educ_Young_Prop,Educ_Young_Prop$Period=="Pre")
Educ_Young_Prop_C = subset(Educ_Young_Prop,Educ_Young_Prop$Period=="Crisis")
Educ_Young_Prop_Po = subset(Educ_Young_Prop,Educ_Young_Prop$Period=="Post")

# For centering by all titles
theme_update(plot.title = element_text(hjust = 0.5))


u = ggplot(Post, aes(x=Post_EMP_R_U, y=GVA_EMP_post))  +
  theme(legend.position = "none") +  
  geom_smooth(method=lm, color = "blue", color = "blue", se=FALSE, fullrange=TRUE)+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(color = factor(Geo)), size = 3.5) +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = rownames(Post), color = factor(Geo)),size = 3.5) +
  ylab("Produktivitetsvækstrate (pct.)") +
  xlab("KULTUR-FORLYSTELSER") 


a=  ggplot(Educ_Young_Prop_Pr, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Educ_Young_Prop_Pr[,9])), size = 3.5) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  #    facet_grid(. ~ Period, labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = Educ_Young_Prop_Pr[,1], color = factor(Educ_Young_Prop_Pr[,9])),size = 3.5) +
  xlab("Andelen af de 15-24-årige af arbejdsstyrken") +
  ylab("Produktivitetsvækstraten før finanskrisen (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Young_Prop_C, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Educ_Young_Prop_C[,9])), size = 3.5) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  #    facet_grid(. ~ Period, labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = Educ_Young_Prop_C[,1], color = factor(Educ_Young_Prop_C[,9])),size = 3.5) +
  xlab("Andelen af de 15-24-årige af arbejdsstyrken") +
  ylab("Produktivitetsvækstraten under finanskrisen (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Young_Prop_Po, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Educ_Young_Prop_Po[,9])), size = 3.5) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  #    facet_grid(. ~ Period, labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue")) + 
  geom_text_repel(aes(label = Educ_Young_Prop_Po[,1], color = factor(Educ_Young_Prop_Po[,9])),size = 3.5) +
  xlab("Andelen af de 15-24-årige af arbejdsstyrken") +
  ylab("Produktivitetsvækstraten efter finanskrisen (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Young_Prop_Pr)

# The same as before with GVA_SAL

# d=  ggplot(Educ_Young_Prop_Pr, aes(EMP_Y15_24, GVA_SAL)) +
#   geom_point(aes(color = factor(Educ_Young_Prop_Pr[,9])), size = 3.5) +
#   geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
#   #    facet_grid(. ~ Period, labeller = label_both, margins = F) +
#   theme_bw() + theme(legend.position = "none") +
#   scale_color_manual(values = c("red", "darkblue")) + 
#   geom_text_repel(aes(label = Educ_Young_Prop_Pr[,1], color = factor(Educ_Young_Prop_Pr[,9])),size = 3.5) +
#   xlab("Andelen af de 15-24 årige af arbejdsstyrken (pct.)") +
#   ylab("Produktivitetsvækstraten per lønmodtager før finanskrisen (pct.)") +
#   ggtitle("Før Krisen") + 
#   theme(plot.title = element_text(hjust = 0.5))
# 
# e=  ggplot(Educ_Young_Prop_C, aes(EMP_Y15_24, GVA_SAL)) +
#   geom_point(aes(color = factor(Educ_Young_Prop_C[,9])), size = 3.5) +
#   geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
#   #    facet_grid(. ~ Period, labeller = label_both, margins = F) +
#   theme_bw() + theme(legend.position = "none") +
#   scale_color_manual(values = c("red", "darkblue")) + 
#   geom_text_repel(aes(label = Educ_Young_Prop_C[,1], color = factor(Educ_Young_Prop_C[,9])),size = 3.5) +
#   xlab("Andelen af de 15-24 årige af arbejdsstyrken (pct.)") +
#   ylab("Produktivitetsvækstraten per lønmodtager under finanskrisen (pct.)") +
#   ggtitle("Under Krisen") + 
#   theme(plot.title = element_text(hjust = 0.5))
# 
# f=  ggplot(Educ_Young_Prop_Po, aes(EMP_Y15_24, GVA_SAL)) +
#   geom_point(aes(color = factor(Educ_Young_Prop_Po[,9])), size = 3.5) +
#   geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
#   #    facet_grid(. ~ Period, labeller = label_both, margins = F) +
#   theme_bw() + theme(legend.position = "none") +
#   scale_color_manual(values = c("red", "darkblue")) + 
#   geom_text_repel(aes(label = Educ_Young_Prop_Po[,1], color = factor(Educ_Young_Prop_Po[,9])),size = 3.5) +
#   xlab("Andelen af de 15-24 årige af arbejdsstyrken (pct.)") +
#   ylab("Produktivitetsvækstraten per lønmodtager efter finanskrisen (pct.)") +
#   ggtitle("Efter Krisen") + 
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# x11()
# grid.arrange(d, e, f,  ncol= 3)
# 


# Eduction level of Young Population effect on productivity

Educ_Young_ED<- read.csv("Educ_long_Young_ED.csv", header = T)
str(Educ_Young_ED)
names(Educ_Young_ED)


# Educ_Young_ED$Delperiod = factor(Educ_Young_ED$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Young_ED$Period)[1] <- 2
# levels(Educ_Young_ED$Period)[2] <- 3
# levels(Educ_Young_ED$Period)[3] <- 1


names(Educ_Young_ED)


library(ggplot2)

dev.off()

head(Educ_Young_ED_Pr_ED0)
str(Educ_Young_ED_Pr_ED0)

# Subsetting LONG data for Graphical purposes
Educ_Young_ED_Pr_ED0 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Pre" & Educ_Young_ED$Educ=="ED0")
Educ_Young_ED_Pr_ED3 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Pre" & Educ_Young_ED$Educ=="ED3")
Educ_Young_ED_Pr_ED5 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Pre" & Educ_Young_ED$Educ=="ED5")

Educ_Young_ED_C_ED0 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Crisis" & Educ_Young_ED$Educ=="ED0")
Educ_Young_ED_C_ED3 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Crisis" & Educ_Young_ED$Educ=="ED3")
Educ_Young_ED_C_ED5 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Crisis" & Educ_Young_ED$Educ=="ED5")

Educ_Young_ED_Po_ED0 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Post" & Educ_Young_ED$Educ=="ED0")
Educ_Young_ED_Po_ED3 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Post" & Educ_Young_ED$Educ=="ED3")
Educ_Young_ED_Po_ED5 = subset(Educ_Young_ED,Educ_Young_ED$Period=="Post" & Educ_Young_ED$Educ=="ED5")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Young_ED_Pr_ED0, aes(EMP_Y15_24_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_Pr_ED0[,1], color = factor(Educ_Young_ED_Pr_ED0[,10])),size = 3.5) +
  xlab("Andelen af de 15-24-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Young_ED_C_ED0, aes(EMP_Y15_24_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_C_ED0[,1], color = factor(Educ_Young_ED_C_ED0[,10])),size = 3.5) +
  xlab("Andelen af de 15-24-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget under finanskrisen (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Young_ED_Po_ED0, aes(EMP_Y15_24_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_Po_ED0[,1], color = factor(Educ_Young_ED_Po_ED0[,10])),size = 3.5) +
  xlab("Andelen af de 15-24-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget efter finanskrisen (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Young_ED_Pr)


d= ggplot(Educ_Young_ED_Pr_ED3, aes(EMP_Y15_24_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_Pr_ED3[,1], color = factor(Educ_Young_ED_Pr_ED3[,10])),size = 3.5) +
  xlab("Andelen af 15-24-årige med mellemlange uddannelser") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Young_ED_C_ED3, aes(EMP_Y15_24_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_C_ED3[,1], color = factor(Educ_Young_ED_C_ED3[,10])),size = 3.5) +
  xlab("Andelen af 15-24-årige med mellemlange uddannelser") +
  ylab("Produktivitetsvækstraten per beskæftiget under finanskrisen (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Young_ED_Po_ED3, aes(EMP_Y15_24_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_Po_ED3[,1], color = factor(Educ_Young_ED_Po_ED3[,10])),size = 3.5) +
  xlab("Andelen af 15-24-årige med mellemlange uddannelser") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)



g= ggplot(Educ_Young_ED_Pr_ED5, aes(EMP_Y15_24_ED, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_Pr_ED5[,1], color = factor(Educ_Young_ED_Pr_ED5[,10])),size = 3.5) +
  xlab("Andelen af 15-24 årige med høje uddannelser (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Før Krisen & Højtuddannede Unge") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Young_ED_C_ED5, aes(EMP_Y15_24_ED, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_C_ED5[,1], color = factor(Educ_Young_ED_C_ED5[,10])),size = 3.5) +
  xlab("Andelen af 15-24 årige med høje uddannelser (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget under finanskrisen (pct.)") +
  ggtitle("Under Krisen & Højtuddannede Unge") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Young_ED_Po_ED5, aes(EMP_Y15_24_ED, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED_Po_ED5[,1], color = factor(Educ_Young_ED_Po_ED5[,10])),size = 3.5) +
  xlab("Andelen af 15-24 årige med høje uddannelser(pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Efter Krisen & Højtuddannede Unge") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)


# Eduction level of Adult Population effect on productivity

Educ_Adults_ED<- read.csv("Educ_long_Adults_ED.csv", header = T)
str(Educ_Adults_ED)
names(Educ_Adults_ED)


# Educ_Adults_ED$Delperiod = factor(Educ_Adults_ED$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Adults_ED$Period)[1] <- 2
# levels(Educ_Adults_ED$Period)[2] <- 3
# levels(Educ_Adults_ED$Period)[3] <- 1


names(Educ_Adults_ED)


library(ggplot2)

dev.off()

head(Educ_Adults_ED_Pr_ED0)
str(Educ_Adults_ED_Pr_ED0)

# Subsetting LONG data for Graphical purposes
Educ_Adults_ED_Pr_ED0 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Pre" & Educ_Adults_ED$Educ=="ED0")
Educ_Adults_ED_Pr_ED3 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Pre" & Educ_Adults_ED$Educ=="ED3")
Educ_Adults_ED_Pr_ED5 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Pre" & Educ_Adults_ED$Educ=="ED5")

Educ_Adults_ED_C_ED0 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Crisis" & Educ_Adults_ED$Educ=="ED0")
Educ_Adults_ED_C_ED3 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Crisis" & Educ_Adults_ED$Educ=="ED3")
Educ_Adults_ED_C_ED5 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Crisis" & Educ_Adults_ED$Educ=="ED5")

Educ_Adults_ED_Po_ED0 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Post" & Educ_Adults_ED$Educ=="ED0")
Educ_Adults_ED_Po_ED3 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Post" & Educ_Adults_ED$Educ=="ED3")
Educ_Adults_ED_Po_ED5 = subset(Educ_Adults_ED,Educ_Adults_ED$Period=="Post" & Educ_Adults_ED$Educ=="ED5")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Adults_ED_Pr_ED0, aes(EMP_Y25_64_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_Pr_ED0[,1], color = factor(Educ_Adults_ED_Pr_ED0[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Adults_ED_C_ED0, aes(EMP_Y25_64_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_C_ED0[,1], color = factor(Educ_Adults_ED_C_ED0[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Adults_ED_Po_ED0, aes(EMP_Y25_64_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_Po_ED0[,1], color = factor(Educ_Adults_ED_Po_ED0[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Adults_ED_Pr)


d= ggplot(Educ_Adults_ED_Pr_ED3, aes(EMP_Y25_64_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_Pr_ED3[,1], color = factor(Educ_Adults_ED_Pr_ED3[,10])),size = 3.5) +
  xlab("Andelen af 25-64-årige med mellemlange uddannelser") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Adults_ED_C_ED3, aes(EMP_Y25_64_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_C_ED3[,1], color = factor(Educ_Adults_ED_C_ED3[,10])),size = 3.5) +
  xlab("Andelen af 25-64-årige med mellemlange uddannelser") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Adults_ED_Po_ED3, aes(EMP_Y25_64_ED, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_Po_ED3[,1], color = factor(Educ_Adults_ED_Po_ED3[,10])),size = 3.5) +
  xlab("Andelen af 25-64-årige med mellemlange uddannelser") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)


g= ggplot(Educ_Adults_ED_Pr_ED5, aes(EMP_Y25_64_ED, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_Pr_ED5[,1], color = factor(Educ_Adults_ED_Pr_ED5[,10])),size = 3.5) +
  xlab("Andelen af 25-64 årige med høje uddannelser (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Før Krisen & Højtuddannede Voksne") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Adults_ED_C_ED5, aes(EMP_Y25_64_ED, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_C_ED5[,1], color = factor(Educ_Adults_ED_C_ED5[,10])),size = 3.5) +
  xlab("Andelen af 25-64 årige med høje uddannelser (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Under Krisen & Højtuddannede Voksne") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Adults_ED_Po_ED5, aes(EMP_Y25_64_ED, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED_Po_ED5[,1], color = factor(Educ_Adults_ED_Po_ED5[,10])),size = 3.5) +
  xlab("Andelen af 25-64 årige med høje uddannelser(pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Efter Krisen & Højtuddannede Voksne") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)


# Eduction level ED0 of Adult Population effect on productivity

Educ_Adults_ED0_G<- read.csv("Educ_long_Adults_G_ED0.csv", header = T)
str(Educ_Adults_ED0_G)
names(Educ_Adults_ED0_G)


# Educ_Adults_ED0_G$Delperiod = factor(Educ_Adults_ED0_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Adults_ED0_G$Period)[1] <- 2
# levels(Educ_Adults_ED0_G$Period)[2] <- 3
# levels(Educ_Adults_ED0_G$Period)[3] <- 1


names(Educ_Adults_ED0_G)


library(ggplot2)

dev.off()

head(Educ_Adults_ED0_G_Pr_ED0)
str(Educ_Adults_ED0_G_Pr_ED0)


# Subsetting LONG data for Graphical purposes
Educ_Adults_ED0_G_Pr_M = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Pre" & Educ_Adults_ED0_G$Gender=="M")
Educ_Adults_ED0_G_Pr_F = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Pre" & Educ_Adults_ED0_G$Gender=="F")
Educ_Adults_ED0_G_Pr_T = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Pre" & Educ_Adults_ED0_G$Gender=="T")

Educ_Adults_ED0_G_C_M = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Crisis" & Educ_Adults_ED0_G$Gender=="M")
Educ_Adults_ED0_G_C_F = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Crisis" & Educ_Adults_ED0_G$Gender=="F")
Educ_Adults_ED0_G_C_T = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Crisis" & Educ_Adults_ED0_G$Gender=="T")

Educ_Adults_ED0_G_Po_M = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Post" & Educ_Adults_ED0_G$Gender=="M")
Educ_Adults_ED0_G_Po_F = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Post" & Educ_Adults_ED0_G$Gender=="F")
Educ_Adults_ED0_G_Po_T = subset(Educ_Adults_ED0_G,Educ_Adults_ED0_G$Period=="Post" & Educ_Adults_ED0_G$Gender=="T")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Adults_ED0_G_Pr_M, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_Pr_M[,1], color = factor(Educ_Adults_ED0_G_Pr_M[,10])),size = 3.5) +
  xlab("Mænds andel af de 25-64-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænds andel i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Adults_ED0_G_Pr_F, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_Pr_F[,1], color = factor(Educ_Adults_ED0_G_Pr_F[,10])),size = 3.5) +
  xlab("Kvinders andel af de 25-64 årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinders andel i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Adults_ED0_G_Pr_T, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_Pr_T[,1], color = factor(Educ_Adults_ED0_G_Pr_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("De lavt uddannede i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Adults_ED0_G_Pr)



d= ggplot(Educ_Adults_ED0_G_C_M, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_C_M[,1], color = factor(Educ_Adults_ED0_G_C_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede Mænds andel i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Adults_ED0_G_C_F, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_C_F[,1], color = factor(Educ_Adults_ED0_G_C_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Adults_ED0_G_C_T, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_C_T[,1], color = factor(Educ_Adults_ED0_G_C_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)

g= ggplot(Educ_Adults_ED0_G_Po_M, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_Po_M[,1], color = factor(Educ_Adults_ED0_G_Po_M[,10])),size = 3.5) +
  xlab("Mænds andel af de 25-64-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænds andel i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Adults_ED0_G_Po_F, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_Po_F[,1], color = factor(Educ_Adults_ED0_G_Po_F[,10])),size = 3.5) +
  xlab("Kvinders andel af de 25-64-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinders andel i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Adults_ED0_G_Po_T, aes(EMP_Y25_64_ED0, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED0_G_Po_T[,1], color = factor(Educ_Adults_ED0_G_Po_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med lavt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("De lavtuddannede i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)


# Eduction level ED3 of Adult Population effect on productivity

Educ_Adults_ED3_G<- read.csv("Educ_long_Adults_G_ED3.csv", header = T)
str(Educ_Adults_ED3_G)
names(Educ_Adults_ED3_G)


# Educ_Adults_ED3_G$Delperiod = factor(Educ_Adults_ED3_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Adults_ED3_G$Period)[1] <- 2
# levels(Educ_Adults_ED3_G$Period)[2] <- 3
# levels(Educ_Adults_ED3_G$Period)[3] <- 1


names(Educ_Adults_ED3_G)


library(ggplot2)

dev.off()

head(Educ_Adults_ED3_G_Pr_ED0)
str(Educ_Adults_ED3_G_Pr_ED0)

# Subsetting LONG data for Graphical purposes
Educ_Adults_ED3_G_Pr_M = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Pre" & Educ_Adults_ED3_G$Gender=="M")
Educ_Adults_ED3_G_Pr_F = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Pre" & Educ_Adults_ED3_G$Gender=="F")
Educ_Adults_ED3_G_Pr_T = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Pre" & Educ_Adults_ED3_G$Gender=="T")

Educ_Adults_ED3_G_C_M = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Crisis" & Educ_Adults_ED3_G$Gender=="M")
Educ_Adults_ED3_G_C_F = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Crisis" & Educ_Adults_ED3_G$Gender=="F")
Educ_Adults_ED3_G_C_T = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Crisis" & Educ_Adults_ED3_G$Gender=="T")

Educ_Adults_ED3_G_Po_M = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Post" & Educ_Adults_ED3_G$Gender=="M")
Educ_Adults_ED3_G_Po_F = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Post" & Educ_Adults_ED3_G$Gender=="F")
Educ_Adults_ED3_G_Po_T = subset(Educ_Adults_ED3_G,Educ_Adults_ED3_G$Period=="Post" & Educ_Adults_ED3_G$Gender=="T")

# Centering all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Adults_ED3_G_Pr_M, aes(EMP_Y25_64_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_Pr_M[,1], color = factor(Educ_Adults_ED3_G_Pr_M[,10])),size = 3.5) +
  xlab("Mænds andel af de 25-64-årige med mellemlangt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mellemlangt uddannede mænds andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Adults_ED3_G_Pr_F, aes(EMP_Y25_64_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_Pr_F[,1], color = factor(Educ_Adults_ED3_G_Pr_F[,10])),size = 3.5) +
  xlab("Kvinders andel af de 25-64-årige med mellemlangt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mellemlangt uddannede kvinders andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Adults_ED3_G_Pr_T, aes(EMP_Y25_64_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_Pr_T[,1], color = factor(Educ_Adults_ED3_G_Pr_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med mellemlangt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("De mellemlangt uddannedes andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Adults_ED3_G_Pr)


d= ggplot(Educ_Adults_ED3_G_C_M, aes(EMP_Y25_64_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_C_M[,1], color = factor(Educ_Adults_ED3_G_C_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med mellemlangt uddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("mellemlangtuddannede mænder af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Adults_ED3_G_C_F, aes(EMP_Y25_64_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_C_F[,1], color = factor(Educ_Adults_ED3_G_C_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med mellemlangtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("mellemlangtuddannede kvinder af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Adults_ED3_G_C_T, aes(EMP_Y25_64_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_C_T[,1], color = factor(Educ_Adults_ED3_G_C_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med mellemlangtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("mellemlangtuddannede voksne af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)


g= ggplot(Educ_Adults_ED3_G_Po_M, aes(EMP_Y25_64_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_Po_M[,1], color = factor(Educ_Adults_ED3_G_Po_M[,10])),size = 3.5) +
  xlab("Mænds andel af de 25-64-årige med mellemlangt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mellemlangt uddannede mænds andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Adults_ED3_G_Po_F, aes(EMP_Y25_64_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_Po_F[,1], color = factor(Educ_Adults_ED3_G_Po_F[,10])),size = 3.5) +
  xlab("Kvinders andel af de 25-64-årige med mellemlangt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Mellemlangt uddannede kvinders andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Adults_ED3_G_Po_T, aes(EMP_Y25_64_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED3_G_Po_T[,1], color = factor(Educ_Adults_ED3_G_Po_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med mellemlangt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("De mellemlangt uddannedes andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)


# Eduction level ED3 of Adult Population effect on productivity

Educ_Adults_ED5_G<- read.csv("Educ_long_Adults_G_ED5.csv", header = T)
str(Educ_Adults_ED5_G)
names(Educ_Adults_ED5_G)


# Educ_Adults_ED5_G$Delperiod = factor(Educ_Adults_ED5_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Adults_ED5_G$Period)[1] <- 2
# levels(Educ_Adults_ED5_G$Period)[2] <- 3
# levels(Educ_Adults_ED5_G$Period)[3] <- 1


names(Educ_Adults_ED5_G)


library(ggplot2)

dev.off()

head(Educ_Adults_ED5_G_Pr_ED0)
str(Educ_Adults_ED5_G_Pr_ED0)

# Subsetting LONG data for Graphical purposes
Educ_Adults_ED5_G_Pr_M = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Pre" & Educ_Adults_ED5_G$Gender=="M")
Educ_Adults_ED5_G_Pr_F = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Pre" & Educ_Adults_ED5_G$Gender=="F")
Educ_Adults_ED5_G_Pr_T = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Pre" & Educ_Adults_ED5_G$Gender=="T")

Educ_Adults_ED5_G_C_M = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Crisis" & Educ_Adults_ED5_G$Gender=="M")
Educ_Adults_ED5_G_C_F = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Crisis" & Educ_Adults_ED5_G$Gender=="F")
Educ_Adults_ED5_G_C_T = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Crisis" & Educ_Adults_ED5_G$Gender=="T")

Educ_Adults_ED5_G_Po_M = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Post" & Educ_Adults_ED5_G$Gender=="M")
Educ_Adults_ED5_G_Po_F = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Post" & Educ_Adults_ED5_G$Gender=="F")
Educ_Adults_ED5_G_Po_T = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Post" & Educ_Adults_ED5_G$Gender=="T")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Adults_ED5_G_Pr_M, aes(EMP_Y25.64_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_Pr_M[,1], color = factor(Educ_Adults_ED5_G_Pr_M[,10])),size = 3.5) +
  xlab("Mænds andel af de 25-64-årige med højt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Højtuddannede mænds andel af de 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Adults_ED5_G_Pr_F, aes(EMP_Y25.64_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_Pr_F[,1], color = factor(Educ_Adults_ED5_G_Pr_F[,10])),size = 3.5) +
  xlab("Kvinders andel af de 25-64-årige med højt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Højtuddannede kvinders andel af de 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Adults_ED5_G_Pr_T, aes(EMP_Y25.64_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_Pr_T[,1], color = factor(Educ_Adults_ED5_G_Pr_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med højt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("De højtuddannedes andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Adults_ED5_G_Pr)


d= ggplot(Educ_Adults_ED5_G_C_M, aes(EMP_Y25.64_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_C_M[,1], color = factor(Educ_Adults_ED5_G_C_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Adults_ED5_G_C_F, aes(EMP_Y25.64_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_C_F[,1], color = factor(Educ_Adults_ED5_G_C_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Adults_ED5_G_C_T, aes(EMP_Y25.64_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_C_T[,1], color = factor(Educ_Adults_ED5_G_C_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)



g= ggplot(Educ_Adults_ED5_G_Po_M, aes(EMP_Y25.64_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_Po_M[,1], color = factor(Educ_Adults_ED5_G_Po_M[,10])),size = 3.5) +
  xlab("Mænds andel af de 25-64-årige med højt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Højtuddannede mænds andel af de 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Adults_ED5_G_Po_F, aes(EMP_Y25.64_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_Po_F[,1], color = factor(Educ_Adults_ED5_G_Po_F[,10])),size = 3.5) +
  xlab("Kvinders andel af de 25-64-årige med højt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Højtuddannede kvinders andel af de 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Adults_ED5_G_Po_T, aes(EMP_Y25.64_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Adults_ED5_G_Po_T[,1], color = factor(Educ_Adults_ED5_G_Po_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64-årige med højt uddanelsesniveau") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("De højtuddannedes andel af 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)



# Subsetting LONG data for Graphical purposes
Educ_Adults_ED5_G_Pr_M = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Pre" & Educ_Adults_ED5_G$Gender=="M")
Educ_Adults_ED5_G_Pr_F = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Pre" & Educ_Adults_ED5_G$Gender=="F")
Educ_Adults_ED5_G_Pr_T = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Pre" & Educ_Adults_ED5_G$Gender=="T")

Educ_Adults_ED5_G_C_M = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Crisis" & Educ_Adults_ED5_G$Gender=="M")
Educ_Adults_ED5_G_C_F = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Crisis" & Educ_Adults_ED5_G$Gender=="F")
Educ_Adults_ED5_G_C_T = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Crisis" & Educ_Adults_ED5_G$Gender=="T")

Educ_Adults_ED5_G_Po_M = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Post" & Educ_Adults_ED5_G$Gender=="M")
Educ_Adults_ED5_G_Po_F = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Post" & Educ_Adults_ED5_G$Gender=="F")
Educ_Adults_ED5_G_Po_T = subset(Educ_Adults_ED5_G,Educ_Adults_ED5_G$Period=="Post" & Educ_Adults_ED5_G$Gender=="T")


head(Educ_Adults_ED5_G_Pr_M)
dim(Educ_Adults_ED5_G_Pr_M)


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


# 
# 
# ggbox_x <- function(x) {
#   title <- paste("Boxplot af", x, "fordelt ud fra Landsdelens Navn og Perioden")
#   ggplot(Educ_Young_Prop, aes(EMP_Y15_24, GVA_EMP)) +
#     facet_grid(. ~ Period, labeller = label_both, margins = F) + 
#     geom_point() +
#     scale_y_continuous(name = "Pct.") + 
#     scale_x_discrete(name = "Landsdelsnavn") +
#     ggtitle(title) +
#     theme_bw() +
#     theme(legend.position="none")
# }
# lapply(plotCols0, ggbox)
# 
# 
# 
# ggbox <- function(x) {
#   title <- paste("Scatterplot af", x, "med Produktivitetn på tværs af bestemte EU-regioner")
#   ggplot(Educ_Young_Prop, aes(EMP_Y15_24, GVA_EMP, color=factor(Area))) +
#     geom_point() + # facet_grid(Landsdel_navn ~ Område) +
#     # stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
#     # stat_smooth(aes(group = 1)) +
#     theme_bw()
# }
# 
# lapply(plotCols0, ggbox)
# 



# Eduction level ED0 of Young Population effect on productivity

Educ_Young_ED0_G<- read.csv("Educ_long_Young_G_ED0.csv", header = T)
str(Educ_Young_ED0_G)
names(Educ_Young_ED0_G)


# Educ_Young_ED0_G$Delperiod = factor(Educ_Young_ED0_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Young_ED0_G$Period)[1] <- 2
# levels(Educ_Young_ED0_G$Period)[2] <- 3
# levels(Educ_Young_ED0_G$Period)[3] <- 1


names(Educ_Young_ED0_G)


library(ggplot2)

dev.off()

head(Educ_Young_ED0_G_Pr_ED0)
str(Educ_Young_ED0_G_Pr_ED0)


# Subsetting LONG data for Graphical purposes
Educ_Young_ED0_G_Pr_M = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Pre" & Educ_Young_ED0_G$Gender=="M")
Educ_Young_ED0_G_Pr_F = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Pre" & Educ_Young_ED0_G$Gender=="F")
Educ_Young_ED0_G_Pr_T = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Pre" & Educ_Young_ED0_G$Gender=="T")

Educ_Young_ED0_G_C_M = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Crisis" & Educ_Young_ED0_G$Gender=="M")
Educ_Young_ED0_G_C_F = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Crisis" & Educ_Young_ED0_G$Gender=="F")
Educ_Young_ED0_G_C_T = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Crisis" & Educ_Young_ED0_G$Gender=="T")

Educ_Young_ED0_G_Po_M = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Post" & Educ_Young_ED0_G$Gender=="M")
Educ_Young_ED0_G_Po_F = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Post" & Educ_Young_ED0_G$Gender=="F")
Educ_Young_ED0_G_Po_T = subset(Educ_Young_ED0_G,Educ_Young_ED0_G$Period=="Post" & Educ_Young_ED0_G$Gender=="T")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Young_ED0_G_Pr_M, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_Pr_M[,1], color = factor(Educ_Young_ED0_G_Pr_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Young_ED0_G_Pr_F, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_Pr_F[,1], color = factor(Educ_Young_ED0_G_Pr_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Young_ED0_G_Pr_T, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_Pr_T[,1], color = factor(Educ_Young_ED0_G_Pr_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Young_ED0_G_Pr)



d= ggplot(Educ_Young_ED0_G_C_M, aes(EMP_Y15_24, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_C_M[,1], color = factor(Educ_Young_ED0_G_C_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Young_ED0_G_C_F, aes(EMP_Y15_24, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_C_F[,1], color = factor(Educ_Young_ED0_G_C_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Young_ED0_G_C_T, aes(EMP_Y15_24, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_C_T[,1], color = factor(Educ_Young_ED0_G_C_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)



g= ggplot(Educ_Young_ED0_G_Po_M, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_Po_M[,1], color = factor(Educ_Young_ED0_G_Po_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Young_ED0_G_Po_F, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_Po_F[,1], color = factor(Educ_Young_ED0_G_Po_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Young_ED0_G_Po_T, aes(EMP_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED0_G_Po_T[,1], color = factor(Educ_Young_ED0_G_Po_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)



# Eduction level ED3 of Adult Population effect on productivity

Educ_Young_ED3_G<- read.csv("Educ_long_Young_G_ED3.csv", header = T)
str(Educ_Young_ED3_G)
names(Educ_Young_ED3_G)


# Educ_Young_ED3_G$Delperiod = factor(Educ_Young_ED3_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Young_ED3_G$Period)[1] <- 2
# levels(Educ_Young_ED3_G$Period)[2] <- 3
# levels(Educ_Young_ED3_G$Period)[3] <- 1


names(Educ_Young_ED3_G)


library(ggplot2)

dev.off()

head(Educ_Young_ED3_G_Pr_ED0)
str(Educ_Young_ED3_G_Pr_ED0)

# Subsetting LONG data for Graphical purposes
Educ_Young_ED3_G_Pr_M = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Pre" & Educ_Young_ED3_G$Gender=="M")
Educ_Young_ED3_G_Pr_F = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Pre" & Educ_Young_ED3_G$Gender=="F")
Educ_Young_ED3_G_Pr_T = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Pre" & Educ_Young_ED3_G$Gender=="T")

Educ_Young_ED3_G_C_M = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Crisis" & Educ_Young_ED3_G$Gender=="M")
Educ_Young_ED3_G_C_F = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Crisis" & Educ_Young_ED3_G$Gender=="F")
Educ_Young_ED3_G_C_T = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Crisis" & Educ_Young_ED3_G$Gender=="T")

Educ_Young_ED3_G_Po_M = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Post" & Educ_Young_ED3_G$Gender=="M")
Educ_Young_ED3_G_Po_F = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Post" & Educ_Young_ED3_G$Gender=="F")
Educ_Young_ED3_G_Po_T = subset(Educ_Young_ED3_G,Educ_Young_ED3_G$Period=="Post" & Educ_Young_ED3_G$Gender=="T")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Young_ED3_G_Pr_M, aes(EMP_Y15_24_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_Pr_M[,1], color = factor(Educ_Young_ED3_G_Pr_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Young_ED3_G_Pr_F, aes(EMP_Y15_24_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_Pr_F[,1], color = factor(Educ_Young_ED3_G_Pr_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Young_ED3_G_Pr_T, aes(EMP_Y15_24_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_Pr_T[,1], color = factor(Educ_Young_ED3_G_Pr_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Young_ED3_G_Pr)



d= ggplot(Educ_Young_ED3_G_C_M, aes(EMP_Y15_24_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_C_M[,1], color = factor(Educ_Young_ED3_G_C_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Young_ED3_G_C_F, aes(EMP_Y15_24_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_C_F[,1], color = factor(Educ_Young_ED3_G_C_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Young_ED3_G_C_T, aes(EMP_Y15_24_ED3, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_C_T[,1], color = factor(Educ_Young_ED3_G_C_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)



g= ggplot(Educ_Young_ED3_G_Po_M, aes(EMP_Y15_24_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_Po_M[,1], color = factor(Educ_Young_ED3_G_Po_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Young_ED3_G_Po_F, aes(EMP_Y15_24_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_Po_F[,1], color = factor(Educ_Young_ED3_G_Po_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Young_ED3_G_Po_T, aes(EMP_Y15_24_ED3, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED3_G_Po_T[,1], color = factor(Educ_Young_ED3_G_Po_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)




# Eduction level ED3 of Adult Population effect on productivity

Educ_Young_ED5_G<- read.csv("Educ_long_Young_G_ED5.csv", header = T)
str(Educ_Young_ED5_G)
names(Educ_Young_ED5_G)


# Educ_Young_ED5_G$Delperiod = factor(Educ_Young_ED5_G$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Young_ED5_G$Period)[1] <- 2
# levels(Educ_Young_ED5_G$Period)[2] <- 3
# levels(Educ_Young_ED5_G$Period)[3] <- 1


names(Educ_Young_ED5_G)


library(ggplot2)

dev.off()

head(Educ_Young_ED5_G_Pr_ED0)
str(Educ_Young_ED5_G_Pr_ED0)

# Subsetting LONG data for Graphical purposes
Educ_Young_ED5_G_Pr_M = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Pre" & Educ_Young_ED5_G$Gender=="M")
Educ_Young_ED5_G_Pr_F = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Pre" & Educ_Young_ED5_G$Gender=="F")
Educ_Young_ED5_G_Pr_T = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Pre" & Educ_Young_ED5_G$Gender=="T")

Educ_Young_ED5_G_C_M = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Crisis" & Educ_Young_ED5_G$Gender=="M")
Educ_Young_ED5_G_C_F = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Crisis" & Educ_Young_ED5_G$Gender=="F")
Educ_Young_ED5_G_C_T = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Crisis" & Educ_Young_ED5_G$Gender=="T")

Educ_Young_ED5_G_Po_M = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Post" & Educ_Young_ED5_G$Gender=="M")
Educ_Young_ED5_G_Po_F = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Post" & Educ_Young_ED5_G$Gender=="F")
Educ_Young_ED5_G_Po_T = subset(Educ_Young_ED5_G,Educ_Young_ED5_G$Period=="Post" & Educ_Young_ED5_G$Gender=="T")

# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Educ_Young_ED5_G_Pr_M, aes(EMP_Y15_24_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_Pr_M[,1], color = factor(Educ_Young_ED5_G_Pr_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Educ_Young_ED5_G_Pr_F, aes(EMP_Y15_24_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_Pr_F[,1], color = factor(Educ_Young_ED5_G_Pr_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Educ_Young_ED5_G_Pr_T, aes(EMP_Y15_24_ED5, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_Pr_T[,1], color = factor(Educ_Young_ED5_G_Pr_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Educ_Young_ED5_G_Pr)



d= ggplot(Educ_Young_ED5_G_C_M, aes(EMP_Y15_24_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_C_M[,1], color = factor(Educ_Young_ED5_G_C_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Educ_Young_ED5_G_C_F, aes(EMP_Y15_24_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_C_F[,1], color = factor(Educ_Young_ED5_G_C_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Educ_Young_ED5_G_C_T, aes(EMP_Y15_24_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_C_T[,1], color = factor(Educ_Young_ED5_G_C_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)


g= ggplot(Educ_Young_ED5_G_Po_M, aes(EMP_Y15_24_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_Po_M[,1], color = factor(Educ_Young_ED5_G_Po_M[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede mænder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Educ_Young_ED5_G_Po_F, aes(EMP_Y15_24_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_Po_F[,1], color = factor(Educ_Young_ED5_G_Po_F[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede kvinder i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Educ_Young_ED5_G_Po_T, aes(EMP_Y15_24_ED5, GVA_SAL)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Educ_Young_ED5_G_Po_T[,1], color = factor(Educ_Young_ED5_G_Po_T[,10])),size = 3.5) +
  xlab("Andelen af de 25-64 årige med lavtuddanelsesniveau (pct.)") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Lavtuddannede voksne i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)

# 
# 
# ggbox_x <- function(x) {
#   title <- paste("Boxplot af", x, "fordelt ud fra Landsdelens Navn og Perioden")
#   ggplot(Educ_Young_Prop, aes(EMP_Y15_24, GVA_EMP)) +
#     facet_grid(. ~ Period, labeller = label_both, margins = F) + 
#     geom_point() +
#     scale_y_continuous(name = "Pct.") + 
#     scale_x_discrete(name = "Landsdelsnavn") +
#     ggtitle(title) +
#     theme_bw() +
#     theme(legend.position="none")
# }
# lapply(plotCols0, ggbox)
# 
# 
# 
# ggbox <- function(x) {
#   title <- paste("Scatterplot af", x, "med Produktivitetn på tværs af bestemte EU-regioner")
#   ggplot(Educ_Young_Prop, aes(EMP_Y15_24, GVA_EMP, color=factor(Area))) +
#     geom_point() + # facet_grid(Landsdel_navn ~ Område) +
#     # stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
#     # stat_smooth(aes(group = 1)) +
#     theme_bw()
# }
# 
# lapply(plotCols0, ggbox)








