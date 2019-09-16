

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


Emp_Age_Prop<- read.csv("Prof_long_Age_Prop.csv", header = T)
str(Emp_Age_Prop)
names(Emp_Age_Prop)
head(Emp_Age_Prop)


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
Emp_Age_Prop_Pr_Y = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Pre" & Emp_Age_Prop$Age=="Y15_24")
Emp_Age_Prop_Pr_A = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Pre" & Emp_Age_Prop$Age=="Y25_64")
Emp_Age_Prop_Pr_O = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Pre" & Emp_Age_Prop$Age=="Y65_74")

Emp_Age_Prop_C_Y = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Crisis" & Emp_Age_Prop$Age=="Y15_24")
Emp_Age_Prop_C_A = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Crisis" & Emp_Age_Prop$Age=="Y25_64")
Emp_Age_Prop_C_O = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Crisis" & Emp_Age_Prop$Age=="Y65_74")

Emp_Age_Prop_Po_Y = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Post" & Emp_Age_Prop$Age=="Y15_24")
Emp_Age_Prop_Po_A = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Post" & Emp_Age_Prop$Age=="Y25_64")
Emp_Age_Prop_Po_O = subset(Emp_Age_Prop,Emp_Age_Prop$Period=="Post" & Emp_Age_Prop$Age=="Y65_74")

# Centering all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Emp_Age_Prop_Pr_Y, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_Pr_Y[,1], color = factor(Emp_Age_Prop_Pr_Y[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Emp_Age_Prop_Pr_A, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_Pr_A[,1], color = factor(Emp_Age_Prop_Pr_A[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Emp_Age_Prop_Pr_O, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_Pr_O[,1], color = factor(Emp_Age_Prop_Pr_O[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 65_74") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Emp_Age_Prop_Pr)



d= ggplot(Emp_Age_Prop_C_Y, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_C_Y[,1], color = factor(Emp_Age_Prop_C_Y[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Emp_Age_Prop_C_A, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_C_A[,1], color = factor(Emp_Age_Prop_C_A[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Emp_Age_Prop_C_O, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_C_O[,1], color = factor(Emp_Age_Prop_C_O[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 65_74") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)


g= ggplot(Emp_Age_Prop_Po_Y, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_Po_Y[,1], color = factor(Emp_Age_Prop_Po_Y[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 15_24") + 
  theme(plot.title = element_text(hjust = 0.5))

h=  ggplot(Emp_Age_Prop_Po_A, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_Po_A[,1], color = factor(Emp_Age_Prop_Po_A[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 25_64") + 
  theme(plot.title = element_text(hjust = 0.5))

i=  ggplot(Emp_Age_Prop_Po_O, aes(EMP_Prop, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Educ), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Emp_Age_Prop_Po_O[,1], color = factor(Emp_Age_Prop_Po_O[,10])),size = 3.5) +
  xlab("Beskæftigelsesratio") +
  ylab("Produktivitetsvækstraten per beskæftiget (pct.)") +
  ggtitle("Samlet beskæftigelsesrate i alderen 65_74") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(g, h, i,  ncol= 3)


# Eduction level of Young Population effect on productivity

Prof_Young<- read.csv("Prof_long_Young.csv", header = T)
str(Prof_Young)
names(Prof_Young)


# Educ_Young_ED$Delperiod = factor(Educ_Young_ED$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Young_ED$Period)[1] <- 2
# levels(Educ_Young_ED$Period)[2] <- 3
# levels(Educ_Young_ED$Period)[3] <- 1


names(Educ_Young_ED)


library(ggplot2)

dev.off()

head(Prof_Young_Pr_SAL)
str(Prof_Young_Pr_SAL)

# Subsetting LONG data for Graphical purposes
Prof_Young_Pr_SAL = subset(Prof_Young,Prof_Young$Period=="Pre" & Prof_Young$Profession=="SAL")
Prof_Young_Pr_SELF = subset(Prof_Young,Prof_Young$Period=="Pre" & Prof_Young$Profession=="SELF")


Prof_Young_C_SAL = subset(Prof_Young,Prof_Young$Period=="Crisis" & Prof_Young$Profession=="SAL")
Prof_Young_C_SELF = subset(Prof_Young,Prof_Young$Period=="Crisis" & Prof_Young$Profession=="SELF")


Prof_Young_Po_SAL = subset(Prof_Young,Prof_Young$Period=="Post" & Prof_Young$Profession=="SAL")
Prof_Young_Po_SELF = subset(Prof_Young,Prof_Young$Period=="Post" & Prof_Young$Profession=="SELF")


# Centering by default all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Prof_Young_Pr_SAL, aes(Prof_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Young_Pr_SAL[,1], color = factor(Prof_Young_Pr_SAL[,10])),size = 3.5) +
  xlab("Lønmodtagersandel af de 15-24-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Prof_Young_C_SAL, aes(Prof_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Young_C_SAL[,1], color = factor(Prof_Young_C_SAL[,10])),size = 3.5) +
  xlab("Lønmodtagersandel af de 15-24-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget under finanskrisen (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Prof_Young_Po_SAL, aes(Prof_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Young_Po_SAL[,1], color = factor(Prof_Young_Po_SAL[,10])),size = 3.5) +
  xlab("Lønmodtagersandel af de 15-24-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget efter finanskrisen (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Prof_Young_Pr)


d= ggplot(Prof_Young_Pr_SELF, aes(Prof_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Young_Pr_SELF[,1], color = factor(Prof_Young_Pr_SELF[,10])),size = 3.5) +
  xlab("De selvstændiges andel af de 15-24-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Prof_Young_C_SELF, aes(Prof_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Young_C_SELF[,1], color = factor(Prof_Young_C_SELF[,10])),size = 3.5) +
  xlab("De selvstændiges andel af de 15-24-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget under finanskrisen (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Prof_Young_Po_SELF, aes(Prof_Y15_24, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Young_Po_SELF[,1], color = factor(Prof_Young_Po_SELF[,10])),size = 3.5) +
  xlab("De selvstændiges andel af de 15-24-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget efter finanskrisen (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)


# Eduction level of Adult Population effect on productivity

Prof_Adult<- read.csv("Prof_long_Adult.csv", header = T)
str(Prof_Adult)
names(Prof_Adult)


# Educ_Young_ED$Delperiod = factor(Educ_Young_ED$Period, levels=c("Crisis","Post","Pre"), labels=c("Krisen", "Pre","Post")) 
# 
# levels(Educ_Young_ED$Period)[1] <- 2
# levels(Educ_Young_ED$Period)[2] <- 3
# levels(Educ_Young_ED$Period)[3] <- 1


names(Educ_Young_ED)


library(ggplot2)

dev.off()

head(Prof_Adult_Pr_SAL)
str(Prof_Adult_Pr_SAL)


# Subsetting LONG data for Graphical purposes
Prof_Adult_Pr_SAL = subset(Prof_Adult,Prof_Adult$Period=="Pre" & Prof_Adult$Profession=="SAL")
Prof_Adult_Pr_SELF = subset(Prof_Adult,Prof_Adult$Period=="Pre" & Prof_Adult$Profession=="SELF")


Prof_Adult_C_SAL = subset(Prof_Adult,Prof_Adult$Period=="Crisis" & Prof_Adult$Profession=="SAL")
Prof_Adult_C_SELF = subset(Prof_Adult,Prof_Adult$Period=="Crisis" & Prof_Adult$Profession=="SELF")


Prof_Adult_Po_SAL = subset(Prof_Adult,Prof_Adult$Period=="Post" & Prof_Adult$Profession=="SAL")
Prof_Adult_Po_SELF = subset(Prof_Adult,Prof_Adult$Period=="Post" & Prof_Adult$Profession=="SELF")


# Centering all titles
# theme_update(plot.title = element_text(hjust = 0.5))


a= ggplot(Prof_Adult_Pr_SAL, aes(Prof_Y25_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Adult_Pr_SAL[,1], color = factor(Prof_Adult_Pr_SAL[,10])),size = 3.5) +
  xlab("Lønmodtagersandel af de 25-64-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

b=  ggplot(Prof_Adult_C_SAL, aes(Prof_Y25_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Adult_C_SAL[,1], color = factor(Prof_Adult_C_SAL[,10])),size = 3.5) +
  xlab("Lønmodtagersandel af de 25-64-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget under finanskrisen (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

c=  ggplot(Prof_Adult_Po_SAL, aes(Prof_Y25_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Adult_Po_SAL[,1], color = factor(Prof_Adult_Po_SAL[,10])),size = 3.5) +
  xlab("Lønmodtagersandel af de 25-64-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget efter finanskrisen (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


require(gridExtra)

x11()
grid.arrange(a, b, c,  ncol= 3)

names(Prof_Adult_Pr)


d= ggplot(Prof_Adult_Pr_SELF, aes(Prof_Y25_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Adult_Pr_SELF[,1], color = factor(Prof_Adult_Pr_SELF[,10])),size = 3.5) +
  xlab("De selvstændiges andel af de 25-64-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget før finanskrisen (pct.)") +
  ggtitle("Før Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

e=  ggplot(Prof_Adult_C_SELF, aes(Prof_Y25_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Adult_C_SELF[,1], color = factor(Prof_Adult_C_SELF[,10])),size = 3.5) +
  xlab("De selvstændiges andel af de 25-64-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget under finanskrisen (pct.)") +
  ggtitle("Under Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))

f=  ggplot(Prof_Adult_Po_SELF, aes(Prof_Y25_64, GVA_EMP)) +
  geom_point(aes(color = factor(Area), size = 3.5)) +
  geom_smooth(method=lm, color = "blue", se=FALSE, fullrange=TRUE) +
  # facet_grid(. ~ factor(Profession), labeller = label_both, margins = F) +
  theme_bw() + theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkblue"))   + 
  geom_text_repel(aes(label = Prof_Adult_Po_SELF[,1], color = factor(Prof_Adult_Po_SELF[,10])),size = 3.5) +
  xlab("De selvstændiges andel af de 25-64-årige") +
  ylab("Produktivitetsvækstraten per beskæftiget efter finanskrisen (pct.)") +
  ggtitle("Efter Finanskrisen") + 
  theme(plot.title = element_text(hjust = 0.5))


x11()
grid.arrange(d, e, f,  ncol= 3)





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








