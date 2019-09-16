## CONTENU ##


# OLD PCA - no ÅRLIGE VÆKSRATER -MAIS PAS TRES UTILE DE LES INCLURES


# Produktivitet variabler (årlige rater)
# Summary statistics par Region (Kommunes en un, Hovedstaden ou le reste)
# reshaping data LONG pour BOXPLOT!
# BOXPLOT årlige vækstrater for produktivitet per REGIONER



library(FactoMineR)
library(ggplot2)
library(reshape2)


# DATA Preparation DONE for Boxplot analysis of variables because missed earlier
# PCA Analysis from FULL DATA but Sjælland/Stor København as supplementary individuals


data_NUM<- read.csv("KL_3_Periods_LAST_NEAT.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM)
head(data_NUM)
# 
 summary(data_NUM[,3])
# 
# 
# # Periods as NUM instead on Post, Pre, Crisis
# 
#  data_NUM<- read.csv("KL_3_Periods_NUM_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_NUM)
# 
# 
# ?reshape
# 
long = reshape(data = data_NUM,
             idvar = "kom",
             varying = colnames(data_NUM)[-c(1,2,3)],
             sep = ".",
             timevar = "period",
             times = c(1,2,3),
             direction = "long")
# 
head(long)
# 
# 
 long$Period <- factor(long$period)
# 
 levels(long$Period)[1] <- "Pre"
 levels(long$Period)[2] <- "Crisis"
 levels(long$Period)[3] <- "Post"
# 
 head(long)
# 
 long = long[order(long$kom,long$period),]
# 
 write.csv(long,"KL_3_Periods_LONG_LAST_NEAT.csv")


################################################# 
# NO NEED TO SEPARATE ANYMORE FOR BOXPLOT! 
################################################# 

# For SEPARATE ANALYSIS! DATA ORIGINALLY WIDE!

data<- read.csv("KL_3_Periods_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data)

summary(data$Region_navn)
summary(data$Area)

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

# Crisis PREPARING for ggplot2 boxplots

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


###############################################
# FOR JOINT ANALYSIS - DATA already LONG!!!
##############################################


# LONG data for BOXPLOT facets/categories (3 periods)

data_lg<- read.csv("KL_3_Periods_LONG_LAST_NEAT.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_lg)


summary(data_lg$Region_navn)
# summary(data$Area)


# Check: INITIALY 31 hovedstadens kommune Vs 29

93/3
# 31

summary(as.factor(data_lg$kom))


# Check with CORRECTION

638/22

374/22# 253 og 259 til Sjælland!

# Region_navn
data_lg$Region_navn <- as.character(data_lg$Region_navn)
data_lg$Region_navn[data_lg$kom==253] <- "Sjælland"
data_lg$Region_navn <- as.factor(data_lg$Region_navn)


data_lg$Region_navn <- as.character(data_lg$Region_navn)
data_lg$Region_navn[data_lg$kom==259] <- "Sjælland"
data_lg$Region_navn <- as.factor(data_lg$Region_navn)


# Område
data_lg$Område <- as.character(data_lg$Område)
data_lg$Område[data_lg$kom==253] <- "Øvrige Kommuner"
data_lg$Område <- as.factor(data_lg$Område)


data_lg$Område <- as.character(data_lg$Område)
data_lg$Område[data_lg$kom==259] <- "Øvrige Kommuner"
data_lg$Område <- as.factor(data_lg$Område)


# Area
data_lg$Area <- as.character(data_lg$Area)
data_lg$Area[data_lg$kom==253] <- "Sjælland"
data_lg$Area <- as.factor(data_lg$Area)


data_lg$Area <- as.character(data_lg$Area)
data_lg$Area[data_lg$kom==259] <- "Sjælland"
data_lg$Area <- as.factor(data_lg$Area)

summary(data_lg)



# Excluding Danmark
data_lg_DK = subset(data_lg, data_lg$kom!=999)
tail(data_lg_DK)


# ATTENTION: Hov = Hovedstaden EXCLUDED
# Excluding Danmark and Hovedstad
data_lg_Hov = subset(data_lg,!(data_lg$Region_navn=="Danmark" | data_lg$Region_navn=="Hovedstaden"))
summary(data_lg_Hov$Region_navn)

data_lg_Hov$Region_navn = factor(data_lg_Hov$Region_navn)
levels(data_lg_Hov$Region_navn)

data_lg_Hov$Landsdel_navn = factor(data_lg_Hov$Landsdel_navn)
levels(data_lg_Hov$Landsdel_navn)

# HOvedstad ALONE
data_lg_Hov_ALONE = subset(data_lg_DK, data_lg_DK$Region_navn=="Hovedstaden")
summary(data_lg_Hov_ALONE$Region_navn)

data_lg_Hov_ALONE$Region_navn = factor(data_lg_Hov_ALONE$Region_navn)
levels(data_lg_Hov_ALONE$Region_navn)

data_lg_Hov_ALONE$Landsdel_navn = factor(data_lg_Hov_ALONE$Landsdel_navn)
levels(data_lg_Hov_ALONE$Landsdel_navn)



# # Excluding Danmark and Sjælland
# data_lg_Rest = subset(data_lg,!(data_lg$Region_navn=="Sjælland" | data_lg$Region_navn=="Danmark" | data_lg$Region_navn=="Hovedstaden"))
# 
# head(data_lg_Rest)



# No Hovedstad!
# data_lg_Hov=data_lg_Hov[,-c(2:4)]
head(data_lg_Hov)
unique(data_lg_Hov$kom)

unique(data_lg_Hov$Area)

# data_lg_Hov$Landsdel_navn = factor(data_lg_Hov$Landsdel_navn)
# levels(data_lg_Hov$Landsdel_navn)


# Summary stat per Landsdel

library(dplyr)

# convert to local data frame

Hov <- tbl_df(data_lg_Hov)

Sum1 = Hov %>%
  group_by(Period, Area) %>%
  summarise(mean = mean(EMP_P_v), sd = sd(EMP_P_v), na.rm=TRUE)

data.frame(Sum1)

Sum = Hov %>%
  group_by(Period) %>%
  summarise(mean = mean(EMP_P_v), sd = sd(EMP_P_v), na.rm=TRUE)

data.frame(Sum)


# Average growth
tapply(data_lg__Hov$YL_P_g, data_lg__Hov$Landsdel_navn, mean, na.rm=TRUE)

mean(data_lg__Hov$YL_P_g)


# Hovedstad ALONE!
data_lg_Hov_ALONE=data_lg_Hov_ALONE[,-c(2:4)]
head(data_lg_Hov_ALONE)

unique(data_lg_Hov_ALONE$kom)

# Summary stat per Landsdel
data_lg_Hov_ALONE$Landsdel_navn = factor(data_lg_Hov_ALONE$Landsdel_navn)
levels(data_lg_Hov_ALONE$Landsdel_navn)


subset(data_lg_Hov_ALONE, data_lg_Hov_ALONE$Landsdel_navn=="Københavns omegn" & 
         data_lg_Hov_ALONE$Period=="Pre")

library(dplyr)

# convert to local data frame

Hov_ALONE <- tbl_df(data_lg_Hov_ALONE)

Sum1 = Hov_ALONE %>%
  group_by(Period, Landsdel_navn) %>%
  summarise(mean = mean(YL_P_g), sd = sd(YL_P_g), na.rm=TRUE)

data.frame(Sum1)

Sum = Hov_ALONE %>%
  group_by(Period) %>%
  summarise(mean = mean(YL_P_g), sd = sd(YL_P_g), na.rm=TRUE)

data.frame(Sum)



# # No Region!
# data_lg_Rest=data_lg_Rest[,-c(2:4)]
# head(data_lg_Rest)


library(reshape2)

data_resh <- melt(data_lg_DK[,-c(2:4)], id.vars=c("kom","Period"))
head(data_resh)
names(data_resh)

# DK = which(data_resh$kom==999)

# max(data_resh$POP_g)

data_resh$tid = factor(data_resh$Period, levels=c("Pre","Crisis","Post"), labels=c("Før","Krisen","Efter")) 

levels(data_resh$Period)

levels(data_resh$variable)



#############################################################
# BOXPLOTS of average Productivity (LEVELS and Rates)



# ALL regions - gennemsnitlige niveauer'


# # FOKUS on Y_T and Y_P
# data_Y_g = subset(data_resh, data_resh$variable=="YL_T_g" | data_resh$variable=="YL_P_g") 
# head(data_Y_g)
# 
# data_Y_g$variable = factor(data_Y_g$variable)
# levels(data_Y_g$variable)


# NYT
# FOKUS on Y_P ALONE (Forget Y_T now!!!)
data_Y_g = subset(data_resh, data_resh$variable=="YL_P_g" ) 
head(data_Y_g)
str(data_Y_g)

# Average amount 
tapply(data_Y_g$value, data_Y_g$tid, median, na.rm=TRUE)

data_Y_g$variable = factor(data_Y_g$variable)
levels(data_Y_g$variable)


data_Y_v = subset(data_resh,  data_resh$variable=="YL_P_v") 
head(data_Y_v)

data_Y_v$variable = factor(data_Y_v$variable)
levels(data_Y_v$variable)

# Average growth
tapply(data_Y_v$value, data_Y_g$tid, mean, na.rm=TRUE)


# x11()
# box_ALL_g <- ggplot(data_Y_g, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~variable, scales = "free") +
#   scale_y_continuous(name = "Produktivitet per Beskæftigede") + 
#   scale_x_discrete(name = "Delperioder") +
#   ggtitle("Produktivitets Gennemsnitlige Niveau (YL_T_g) og Vækstrate (YL_P_v) per Beskæftiget Person") +
#   theme_bw() +
#   theme(legend.position="none")
# box_ALL_g


# dev.off()
# par(mfrow=c(2,1))

library(gridExtra)
library(ggplot2)

x11()
box_ALL_g <- ggplot(data_Y_g, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Produktivitet per Beskæftiget Person i kr.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Produktivitets Gennemsnitlige Niveau per Beskæftiget Person (YL_P_g)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_g


box_ALL_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Produktivitetsvækstrate i pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Produktivitetsvækstrater (YL_P_v)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_v


require(gridExtra)

grid.arrange(box_ALL_g, box_ALL_v, ncol=2)

# print(box_ALL_g, position = c(0, 0, 0.5, 1), more = TRUE)
# print(box_ALL_v, position = c(0.5, 0, 1, 1))


# # ALL regions - vækstrater
# 
# data_Y_v = subset(data_resh, data_resh$variable=="YL_T_v" | data_resh$variable=="YL_P_v") 
# head(data_Y_v)
# 
# data_Y_v$variable = factor(data_Y_v$variable)
# levels(data_Y_v$variable)
# 
# data_Y_v$var = factor(data_Y_v$variable, levels=c("YL_T_v","YL_P_v"), labels=c("YL_T_v","YL_P_v")) 
# 
# 
# x11()
# box_ALL_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~var, scales = "free") +
#   scale_y_continuous(name = "Vækstrater i Produktivitet i Pct.") + 
#   scale_x_discrete(name = "Delperioder") +
#   ggtitle("Produktivitets Gennemsnitlige Vækstrater per Beskæftigede Timer (YL_T_v) eller Personer (YL_P_v)") +
#   theme_bw() +
#   theme(legend.position="none")
# box_ALL_v


# ATTENTION: Hov = Hovedstaden EXCLUDERET
# NO Hovedstad

library(reshape2)

str(data_lg_Hov)
198/3

data_resh_Hov <- melt(data_lg_Hov[,-c(2,3,4,24)], id.vars=c("kom","Period"))
head(data_resh_Hov)

str(data_resh_Hov)

data_resh_Hov$tid = factor(data_resh_Hov$Period, levels=c("Pre","Crisis","Post"), labels=c("Før","Krisen","Efter")) 

levels(data_resh_Hov$Period)



# Productivity
# ALL regions EXCLUDED Hovedstaden - gennemsnitlige niveauer


# OLD - Y_T and Y_P TOGETHER
# data_Y_g = subset(data_resh_Hov, data_resh_Hov$variable=="YL_T_g" | data_resh_Hov$variable=="YL_P_g") 
# head(data_Y_g)
# 
# data_Y_g$variable = factor(data_Y_g$variable)
# levels(data_Y_g$variable)
# 
# 
# x11()
# box_Hov_g <- ggplot(data_Y_g, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~variable, scales = "free") +
#   scale_y_continuous(name = "Produktivitetsniveauer per Beskæftigede") + 
#   scale_x_discrete(name = "Delperioder") +
#   ggtitle("Produktivitets Gennemsnitlige Niveauer per Beskæftigede Timer (YL_T_g) eller Personer (YL_P_g)") +
#   theme_bw() +
#   theme(legend.position="none")
# box_Hov_g
# 
# # Productivity
# # ALL regions EXCLUDED Hovedstaden - vækstrater
# 
# data_Y_v = subset(data_resh_Hov, data_resh_Hov$variable=="YL_T_v" | data_resh_Hov$variable=="YL_P_v") 
# head(data_Y_v)
# 
# data_Y_v$variable = factor(data_Y_v$variable)
# levels(data_Y_v$variable)
# 
# data_Y_v$var = factor(data_Y_v$variable, levels=c("YL_T_v","YL_P_v"), labels=c("YL_T_v","YL_P_v")) 
# 
# 
# x11()
# box_Hov_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~var, scales = "free") +
#   scale_y_continuous(name = "Vækstrater i Produktivitet i Pct.") + 
#   scale_x_discrete(name = "Delperioder") +
#   ggtitle("Produktivitets Gennemsnitlige Vækstrater per Beskæftigede Timer (YL_T_v) eller Personer (YL_P_v)") +
#   theme_bw() +
#   theme(legend.position="none")
# box_Hov_v



# NYT
# Fokus on Y_P, forget Y_T!!!

str(data_resh_Hov)

# data_resh_Hov$value=as.numeric(data_resh_Hov$value)

data_Y_g = subset(data_resh_Hov, data_resh_Hov$variable=="YL_P_g") 
head(data_Y_g)
str(data_Y_g)

data_Y_g$variable = factor(data_Y_g$variable)
levels(data_Y_g$variable)



data_Y_v = subset(data_resh_Hov, data_resh_Hov$variable=="YL_P_v") 
head(data_Y_v)

data_Y_v$variable = factor(data_Y_v$variable)
levels(data_Y_v$variable)

# Average growth
tapply(data_Y_v$value, data_Y_v$tid, mean, na.rm=TRUE)


# dev.off()
# par(mfrow=c(2,1))

library(gridExtra)
library(ggplot2)

x11()
box_ALL_g <- ggplot(data_Y_g, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Produktivitet per Beskæftiget Person i kr.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Produktivitets Gennemsnitlige Niveau per Beskæftiget Person (YL_P_g)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_g


box_ALL_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Produktivitetsvækstrate i pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Produktivitetsvækstrater (YL_P_v)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_v


require(gridExtra)

grid.arrange(box_ALL_g, box_ALL_v, ncol=2)


# # OLD CODE
# 
# x11()
# box_Hov <- ggplot(data_resh_Hov, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~variable, scales = "free") 
# box_Hov



# Hovedstad ALONE

library(reshape2)

str(data_lg_Hov_ALONE)

data_resh_Hov_ALONE <- melt(data_lg_Hov_ALONE[,-c(2,3,4,24)], id.vars=c("kom","Period"))
head(data_resh_Hov_ALONE)

data_resh_Hov_ALONE$tid = factor(data_resh_Hov_ALONE$Period, levels=c("Pre","Crisis","Post"), labels=c("Før","Krisen","Efter")) 

levels(data_resh_Hov_ALONE$Period)
levels(data_resh_Hov_ALONE$variable)


# Hovedstaden - gennemsnitlige niveauer


# Y_T and Y_P TOGETHER!

# data_Y_g = subset(data_resh_Hov_ALONE, data_resh_Hov_ALONE$variable=="YL_T_g" | data_resh_Hov_ALONE$variable=="YL_P_g") 
# head(data_Y_g)
# 
# data_Y_g$variable = factor(data_Y_g$variable)
# levels(data_Y_g$variable)
# 
# 
# x11()
# box_Hov_ALONE_g <- ggplot(data_Y_g, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~variable, scales = "free") +
#   scale_y_continuous(name = "Produktivitetsniveauer per Beskæftigede") + 
#   scale_x_discrete(name = "Delperioder") +
#   ggtitle("Produktivitets Gennemsnitlige Niveauer per Beskæftigede Timer (YL_T_g) eller Personer (YL_P_g)") +
#   theme_bw() +
#   theme(legend.position="none")
# box_Hov_ALONE_g
# 
# 
# #  Hovedstaden - vækstrater
# 
# data_Y_v = subset(data_resh_Hov_ALONE, data_resh_Hov_ALONE$variable=="YL_T_v" | data_resh_Hov_ALONE$variable=="YL_P_v") 
# head(data_Y_v)
# 
# data_Y_v$variable = factor(data_Y_v$variable)
# levels(data_Y_v$variable)
# 
# data_Y_v$var = factor(data_Y_v$variable, levels=c("YL_T_v","YL_P_v"), labels=c("YL_T_v","YL_P_v")) 
# 
# 
# x11()
# box_Hov_ALONE_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~var, scales = "free") +
#   scale_y_continuous(name = "Vækstrater i Produktivitet i Pct.") + 
#   scale_x_discrete(name = "Delperioder") +
#   ggtitle("Produktivitets Gennemsnitlige Vækstrater per Beskæftigede Timer (YL_T_v) eller Personer (YL_P_v)") +
#   theme_bw() +
#   theme(legend.position="none")
# box_Hov_ALONE_v




# NYT
# Fokus on Y_P, forget Y_T!!!


data_Y_g = subset(data_resh_Hov_ALONE, data_resh_Hov_ALONE$variable=="YL_P_g") 
head(data_Y_g)

data_Y_g$variable = factor(data_Y_g$variable)
levels(data_Y_g$variable)



data_Y_v = subset(data_resh_Hov_ALONE, data_resh_Hov_ALONE$variable=="YL_P_v") 
head(data_Y_v)

data_Y_v$variable = factor(data_Y_v$variable)
levels(data_Y_v$variable)

# Average growth
tapply(data_Y_v$value, data_Y_v$tid, median, na.rm=TRUE)


# dev.off()
# par(mfrow=c(2,1))

library(gridExtra)
library(ggplot2)

x11()
box_ALL_g <- ggplot(data_Y_g, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Produktivitet per Beskæftiget Person i kr.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Produktivitets Gennemsnitlige Niveau per Beskæftiget Person (YL_P_g)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_g


box_ALL_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Produktivitetsvækstrate i pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Produktivitetsvækstrater (YL_P_v)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_v


require(gridExtra)

grid.arrange(box_ALL_g, box_ALL_v, ncol=2)









# # OLD CODE
# 
# x11()
# box_Hov_ALONE <- ggplot(data_resh_Hov_ALONE, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~variable, scales = "free") 
# box_Hov_ALONE




#############################################################
# BOXPLOTS of EMPLOYMENT Rates, BVT growth and Employment ratio
# ALL regions - vækstrater

data_Y_v = subset(data_resh, data_resh$variable=="EMP_T_v" | data_resh$variable=="EMP_P_v") 
head(data_Y_v)

data_Y_v$variable = factor(data_Y_v$variable)
levels(data_Y_v$variable)


x11()
box_ALL_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Vækstrater i Beskæftigelse i Pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Beskæftigelses Gennemsnitlige Vækstrater i Timer (EMP_T_v) eller Personer (EMP_P_v)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_v


# BOXPLOTS of BVT and Beskæftigelsesrates
# ALL regions - vækstrater

data_Y_r = subset(data_resh, data_resh$variable=="BVT_K" | data_resh$variable=="Besk_R") 
head(data_Y_r)

data_Y_r$variable = factor(data_Y_r$variable)
levels(data_Y_r$variable)


x11()
box_ALL_r <- ggplot(data_Y_r, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Vækstrater i BVT og Beskæftigelsesratio") +
  theme_bw() +
  theme(legend.position="none")
box_ALL_r


# ATTENTION: Hov = Hovedstaden EXCLUDERET
# NO Hovedstad

data_Y_v = subset(data_resh_Hov, data_resh_Hov$variable=="EMP_T_v" | data_resh_Hov$variable=="EMP_P_v") 
head(data_Y_v)

data_Y_v$variable = factor(data_Y_v$variable)
levels(data_Y_v$variable)


x11()
box_hov_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Vækstrater i Beskæftigelse i Pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Beskæftigelses Gennemsnitlige Vækstrater i Timer (EMP_T_v) eller Personer (EMP_P_v)") +
  theme_bw() +
  theme(legend.position="none")
box_hov_v


# BOXPLOTS of BVT and Beskæftigelsesrates
# ALL regions - vækstrater

data_Y_r = subset(data_resh_Hov, data_resh_Hov$variable=="BVT_K" | data_resh_Hov$variable=="Besk_R") 
head(data_Y_r)

data_Y_r$variable = factor(data_Y_r$variable)
levels(data_Y_r$variable)


x11()
box_hov_r <- ggplot(data_Y_r, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Vækstrater i BVT og Beskæftigelsesratio") +
  theme_bw() +
  theme(legend.position="none")
box_hov_r




# # OLD CODE
# 
# x11()
# box_Hov <- ggplot(data_resh_Hov, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() + facet_wrap(~variable, scales = "free") 
# box_Hov



# Hovedstad ALONE

library(reshape2)

data_resh_Hov_ALONE <- melt(data_lg_Hov_ALONE, id.vars=c("kom","Period"))
head(data_resh_Hov_ALONE)

data_resh_Hov_ALONE$tid = factor(data_resh_Hov_ALONE$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 

levels(data_resh_Hov_ALONE$Period)



#  Hovedstaden - vækstrater

data_Y_v = subset(data_resh_Hov_ALONE, data_resh_Hov_ALONE$variable=="EMP_T_v" | data_resh_Hov_ALONE$variable=="EMP_P_v") 
head(data_Y_v)

data_Y_v$variable = factor(data_Y_v$variable)
levels(data_Y_v$variable)


x11()
box_Hov_ALONE_v <- ggplot(data_Y_v, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Vækstrater i Beskæftigelsen i Pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Beskæftigelses Gennemsnitlige Vækstrater i Timer (EMP_T_v) eller Personer (EMP_P_v)") +
  theme_bw() +
  theme(legend.position="none")
box_Hov_ALONE_v


# BOXPLOTS of BVT and Beskæftigelsesrates
# Hovestaden ALONE - vækstrater

data_Y_r = subset(data_resh_Hov_ALONE, data_resh_Hov_ALONE$variable=="BVT_K" | data_resh_Hov_ALONE$variable=="Besk_R") 
head(data_Y_r)

data_Y_r$variable = factor(data_Y_r$variable)
levels(data_Y_r$variable)


x11()
box_hov_ALONE_r <- ggplot(data_Y_r, aes(x=factor(tid), y =value, fill=factor(tid))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_y_continuous(name = "Pct.") + 
  scale_x_discrete(name = "Delperioder") +
  ggtitle("Vækstrater i BVT og Beskæftigelsesratio") +
  theme_bw() +
  theme(legend.position="none")
box_hov_ALONE_r





##########################################################################
# FACETTING by Period AND Main Regions (Hovedstaden, Sjælland, Rest)

head(data_lg_DK)
summary(data_lg_DK)

data_lg_DK_Facet=data_lg_DK[,-c(2,3)]
head(data_lg_DK_Facet)
summary(data_lg_DK_Facet$Area)

# max(data_lg_DK_Facet$EMP_P_g)
# max = subset(data_lg_DK_Facet,data_lg_DK_Facet$EMP_P_g > 400000)


# For simpler BOXPLOT

# RECODING Hovedstad and Ikke Hovedstad!!!
# levels: 0, 1, 2, 3 to be recoded into 0 and 1+

data_lg_DK_Facet$Områder <- factor(data_lg_DK_Facet$Area)
levels(data_lg_DK_Facet$Områder)[2:3] <- "Øvrige Kommuner"

tail(data_lg_DK_Facet)
summary(data_lg_DK_Facet$Områder)

# Old - NOW 87 since 3x29
93/3
31

192/3
64

str(data_lg_DK_Facet)

data_lg_DK_Facetx = data_lg_DK_Facet[,c(1,23,3:21)]
head(data_lg_DK_Facetx)

str(data_lg_DK_Facetx)
summary(data_lg_DK_Facetx$Områder)

colnames(data_lg_DK_Facetx)




library(reshape2)

# data_resh_Facet <- melt(data_lg_DK_Facet, id.vars=c("kom","Area", "Period"))
# head(data_resh_Facet)
# 
# data_lg_DK_Facet$Delperiode = factor(data_lg_DK_Facet$Period, levels=c("Pre","Crisis","Post"), labels=c("Før","Krisen","Efter")) 
# 
# levels(data_lg_DK_Facet$Period)
# levels(data_lg_DK_Facet$Area)
# 
#  
# data_lg_DK_Facet$region = factor(data_lg_DK_Facet$Area, levels=c("Hovedstaden","Sjælland","Rest"), labels=c("Hovedstaden","Sjælland","Rest")) 
# 
# levels(data_lg_DK_Facet$region)




data_resh_Facetx <- melt(data_lg_DK_Facetx, id.vars=c("kom","Områder", "Period"))
tail(data_resh_Facetx)
summary(data_resh_Facetx)

data_lg_DK_Facetx$Delperiode = factor(data_lg_DK_Facetx$Period, levels=c("Pre","Crisis","Post"), labels=c("Før","Krisen","Efter")) 

levels(data_lg_DK_Facetx$Period)
levels(data_lg_DK_Facetx$Områder)


# data_lg_DK_Facetx$region = factor(data_lg_DK_Facetx$Area, levels=c("Hovedstaden","Sjælland","Rest"), labels=c("Hovedstaden","Sjælland","Rest")) 
# 
# levels(data_lg_DK_Facetx$region)



# levels(data_resh_Facet$variable)

plotCols <- c("YL_T_v",
              "YL_P_v")

plotCols1 <- c("YL_T_g",
              "YL_P_g",
              "YL_P_v",
              "YL_T_v",
              "Ypc",
              "Ydpc",
              "INVpc",
              "INV_K")

plotCols0 <- c("Besk_R")
, 
               "EMP_T_g",
               "EMP_P_g",
               "EMP_T_v",
               "EMP_P_v")


# box fill: aes(fill = factor(TYP))

"INV_K",
"INVpc"



ggbox <- function(x) {
  title <- paste("Boxplot of", x, "by Delperiode and Område")
  ggplot(data_lg_DK_Facetx, aes_string('factor(tid)', x)) +
    facet_grid(Områder ~ ., labeller = label_both, margins = T) + 
    geom_boxplot(aes(fill = factor(tid))) +
    scale_y_continuous(name = "Pct.", limits=c(-12, 12)) + 
    scale_x_discrete(name = "Delperioder") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position="none")
}


lapply(plotCols, ggbox)




# Even better boxplot

ggbox_x <- function(x) {
  title <- paste("Boxplot af", x, "fordelt ud fra Området og Delperioden")
  ggplot(data_lg_DK_Facetx, aes_string('factor(Områder)', x)) +
    facet_grid(. ~ Delperiode, labeller = label_both, margins = F) + 
    geom_boxplot(aes(fill = factor(Delperiode))) +
    scale_y_continuous(name = "Pct.") + 
    scale_x_discrete(name = "Området") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position="none")
}


lapply(plotCols, ggbox_x)




# Split au lieu de FACET
data_lg_DK_Split=data_lg_DK
head(data_lg_DK_Split)

data_lg_DK_Split$Delperiode = factor(data_lg_DK_Facet$Period, levels=c("Pre","Crisis","Post"), labels=c("Før","Krisen","Efter")) 

summary(data_lg_DK_Split$Area)

levels(data_lg_DK_Split$Area)


# For more detailed BOXPLOTs for Hovedstaden and ØVRIG (NOT Hovedstaden) 

# RECODING Hovedstad and Ikke Hovedstad!!!
# levels: 0, 1, 2, 3 to be recoded into 0 and 1+

data_lg_DK_Split$Områder <- factor(data_lg_DK_Split$Area)
levels(data_lg_DK_Split$Områder)[2:3] <- "Øvrige"


head(data_lg_DK_Split)
str(data_lg_DK_Split)
colnames(data_lg_DK_Split)


data_lg_DK_Splitx = data_lg_DK_Split[,c(1:3,25:26,6:23)]
head(data_lg_DK_Splitx)

str(data_lg_DK_Splitx)
summary(data_lg_DK_Splitx$Områder)

colnames(data_lg_DK_Splitx)



# SPlit for Hovedstaden

Split_Hoved=subset(data_lg_DK_Splitx,data_lg_DK_Splitx$Områder=="Hovedstaden")
str(Split_Hoved)

summary(Split_Hoved$Landsdel_navn)
summary(Split_Hoved$Områder)


Split_Hoved$Landsdel_navn = factor(Split_Hoved$Landsdel_navn)
levels(Split_Hoved$Landsdel_navn)

head(Split_Hoved)


# dplyr for summary statistics (gennemsnit)

Spl1 <- tbl_df(Split_Hoved)

Spl1 %>%
  group_by(Landsdel_navn, Delperiode) %>%
  summarise(mean = mean(YL_P_v), na.rm=TRUE)



# Split_Hoved$Landsdel_navn

levels(Split_Hoved$Landsdel_navn)[levels(Split_Hoved$Landsdel_navn)=="Byen København"] <- "Byen Kbh"

levels(Split_Hoved$Landsdel_navn)[levels(Split_Hoved$Landsdel_navn)=="Københavns omegn"] <- "Kbhs omegn"

Split_Hoved$Landsdel_navn = factor(Split_Hoved$Landsdel_navn)
levels(Split_Hoved$Landsdel_navn)


# Split_Hoved$Perioden = factor(Split_Hoved$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Krisen","Post")) 

levels(Split_Hoved$Delperiode)


# Nyt

ggbox_x <- function(x) {
  title <- paste("Boxplot af", x, "fordelt ud fra Landsdelens Navn og Perioden")
  ggplot(Split_Hoved, aes_string('factor(Landsdel_navn)', x, fill = 'Landsdel_navn')) +
    facet_grid(. ~ Delperiode, labeller = label_both, margins = F) + 
    geom_boxplot() +
    scale_y_continuous(name = "Pct.") + 
    scale_x_discrete(name = "Landsdelsnavn") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position="none")
}


lapply(plotCols, ggbox_x)


levels(data_lg_DK_Splitx$Områder)


# SPlit for ØVRIGE

Split_Øvrig=subset(data_lg_DK_Splitx,data_lg_DK_Splitx$Områder=="Øvrige")
str(Split_Øvrig)

summary(Split_Øvrig$Landsdel_navn)

Split_Øvrig$Landsdel_navn = factor(Split_Øvrig$Landsdel_navn)
levels(Split_Øvrig$Landsdel_navn)


levels(Split_Øvrig$Landsdel_navn)[levels(Split_Øvrig$Landsdel_navn)=="Nordjylland"] <- "Nordjyl."

levels(Split_Øvrig$Landsdel_navn)[levels(Split_Øvrig$Landsdel_navn)=="Østjylland"] <- "Østjyl."

levels(Split_Øvrig$Landsdel_navn)[levels(Split_Øvrig$Landsdel_navn)=="Østsjælland"] <- "Østsjæl."


levels(Split_Øvrig$Landsdel_navn)[levels(Split_Øvrig$Landsdel_navn)=="Sydjylland"] <- "Sydjyl."


levels(Split_Øvrig$Landsdel_navn)[levels(Split_Øvrig$Landsdel_navn)=="Vest- og Sydsjælland"] <- "V/Sydsjæl."


levels(Split_Øvrig$Landsdel_navn)[levels(Split_Øvrig$Landsdel_navn)=="Vestjylland"] <- "Vestjyl."



Split_Øvrig$Landsdel_navn = factor(Split_Øvrig$Landsdel_navn)
levels(Split_Øvrig$Landsdel_navn)


# Split_Øvrig$Perioden = factor(Split_Øvrig$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 

levels(Split_Øvrig$Delperiode)


head(Split_Øvrig)

# MARCHE pAS!!!

# dplyr for summary statistics (gennemsnit)
#
# Spl2 <- tbl_df(Split_Øvrig)
# 
# Spl2 %>%
#   group_by(Landsdel_navn, Delperiode) %>%
#   summarise_each(funs(mean(., na.rm=TRUE), contains("YL_P_v")))
# 
# data.frame(Spl2)


# Substitute
with(Split_Øvrig, tapply(YL_P_v, list(Landsdel_navn, Delperiode), mean))


# library(reshape2)
# 
# #use subset to only grab the variables of interest...
# mtcars.m <- melt(subset(mtcars, select = c("mpg", "gear", "cyl")), measure.vars="mpg")
# #cast into appropriate format
# dcast(mtcars.m, cyl ~ gear, fun.aggregate=sum, value.var="value")


# Nyt

ggbox_x <- function(x) {
  title <- paste("Boxplot af", x, "fordelt ud fra Landsdelens Navn og Perioden")
  ggplot(Split_Øvrig, aes_string('factor(Landsdel_navn)', x, fill = 'Landsdel_navn')) +
    facet_grid(. ~ Delperiode, labeller = label_both, margins = F) + 
    geom_boxplot() +
    scale_y_continuous(name = "Pct.") + 
    scale_x_discrete(name = "Landsdelsnavn") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position="none")
}


lapply(plotCols, ggbox_x)


# x11()
# box_Facet <- ggplot(data_resh_Facet, aes(x=factor(tid), y =value, fill=factor(tid))) +
#   geom_boxplot() +
#   facet_grid(Area ~ ., labeller = label_both, margins = TRUE) +
#   facet_wrap(~variable, scales = "free")
# box_Facet



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



##################################################

# OLD PCA for 1st report
# for averages - NO YEARLY growth rates ...

#################################################



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


