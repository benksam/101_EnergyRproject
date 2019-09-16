## CONTENU ##

# Produktivitet variabler (årlige rater)
# Summary statistics par Region (Kommunes en un, Hovedstaden ou le reste)
# reshaping data LONG pour BOXPLOT!
# BOXPLOT årlige vækstrater for produktivitet per REGIONER



# Correlations des variables et histogrammes



library(FactoMineR)
library(ggplot2)
library(reshape2)
library(psych)


# DATA Preparation DONE for Boxplot analysis of variables because missed earlier
# PCA Analysis from FULL DATA but Sjælland/Stor København as supplementary individuals


data_NUM<- read.csv("KL_Chained_Yearly_Productivity.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM)
head(data_NUM)

summary(data_NUM[,3])


# # More COMPLETE DATASÆT for MERGE and USE FIRST COLUMNS
# 
data_ref<- read.csv("KL_3_Periods_NUM_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_ref)
head(data_ref)

data_ref=data_ref[,c(1,2,3)]


data_YL = merge(data_ref, data_NUM, by = c("kom"), all = T)
tail(data_YL)

summary(data_YL$Region_navn)


# Pour PCA de Annual Productivity Growth Rates


# write.csv(data_YL,"PCA_Yearly_Productivity.csv")


data_YL=data_YL[-96,]
tail(data_YL)


# PBM avec DESCRIPTION : non WEIGHTED AVERAGES with GROWTH RATES!

# Summary all regions
description = describe(data_YL)

write.csv(description,"Summary_Stat_Productivity.csv")


# Summary Hovedstaden
description_Hov = describe(data_YL[data_YL$Region_navn=="Hovedstaden",])
head(description_Hov)

write.csv(description_Hov,"Summary_Stat_Productivity_Hovedstaden.csv")


# Summary IKKE Hovedstaden
description_NO_Hov = describe(data_YL[data_YL$Region_navn!="Hovedstaden",])
head(description_NO_Hov)

write.csv(description_NO_Hov,"Summary_Stat_Productivity_NO_Hovedstaden.csv")


# Summary IKKE Hovedstaden og Sjælland
description_NO_Sjael = describe(data_YL[data_YL$Region_navn!=c("Hovedstaden","Sjælland"),])
head(description_NO_Sjael)

write.csv(description_NO_Sjael,"Summary_Stat_Productivity_NO_Sjaelland.csv")



# ?reshape
# 
long = reshape(data = data_YL,
             idvar = "kom",
             varying = colnames(data_YL)[-c(1,2,3)],
             sep = ".",
             timevar = "Year",
             times = c(2004,2005,2006,2007,2008,2009,2010,2011,2012),  # MISTAKE - USE FULL DATA!!!
             direction = "long")
 
head(long)

#  long$Period <- factor(long$period)
# # 
#  levels(long$Period)[1] <- "Pre"
#  levels(long$Period)[2] <- "Crisis"
#  levels(long$Period)[3] <- "Post"
# # 
#  head(long)
# 

long = long[order(long$kom,long$Year),]
 
# write.csv(long,"KL_Productivity_LONG.csv")

 
 ###########################################
 # FOR SEPARATE ANALYSIS - DATA already LONG!!!
 ###########################################
 
 

################################################# 
# NO NEED TO SEPARATE ANYMORE FOR BOXPLOT! 
################################################# 

# For SEPARATE ANALYSIS! DATA ORIGINALLY WIDE!

data<- read.csv("KL_3_Periods_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data)


# Subsetting WITHOUT SJÆLLAND!


levels(data$Region_navn)

# Sjælland = c("Hovedstaden", "Sjælland","Danmark")
# Danmark = 96

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



###########################################
# FOR JOINT ANALYSIS - DATA already LONG!!! 2004-2012 ONLY!!! FULL SAMPLE YEARS !!!
###########################################


######################################################
# LONG data for BOXPLOT facets/categories (3 periods) 2004-2012 ONLY
######################################################

data_lg<- read.csv("KL_Productivity_LONG.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_lg)
summary(data_lg)

864/9
# 96

# Excluding Danmark
data_lg_DK = subset(data_lg, data_lg$kom!=999)
head(data_lg_DK)


# Excluding Danmark BUT HOVEDSTAD ALONE!
data_lg_Hov_ALONE = subset(data_lg_DK, data_lg_DK$Region_navn=="Hovedstaden")
head(data_lg_Hov_ALONE)

# Hovedstad ALONE!
data_lg_Hov_ALONE=data_lg_Hov_ALONE[,-c(2,3)]
head(data_lg_Hov_ALONE)

# ATTENTION: HOV = Hovestaden EXCULDED
# Excluding Danmark and Hovedstad - 
data_lg_Hov = subset(data_lg,!(data_lg$Region_navn=="Danmark" | data_lg$Region_navn=="Hovedstaden"))


# # Excluding Danmark and Sjælland
data_lg_Rest = subset(data_lg,!(data_lg$Region_navn=="Sjælland" | data_lg$Region_navn=="Danmark" | data_lg$Region_navn=="Hovedstaden"))

head(data_lg_Rest)

 # No Hovedstad!
data_lg_Hov=data_lg_Hov[,-c(2,3)]
head(data_lg_Hov)


# # No Region!
# data_lg_Rest=data_lg_Rest[,-c(2,3)]
# head(data_lg_Rest)


library(reshape2)

data_resh <- melt(data_lg_DK[,-c(2,3)], id.vars=c("kom","Year"))
head(data_resh)
names(data_resh)

# DK = which(data_resh$kom==999)

# max(data_resh$POP_g)

# data_resh$tid = factor(data_resh$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 

# levels(data_resh$Period)


x11()
box_ALL <- ggplot(data_resh, aes(x=factor(Year), y =value, fill=factor(Year))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_x_discrete(name = "År") +
  scale_y_continuous(name = "Produktivitets Vækstrater i Pct.") + 
  ggtitle("Årlige Produktivitets Vækstrater per Beskæftigede Timer (Y_T_år) eller Personer (Y_P_år)") +
  theme_bw() +
  theme(legend.position="none")
box_ALL



# Hovedstad ALONE

library(reshape2)

data_resh_Hov_ALONE <- melt(data_lg_Hov_ALONE, id.vars=c("kom","Year"))
head(data_resh_Hov_ALONE)

# data_resh_Hov$tid = factor(data_resh_Hov$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 
# 
# levels(data_resh_Hov$Period)

x11()
box_Hov_ALONE <- ggplot(data_resh_Hov_ALONE, aes(x=factor(Year), y =value, fill=factor(Year))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free")  +
  scale_x_discrete(name = "År") +
  scale_y_continuous(name = "Produktivitets Vækstrater i Pct.") + 
  ggtitle("Årlige Produktivitets Vækstrater per Beskæftigede Timer (Y_T_år) eller Personer (Y_P_år)") +
  theme_bw() +
  theme(legend.position="none")
box_Hov_ALONE



# NO Hovedstad

library(reshape2)

data_resh_Hov <- melt(data_lg_Hov, id.vars=c("kom","Year"))
head(data_resh_Hov)

# data_resh_Hov$tid = factor(data_resh_Hov$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 
# 
# levels(data_resh_Hov$Period)

x11()
box_Hov <- ggplot(data_resh_Hov, aes(x=factor(Year), y =value, fill=factor(Year))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") +
  scale_x_discrete(name = "År") +
  scale_y_continuous(name = "Produktivitets Vækstrater i Pct.") + 
  ggtitle("Årlige Produktivitets Vækstrater per Beskæftigede Timer (Y_T_år) eller Personer (Y_P_år)") +
  theme_bw() +
  theme(legend.position="none")
box_Hov



######################################################
# LONG data for BOXPLOT facets/categories (3 periods) AND 2 Områder - 2004-2012 ONLY!!!
######################################################

data_lg_x<- read.csv("KL_Productivity_LONG_LAST.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_lg_x)
summary(data_lg_x)

864/9
# 96

# Excluding Danmark
data_lg_x_DK = subset(data_lg_x, data_lg_x$kom!=999)
head(data_lg_x_DK)
str(data_lg_x_DK)


data_lg_x_DK$Region_navn = factor(data_lg_x_DK$Region_navn)
levels(data_lg_x_DK$Region_navn)


plotCols0 <- c("Y_T",
               "Y_P")


ggbox <- function(x) {
  title <- paste("Boxplot af", x, "fordelt ud fra Området")
  ggplot(data_lg_x_DK, aes_string('factor(Område)', x)) +
    facet_grid(. ~ År, labeller = label_both, margins = T) + 
    geom_boxplot(aes(fill = factor(Område))) +
    scale_y_continuous(name = "Pct.") + 
    ggtitle(title) +
    theme_bw() 
}


lapply(plotCols0, ggbox)





##################################################
# Plotmeans - FOR FULL DATA SET !!!
#################################################


##################################################
# DATA PREPARATION - Growth RATES and LEVELS
#################################################

#############
# Growth RATES
#############

# FULL Yearly DATA for Y_L and EMP for SPAGHETTI PLOTS

# data_bvt<- read.csv("B1G_Year_FULL.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_bvt)
# head(data_bvt)

# write.csv(data_bvt,"bvt.csv")

data_bvt<- read.csv("bvt.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_bvt)
head(data_bvt)


# data_emp_t<- read.csv("EMP_T_Year_FULL.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_emp_t)
# head(data_emp_t)
# 
# write.csv(data_emp_t,"emp_t.csv")

data_emp_t<- read.csv("emp_t.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_emp_t)
head(data_emp_t)



# data_emp_p<- read.csv("EMP_P_Year_FULL.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_emp_p)
# head(data_emp_p)
# 
# write.csv(data_emp_p,"emp_p.csv")

data_emp_p<- read.csv("emp_p.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_emp_p)
head(data_emp_p)


# LONGER series for PRODUKTIVITY!!!

# data_y_t<- read.csv("Y_T_Year_FULL.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_y_t)
# head(data_y_t)
# # 
# write.csv(data_y_t,"y_t.csv")

data_y_t<- read.csv("y_t.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y_t)
head(data_y_t)


# LONGER series for PRODUKTIVITY!!!


data_y_p<- read.csv("Y_P_Year_FULL.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y_p)
head(data_y_p)
# 
write.csv(data_y_p,"y_p.csv")

data_y_p<- read.csv("y_p.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y_p)
head(data_y_p)


data_ref=data_ref[,c(1,2,3)]
head(data_ref)

merge1 = merge(data_ref, data_bvt, by = c("kom"), all = T)
tail(merge1)

merge2 = merge(merge1, data_emp_t, by = c("kom"), all = T)
tail(merge2)

merge3 = merge(merge2, data_emp_p, by = c("kom"), all = T)
tail(merge3)


mergeA = merge(data_ref, data_y_t, by = c("kom"), all = T)
tail(mergeA)


mergeB = merge(mergeA, data_y_p, by = c("kom"), all = T)
tail(mergeB)



# write.csv(merge3,"YL_Components_År_WIDE.csv")
# write.csv(mergeB,"Y_T_P_År_WIDE.csv")



# summary(data_YL$Region_navn)

data_YL_wide<- read.csv("Produktivitet_År_WIDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_wide)
head(data_YL_wide)



# AVOID THAT PROBLEM - FULL YEAR SERIES AND THEN SUBSET!!!

# ADDING Y_L_T LONGER Time Series
data_YL_wideX<- read.csv("Y_T_P_År_WIDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_wideX)
head(data_YL_wideX)
tail(data_YL_wideX)

# ADDING Y_L_T Components (Originally FULL Time Series)
data_Comp_wide<- read.csv("YL_Components_År_WIDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_Comp_wide)
head(data_Comp_wide)
tail(data_Comp_wide)


# write.csv(data_YL,"PCA_Yearly_Productivity.csv")


data_YL_wide=data_YL_wide[-97,]
tail(data_YL_wide)

data_YL_wideX=data_YL_wideX[-97,]
tail(data_YL_wideX)

data_Comp_wide=data_Comp_wide[-97,]
tail(data_Comp_wide)


# SUMMARY DESCRIPTION (PSY LIBRARY)

# # Summary all regions
# description = describe(data_YL)
# 
# write.csv(description,"Summary_Stat_Productivity.csv")
# 
# 
# 
# # Summary Hovedstaden
# description_Hov = describe(data_YL[data_YL$Region_navn=="Hovedstaden",])
# head(description_Hov)
# 
# write.csv(description_Hov,"Summary_Stat_Productivity_Hovedstaden.csv")
# 
# 
# # Summary IKKE Hovedstaden
# description_NO_Hov = describe(data_YL[data_YL$Region_navn!="Hovedstaden",])
# head(description_NO_Hov)
# 
# write.csv(description_NO_Hov,"Summary_Stat_Productivity_NO_Hovedstaden.csv")
# 
# 
# # Summary IKKE Hovedstaden og Sjælland
# description_NO_Sjael = describe(data_YL[data_YL$Region_navn!=c("Hovedstaden","Sjælland"),])
# head(description_NO_Sjael)
# 
# write.csv(description_NO_Sjael,"Summary_Stat_Productivity_NO_Sjaelland.csv")



# ?reshape
# 
data_Comp_long = reshape(data = data_Comp_wide,
               idvar = "kom",
               varying = colnames(data_Comp_wide)[-c(1,2,3)],
               sep = ".",
               timevar = "År",
               times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
               direction = "long")
#
head(data_Comp_long)
tail(data_Comp_long)
str(data_Comp_long)


# write.csv(data_Comp_long,"KL_YL_Comp_År_LONG.csv")


data_YL_longX = reshape(data = data_YL_wideX,
                        idvar = "kom",
                        varying = colnames(data_YL_wideX)[-c(1,2,3,4)],
                        sep = ".",
                        timevar = "År",
                        times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015),
                        direction = "long")
#
head(data_YL_longX)
tail(data_YL_longX)
str(data_YL_longX)





data_YL_long<- read.csv("KL_Productivity_År_LONG.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_long)
head(data_YL_long)


data_YL_longX<- read.csv("KL_Productivity_År_LONGX.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_longX)
head(data_YL_longX)



data_Comp_long<- read.csv("KL_YL_Comp_År_LONG.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_Comp_long)
head(data_Comp_long)




# tail(data_lg_x_DK)


# Subsetting for (2004-2012)!!! THE CORRECT WAY!
data_YL_long_sh = subset(data_YL_long,!(data_YL_long$År==1994 | data_YL_long$År==1995 | data_YL_long$År==1996 
                                        | data_YL_long$År==1997 | data_YL_long$År==1998 | data_YL_long$År==1999 
                                        | data_YL_long$År==2000 | data_YL_long$År==2001 | data_YL_long$År==2002 
                                        | data_YL_long$År==2003 | data_YL_long$År==2013 | data_YL_long$År==2014
                                        | data_YL_long$År==2015))



# Merging PRODUKTIVITET and COMPONENTS (BVT and EMP)

str(data_lg_x_DK)
str(data_YL_long_sh)

# OLD SHORTER SAMPLE!
merge4 = merge(data_lg_x_DK, data_YL_long_sh, by = c("kom",("År")), all = T)
tail(merge4)

write.csv(merge4,"LONG_ÅR_FULL.csv")


# NEW FULLER SAMPLE!!!

mergeX = merge(data_YL_longX, data_Comp_long, by = c("kom",("År")), all = T)
tail(mergeX)

# write.csv(mergeX,"LONG_ÅR_FULLX.csv")



#############
# LEVELS
#############


# FULL Yearly DATA for Y_L and EMP for SPAGHETTI PLOTS



data_bvt_L<- read.csv("BVT_Year_FULL_LEVELS.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_bvt_L)
head(data_bvt_L)

write.csv(data_bvt_L,"bvt.csv")

data_bvt_L<- read.csv("bvt.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_bvt_L)
head(data_bvt_L)




data_emp_t_L<- read.csv("EMP_T_LEVELS.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_emp_t_L)
head(data_emp_t_L)

write.csv(data_emp_t_L,"emp_t.csv")

data_emp_t_L<- read.csv("emp_t.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_emp_t_L)
head(data_emp_t_L)



data_emp_p_L<- read.csv("EMP_P_LEVELS.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_emp_p_L)
head(data_emp_p_L)

write.csv(data_emp_p_L,"emp_p.csv")

data_emp_p_L<- read.csv("emp_p.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_emp_p_L)
head(data_emp_p_L)



# LONGER series for PRODUKTIVITY!!!


data_y_t_L<- read.csv("Y_T_Year_FULL_LEVELS.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y_t_L)
head(data_y_t_L)
#
write.csv(data_y_t_L,"y_t.csv")

data_y_t_L<- read.csv("y_t.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y_t_L)
head(data_y_t_L)


# LONGER series for PRODUKTIVITY!!!


data_y_p_L<- read.csv("Y_P_Year_FULL_LEVELS.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y_p_L)
head(data_y_p_L)
# 
write.csv(data_y_p_L,"y_p.csv")

data_y_p_L<- read.csv("y_p.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_y_p_L)
head(data_y_p_L)


data_ref=data_ref[,c(1,2,3)]
head(data_ref)

mergeA = merge(data_ref, data_y_t_L, by = c("kom"), all = T)
tail(mergeA)


mergeB = merge(mergeA, data_y_p_L, by = c("kom"), all = T)
tail(mergeB)


mergeC = merge(mergeB, data_bvt_L, by = c("kom"), all = T)
tail(mergeC)

mergeD = merge(mergeC, data_emp_t_L, by = c("kom"), all = T)
tail(mergeD)

mergeE = merge(mergeD, data_emp_p_L, by = c("kom"), all = T)
tail(mergeE)

# write.csv(mergeE,"Produktivitet_LEVELS_WIDE.csv")

# write.csv(merge3,"YL_Components_År_WIDE.csv")
# write.csv(mergeB,"Y_T_P_År_WIDE.csv")



# summary(data_YL$Region_navn)

data_YL_NIVEAU_wide<- read.csv("Produktivitet_LEVELS_WIDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_NIVEAU_wide)
tail(data_YL_NIVEAU_wide)


# write.csv(data_YL,"PCA_Yearly_Productivity.csv")


# data_YL_wide=data_YL_wide[-97,]
# tail(data_YL_wide)
# 
# data_YL_wideX=data_YL_wideX[-97,]
# tail(data_YL_wideX)
# 
# data_Comp_wide=data_Comp_wide[-97,]
# tail(data_Comp_wide)

# 
# # Summary all regions
# description = describe(data_YL)
# 
# write.csv(description,"Summary_Stat_Productivity.csv")
# 
# 
# 
# # Summary Hovedstaden
# description_Hov = describe(data_YL[data_YL$Region_navn=="Hovedstaden",])
# head(description_Hov)
# 
# write.csv(description_Hov,"Summary_Stat_Productivity_Hovedstaden.csv")
# 
# 
# # Summary IKKE Hovedstaden
# description_NO_Hov = describe(data_YL[data_YL$Region_navn!="Hovedstaden",])
# head(description_NO_Hov)
# 
# write.csv(description_NO_Hov,"Summary_Stat_Productivity_NO_Hovedstaden.csv")
# 
# 
# # Summary IKKE Hovedstaden og Sjælland
# description_NO_Sjael = describe(data_YL[data_YL$Region_navn!=c("Hovedstaden","Sjælland"),])
# head(description_NO_Sjael)
# 
# write.csv(description_NO_Sjael,"Summary_Stat_Productivity_NO_Sjaelland.csv")



# ?reshape
# 
data_NIVEAU_long = reshape(data = data_YL_NIVEAU_wide,
                         idvar = "kom",
                         varying = colnames(data_YL_NIVEAU_wide)[-c(1,2,3,4)],
                         sep = ".",
                         timevar = "År",
                         times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                   2008,2009,2010,2011,2012, 2013, 2014, 2015),
                         direction = "long")
#
head(data_NIVEAU_long)
tail(data_NIVEAU_long)
str(data_NIVEAU_long)


write.csv(data_NIVEAU_long,"KL_YL_NIVEAU_LONG.csv")



# tail(data_lg_x_DK)

# Subsetting for (2004-2012)

data_YL_long_sh = subset(data_YL_long,!(data_YL_long$År==1994 | data_YL_long$År==1995 | data_YL_long$År==1996 
                                        | data_YL_long$År==1997 | data_YL_long$År==1998 | data_YL_long$År==1999 
                                        | data_YL_long$År==2000 | data_YL_long$År==2001 | data_YL_long$År==2002 
                                        | data_YL_long$År==2003 | data_YL_long$År==2013 | data_YL_long$År==2014
                                        | data_YL_long$År==2015))



#############################
#### Growth RATE ANALYSE ####
#############################



######################
# Summary statistics with
# dplyr functions
######################



data_YL_longX<- read.csv("LONG_ÅR_FULLX.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_longX)
head(data_YL_longX)

summary(data_YL_longX)

# Check: INITIALY 31 hovedstadens kommune Vs 29!!! AVOID INITIALLY AND CHECK OUTLIERS - INCONSISTENCIES IN DATA!!!
682/22

330/22

# Check with CORRECTION

638/22

374/22# 253 og 259 til Sjælland!


# RENAME A FACTOR LEVEL FOR A FACTOR
# Region_navn
data_YL_longX$Region_navn <- as.character(data_YL_longX$Region_navn)
data_YL_longX$Region_navn[data_YL_longX$kom==253] <- "Sjælland"
data_YL_longX$Region_navn <- as.factor(data_YL_longX$Region_navn)


data_YL_longX$Region_navn <- as.character(data_YL_longX$Region_navn)
data_YL_longX$Region_navn[data_YL_longX$kom==259] <- "Sjælland"
data_YL_longX$Region_navn <- as.factor(data_YL_longX$Region_navn)


# Område
data_YL_longX$Område <- as.character(data_YL_longX$Område)
data_YL_longX$Område[data_YL_longX$kom==253] <- "Øvrige Kommuner"
data_YL_longX$Område <- as.factor(data_YL_longX$Område)


data_YL_longX$Område <- as.character(data_YL_longX$Område)
data_YL_longX$Område[data_YL_longX$kom==259] <- "Øvrige Kommuner"
data_YL_longX$Område <- as.factor(data_YL_longX$Område)

summary(data_YL_longX)



unique(data_YL_longX$kom)

# NO DK already, BUT NO "Uden REgion" ALSO!!!
data_YL_longXX = subset(data_YL_longX, data_YL_longX$kom!=961)
head(data_YL_longXX)

unique(data_YL_longXX$kom)


# LGX <- tbl_df(data_YL_longX)
LGXX <- tbl_df(data_YL_longXX) # To compare effects on SUMMARY STATISTICS


# With UDEN FOR REGION

SumX = LGX %>%
  group_by(Område) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(SumX)

# write.csv(SumX,"Summary_Stat_YEARLY_OMRÅDE_Unden_Region_INCLU.csv")


# DISTURBANCE "UDEN FOR REGION" KOMMUNE - SKIPPED!

# "Uden Region" a EFFET sur SD, MAX et MIN => EXCLU!!!

# Uden for Region EXCLUDED!!!


SumXX = LGXX %>%
  group_by(Område) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(SumXX)


# write.csv(SumXX,"Summary_Stat_YEARLY_OMRÅDE_Unden_Region_EXCLU_29.csv")


SumXX_ÅR = LGXX %>%
  group_by(Område, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(SumXX_ÅR)


# write.csv(SumXX_ÅR,"Summary_GROWTH_Per_Year_OMRÅDE_Unden_Region_EXCLU_29.csv")


# CLUMSY with EXCEL for data management ... Suffice to have the numbers without REORGANIZING!!! 
# USE DPLYR, TAPPLY, AGGREGATE ...


# PLOTS with ggplot2 and FACETS

STAT_GROWTH_Område_Year<- read.csv("Summary_GROWTH_Per_Year_OMRÅDE_Unden_Region_EXCLU_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_GROWTH_Område_Year)
head(STAT_GROWTH_Område_Year)

STAT_GROWTH_Område_Year$År = as.factor(STAT_GROWTH_Område_Year$År)

long_gr_omr = reshape(data = STAT_GROWTH_Område_Year,
               idvar = "Obs",
               varying = colnames(STAT_GROWTH_Område_Year)[4:28],
               sep = ".",
               timevar = "Stat",
               times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
               direction = "long")
# 
head(long_gr_omr)

# write.csv(long_gr_omr,"Summary_lg_GROWTH_Per_Year_OMRÅDE_29.csv")


STAT_lg_GROWTH_Område_Year<- read.csv("Summary_lg_GROWTH_Per_Year_OMRÅDE_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_GROWTH_Område_Year)
head(STAT_lg_GROWTH_Område_Year)

STAT_lg_GROWTH_Område_Year$År <- as.numeric(as.character
                                            (STAT_lg_GROWTH_Område_Year$År))

STAT_lg_GROWTH_Område_Year$Stats = factor(STAT_lg_GROWTH_Område_Year$Stat, levels=c("median","mean","sd","min", "max"), 
                                          labels=c("Median","Gennemsnit","SD","Min", "Max")) 

# STAT_lg_GROWTH_Område_Year = STAT_lg_GROWTH_Område_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_GROWTH_Område_Year, aes(x = År, y = Y_T_År, color = Område))  

St_YT + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for Y_T")


x11()
St_YP <- ggplot(STAT_lg_GROWTH_Område_Year, aes(x = År, y = Y_P_År, color = Område))  

St_YP + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Produktivitetsvækstrater i pct.")


x11()
St_BVT <- ggplot(STAT_lg_GROWTH_Område_Year, aes(x = År, y = BVT_År, color = Område))  

St_BVT + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for BVT")


x11()
St_EMP_T <- ggplot(STAT_lg_GROWTH_Område_Year, aes(x = År, y = EMP_T_År, color = Område))  

St_EMP_T + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_T")


x11()
St_EMP_P <- ggplot(STAT_lg_GROWTH_Område_Year, aes(x = År, y = EMP_P_År, color = Område))  

St_EMP_P + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_P")


?geom_line






#################
##### OLD CODE?
##################

# Stats Plots per regions


# STAT_GROWTH_Område_Year<- read.csv("Summary_GROWTH_Per_Year_OMRÅDE_X.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
# str(STAT_GROWTH_Område_Year)
# head(STAT_GROWTH_Område_Year)
# 
# STAT_GROWTH_Område_Year$År = as.factor(STAT_GROWTH_Område_Year$År)
# 
# long_gr_omr = reshape(data = STAT_GROWTH_Område_Year,
#                       idvar = "Obs",
#                       varying = colnames(STAT_GROWTH_Område_Year)[4:28],
#                       sep = ".",
#                       timevar = "Stat",
#                       times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
#                       direction = "long")
# # 
# head(long_gr_omr)
# 
# # write.csv(long_gr_omr,"Summary_lg_GROWTH_Per_Year_OMRÅDE.csv")
# 
# STAT_lg_GROWTH_Område_Year<- read.csv("Summary_lg_GROWTH_Per_Year_OMRÅDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
# str(STAT_lg_GROWTH_Område_Year)
# head(STAT_lg_GROWTH_Område_Year)
# 
# STAT_lg_GROWTH_Område_Year$År <- as.numeric(as.character
#                                             (STAT_lg_GROWTH_Område_Year$År))
# 
# STAT_lg_GROWTH_Område_Year$Stats = factor(STAT_lg_GROWTH_Område_Year$Stat, levels=c("median","mean","sd","min", "max"), 
#                                           labels=c("median","mean","sd","min", "max")) 
# 
# STAT_lg_GROWTH_Område_Year = STAT_lg_GROWTH_Område_Year[,-5]
# 

######################################
# NOT WORKING AS WISHED with X-LABELS
######################################

# plotCols0 <- c("Y_T_År",
#                "Y_P_År",
#                "BVT_År",
#                "EMP_T_År",
#                "EMP_P_År")
# 
# 
# ggbox <- function(x) {
#   title <- paste("Tidsserie af", x, "per Område")
#   ggplot(STAT_lg_GROWTH_Område_Year, aes_string('factor(År)', x))  +
#     
#     geom_line(aes(group = Område, colour = factor(Område))) + 
#     facet_grid( . ~ Stats) +
#     scale_y_continuous(name = "Vækstrater i pct.") + 
#     ggtitle(title) +
#     scale_color_grey() + theme_classic() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     theme_bw() 
# }
# 
# lapply(plotCols0, ggbox)





?geom_line


# library(reshape2)
# 
# head(STAT_lg_GROWTH_Område_Year)
# str(STAT_lg_GROWTH_Område_Year)
# 
# data_resh <- melt(STAT_lg_GROWTH_Område_Year, id.vars=c("Obs","År"), measure.vars = colnames(STAT_lg_GROWTH_Område_Year)[5:9])
# tail(data_resh)
# 
# str(data_resh)



# x11()
# box_Rest <- ggplot(data_resh, aes(x=factor(År), y =value)) +
#   geom_line() + facet_wrap(~variable, scales = "free") 
# box_Rest



?melt
# 
# data_reshX <- melt(STAT_lg_GROWTH_Område_Year[,-c(4:8)], id.vars=c("Obs","År"))
# tail(data_reshX)


# OMR1 = Hovedstaden

data_YL_longX_Omr1 = subset(data_YL_longXX, data_YL_longXX$Region_navn=="Hovedstaden")
summary(data_YL_longX_Omr1)

data_YL_longX_Omr1$Region_navn = factor(data_YL_longX_Omr1$Region_navn)
levels(data_YL_longX_Omr1$Region_navn)

data_YL_longX_Omr1$Landsdel_navn = factor(data_YL_longX_Omr1$Landsdel_navn)
levels(data_YL_longX_Omr1$Landsdel_navn)


# OMR2 = Øvrige

data_YL_longX_Omr2 = subset(data_YL_longXX, data_YL_longXX$Region_navn!="Hovedstaden")
summary(data_YL_longX_Omr2)

data_YL_longX_Omr2$Region_navn = factor(data_YL_longX_Omr2$Region_navn)
levels(data_YL_longX_Omr2$Region_navn)

data_YL_longX_Omr2$Landsdel_navn = factor(data_YL_longX_Omr2$Landsdel_navn)
levels(data_YL_longX_Omr2$Landsdel_navn)


# # convert to local data frame

LG1 <- tbl_df(data_YL_longX_Omr1)
LG2 <- tbl_df(data_YL_longX_Omr2)

summary(data_YL_longX_Omr1)

byen_kbh = subset(data_YL_longX_Omr1, data_YL_longX_Omr1$Landsdel_navn=="Byen København")
head(byen_kbh)

tapply(byen_kbh$Y_P_År, byen_kbh$kom, mean, na.rm = T)

tapply(byen_kbh$Y_T_År, byen_kbh$kom, mean, na.rm = T)


# # Average amount of iron sorted by high and low protein?
# tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)

# # printing only shows 10 rows and as many columns as can fit on your screen
# flights
# 
# # you can specify that you want to see more rows
# print(flights, n=20)
# 
# # convert to a normal data frame to see all of the columns
# data.frame(head(flights))


# # dplyr approach to CONDITIONING (Filter)
# # note: you can use comma or ampersand (&) to represent the AND condition
# filter(flights, Month==1, DayofMonth==1) # or
# filter(flights, Month==1 & DayofMonth==1)
# 
# # use pipe for OR-condition

# LG1 = filter(LG, Område=="Hovedstaden")
# LG2 = filter(LG, Område=="Øvrige Kommuner")

# # dplyr approach to SELECT
# select(flights, DepTime, ArrTime, FlightNum)
# 
# 
# # use colon to select multiple contiguous columns, 
# # and use `contains` to match columns by name
# # `starts_with`, `ends_with`, and `matches` (for regular expressions) 
# # can also be used to match columns by name
# 
# select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))
# select(flights, starts_with("Tail"), ends_with("Time"))
# 
# 
# ## CHAINING (THEN ...)
# 
# # nesting method to select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes
# filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)
# 
# # CHAINING method
# flights %>%
#   select(UniqueCarrier, DepDelay) %>%
#   filter(DepDelay > 60)
# 
# 
# # # SIMPLIFYING with CHAINING create two vectors and calculate Euclidian distance between them
# # 
# # x1 <- 1:5
# # x2 <- 2:6
# #
# # sqrt(sum((x1-x2)^2))
# # 
# # # chaining method
# # (x1-x2)^2 %>% sum() %>% sqrt()


# # MUTATING / TRANSFORMING - CALCULATING ...
# 
# # basic R approach to create a new variable Speed (in mph)
# flights$Speed <- flights$Distance / flights$AirTime*60
# flights[, c("Distance", "AirTime", "Speed")]
# 
# 
# # dplyr approach (prints the new variable BUT DOES NOT STORE IT!!!)
# flights %>%
#   select(Distance, AirTime) %>%
#   mutate(Speed = Distance/AirTime*60)
# 
# # store the new variable
# flights <- flights %>% mutate(Speed = Distance/AirTime*60)
# 
# # and view it!
# flights %>% 
#   select(Distance, AirTime, Speed)
# 
# 
# # SUMMARISING - AGGREGATING ...
# 
# # basic R approaches to calculate the average arrival delay to each destination
# head(with(flights, tapply(ArrDelay, Dest, mean, na.rm=TRUE)))
# head(aggregate(ArrDelay ~ Dest, flights, mean))
# 
# # dplyr approach: create a table grouped by Dest, 
# # and then summarise each group by taking the mean of ArrDelay


# Test
LG1 %>%
   group_by(Landsdel_navn) %>%
   summarise(sd_rates = sd(Y_T_År), na.rm=TRUE)

Sum1 = LG1 %>%
  group_by(Landsdel_navn) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(Sum1)

write.csv(Sum1,"Summary_Stat_AGGR_HOVEDSTADEN_Regioner_29.csv")


# Test
# LG1_ÅR %>%
#   group_by(Landsdel_navn, År) %>%
#   summarise(sd_rates = sd(Y_T_År), na.rm=TRUE)

Sum1_ÅR = LG1 %>%
  group_by(Landsdel_navn, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(Sum1_ÅR)

# write.csv(Sum1_ÅR,"Summary_Stat_YEARLY_HOVEDSTADEN_Regioner_29.csv")

# Summary_GROWTH_Per_Year_HOVEDSTADEN_Regioner


STAT_GROWTH_Hoved_Region_Year<- read.csv("Summary_Stat_YEARLY_HOVEDSTADEN_Regioner_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_GROWTH_Hoved_Region_Year)
head(STAT_GROWTH_Hoved_Region_Year)

# STAT_GROWTH_Område_Year$År = as.factor(STAT_GROWTH_Område_Year$År)

long_gr_hov_reg = reshape(data = STAT_GROWTH_Hoved_Region_Year,
                      idvar = "Obs",
                      varying = colnames(STAT_GROWTH_Hoved_Region_Year)[4:28],
                      sep = ".",
                      timevar = "Stat",
                      times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
                      direction = "long")
# 
head(long_gr_hov_reg)

# write.csv(long_gr_hov_reg,"Summary_lg_GROWTH_Per_Year_HOV_LAND_29.csv")



STAT_lg_GROWTH_Hoved_Region_Year<- read.csv("Summary_lg_GROWTH_Per_Year_HOV_LAND_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_GROWTH_Hoved_Region_Year)
tail(STAT_lg_GROWTH_Hoved_Region_Year)

440/22

# STAT_lg_GROWTH_Område_Year$År <- as.numeric(as.character
  #                                          (STAT_lg_GROWTH_Område_Year$År)))

STAT_lg_GROWTH_Hoved_Region_Year$Stats = factor(STAT_lg_GROWTH_Hoved_Region_Year$Stat, levels=c("median","mean","sd","min", "max"), 
                                          labels=c("Median","Gennemsnit","SD","Min", "Max")) 

# STAT_lg_GROWTH_Område_Year = STAT_lg_GROWTH_Område_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_GROWTH_Hoved_Region_Year, aes(x = År, y = Y_T_År, color = Landsdel_navn))  

St_YT + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for Y_T")


x11()
St_YP <- ggplot(STAT_lg_GROWTH_Hoved_Region_Year, aes(x = År, y = Y_P_År, color = Landsdel_navn))  

St_YP + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Produktivitetsvækstrater i pct.")


x11()
St_BVT <- ggplot(STAT_lg_GROWTH_Hoved_Region_Year, aes(x = År, y = BVT_År, color = Landsdel_navn))  

St_BVT + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for BVT")


x11()
St_EMP_T <- ggplot(STAT_lg_GROWTH_Hoved_Region_Year, aes(x = År, y = EMP_T_År, color = Landsdel_navn))  

St_EMP_T + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_T")


x11()
St_EMP_P <- ggplot(STAT_lg_GROWTH_Hoved_Region_Year, aes(x = År, y = EMP_P_År, color = Landsdel_navn))  

St_EMP_P + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_P")





# Not corrected for 29 - since not in NOTAT

Sum21 = LG2 %>%
  group_by(Region_navn) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(Sum21)

# write.csv(Sum21,"Summary_Stat_YEARLY_ØVRIGE_Regioner.csv")



Sum21_ÅR = LG2 %>%
  group_by(Region_navn, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(Sum21_ÅR)

write.csv(Sum21_ÅR,"Summary_Stat_YEARLY_ØVRIGE_Regioner.csv")

# Summary_GROWTH_Per_Year_HOVEDSTADEN_Regioner


STAT_GROWTH_Øvrige_Region_Year<- read.csv("Summary_GROWTH_Per_Year_ØVRIGE_Regioner.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_GROWTH_Øvrige_Region_Year)
head(STAT_GROWTH_Øvrige_Region_Year)

# STAT_GROWTH_Område_Year$År = as.factor(STAT_GROWTH_Område_Year$År)

long_gr_øvr_reg = reshape(data = STAT_GROWTH_Øvrige_Region_Year,
                          idvar = "Obs",
                          varying = colnames(STAT_GROWTH_Øvrige_Region_Year)[4:28],
                          sep = ".",
                          timevar = "Stat",
                          times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
                          direction = "long")
# 
head(long_gr_øvr_reg)

# write.csv(long_gr_øvr_reg,"Summary_lg_GROWTH_Per_Year_ØVR_REG.csv")



STAT_lg_GROWTH_Øvrige_Region_Year<- read.csv("Summary_lg_GROWTH_Per_Year_ØVR_REG.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_GROWTH_Øvrige_Region_Year)
head(STAT_lg_GROWTH_Øvrige_Region_Year)

# STAT_lg_GROWTH_Område_Year$År <- as.numeric(as.character
#                                          (STAT_lg_GROWTH_Område_Year$År)))

STAT_lg_GROWTH_Øvrige_Region_Year$Stats = factor(STAT_lg_GROWTH_Øvrige_Region_Year$Stat, levels=c("median","mean","sd","min", "max"), 
                                                labels=c("median","mean","sd","min", "max")) 

# STAT_lg_GROWTH_Område_Year = STAT_lg_GROWTH_Område_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_GROWTH_Øvrige_Region_Year, aes(x = År, y = Y_T_År, color = Region_navn))  

St_YT + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for Y_T")


x11()
St_YP <- ggplot(STAT_lg_GROWTH_Øvrige_Region_Year, aes(x = År, y = Y_P_År, color = Region_navn))  

St_YP + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for Y_P")


x11()
St_BVT <- ggplot(STAT_lg_GROWTH_Øvrige_Region_Year, aes(x = År, y = BVT_År, color = Region_navn))  

St_BVT + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for BVT")


x11()
St_EMP_T <- ggplot(STAT_lg_GROWTH_Øvrige_Region_Year, aes(x = År, y = EMP_T_År, color = Region_navn))  

St_EMP_T + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_T")


x11()
St_EMP_P <- ggplot(STAT_lg_GROWTH_Øvrige_Region_Year, aes(x = År, y = EMP_P_År, color = Region_navn))  

St_EMP_P + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_P")



# DPLYR AGGREGATION - PBM WITH UNWEIGHTED AVERAGES AND PRODUCTIVITY AVERAGES!!!

Sum22 = LG2 %>%
  group_by(Landsdel_navn) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(Sum22)

# write.csv(Sum22,"Summary_Stat_YEARLY_ØVRIGE_Landsdel_29.csv")



Sum22_ÅR = LG2 %>%
  group_by(Landsdel_navn, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("År"))

data.frame(Sum22_ÅR)

# write.csv(Sum22_ÅR,"Summary_Stat_Per_Year_ØVRIGE_Landsdel_29.csv")

# Summary_GROWTH_Per_Year_HOVEDSTADEN_Regioner



STAT_GROWTH_Øvrige_Landsdel_Year<- read.csv("Summary_Stat_Per_Year_ØVRIGE_Landsdel_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_GROWTH_Øvrige_Landsdel_Year)
head(STAT_GROWTH_Øvrige_Landsdel_Year)

# STAT_GROWTH_Område_Year$År = as.factor(STAT_GROWTH_Område_Year$År)

long_gr_øvr_land = reshape(data = STAT_GROWTH_Øvrige_Landsdel_Year,
                          idvar = "Obs",
                          varying = colnames(STAT_GROWTH_Øvrige_Landsdel_Year)[4:28],
                          sep = ".",
                          timevar = "Stat",
                          times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
                          direction = "long")
# 
head(long_gr_øvr_land)

# write.csv(long_gr_øvr_land,"Summary_lg_GROWTH_Per_Year_ØVR_LAND_29.csv")



STAT_lg_GROWTH_Øvrige_Landsdel_Year<- read.csv("Summary_lg_GROWTH_Per_Year_ØVR_LAND_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_GROWTH_Øvrige_Landsdel_Year)
head(STAT_lg_GROWTH_Øvrige_Landsdel_Year)

STAT_lg_GROWTH_Område_Year$År <- as.numeric(as.character
                                          (STAT_lg_GROWTH_Område_Year$År))

STAT_lg_GROWTH_Øvrige_Landsdel_Year$Stats = factor(STAT_lg_GROWTH_Øvrige_Landsdel_Year$Stat, levels=c("median","mean","sd","min", "max"), 
                                                 labels=c("Median","Gennemsnit","SD","Min", "Max")) 

# STAT_lg_GROWTH_Område_Year = STAT_lg_GROWTH_Område_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_GROWTH_Øvrige_Landsdel_Year, aes(x = År, y = Y_T_År, color = Landsdel_navn))  

St_YT + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Produktivitetsvækstrater i pct.")


x11()
St_YP <- ggplot(STAT_lg_GROWTH_Øvrige_Landsdel_Year, aes(x = År, y = Y_P_År, color = Landsdel_navn))  

St_YP + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Produktivitetsvækstrater i pct.")


x11()
St_BVT <- ggplot(STAT_lg_GROWTH_Øvrige_Landsdel_Year, aes(x = År, y = BVT_År, color = Landsdel_navn))  

St_BVT + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for BVT")


x11()
St_EMP_T <- ggplot(STAT_lg_GROWTH_Øvrige_Landsdel_Year, aes(x = År, y = EMP_T_År, color = Landsdel_navn))  

St_EMP_T + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_T")


x11()
St_EMP_P <- ggplot(STAT_lg_GROWTH_Øvrige_Landsdel_Year, aes(x = År, y = EMP_P_År, color = Landsdel_navn))  

St_EMP_P + geom_line(size=1) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_P")






########################################
############ FIN DLYPR - Growth Rates ###########
########################################



# # Average amount of iron sorted by high and low protein?
# tapply(data_YL_longX$Y_T_År, data_YL_longX$År, mean, na.rm=TRUE)
# 
# 
# # Checking CONSTANT variables
# 
# sd(tapply(data$educ, data$nr, sd))
# sd(tapply(data$hisp, data$nr, sd))
# sd(tapply(data$expersq, data$nr, sd))
# 
# sapply(data8,class)  # classses of variables
# sapply(data8,sd)
# 
# 
# rbind(sapply(data8,mean),sapply(data8,sd))  # means/sd
# 
# 
# summary(data8[,c("famsize","educhead")])
# summary(data8[grep("exp", colnames(data8))])
# 
# sapply(data8[grep("exp", colnames(data8))],sd)
# 
# rbind(sapply(data8[grep("exp", colnames(data8))],mean),
#       sapply(data8[grep("exp", colnames(data8))],sd))



library(psych)

describe(data8[,c("famsize","educhead")])
describe(data8)

# Table
tab=table(data_YL_longX$Region_navn)
tab/22

land

tabx=table(data_YL_longX$Region_navn, data_YL_longX$Landsdel_navn)
tabx/22


# prop.table(tabx,1)
# prop.table(tabx,2)
# 
# addmargins(tabx)


# # In compact form and listed
# mytable=table(data8$dfmfd, data8$sexhead)
# 
# list(counts = mytable,
#      percent.row = prop.table(mytable,1),
#      percent.col = prop.table(mytable,2),
#      count.row = margin.table(mytable,1),
#      count.col = margin.table(mytable,2))
# addmargins(mytable)



###########################################
###########################################
###########################################

# EARLIER DATASÆT 2004-2012
data_YL_long<- read.csv("LONG_ÅR_FULL.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_long)
head(data_YL_long)


# FINAL NEAT DATASÆT 1993-2015 (IMPORTANT LAST YEARS!) - GROWTH RATES
data_YL_longX<- read.csv("LONG_ÅR_FULLX.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_longX)
tail(data_YL_longX)


# ATTENTION!!! "Uden for Region" INCLU!!!
2112/22
96

summary(data_YL_longX)

# FINAL NEAT DATASÆT 1993-2015 (IMPORTANT LAST YEARS!) - LEVELS
data_YL_longLX<- read.csv("LONG_ÅR_FULLX_L.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_longLX)
head(data_YL_longLX)



# 
# # 
#  long$Period <- factor(long$period)
# # 
#  levels(long$Period)[1] <- "Pre"
#  levels(long$Period)[2] <- "Crisis"
#  levels(long$Period)[3] <- "Post"
# # 
#  head(long)
# 
# long = long[order(long$kom,long$Year),]
# # 
# write.csv(long,"KL_Productivity_LONG.csv")


# Graphs and plots 

library(gplots)
library(car)
library(CorrMixed)


# LIMITED DATASET without BVT and EMP - BUT FULL PERIOD

# plotmeans(Y_T ~ År, main="Udvikling i Bruto Værdi Tilvækst Per Capita (fordelt per kommuner)", 
#           data_lg_x_DK)
# 
# head(data_lg_x_DK)
# summary(data_lg_x_DK)
# 
# data_lg_x_DK$Region_navn = factor(data_lg_x_DK$Region_navn)
# levels(data_lg_x_DK$Region_navn)
# 
# data_lg_x_DK$Landsdel_navn = factor(data_lg_x_DK$Landsdel_navn)
# levels(data_lg_x_DK$Landsdel_navn)



head(data_YL_longX)


# TITLEN OPRINDELIGT - OK FOR Y_T og Y_P - MANGLER NIVEAEUR FOR BESKÆFTIGELSEN!!!
# Udvikling i Bruto Værdi Tilvækst Per Capita (fordelt per kommuner)

x11()
plotmeans(Y_T_År ~ År,  main="Vækstraten i Produktiviteten Per Beskæftigede Timer (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(Y_P_År ~ År, main="Vækstraten i Produktiviteten Per Beskæftigede Personer (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(BVT_År ~ År, main="Vækstraten i Brutoværditilvæksten (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(EMP_T_År ~ År, main="Vækstraten i Beskæftigelsen i Timer (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(EMP_P_År ~ År, main="Vækstraten i Beskæftigelsen i Personer (inkl. den Kommunale Variation)", 
          data_YL_longX)


# plotCols <- c("Y_T_År",
#               "Y_P_År",
#               "BVT_År",
#                "EMP_T_År",
#                "EMP_P_År")
# 
# plotm <- function(x) {
#   title <- paste("Scatterplot of", x, "fordelt ud fra Området")
#   plotmeans(x ~ factor(År),  main="Udvikling i Bruto Værdi Tilvækst Per Capita (fordelt per kommuner)", 
#             data_YL_long)
#     theme_bw() 
# }
# 
# lapply(plotCols, plotm)



# Subsetting by Område: Hovedstaden Vs Øvrige Kommuner
# Subsetting by OMRÅDE

# dev.off()
# 
# graphics.off()
# par("mar")
# par(mar=c(1,1,1,1))


x11()
scatterplot(Y_T_År ~ År|Område, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK )
scatterplot(Y_T_År ~ År|Region_navn, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK )
scatterplot(Y_T_År ~ År|Landsdel_navn, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK )
scatterplot(Y_T_År ~ År|kom, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK)



# SPAGHETTI PLOT for VARIANCE Description

str(data_YL_longX)

2112/22
96

unique(data_YL_longX$kom)
length(data_YL_longX$kom)/22

# WITH 961 "Uden for Region"

# Without 961!!!
data_YL_longXX = subset(data_YL_longX, data_YL_longX$kom!=961)

## define base for the graphs and store in object 'p'
x11()
p_T <- ggplot(data = data_YL_longX, aes(x = År, y = Y_T_År, group = factor(kom), color = Landsdel_navn))

p_T + geom_line() +  
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

## define base for the graphs and store in object 'p' - NO 961!
p_P <- ggplot(data = data_YL_longXX, aes(x = År, y = Y_P_År, group = factor(kom), color = Landsdel_navn))

p_P + geom_line() +
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw()



x11()
p_T + geom_line() + facet_grid(. ~ Område) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

p_P + geom_line() + facet_grid(. ~ Område) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) + scale_y_continuous(name = "Vækstrater i pct.") +
  theme_bw() 



#######################################################
# SPAGHETTI GRAPHS per OMRÅDE with FULL SAMPLES!
#########################################################


# OMRÅDE 1

## define base for the graphs and store in object 'p'
p1_T <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = Y_T_År, group = factor(kom), color = Landsdel_navn))
p1_P <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = Y_P_År, group = factor(kom), color = Landsdel_navn))
p1_BVT <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = BVT_År, group = factor(kom), color = Landsdel_navn))
p1_EMP_T <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = EMP_T_År, group = factor(kom), color = Landsdel_navn))
p1_EMP_P <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = EMP_P_År, group = factor(kom), color = Landsdel_navn))


x11()
p1_T + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_P + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_BVT + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_EMP_T + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_EMP_P + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 


# LOOPS - PAS NECESSAIRE!!! EN points!

head(data_YL_longX_Omr1)

plotCols0 <- c("BVT_År",
               "EMP_T_År",
               "EMP_P_År")


ggbox <- function(x) {
  title <- paste("Scatterplot", x, "fordelt ud fra Området")
  ggplot(data_YL_longX_Omr1, aes_string('factor(År)', x, colour = 'Landsdel_navn')) +
    geom_point() + facet_grid(Landsdel_navn ~ Område) + 
    stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
    stat_smooth(aes(group = 1)) +
    theme_bw() 
}

lapply(plotCols0, ggbox)


ggbox <- function(x) {
  title <- paste("Scatterplot", x, "fordelt ud fra Området")
  ggplot(data_YL_longX_Omr2, aes_string('factor(År)', x, colour = 'Landsdel_navn')) +
    geom_point() + facet_grid(Region_navn ~ Område) + 
    stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
    stat_smooth(aes(group = 1)) +
    theme_bw() 
}



# OMRÅDE 2

## define base for the graphs and store in object 'p'
p2_T <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = Y_T_År, group = factor(kom), color = Landsdel_navn))
p2_P <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = Y_P_År, group = factor(kom), color = Landsdel_navn))
p2_BVT <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = BVT_År, group = factor(kom), color = Landsdel_navn))
p2_EMP_T <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = EMP_T_År, group = factor(kom), color = Landsdel_navn))
p2_EMP_P <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = EMP_P_År, group = factor(kom), color = Landsdel_navn))

x11()
p2_T + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_P + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_BVT + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_EMP_T + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_EMP_P + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 


# LOOPS - PAS NECESSAIRE!!! EN points!

head(data_YL_longX_Omr2)

plotCols0 <- c("BVT_År",
               "EMP_T_År",
               "EMP_P_År")


ggbox <- function(x) {
  title <- paste("Scatterplot", x, "fordelt ud fra Området")
  ggplot(data_YL_longX_Omr2, aes_string('factor(År)', x, colour = 'Landsdel_navn')) +
  geom_point() + facet_grid(Region_navn ~ Område) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 
}

lapply(plotCols0, ggbox)





########################
#### NIVEAU ANALYSE ####  Hovedstaden NEW: 29 Vs 31  - 
########################


data_NIVEAU_long<- read.csv("KL_YL_NIVEAU_LONG.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NIVEAU_long)
tail(data_NIVEAU_long)

# 253 og 259 til Sjælland!

# Region_navn
data_NIVEAU_long$Region_navn <- as.character(data_NIVEAU_long$Region_navn)
data_NIVEAU_long$Region_navn[data_NIVEAU_long$kom==253] <- "Sjælland"
data_NIVEAU_long$Region_navn <- as.factor(data_NIVEAU_long$Region_navn)


data_NIVEAU_long$Region_navn <- as.character(data_NIVEAU_long$Region_navn)
data_NIVEAU_long$Region_navn[data_NIVEAU_long$kom==259] <- "Sjælland"
data_NIVEAU_long$Region_navn <- as.factor(data_NIVEAU_long$Region_navn)


# Område
data_NIVEAU_long$Område <- as.character(data_NIVEAU_long$Område)
data_NIVEAU_long$Område[data_NIVEAU_long$kom==253] <- "Øvrige Kommuner"
data_NIVEAU_long$Område <- as.factor(data_NIVEAU_long$Område)


data_NIVEAU_long$Område <- as.character(data_NIVEAU_long$Område)
data_NIVEAU_long$Område[data_NIVEAU_long$kom==259] <- "Øvrige Kommuner"
data_NIVEAU_long$Område <- as.factor(data_NIVEAU_long$Område)

summary(data_NIVEAU_long)

# CHECK
# Before change
713/23

345/23

# After change
667/23

391/23



library(dplyr)

head(data_NIVEAU_long)
summary(data_NIVEAU_long)
unique(data_NIVEAU_long$kom)
str(data_NIVEAU_long)


# NO DK !
data_NIVEAU_long_DK = subset(data_NIVEAU_long, !(data_NIVEAU_long$kom==999))
head(data_NIVEAU_long_DK)
str(data_NIVEAU_long_DK)
unique(data_NIVEAU_long_DK$kom)


# NO DK already AND NO "Uden REgion" ALSO!
data_NIVEAU_long_sh = subset(data_NIVEAU_long, !(data_NIVEAU_long$kom==961 | data_NIVEAU_long$kom==999))
head(data_NIVEAU_long_sh)
str(data_NIVEAU_long_sh)
unique(data_NIVEAU_long_sh$kom)



#########################################
# PBM = ALL-YEARS AGGREGATE for LEVELS!!!
#########################################

# DPLYR Data SUMMARY
# L for LEVEL!

# LG_LX <- tbl_df(data_NIVEAU_long_DK) # 
LG_LXX <- tbl_df(data_NIVEAU_long_sh) # To compare effects on SUMMARY STATISTICS


SumXX = LG_LXX %>%
  group_by(Område) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(SumXX)

# write.csv(SumXX,"Summary_NIVEAU_YEARLY_OMRÅDE_Unden_Region_EXCLU_29.csv")
# "Uden Region" a EFFET sur SD, MAX et MIN => EXCLU!!!



###################################
# AGGREGATES for EVERY YEAR #######
###################################


SumXX_ÅR = LG_LXX %>%
  group_by(Område, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(SumXX_ÅR)

# write.csv(SumXX_ÅR,"Summary_LEVELS_PER_YEAR_OMRÅDE_29.csv")


STAT_LEVELS_Område_Year<- read.csv("Summary_LEVELS_PER_YEAR_OMRÅDE_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_LEVELS_Område_Year)
head(STAT_LEVELS_Område_Year)

# STAT_LEVELS_Område_Year$År = as.factor(STAT_LEVELS_Område_Year$År)



long_lev_omr = reshape(data = STAT_LEVELS_Område_Year,
                      idvar = "Obs",
                      varying = colnames(STAT_LEVELS_Område_Year)[3:27],
                      sep = ".",
                      timevar = "Stat",
                      times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
                      direction = "long")
# 
head(long_lev_omr)

# write.csv(long_lev_omr,"Summary_lg_LEVELS_Per_Year_OMRÅDE_29.csv")

STAT_lg_LEVELS_Område_Year<- read.csv("Summary_lg_LEVELS_Per_Year_OMRÅDE_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_LEVELS_Område_Year)
head(STAT_lg_LEVELS_Område_Year)

STAT_lg_LEVELS_Område_Year$År <- as.numeric(as.character(STAT_lg_LEVELS_Område_Year$År))

STAT_lg_LEVELS_Område_Year$Stats = factor(STAT_lg_LEVELS_Område_Year$Stat, levels=c("median","mean","sd","min", "max"), labels=c("Median","Gennemsnit","SD","Min", "Max")) 

# STAT_lg_LEVELS_Område_Year = STAT_lg_LEVELS_Område_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_LEVELS_Område_Year, aes(x = År, y = Y_T_niveau, color = Område))  

St_YT + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for Y_T")


x11()
St_YP <- ggplot(STAT_lg_LEVELS_Område_Year, aes(x = År, y = Y_P_niveau, color = Område))  

St_YP + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Produktivitetsniveau (kædede, i tusinde kroner)",
                                  labels = scales::unit_format("", 1e-3))


x11()
St_BVT <- ggplot(STAT_lg_LEVELS_Område_Year, aes(x = År, y = BVT_niveau, color = Område))  

St_BVT + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for BVT")


x11()
St_EMP_T <- ggplot(STAT_lg_LEVELS_Område_Year, aes(x = År, y = EMP_T_niveau, color = Område))  

St_EMP_T + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_T")


x11()
St_EMP_P <- ggplot(STAT_lg_LEVELS_Område_Year, aes(x = År, y = EMP_P_niveau, color = Område))  

St_EMP_P + geom_line(size=2) + facet_grid( . ~ Stats) + scale_color_grey() + theme_classic() +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_P")




# SumYT_ÅR = LG_LXX %>%
#   group_by(Område, År) %>%
#   summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
#                       min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("Y_T_niveau"))
# 
# data.frame(SumYT_ÅR)
# 
# write.csv(SumYT_ÅR,"Summary_Y-T_PER_YEAR_OMRÅDE_Unden_Region_EXCLU.csv")
# 

# SumYP_ÅR = LG_LXX %>%
#   group_by(Område, År) %>%
#   summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
#                       min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("Y_P_niveau"))
# 
# data.frame(SumYP_ÅR)
# 
# write.csv(SumYP_ÅR,"Summary_Y-P_PER_YEAR_OMRÅDE_Unden_Region_EXCLU.csv")
# 
# 
# SumBVT_ÅR = LG_LXX %>%
#   group_by(Område, År) %>%
#   summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
#                       min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("BVT_niveau"))
# 
# data.frame(SumBVT_ÅR)
# 
# write.csv(SumBVT_ÅR,"Summary_BVT_PER_YEAR_OMRÅDE_Unden_Region_EXCLU.csv")
# 
# 
# SumEMP_T_ÅR = LG_LXX %>%
#   group_by(Område, År) %>%
#   summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
#                       min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("EMP_T_niveau"))
# 
# data.frame(SumEMP_T_ÅR)
# 
# write.csv(SumEMP_T_ÅR,"Summary_EMP_T_PER_YEAR_OMRÅDE_Unden_Region_EXCLU.csv")
# 
# 
# SumEMP_P_ÅR = LG_LXX %>%
#   group_by(Område, År) %>%
#   summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
#                       min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("EMP_P_niveau"))
# 
# data.frame(SumEMP_P_ÅR)
# 
# write.csv(SumEMP_P_ÅR,"Summary_EMP_P_PER_YEAR_OMRÅDE_Unden_Region_EXCLU.csv")
# 


# PAS BESOIN DE POUSSER L ANALYSE PAR ANNEE AVEC NIVEAU ... a reporter peut etre
# L ANALYSE PAR ANNEE AVEC Growth Rates plutot! ...



# LEVEL Per Regions
# OMR1 = Hovedstaden


data_NIVEAU_long_sh__Omr1 = subset(data_NIVEAU_long_sh, data_NIVEAU_long_sh$Region_navn=="Hovedstaden")
summary(data_NIVEAU_long_sh__Omr1)

data_NIVEAU_long_sh__Omr1$Region_navn = factor(data_NIVEAU_long_sh__Omr1$Region_navn)
levels(data_NIVEAU_long_sh__Omr1$Region_navn)

data_NIVEAU_long_sh__Omr1$Landsdel_navn = factor(data_NIVEAU_long_sh__Omr1$Landsdel_navn)
levels(data_NIVEAU_long_sh__Omr1$Landsdel_navn)


# OMR2 = Øvrige

data_NIVEAU_long_sh__Omr2 = subset(data_NIVEAU_long_sh, data_NIVEAU_long_sh$Region_navn!="Hovedstaden")
summary(data_NIVEAU_long_sh__Omr2)

data_NIVEAU_long_sh__Omr2$Region_navn = factor(data_NIVEAU_long_sh__Omr2$Region_navn)
levels(data_NIVEAU_long_sh__Omr2$Region_navn)

data_NIVEAU_long_sh__Omr2$Landsdel_navn = factor(data_NIVEAU_long_sh__Omr2$Landsdel_navn)
levels(data_NIVEAU_long_sh__Omr2$Landsdel_navn)


# # convert to local data frame

LG_L1 <- tbl_df(data_NIVEAU_long_sh__Omr1)
LG_L2 <- tbl_df(data_NIVEAU_long_sh__Omr2)



# AGGREGATES for ALL YEARS

# # Test
# LG1 %>%
#   group_by(Landsdel_navn) %>%
#   summarise(sd_rates = sd(Y_T_År), na.rm=TRUE)

Sum1 = LG_L1 %>%
  group_by(Landsdel_navn) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(Sum1)

# write.csv(Sum1,"Summary_NIVEAU_YEARLY_HOVEDSTADEN_Regioner_29.csv")

Sum1_ÅR = LG_L1 %>%
  group_by(Landsdel_navn, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(Sum1_ÅR)




# write.csv(Sum1_ÅR,"Summary_LEVELS_PER_YEAR_HOV_LAND_29.csv")


STAT_LEVELS_Hov_Land_Year<- read.csv("Summary_LEVELS_PER_YEAR_HOV_LAND_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_LEVELS_Hov_Land_Year)
head(STAT_LEVELS_Hov_Land_Year)

# STAT_LEVELS_Hov_Land_Year$År = as.factor(STAT_LEVELS_Hov_Land_Year$År)

long_lev_hov_land = reshape(data = STAT_LEVELS_Hov_Land_Year,
                       idvar = "Obs",
                       varying = colnames(STAT_LEVELS_Hov_Land_Year)[4:28],
                       sep = ".",
                       timevar = "Stat",
                       times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
                       direction = "long")
# 
head(long_lev_hov_land)

# write.csv(long_lev_hov_land,"Summary_lg_LEVELS_Per_Year_Hov_Land_29.csv")

STAT_lg_LEVELS_Hov_Land_Year<- read.csv("Summary_lg_LEVELS_Per_Year_Hov_Land_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_LEVELS_Hov_Land_Year)
head(STAT_lg_LEVELS_Hov_Land_Year)

STAT_lg_LEVELS_Hov_Land_Year$År <- as.numeric(as.character
                                             (STAT_lg_LEVELS_Hov_Land_Year$År))

STAT_lg_LEVELS_Hov_Land_Year$Stats = factor(STAT_lg_LEVELS_Hov_Land_Year$Stat, levels=c("median","mean","sd","min", "max"), 
                                          labels=c("Median","Gennemsnit","SD","Min", "Max")) 

# STAT_lg_LEVELS_Hov_Land_Year = STAT_lg_LEVELS_Hov_Land_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_LEVELS_Hov_Land_Year, aes(x = År, y = Y_T_niveau, color = Landsdel_navn))  

St_YT + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for Y_T")


x11()
St_YP <- ggplot(STAT_lg_LEVELS_Hov_Land_Year, aes(x = År, y = Y_P_niveau, color = Landsdel_navn))  

St_YP + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Produktivitetsniveau (kædede, i tusinde kroner)", 
                                  labels = scales::unit_format("", 1e-3))
?scale_y_continuous


x11()
St_BVT <- ggplot(STAT_lg_LEVELS_Hov_Land_Year, aes(x = År, y = BVT_niveau, color = Landsdel_navn))  

St_BVT + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for BVT")


x11()
St_EMP_T <- ggplot(STAT_lg_LEVELS_Hov_Land_Year, aes(x = År, y = EMP_T_niveau, color = Landsdel_navn))  

St_EMP_T + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_T")


x11()
St_EMP_P <- ggplot(STAT_lg_LEVELS_Hov_Land_Year, aes(x = År, y = EMP_P_niveau, color = Landsdel_navn))  

St_EMP_P + geom_line(size=2) + facet_grid( . ~ Stats) +
  theme_bw() + scale_y_continuous(name = "Vækstrater i pct. for EMP_P")





Sum21 = LG_L2 %>%
  group_by(Region_navn) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(Sum21)

# write.csv(Sum21,"Summary_NIVEAU_YEARLY_ØVRIGE_Regioner.csv")

# NOT CORRECTED with 29 Vs ... (not in NOTAT)
Sum21_ÅR = LG_L2 %>%
  group_by(Region_navn, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(Sum21_ÅR)

write.csv(Sum21_ÅR,"Summary_NIVEAU_Per_Year_ØVRIGE_Regioner.csv")




STAT_NIVEAU_ØVRIGE_Regioner_Year<- read.csv("Summary_NIVEAU_PER_YEAR_ØVRIGE_Regioner.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_NIVEAU_ØVRIGE_Regioner_Year)
head(STAT_NIVEAU_ØVRIGE_Regioner_Year)

# STAT_NIVEAU_ØVRIGE_Regioner_Year$År = as.factor(STAT_NIVEAU_ØVRIGE_Regioner_Year$År)

long_lev_ØVRIGE_Regioner = reshape(data = STAT_NIVEAU_ØVRIGE_Regioner_Year,
                            idvar = "Obs",
                            varying = colnames(STAT_NIVEAU_ØVRIGE_Regioner_Year)[4:28],
                            sep = ".",
                            timevar = "Stat",
                            times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
                            direction = "long")
# 
head(long_lev_ØVRIGE_Regioner)

# write.csv(long_lev_ØVRIGE_Regioner,"Summary_lg_NIVEAU_Per_Year_ØVRIGE_Regioner.csv")

STAT_lg_NIVEAU_ØVRIGE_Regioner_Year<- read.csv("Summary_lg_NIVEAU_Per_Year_ØVRIGE_Regioner.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year)
head(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year)

STAT_lg_NIVEAU_ØVRIGE_Regioner_Year$År <- as.numeric(as.character
                                            (STAT_lg_NIVEAU_ØVRIGE_Regioner_Year$År))

STAT_lg_NIVEAU_ØVRIGE_Regioner_Year$Stats = factor(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year$Stat, levels=c("median","mean","sd","min", "max"), 
                                            labels=c("Median","Gennemsnit","SD","Min", "Max")) 

# STAT_lg_NIVEAU_ØVRIGE_Regioner_Year = STAT_lg_NIVEAU_ØVRIGE_Regioner_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year, aes(x = År, y = Y_T_niveau, color = Region_navn))  

St_YT + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Niveau for Y_T (kr. per besk. time)")


x11()
St_YP <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year, aes(x = År, y = Y_P_niveau, color = Region_navn))  

St_YP + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Produktivitetsniveau (kædede, i tusinde kroner))",
                                  labels = scales::unit_format("", 1e-3))


x11()
St_BVT <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year, aes(x = År, y = BVT_niveau, color = Region_navn))  

St_BVT + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "niveau for BVT - kædede værdier i tusinder kr.)")


x11()
St_EMP_T <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year, aes(x = År, y = EMP_T_niveau, color = Region_navn))  

St_EMP_T + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Niveau for EMP_T i besk. timer)")


x11()
St_EMP_P <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Regioner_Year, aes(x = År, y = EMP_P_niveau, color = Region_navn))  

St_EMP_P + geom_line(size=2) + facet_grid( . ~ Stats) +
  theme_bw() + scale_y_continuous(name = "Niveau for EMP_P i besk. personer)")




Sum22 = LG_L2 %>%
  group_by(Landsdel_navn) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(Sum22)

# write.csv(Sum22,"Summary_NIVEAU_AGGR_ØVRIGE_Landsdel_29.csv")


Sum22_ÅR = LG_L2 %>%
  group_by(Landsdel_navn, År) %>%
  summarise_each(funs(median(., na.rm=TRUE), mean(., na.rm=TRUE), sd(., na.rm=TRUE), 
                      min(., na.rm=TRUE), max(., na.rm=TRUE)), contains("niveau"))

data.frame(Sum22_ÅR)

# write.csv(Sum22_ÅR,"Summary_NIVEAU_YEARLY_ØVRIGE_Landsdel_29.csv")



STAT_NIVEAU_ØVRIGE_Landsdel_Year<- read.csv("Summary_NIVEAU_YEARLY_ØVRIGE_Landsdel_29.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_NIVEAU_ØVRIGE_Landsdel_Year)
head(STAT_NIVEAU_ØVRIGE_Landsdel_Year)

# STAT_NIVEAU_ØVRIGE_Landsdel_Year$År = as.factor(STAT_NIVEAU_ØVRIGE_Landsdel_Year$År)

long_lev_ØVRIGE_Landsdel = reshape(data = STAT_NIVEAU_ØVRIGE_Landsdel_Year,
                                   idvar = "Obs",
                                   varying = colnames(STAT_NIVEAU_ØVRIGE_Landsdel_Year)[4:28],
                                   sep = ".",
                                   timevar = "Stat",
                                   times = c("median","mean","sd","min","max"), # times = c(1,2,3,4,5),
                                   direction = "long")
# 
head(long_lev_ØVRIGE_Landsdel)

# write.csv(long_lev_ØVRIGE_Landsdel,"Summary_lg_NIVEAU_Per_Year_ØVRIGE_Landsdel_29.csv")


STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year<- read.csv("Summary_lg_NIVEAU_Per_Year_ØVRIGE_Landsdel.csv",header=TRUE,sep=",", dec=".")  # , row.names=1 _Unden_Region_EXCLU
str(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year)
head(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year)

STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year$År <- as.numeric(as.character
                                            (STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year$År))

STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year$Stats = factor(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year$Stat, levels=c("median","mean","sd","min", "max"), 
                                                   labels=c("Median","Gennemsnit","SD","Min", "Max")) 

# STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year = STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year[,-5]


x11()
St_YT <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year, aes(x = År, y = Y_T_niveau, color = Landsdel_navn))  

St_YT + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Niveau for Y_T (kr. per besk. time)")


x11()
St_YP <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year, aes(x = År, y = Y_P_niveau, color = Landsdel_navn))  

St_YP + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Produktivitetsniveau (kædede, i tusinde kroner)",
                                  labels = scales::unit_format("", 1e-3))


x11()
St_BVT <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year, aes(x = År, y = BVT_niveau, color = Landsdel_navn))  

St_BVT + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "niveau for BVT - kædede værdier i tusinder kr.)")


x11()
St_EMP_T <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year, aes(x = År, y = EMP_T_niveau, color = Landsdel_navn))  

St_EMP_T + geom_line(size=2) + facet_grid( . ~ Stats) + 
  theme_bw() + scale_y_continuous(name = "Niveau for EMP_T i besk. timer)")


x11()
St_EMP_P <- ggplot(STAT_lg_NIVEAU_ØVRIGE_Landsdel_Year, aes(x = År, y = EMP_P_niveau, color = Landsdel_navn))  

St_EMP_P + geom_line(size=2) + facet_grid( . ~ Stats) +
  theme_bw() + scale_y_continuous(name = "Niveau for EMP_P i besk. personer)")









########################################
############ FIN DLYPR #################
########################################



# # Average amount of iron sorted by high and low protein?
# tapply(data_YL_longX$Y_T_År, data_YL_longX$År, mean, na.rm=TRUE)
# 
# 
# # Checking CONSTANT variables
# 
# sd(tapply(data$educ, data$nr, sd))
# sd(tapply(data$hisp, data$nr, sd))
# sd(tapply(data$expersq, data$nr, sd))
# 
# sapply(data8,class)  # classses of variables
# sapply(data8,sd)
# 
# 
# rbind(sapply(data8,mean),sapply(data8,sd))  # means/sd
# 
# 
# summary(data8[,c("famsize","educhead")])
# summary(data8[grep("exp", colnames(data8))])
# 
# sapply(data8[grep("exp", colnames(data8))],sd)
# 
# rbind(sapply(data8[grep("exp", colnames(data8))],mean),
#       sapply(data8[grep("exp", colnames(data8))],sd))



library(psych)

describe(data8[,c("famsize","educhead")])
describe(data8)

# Table
tab=table(data_YL_longX$Region_navn)
tab/22

land

tabx=table(data_YL_longX$Region_navn, data_YL_longX$Landsdel_navn)
tabx/22


# prop.table(tabx,1)
# prop.table(tabx,2)
# 
# addmargins(tabx)


# # In compact form and listed
# mytable=table(data8$dfmfd, data8$sexhead)
# 
# list(counts = mytable,
#      percent.row = prop.table(mytable,1),
#      percent.col = prop.table(mytable,2),
#      count.row = margin.table(mytable,1),
#      count.col = margin.table(mytable,2))
# addmargins(mytable)



###########################################
##############SUMMARY of DATASETS - GROWTH AND LEVELS ################
###########################################

# EARLIER DATASÆT 2004-2012
data_YL_long<- read.csv("LONG_ÅR_FULL.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_long)
head(data_YL_long)


# FINAL NEAT DATASÆT 1993-2015 (IMPORTANT LAST YEARS!) - GROWTH RATES
data_YL_longX<- read.csv("LONG_ÅR_FULLX.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_longX)
head(data_YL_longX)


# FINAL NEAT DATASÆT 1993-2015 (IMPORTANT LAST YEARS!) - LEVELS
data_YL_longLX<- read.csv("LONG_ÅR_FULLX_L.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_YL_longLX)
head(data_YL_longLX)


# PERFECT WIDE DATA - LONG TRANSFORMATION VARIABLE by VARIABLE then MERGE!!!
# Originally WIDE LEVELS transformed to LONG

# # # FINAL NEAT DATASÆT 1993-2015 (IMPORTANT LAST YEARS!) - LEVELS
# data_YL_long<- read.csv("Produktivitet_LEVELS_WIDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_YL_long)
# head(data_YL_long)
# # 
# # # write.csv(data_Y_P_long,"LONG_ÅR_FULLX_L.csv")
# 
# # write.csv(data_y_t_L,"y_t.csv")
# 
# data_YL_long<- read.csv("Produktivitet_LEVELS_WIDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_YL_long)
# head(data_YL_long)
# 
# 
# long_lev = reshape(data = data_YL_long,
#                idvar = "kom",
#                varying = colnames(data_YL_long)[-c(1,2,3,4)],
#                sep = ".",
#                timevar = "Year",
#                times = c(1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
#                direction = "long")
# #
# head(long_lev)
# 
# 
# write.csv(long_lev,"LONG_ÅR_FULLX_L.csv")


# 
# # 
#  long$Period <- factor(long$period)
# # 
#  levels(long$Period)[1] <- "Pre"
#  levels(long$Period)[2] <- "Crisis"
#  levels(long$Period)[3] <- "Post"
# # 
#  head(long)
# 
# long = long[order(long$kom,long$Year),]
# # 
# write.csv(long,"KL_Productivity_LONG.csv")


# Graphs and plots 

library(gplots)
library(car)
library(CorrMixed)


# LIMITED DATASET without BVT and EMP - BUT FULL PERIOD

# plotmeans(Y_T ~ År, main="Udvikling i Bruto Værdi Tilvækst Per Capita (fordelt per kommuner)", 
#           data_lg_x_DK)
# 
# head(data_lg_x_DK)
# summary(data_lg_x_DK)
# 
# data_lg_x_DK$Region_navn = factor(data_lg_x_DK$Region_navn)
# levels(data_lg_x_DK$Region_navn)
# 
# data_lg_x_DK$Landsdel_navn = factor(data_lg_x_DK$Landsdel_navn)
# levels(data_lg_x_DK$Landsdel_navn)


# PLOT MEANS FOR GROWTH!!!

head(data_YL_longX)


# TITLEN OPRINDELIGT - OK FOR Y_T og Y_P - MANGLER NIVEAEUR FOR BESKÆFTIGELSEN!!!
# Udvikling i Bruto Værdi Tilvækst Per Capita (fordelt per kommuner)

x11()
plotmeans(Y_T_År ~ År,  main="Vækstraten i Produktiviteten Per Beskæftigede Timer (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(Y_P_År ~ År, main="Vækstraten i Produktiviteten Per Beskæftigede Personer (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(BVT_År ~ År, main="Vækstraten i Brutoværditilvæksten (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(EMP_T_År ~ År, main="Vækstraten i Beskæftigelsen i Timer (inkl. den Kommunale Variation)", 
          data_YL_longX)
plotmeans(EMP_P_År ~ År, main="Vækstraten i Beskæftigelsen i Personer (inkl. den Kommunale Variation)", 
          data_YL_longX)



# PLOT MEANS FOR LEVELS!!!

tail(data_YL_longLX, 30)
str(data_YL_longLX)

data_YL_longLX = subset(data_YL_longLX, !(data_YL_longLX$kom==999 |
                                          data_YL_longLX$kom==961))

?plotmeans

# TITLEN OPRINDELIGT - OK FOR Y_T og Y_P - MANGLER NIVEAEUR FOR BESKÆFTIGELSEN!!!
# Udvikling i Bruto Værdi Tilvækst Per Capita (fordelt per kommuner)

x11()
plotmeans(Y_T_niveau ~ År,  main="Niveauet for Produktiviteten Per Beskæftiget Time (inkl. den Kommunale Variation)", 
          data_YL_longLX)
plotmeans(Y_P_niveau ~ År, main="Niveauet for Produktiviteten Per Beskæftiget Person (inkl. den Kommunale Variation)", ylab="Produktivitet Per Beskæftiget Person (kædede kroner)", 
          data_YL_longLX)
plotmeans(BVT_niveau ~ År, main="Niveauet for Brutoværditilvækst (inkl. den Kommunale Variation)", 
          data_YL_longLX)
plotmeans(EMP_T_niveau ~ År, main="Niveauet for Beskæftigelsen i Timer (inkl. den Kommunale Variation)", 
          data_YL_longLX)
plotmeans(EMP_P_niveau ~ År, main="Niveauet for Beskæftigelsen i Personer (inkl. den Kommunale Variation)", 
          data_YL_longLX)




# plotCols <- c("Y_T_År",
#               "Y_P_År",
#               "BVT_År",
#                "EMP_T_År",
#                "EMP_P_År")
# 
# plotm <- function(x) {
#   title <- paste("Scatterplot of", x, "fordelt ud fra Området")
#   plotmeans(x ~ factor(År),  main="Udvikling i Bruto Værdi Tilvækst Per Capita (fordelt per kommuner)", 
#             data_YL_long)
#     theme_bw() 
# }
# 
# lapply(plotCols, plotm)



# Subsetting by Område: Hovedstaden Vs Øvrige Kommuner
# Subsetting by OMRÅDE

# dev.off()
# 
# graphics.off()
# par("mar")
# par(mar=c(1,1,1,1))


x11()
scatterplot(Y_T_År ~ År|Område, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK )
scatterplot(Y_T_År ~ År|Region_navn, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK )
scatterplot(Y_T_År ~ År|Landsdel_navn, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK )
scatterplot(Y_T_År ~ År|kom, boxplot = F, smooth = T, reg.line = F, data = data_lg_x_DK)



# SPAGHETTI PLOT for VARIANCE Description

## define base for the graphs and store in object 'p'
x11()
p_T <- ggplot(data = data_YL_longX, aes(x = År, y = Y_T_År, group = factor(kom), color = Landsdel_navn))

p_T + geom_line() +  
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

## define base for the graphs and store in object 'p'
p_P <- ggplot(data = data_YL_longX, aes(x = År, y = Y_P_År, group = factor(kom), color = Landsdel_navn))

p_P + geom_line() +
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw()



x11()
p_T + geom_line() + facet_grid(. ~ Område) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

p_P + geom_line() + facet_grid(. ~ Område) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 



#######################################################
# SPAGHETTI GRAPHS per OMRÅDE with FULL SAMPLES!
#########################################################


# OMRÅDE 1

## define base for the graphs and store in object 'p'
p1_T <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = Y_T_År, group = factor(kom), color = Landsdel_navn))
p1_P <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = Y_P_År, group = factor(kom), color = Landsdel_navn))
p1_BVT <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = BVT_År, group = factor(kom), color = Landsdel_navn))
p1_EMP_T <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = EMP_T_År, group = factor(kom), color = Landsdel_navn))
p1_EMP_P <- ggplot(data = data_YL_longX_Omr1, aes(x = År, y = EMP_P_År, group = factor(kom), color = Landsdel_navn))


x11()
p1_T + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_P + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_BVT + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_EMP_T + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p1_EMP_P + geom_line() + facet_grid(. ~ Landsdel_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 


# LOOPS - PAS NECESSAIRE!!! EN points!

head(data_YL_longX_Omr1)

plotCols0 <- c("BVT_År",
               "EMP_T_År",
               "EMP_P_År")


ggbox <- function(x) {
  title <- paste("Scatterplot", x, "fordelt ud fra Området")
  ggplot(data_YL_longX_Omr1, aes_string('factor(År)', x, colour = 'Landsdel_navn')) +
    geom_point() + facet_grid(Landsdel_navn ~ Område) + 
    stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
    stat_smooth(aes(group = 1)) +
    theme_bw() 
}

lapply(plotCols0, ggbox)


ggbox <- function(x) {
  title <- paste("Scatterplot", x, "fordelt ud fra Området")
  ggplot(data_YL_longX_Omr2, aes_string('factor(År)', x, colour = 'Landsdel_navn')) +
    geom_point() + facet_grid(Region_navn ~ Område) + 
    stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
    stat_smooth(aes(group = 1)) +
    theme_bw() 
}



# OMRÅDE 2

## define base for the graphs and store in object 'p'
p2_T <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = Y_T_År, group = factor(kom), color = Landsdel_navn))
p2_P <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = Y_P_År, group = factor(kom), color = Landsdel_navn))
p2_BVT <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = BVT_År, group = factor(kom), color = Landsdel_navn))
p2_EMP_T <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = EMP_T_År, group = factor(kom), color = Landsdel_navn))
p2_EMP_P <- ggplot(data = data_YL_longX_Omr2, aes(x = År, y = EMP_P_År, group = factor(kom), color = Landsdel_navn))

x11()
p2_T + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_P + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_BVT + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_EMP_T + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 

x11()
p2_EMP_P + geom_line() + facet_grid(. ~ Region_navn) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
  stat_smooth(aes(group = 1)) +
  theme_bw() 


# LOOPS - PAS NECESSAIRE!!! EN points!

head(data_YL_longX_Omr2)

plotCols0 <- c("BVT_År",
               "EMP_T_År",
               "EMP_P_År")


ggbox <- function(x) {
  title <- paste("Scatterplot", x, "fordelt ud fra Området")
  ggplot(data_YL_longX_Omr2, aes_string('factor(År)', x, colour = 'Landsdel_navn')) +
    geom_point() + facet_grid(Region_navn ~ Område) + 
    stat_summary(aes(group = 1), geom = "point", fun.y = median, shape = 17, size = 3) +
    stat_smooth(aes(group = 1)) +
    theme_bw() 
}

lapply(plotCols0, ggbox)





#########################
# OLDER PART ... 2004-2012
#########################

##################################################
# Graphs - FOR 2004-2012
#################################################




# ?scatterplot
# 
# coplot(Y_T_År ~ År|kom, type="l", data=data_lg_x_DK) # Lines
# coplot(l.Pr ~ year|state, type="l", data=dat) # Points and lines
# coplot(l.Inc ~ year|state, type="l", data=dat) # Points and lines
# 
# ?coplot

plotCols0 <- c("Y_T",
               "Y_P")

ggbox <- function(x) {
  title <- paste("Scatterplot", x, "fordelt ud fra Området")
  ggplot(data_lg_x_DK, aes_string('factor(År)', x)) +
    geom_point(aes(colour = factor(Område))) +
    stat_summary(fun.y = "median", size = 5, geom = "point", aes(color=Område)) +
    scale_y_continuous(name = "Produktivitet per Beskæftigede") + 
    ggtitle(title) +
    scale_color_grey() + theme_classic() +
    theme_bw() 
}


lapply(plotCols0, ggbox)


# d <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
# d + stat_summary(fun.y = "mean", colour = "red", size = 2, geom = "point")

# geom = "point", aes(group=factor(Område))
# 
# stat_summary(fun.y=mean, geom="line", size=1.5,
#              linetype="dotted", aes(color=paste("mean", Species)))
# 
# ?scale_color_grey()
# 
# ?geom_point()
# 
# ?stat_summary
# 
# aes(group=factor(grouping))


# Particularite 2006 for ENTIRE DATA
data_lg_x_06 = subset(data_lg_x_DK, data_lg_x_DK$År==2006)


# Group median for dashed lines
library(plyr)
mu <- ddply(data_lg_x_06, "Område", summarise, grp.median=median(Y_T))
head(mu)

# Overlaid histograms
ggplot(data_lg_x_06, aes(x=Y_T, color=Område)) + 
  geom_histogram(aes(y = ..density.., fill = Område), 
                 binwidth=2, position = "dodge") + 
  geom_density(size=1) + 
  theme_bw() +
  geom_vline(data=mu, aes(xintercept=grp.median, color=Område),linetype="dashed", size=1,
             show.legend = F) +
  scale_x_continuous(name = "Produktivitet per Beskæftigede Timer")
  

# Particularite 2010 for ENTIRE DATA
data_lg_x_10 = subset(data_lg_x_DK, data_lg_x_DK$År==2010)


# Group median for dashed lines
library(plyr)
mu <- ddply(data_lg_x_10, "Område", summarise, grp.median=median(Y_T))
head(mu)

# Overlaid histograms
ggplot(data_lg_x_10, aes(x=Y_T, color=Område)) + 
  geom_histogram(aes(y = ..density.., fill = Område), 
                 binwidth=2, position = "dodge") + 
  geom_density(size=1) + 
  theme_bw() +
  geom_vline(data=mu, aes(xintercept=grp.median, color=Område),linetype="dashed", size=1,
             show.legend = F) +
  scale_x_continuous(name = "Produktivitet per Beskæftigede Timer")





# ?geom_histogram
# ?geom_density
# 
# geom_vline(data=mu, aes(xintercept=grp.median, color=Område,linetype="dashed"), 
#            show.legend = F) +
# theme(legend.position="none")
# scale_x_continuous(name = "Produktivitet per Beskæftigede Timer") +





# Particularite 2006 for Øvrige Kommuner 

data_lg_x_06_Øv = subset(data_lg_x_DK, data_lg_x_DK$Område=="Øvrige Kom." & data_lg_x_DK$År==2006)
head(data_lg_x_06_Øv)
str(data_lg_x_06_Øv)

summary(data_lg_x_06_Øv$Landsdel_navn)

data_lg_x_06_Øv = subset(data_lg_x_06_Øv, 
                         data_lg_x_06_Øv$Landsdel_navn=="Fyn" |
                         data_lg_x_06_Øv$Landsdel_navn=="Nordjylland"  |
                           data_lg_x_06_Øv$Landsdel_navn=="Østjylland"  |
                           data_lg_x_06_Øv$Landsdel_navn=="Østsjælland"  |
                           data_lg_x_06_Øv$Landsdel_navn=="Sydjylland" |
                           data_lg_x_06_Øv$Landsdel_navn=="Vest- og Sydsjælland"  |
                         data_lg_x_06_Øv$Landsdel_navn=="Vestjylland" |
                           data_lg_x_06_Øv$Landsdel_navn=="Ø-kommuner") 
                         

head(data_lg_x_06_Øv)
str(data_lg_x_06_Øv)

summary(data_lg_x_06_Øv$Landsdel_navn)

data_lg_x_06_Øv$Landsdel_navn = factor(data_lg_x_06_Øv$Landsdel_navn)
levels(data_lg_x_06_Øv$Landsdel_navn)


plotCols0 <- c("Y_T",
               "Y_P")


ggbox_06 <- function(x) {
  title <- paste("Boxplot af", x, "fordelt ud fra Landsdel_navn")
  ggplot(data_lg_x_06_Øv, aes_string('factor(Landsdel_navn)', x)) +

    geom_boxplot(aes(fill = factor(Landsdel_navn))) +
    scale_y_continuous(name = "Pct.") + 
    ggtitle(title) +
    theme_bw() 
}


lapply(plotCols0, ggbox_06)



# Particularite 2010 for Hovedstaden
data_lg_x_10_Hov = subset(data_lg_x_DK, data_lg_x_DK$Område=="Hovedstad" & data_lg_x_DK$År==2010)
head(data_lg_x_10_Hov)
str(data_lg_x_10_Hov)

summary(data_lg_x_10_Hov$Landsdel_navn)

data_lg_x_10_Hov = subset(data_lg_x_10_Hov, 
                         data_lg_x_10_Hov$Landsdel_navn=="Bornholm" |
                           data_lg_x_10_Hov$Landsdel_navn=="Byen København"  |
                           data_lg_x_10_Hov$Landsdel_navn=="Københavns omegn"  |
                           data_lg_x_10_Hov$Landsdel_navn=="Nordsjælland"  |
                           data_lg_x_10_Hov$Landsdel_navn=="Østsjælland") 


head(data_lg_x_10_Hov)
str(data_lg_x_10_Hov)

summary(data_lg_x_10_Hov$Landsdel_navn)

data_lg_x_10_Hov$Landsdel_navn = factor(data_lg_x_10_Hov$Landsdel_navn)
levels(data_lg_x_10_Hov$Landsdel_navn)


plotCols0 <- c("Y_T",
               "Y_P")


ggbox_10 <- function(x) {
  title <- paste("Boxplot af", x, "fordelt ud fra Landsdel_navn")
  ggplot(data_lg_x_10_Hov, aes_string('factor(Landsdel_navn)', x)) +
    
    geom_boxplot(aes(fill = factor(Landsdel_navn))) +
    scale_y_continuous(name = "Pct.") + 
    ggtitle(title) +
    theme_bw() 
}


lapply(plotCols0, ggbox_10)


plotCols0 <- c("Y_T",
               "Y_P")


head(data_lg_x)


# OK

plotgg <- function(x){
  title <- paste("Årlige", x, " fordelt efter Regionnes Navn samt Landsdelens Navn")
  ggplot(data_lg_x_DK, aes_string(x, "Region_navn")) +
    geom_point(aes(color = factor(Landsdel_navn))) +
    facet_grid(. ~ År) +
    scale_x_continuous(paste("Årlige", x, " i Pct.")) + 
    ggtitle(title) +
    theme_bw() 
}
lapply(plotCols0, plotgg)

?geom_vline

?geom_point

?geom_line


plotgg <- function(x){
  title <- paste("Årlige", x, " fordelt efter Regionnes Navn samt Landsdelets Navn")
  ggplot(data_lg_x_DK, aes_string('factor(År)', x)) +
    geom_jitter(aes(color = factor(Region_navn))) +
    ggtitle(title) 
  
}
lapply(plotCols0, plotgg)














# theme(legend.position="none")
#     scale_x_discrete(name = "År")

# Even better boxplot

ggbox_x <- function(x) {
  title <- paste("Boxplot af", x, "fordelt ud fra Området og Perioden")
  ggplot(data_lg_DK_Facetx, aes_string('factor(Områder)', x)) +
    facet_grid(. ~ tid, labeller = label_both, margins = F) + 
    geom_boxplot(aes(fill = factor(tid))) +
    scale_y_continuous(name = "Pct.") + 
    scale_x_discrete(name = "Området") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position="none")
}


lapply(plotCols, ggbox_x)









# NO SJÆLLAND

library(reshape2)

head(data_lg_Rest)

data_resh_Rest <- melt(data_lg_Rest[,-c(2,3)], id.vars=c("kom","Year"))
head(data_resh_Rest)

# data_resh_Rest$tid = factor(data_resh_Rest$Period, levels=c("Pre","Crisis","Post"), labels=c("Pre","Crisis","Post")) 
# 
# levels(data_resh_Rest$Period)

x11()
box_Rest <- ggplot(data_resh_Rest, aes(x=factor(Year), y =value, fill=factor(Year))) +
  geom_boxplot() + facet_wrap(~variable, scales = "free") 
box_Rest








# use THIS 

library("PerformanceAnalytics")

chart.Correlation(mydata, histogram=TRUE, pch=19)



# OR ... CLASSIC

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
pairs(data_NUM[,2:10], upper.panel = panel.cor,
      panel=function(x,y){
        points(x,y)
        abline(lm(y~x), col='red')
      })

x11()
pairs(data_NUM[,11:19], upper.panel = panel.cor,
      panel=function(x,y){
        points(x,y)
        abline(lm(y~x), col='red')
      })














