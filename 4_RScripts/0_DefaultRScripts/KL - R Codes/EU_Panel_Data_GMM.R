

#####################################
# Initial Panel Data and GMM Analysis
#####################################



####################################
# Data manipulations 
# DATA LONG pour PLOTMEANS
# DATA LONG pour GMM peut etre (MERGED)
# DATA LONG back to WIDE to construct PRE, CRISIS, POST and PRODUCTIVITY VARIABLES / RATES
####################################




library(gplots)
library(reshape2)


# GVA
GVA <- read.csv("2GVA_gr.csv", header = T)

head(GVA)
str(GVA)


write.csv(GVA,"2GVA_wide.csv")

GVA <- read.csv("2GVA_wide.csv", header = T)
names(GVA)


GVA_long = reshape(data = GVA,
                           idvar = "Region",
                            varying=list(names(GVA)[4:ncol(GVA)]),
                           sep = ".",
                           timevar = "Year",
                           times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                     2008,2009,2010,2011,2012, 2013, 2014, 2015),
                           direction = "long")
#
head(GVA_long)


write.csv(GVA_long,"2GVA_long.csv")


# GVA wide

GVA_long = read.csv("2GVA_long.csv", header = T)

GVA_wide = reshape(data = GVA_long,
                  idvar = "Region",
                  v.names = c("GVA"),
                  timevar = "Year",
                  direction = "wide")
head(GVA_wide)


write.csv(GVA_wide,"GVA_wide.csv")



# GDP
GDP <- read.csv("2GDP.csv", header = T)

head(GDP)
str(GDP)


write.csv(GDP,"2GDP_wide.csv")

GDP <- read.csv("2GDP_wide.csv", header = T)
names(GDP)
str(GDP)

summary(GDP)


# Split data per UNITS (ROWS)
GDP_split <- split(GDP, GDP$Units)
new_names <- c("EUR_HAB","EUR_HAB_EU","MIO_EUR","MIO_PPS","PPS_HAB","PPS_HAB_EU")    

for (i in 1:length(GDP_split)) {
  assign(new_names[i], GDP_split[[i]])
}


write.csv(EUR_HAB,"GDP_EUR_HAB.csv")
write.csv(EUR_HAB_EU,"GDP_EUR_HAB_EU.csv")
write.csv(MIO_EUR,"GDP_MIO_EUR.csv")
write.csv(MIO_PPS,"GDP_MIO_PPS.csv")
write.csv(PPS_HAB,"GDP_PPS_HAB.csv")
write.csv(PPS_HAB_EU,"GDP_PPS_HAB_EU.csv")


# GDP_EUR_HAB_EU

GDP_EUR_HAB <- read.csv("GDP_EUR_HAB.csv", header = T)

str(GDP_EUR_HAB)


GDP_EUR_HAB_long = reshape(data = GDP_EUR_HAB,
                   idvar = "Region",
                   varying=list(names(GDP_EUR_HAB)[4:ncol(GDP_EUR_HAB)]),
                   sep = ".",
                   timevar = "Year",
                   times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                             2008,2009,2010,2011,2012, 2013, 2014, 2015),
                   direction = "long")
head(GDP_EUR_HAB_long)


write.csv(GDP_EUR_HAB_long,"GDP_EUR_HAB_long.csv")




# GDP_EUR_HAB_EU

GDP_EUR_HAB_EU <- read.csv("GDP_EUR_HAB_EU.csv", header = T)

str(GDP_EUR_HAB_EU)


GDP_EUR_HAB_EU_long = reshape(data = GDP_EUR_HAB_EU,
                           idvar = "Region",
                           varying=list(names(GDP_EUR_HAB_EU)[4:ncol(GDP_EUR_HAB_EU)]),
                           sep = ".",
                           timevar = "Year",
                           times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                     2008,2009,2010,2011,2012, 2013, 2014, 2015),
                           direction = "long")
head(GDP_EUR_HAB_EU_long)



write.csv(GDP_EUR_HAB_EU_long,"GDP_EUR_HAB_EU_long.csv")




# GDP_MIO_EUR

GDP_MIO_EUR <- read.csv("GDP_MIO_EUR.csv", header = T)

str(GDP_MIO_EUR)


GDP_MIO_EUR_long = reshape(data = GDP_MIO_EUR,
                              idvar = "Region",
                              varying=list(names(GDP_MIO_EUR)[4:ncol(GDP_MIO_EUR)]),
                              sep = ".",
                              timevar = "Year",
                              times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                        2008,2009,2010,2011,2012, 2013, 2014, 2015),
                              direction = "long")
head(GDP_MIO_EUR_long)



write.csv(GDP_MIO_EUR_long,"GDP_MIO_EUR_long.csv")




# GDP_MIO_PPS

GDP_MIO_PPS <- read.csv("GDP_MIO_PPS.csv", header = T)

str(GDP_MIO_PPS)


GDP_MIO_PPS_long = reshape(data = GDP_MIO_PPS,
                           idvar = "Region",
                           varying=list(names(GDP_MIO_PPS)[4:ncol(GDP_MIO_PPS)]),
                           sep = ".",
                           timevar = "Year",
                           times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                     2008,2009,2010,2011,2012, 2013, 2014, 2015),
                           direction = "long")
head(GDP_MIO_PPS_long)



write.csv(GDP_MIO_PPS_long,"GDP_MIO_PPS_long.csv")






# GDP_PPS_HAB

GDP_PPS_HAB <- read.csv("GDP_PPS_HAB.csv", header = T)

str(GDP_PPS_HAB)


GDP_PPS_HAB_long = reshape(data = GDP_PPS_HAB,
                           idvar = "Region",
                           varying=list(names(GDP_PPS_HAB)[4:ncol(GDP_PPS_HAB)]),
                           sep = ".",
                           timevar = "Year",
                           times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                     2008,2009,2010,2011,2012, 2013, 2014, 2015),
                           direction = "long")
head(GDP_PPS_HAB_long)



write.csv(GDP_PPS_HAB_long,"GDP_PPS_HAB_long.csv")





# GDP_PPS_HAB_EU

GDP_PPS_HAB_EU <- read.csv("GDP_PPS_HAB_EU.csv", header = T)

str(GDP_PPS_HAB_EU)


GDP_PPS_HAB_EU_long = reshape(data = GDP_PPS_HAB_EU,
                           idvar = "Region",
                           varying=list(names(GDP_PPS_HAB_EU)[4:ncol(GDP_PPS_HAB_EU)]),
                           sep = ".",
                           timevar = "Year",
                           times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                     2008,2009,2010,2011,2012, 2013, 2014, 2015),
                           direction = "long")
head(GDP_PPS_HAB_EU_long)



write.csv(GDP_PPS_HAB_EU_long,"GDP_PPS_HAB_EU_long.csv")




# EMPL/SAL TOTAL only in BRANCHES

# GDP
LAB <- read.csv("2EMP_hr.csv", header = T)

head(LAB)
str(LAB)
summary(LAB)


write.csv(LAB,"2LAB_wide.csv")

LAB <- read.csv("2LAB_wide.csv", header = T)
names(LAB)
str(LAB)

summary(LAB)


# Split data per UNITS (ROWS)
LAB_split <- split(LAB, LAB$Type)
new_names <- c("LAB_EMP","LAB_SAL")    

for (i in 1:length(LAB_split)) {
  assign(new_names[i], LAB_split[[i]])
}


EMP = subset(LAB_EMP, LAB_EMP$Branche=="TOTAL")


str(EMP)


EMP_long = reshape(data = EMP,
                              idvar = "Region",
                              varying=list(names(EMP)[5:ncol(EMP)]),
                              sep = ".",
                              timevar = "Year",
                              times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                        2008,2009,2010,2011,2012, 2013, 2014, 2015),
                              direction = "long")
head(EMP_long)

write.csv(EMP_long,"EMP_long.csv")



# EMP wide



EMP_long = read.csv("EMP_long.csv", header = T)
head(EMP_long)


EMP_wide = reshape(data = EMP_long,
                   idvar = "Region",
                   v.names = c("Lab_EMP"),
                   timevar = "Year",
                   direction = "wide")
head(EMP_wide)


write.csv(EMP_wide,"EMP_wide.csv")




SAL = subset(LAB_SAL, LAB_SAL$Branche=="TOTAL")


str(SAL)


SAL_long = reshape(data = SAL,
                   idvar = "Region",
                   varying=list(names(SAL)[5:ncol(SAL)]),
                   sep = ".",
                   timevar = "Year",
                   times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                             2008,2009,2010,2011,2012, 2013, 2014, 2015),
                   direction = "long")
head(SAL_long)


write.csv(SAL_long,"SAL_long.csv")


#############
### MERGES ##
#############



merge1 = merge(GVA_long, EMP_long, by = c("Region","Year"), all.x = T)
head(merge1)


merge2 = merge(merge1, SAL_long, by = c("Region","Year"), all.x = T)
head(merge2)

str(merge2)

# RENAME A COLUMN
colnames(merge2)[5] <- "GVA"
colnames(merge2)[9] <- "EMP"
colnames(merge2)[13] <- "SAL"

write.csv(merge2,"GVA_LAB.csv")


library(reshape2)

GVA_LAB_melt <- read.csv("GVA_LAB.csv", header = T)
names(GVA_LAB_melt)

GVA_LAB_melt <- melt(GVA_LAB_melt, id.vars=c("Region","Year"), measure.vars = colnames(GVA_LAB_melt)[3:5])
tail(GVA_LAB_melt)
head(GVA_LAB_melt)
#



# RETRANSFORMIG WIDE TO CALCULATE PRODUCTIVITY GROWTH
# DATA LONG to WIDE BUT MERGED with RIGHT GVA regions and EMP/SAL regions!!!


# GVA_W <- read.csv("2GVA_long.csv", header = T)
# names(GVA_W)


PR_wide = reshape(data = GVA_LAB_melt,
                   idvar = "Region",
                   v.names = c("GVA", "EMP", "SAL"),
                   timevar = "Year",
                   direction = "wide")
head(PR_wide)


write.csv(PR_wide,"GVA_LAB_wide.csv")


# SPLITING 3 WIDE variables (SAME REGIONS ENSURED AFTER MERGE)

data <- read.csv("GVA_LAB_wide.csv", header = T)
names(data)

# GVA

data_GVA = data[,c(1,grep("GVA", colnames(data)))]
str(data_GVA)

write.csv(data_GVA,"GVA_prod.csv")

# EMP

data_EMP = data[,c(1,grep("EMP", colnames(data)))]
str(data_EMP)


write.csv(data_EMP,"EMP_prod.csv")

# SAL

data_SAL = data[,c(1,grep("SAL", colnames(data)))]
str(data_SAL)


write.csv(data_SAL,"SAL_prod.csv")





merge3 = merge(merge2, GDP_MIO_PPS_long, by = c("Region","Year"), all.x = T)
head(merge3)


merge4 = merge(merge3, GDP_PPS_HAB_long, by = c("Region","Year"), all.x = T)
head(merge4)

merge5 = merge(merge4, GDP_PPS_HAB_EU_long, by = c("Region","Year"), all.x = T)
head(merge5)



write.csv(merge5,"EU_Panel_Data_long.csv")


GDP_MIO_PPS_long = read.csv("GDP_MIO_PPS_long.csv", header = T)
GDP_PPS_HAB_long = read.csv("GDP_PPS_HAB_long.csv", header = T)
GDP_PPS_HAB_EU_long = read.csv("GDP_PPS_HAB_EU_long.csv", header = T)


head(GDP_PPS_HAB_long)

# # for GDP separate NOT NECESSARY!!!
# mergeA = merge(GVA_long, GDP_MIO_PPS_long, by = c("Region","Year"), all.x = T)
# head(mergeA)
# 
# 
# mergeB = merge(GVA_long, GDP_PPS_HAB_long, by = c("Region","Year"), all.x = T)
# head(mergeB)
# 
# mergeC = merge(GVA_long, GDP_PPS_HAB_EU_long, by = c("Region","Year"), all.x = T)
# head(mergeC)
# 
# 
# 
# library(reshape2)
# 
# mergeA=mergeA[,-4]
# head(mergeA)
# 
# mergeA <- melt(mergeA, id.vars=c("Region","Year"), measure.vars = colnames(mergeA)[3:4])
# tail(mergeA)
# head(mergeA)
# #
# 
# 
# library(reshape2)
# 
# mergeB=mergeB[,-4]
# head(mergeB)
# 
# mergeB <- melt(mergeB, id.vars=c("Region","Year"), measure.vars = colnames(mergeB)[3:4])
# tail(mergeB)
# head(mergeB)
# #
# 
# library(reshape2)
# 
# mergeC=mergeC[,-4]
# head(mergeC)
# 
# mergeC <- melt(mergeC, id.vars=c("Region","Year"), measure.vars = colnames(mergeC)[3:4])
# tail(mergeC)
# head(mergeC)
# #
# 
# 
# # RETRANSFORMIG WIDE TO CALCULATE PRODUCTIVITY GROWTH
# # DATA LONG to WIDE BUT MERGED with RIGHT GVA regions and EMP/SAL regions!!!
# 
# 
# # GVA_W <- read.csv("2GVA_long.csv", header = T)
# # names(GVA_W)
# 
# library(reshape)
# 
# tail(mergeA)
# head(mergeA)
# summary(mergeA)
# levels(mergeA$variable)
# 
# GDP1_wide = reshape(data = mergeA,
#                    idvar = "Region",
#                    v.names = c("GVA","GDP_MIO_PPS"),
#                    timevar = "Year",
#                    direction = "wide")
# 
# head(GDP1_wide)


#ATTENTION AVEC LONG Vs MELT

library(reshape2)

GDP_ONLY_melt <- read.csv("EU_Panel_Data_long_GDP_ONLY.csv", header = T)
names(GDP_ONLY_melt)

# GDP_ONLY_melt <- melt(GDP_ONLY_melt, id.vars=c("Region","Year"), measure.vars = colnames(GDP_ONLY_melt)[3:6])
# tail(GDP_ONLY_melt)
# head(GDP_ONLY_melt)
# #

# RETRANSFORMIG WIDE TO CALCULATE PRODUCTIVITY GROWTH
# DATA LONG to WIDE BUT MERGED with RIGHT GVA regions and EMP/SAL regions!!!


# GVA_W <- read.csv("2GVA_long.csv", header = T)
# names(GVA_W)


levels(GDP_ONLY_melt$variable)

GDP_wide = reshape(data = GDP_ONLY_melt,
                  idvar = "Region",
                  v.names = c("rGVA","GDPm_PPS","GDP_PPS_HABn","GDP_PPS_HAB_EU"),
                  timevar = "Year",
                  direction = "wide")
head(GDP_wide)


write.csv(GDP_wide,"GDP_ONLY_wide.csv")


# SPLITING 3 WIDE variables (SAME REGIONS ENSURED AFTER MERGE)

data <- read.csv("GDP_ONLY_wide.csv", header = T)
names(data)

# GVA

data_GVA = data[,c(1,grep("GVA", colnames(data)))]
str(data_GVA)

write.csv(data_GVA,"GVA_prod.csv")

# GDPm

data_GDPm_PPS = data[,c(1,grep("GDPm", colnames(data)))]
str(data_GDPm_PPS)


write.csv(data_GDPm_PPS,"GDPm_PPS_prod.csv")

# GDP_PPS_HABn

data_GDP_PPS_HABn = data[,c(1,grep("HABn", colnames(data)))]
str(data_GDP_PPS_HABn)


write.csv(data_GDP_PPS_HABn,"GDP_PPS_HABn_prod.csv")



# GDP_PPS_HAB_EU

data_GDP_PPS_HAB_EU = data[,c(1,grep("HAB_EU", colnames(data)))]
str(data_GDP_PPS_HAB_EU)


write.csv(data_GDP_PPS_HAB_EU,"GDP_PPS_HAB_EU_prod.csv")







#############
### PlotMeans + GMM
#############



library(gplots)


# A FEW separate plots for KJ

# GVA
pd <- read.csv("2GVA_long.csv", header = T)
head(pd)


x11()
plotmeans(GVA ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=pd)


# GDP
pd <- read.csv("GDP_PPS_HAB_EU_long.csv", header = T)
head(pd)


x11()
plotmeans(GDP_PPS_HAB_EU ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=pd)


# GDP - NOT STATIONARY
pd <- read.csv("GDP_PPS_HAB_long.csv", header = T)
head(pd)


x11()
plotmeans(GDP_PPS_HAB ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=pd)




# GDP
pd <- read.csv("GDP_EUR_HAB_EU_long.csv", header = T)
head(pd)


x11()
plotmeans(GDP_EUR_HAB_EU ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=pd)


# EMP
pd <- read.csv("EMP_long.csv", header = T)
head(pd)


x11()
plotmeans(Lab_EMP ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=pd)



# EMP (SAL)
pd <- read.csv("SAL_long.csv", header = T)
head(pd)


x11()
plotmeans(Lab_SAL ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=pd)



x11()
plotmeans(PC_ACT ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=PC_ACT_long)

x11()
plotmeans(PC_NE ~ Year, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=PC_NE_long)


PC_ACT_long


####################
# Panel DATA GMM GMM
####################


pd = read.csv("Panel DATA RAW.csv", header = T)
head(pd,20)
str(pd)

pd$Region = as.character(pd$Region)

# Need to get rid of COUNTRIES/NUTS 1
# IDENTIFICATION BY MERGING wide datasets - no duplicates and no duplicates_reg


nodup = read.csv("EU_NO_DUP.csv", header = T)
head(nodup)

nodup_reg = read.csv("EU_NO_DUP_reg.csv", header = T)
head(nodup_reg[,1])
# 
# merge = merge(nodup[,c(1,2)], nodup_reg[,c(1,2)], by = c("Region"), all.x = T)
# head(merge)


D = as.character(nodup[,1])  %in% as.character(nodup_reg[,1]) 

nuts1 = which(D==FALSE)

df_nuts1 = nodup[nuts1,]
str(df_nuts1)
summary(df_nuts1)

c=as.character(df_nuts1[,1])

pd1 = subset(pd, !(pd$Region %in% c))



# FINAL DATA SET for GMM
pdf = subset(pd1, pd1$Year!=2000)
head(pdf)


write.csv(pdf,"Panel_Data_NUTS2.csv")


##################################################
# FULL productivity Panel DATA with dummies
##################################################

library(dplyr)

pdf = read.csv("Panel_Data_NUTS2_dummies.csv", header = T)
head(pdf)
str(pdf)
summary(pdf)

# NOUVEAU - COMBINER PANEL DATA OBSERVATION ET NUTS2 SCREENING!!!
unique(pdf[,1])

NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

# Post
D = as.character(NUTS2[,1]) %in%  as.character(unique(pdf[,1])) 
head(D)

nuts2 = which(D==TRUE)
nuts2

NUTS2[nuts2,1]

subset1 = pdf[pdf$Region %in% as.character(NUTS2[nuts2,1]), ]
unique(subset1[,1])


write.csv(subset1,"Panel_Data_NUTS2_dummies_CORRECT.csv")

###########################################################################
### CORRECTED NUTS2 Observations (AND DK-productivity)!!!
###########################################################################

pdf = read.csv("Panel_Data_NUTS2_dummies_CORRECT.csv", header = T)
head(pdf)
str(pdf)
summary(pdf)

# NOUVEAU - COMBINER PANEL DATA OBSERVATION ET NUTS2 SCREENING!!!
unique(pdf[,1])



pdf$Y_EMP_DK = pdf$Y_EMP*pdf$DK
pdf$Y_EMP_DE = pdf$Y_EMP*pdf$DE
pdf$Y_EMP_ES = pdf$Y_EMP*pdf$ES
pdf$Y_EMP_IT = pdf$Y_EMP*pdf$IT


pdf$Y_SAL_DK = pdf$Y_SAL*pdf$DK
pdf$Y_SAL_DE = pdf$Y_SAL*pdf$DE
pdf$Y_SAL_ES = pdf$Y_SAL*pdf$ES
pdf$Y_SAL_IT = pdf$Y_SAL*pdf$IT


pdf$rGVA_DK = pdf$rGVA*pdf$DK
pdf$rGVA_DE = pdf$rGVA*pdf$DE
pdf$rGVA_ES = pdf$rGVA*pdf$ES
pdf$rGVA_IT = pdf$rGVA*pdf$IT


pdf$lPPS_HAB = log(pdf$GDP_PPS_HABn)
pdf$lPPS_HAB_EU = log(pdf$GDP_PPS_HAB_EU.1)


pdf$lPPS_HAB_DK = pdf$lPPS_HAB*pdf$DK
pdf$lPPS_HAB_DE = pdf$lPPS_HAB*pdf$DE
pdf$lPPS_HAB_ES = pdf$lPPS_HAB*pdf$ES
pdf$lPPS_HAB_IT = pdf$lPPS_HAB*pdf$IT


pdf$lPPS_HAB_EU_DK = pdf$lPPS_HAB_EU*pdf$DK
pdf$lPPS_HAB_EU_DE = pdf$lPPS_HAB_EU*pdf$DE
pdf$lPPS_HAB_EU_ES = pdf$lPPS_HAB_EU*pdf$ES
pdf$lPPS_HAB_EU_IT = pdf$lPPS_HAB_EU*pdf$IT




library(pglm)

# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(lPPS_HAB,1) +  
                    lag(lPPS_HAB_DK,1) + lag(lPPS_HAB_DE,1) + lag(lPPS_HAB_ES,1) + lag(lPPS_HAB_IT,1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:5) + lag(rGVA, 2:5) + lag(lPPS_HAB,2:5), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)

# modelgmmT0X

sargan(modelgmmT0)



#  lag(GDP_PPS_HAB,1) +

# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    lag(lPPS_HAB,1) +  
                    lag(lPPS_HAB_DK,1) + lag(lPPS_HAB_DE,1) + lag(lPPS_HAB_ES,1) + lag(lPPS_HAB_IT,1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:5) + lag(rGVA, 2:5) + lag(lPPS_HAB,2:5), data=pdf, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)



# NEW PLOTMEANS - ONLY Growth rates


library(gplots)


lay1=cbind(c(1,1,1,2,2,2),c(1,1,1,2,2,2),c(1,1,1,2,2,2)) # ,c(1,1,2,2,3,3)
lay1
layout(lay1) 


x11()
plotmeans(Y_EMP ~ Year, main="Vækstraten i Brutoværditilvækst Per Beskæftiget Time (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i Produktiviteten per Beskæftiget time (pct.)", bars=TRUE, barcol="blue", data=pdf)

x11()
plotmeans(rGVA ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdf)


x11()
plotmeans(hEMP ~ Year, main="Vækstraten i Beskæftigelsen (per Time, fordelt per EUs Regioner)", ylab="Tilvækst i Beskæftigelsen per Time (pct.)", bars=TRUE, barcol="blue",data=pdf)


# Plots for Word Noten
#pdf
str(pdf)

x11()
plotmeans(GDP_PPS_HABn ~ Year, main="BNP per Indbygger i købekraftsparitet-Euro (fordelt per EU-Regioner)", ylab="BNP per Indbygger i købekraftsparitet (Euro)", bars=TRUE, barcol="blue",data=pdf)

# pdfff
str(pdfff)

x11()
plotmeans(GFC_EUR ~ Year, main="Vækstraterne i Bruttoinvesteringer (fordelt per EU-Regioner)", ylab="Vækstraterne i Bruttoinvesteringer (pct.)", bars=TRUE, barcol="blue",data=pdfff)


# pdff
str(pdff)
x11()
plotmeans(PC_ACT ~ Year, main="Langtidsledighedskvoten i pct. af arbejdsstyrken", ylab="Langtidsledighedskvoten (pct. af arbejdsstyrken)", bars=TRUE, barcol="blue",data=pdff)

# pdff
str(pdfff)

x11()
plotmeans(PC_NE ~ Year, main="Langtidsledigheden per ledige i pct.", ylab="Langtidsledigheden per ledige (pct.)", bars=TRUE, barcol="blue",data=pdff)


library(tseries)


newdata <- complete.cases(pdf)  # removing NA (only Armenia) for Unit Root in y only!!!


Panel.set<-plm.data(newdata, index = c("Region", "Year"))

adf.test(Panel.set$lPPS_HAB, k=4)  # averaged DF test - trending & no mean stationary





require(gridExtra)

grid.arrange(prod, BVT, EMP, ncol=3)



#### Additional COVARIATES for GMM Productivity 
## Long Term Unemployment in 3 Units ( 2 pct. change per Active pop/ Unemployed and a real growth)
## Gross Capital Formation MAYBE


LTU <- read.csv("LTU.csv", header = T)
names(LTU)
str(LTU)


####################################
## Subsetting LTU with NUTS2 ONLY!!!
####################################


# nodup = read.csv("EU_NO_DUP.csv", header = T)
# head(nodup)

nodup_reg = read.csv("EU_NO_DUP_reg.csv", header = T)
head(nodup_reg[,1])


# merge = merge(nodup[,c(1,2)], nodup_reg[,c(1,2)], by = c("Region"), all.x = T)
# head(merge)


D = as.character(LTU[,3])  %in% as.character(nodup_reg[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

df_nuts2 = LTU[nuts2,3]
str(df_nuts2)
summary(df_nuts1)

c=as.character(df_nuts2)

# pd1 = subset(pd, !(pd$Region %in% c))


LTU_nuts2 = subset(LTU, LTU$Region %in% c)
str(LTU_nuts2)
summary(LTU_nuts2)



# Split data per LTU_UNITS (ROWS)
LTU_nuts2_split <- split(LTU_nuts2, LTU_nuts2$LTU_Unit)
new_names <- c("PC_ACT","PC_NE","THS")    

for (i in 1:length(LTU_nuts2_split)) {
  assign(new_names[i], LTU_nuts2_split[[i]])
}


write.csv(PC_ACT,"LTU_nuts2_PC_ACT.csv")
write.csv(PC_NE,"LTU_nuts2_PC_NE.csv")
write.csv(THS,"LTU_nuts2_THS.csv")



PC_ACT<- read.csv("LTU_PC_ACT.csv", header = T)
str(PC_ACT)
names(PC_ACT)


PC_ACT_long = reshape(data = PC_ACT,
                   idvar = "Region",
                   varying=list(names(PC_ACT)[4:ncol(PC_ACT)]),
                   sep = ".",
                   timevar = "Year",
                   times = c(2001, 2002, 2003, 2004,2005,2006,2007,      # NO 2000
                             2008,2009,2010,2011,2012, 2013, 2014, 2015),
                   direction = "long")
#
head(PC_ACT_long)


write.csv(PC_ACT_long,"PC_ACT_long.csv")



PC_NE<- read.csv("LTU_PC_NE.csv", header = T)
str(PC_NE)
names(PC_NE)


PC_NE_long = reshape(data = PC_NE,
                      idvar = "Region",
                      varying=list(names(PC_NE)[4:ncol(PC_NE)]),
                      sep = ".",
                      timevar = "Year",
                      times = c(2001, 2002, 2003, 2004,2005,2006,2007,     # NO 2000
                                2008,2009,2010,2011,2012, 2013, 2014, 2015),
                      direction = "long")
#
head(PC_NE_long)


write.csv(PC_NE_long,"PC_NE_long.csv")



THS<- read.csv("LTU_THS.csv", header = T)
str(THS)
names(THS)


THS_long = reshape(data = THS,
                     idvar = "Region",
                     varying=list(names(THS)[4:ncol(THS)]),
                     sep = ".",
                     timevar = "Year",
                     times = c(2001, 2002, 2003, 2004,2005,2006,2007,
                               2008,2009,2010,2011,2012, 2013, 2014, 2015),
                     direction = "long")
#
head(THS_long)


write.csv(THS_long,"THS_long.csv")



# Merging LTU data


PC_ACT_long<- read.csv("PC_ACT_long.csv", header = T)
PC_NE_long<- read.csv("PC_NE_long.csv", header = T)
THS_long<- read.csv("THS_long.csv", header = T)




merge1 = merge(PC_ACT_long, PC_NE_long, by = c("Region","Year"), all = T)
head(merge1)


merge2 = merge(merge1, THS_long, by = c("Region","Year"), all = T)
head(merge2)

str(merge2)


write.csv(merge2,"LTU_3Units_long.csv")


### Merging with Productivity Panel Data!!!

pdf = read.csv("Panel_Data_NUTS2_dummies.csv", header = T)
head(pdf)
str(pdf)
summary(pdf)


pdLTU = read.csv("LTU_3Units_long.csv", header = T)
head(pdLTU)
str(pdLTU)
summary(pdLTU)



mergeA = merge(pdf, pdLTU, by = c("Region","Year"), all.x = T)
head(mergeA)



write.csv(mergeA,"Panel_Data_Prod_LTU_sh.csv")



pdff = read.csv("Panel_Data_Prod_LTU_sh.csv", header = T)
head(pdff)
str(pdff)
summary(pdff)


##################################################################
# NOUVEAU - COMBINER PANEL DATA OBSERVATION ET NUTS2 SCREENING!!!

unique(pdff[,1])

NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

# Post
D = as.character(NUTS2[,1]) %in%  as.character(unique(pdff[,1])) 
head(D)

nuts2 = which(D==TRUE)
nuts2

NUTS2[nuts2,1]

subset2 = pdff[pdff$Region %in% as.character(NUTS2[nuts2,1]), ]
unique(subset2[,1])


write.csv(subset2,"Panel_Data_Prod_LTU_CORRECT.csv")



###########################################################################
### CORRECTED NUTS2 Observations (AND DK-productivity)!!!
###########################################################################

pdff = read.csv("Panel_Data_Prod_LTU_CORRECT.csv", header = T)
head(pdff)
str(pdff)
summary(pdff)




pdff$Y_EMP_DK = pdff$Y_EMP*pdff$DK
pdff$Y_EMP_DE = pdff$Y_EMP*pdff$DE
pdff$Y_EMP_ES = pdff$Y_EMP*pdff$ES
pdff$Y_EMP_IT = pdff$Y_EMP*pdff$IT


pdff$Y_SAL_DK = pdff$Y_SAL*pdff$DK
pdff$Y_SAL_DE = pdff$Y_SAL*pdff$DE
pdff$Y_SAL_ES = pdff$Y_SAL*pdff$ES
pdff$Y_SAL_IT = pdff$Y_SAL*pdff$IT


pdff$rGVA_DK = pdff$rGVA*pdff$DK
pdff$rGVA_DE = pdff$rGVA*pdff$DE
pdff$rGVA_ES = pdff$rGVA*pdff$ES
pdff$rGVA_IT = pdff$rGVA*pdff$IT


pdff$THS_DK = pdff$THS*pdff$DK
pdff$THS_DE = pdff$THS*pdff$DE
pdff$THS_ES = pdff$THS*pdff$ES
pdff$THS_IT = pdff$THS*pdff$IT


pdff$PC_NE_DK = pdff$PC_NE*pdff$DK
pdff$PC_NE_DE = pdff$PC_NE*pdff$DE
pdff$PC_NE_ES = pdff$PC_NE*pdff$ES
pdff$PC_NE_IT = pdff$PC_NE*pdff$IT


pdff$PC_ACT_DK = pdff$PC_ACT*pdff$DK
pdff$PC_ACT_DE = pdff$PC_ACT*pdff$DE
pdff$PC_ACT_ES = pdff$PC_ACT*pdff$ES
pdff$PC_ACT_IT = pdff$PC_ACT*pdff$IT



pdff$lPPS_HAB = log(pdff$GDP_PPS_HABn)
pdff$lPPS_HAB_EU = log(pdff$GDP_PPS_HAB_EU.1)


pdff$lPPS_HAB_DK = pdff$lPPS_HAB*pdff$DK
pdff$lPPS_HAB_DE = pdff$lPPS_HAB*pdff$DE
pdff$lPPS_HAB_ES = pdff$lPPS_HAB*pdff$ES
pdff$lPPS_HAB_IT = pdff$lPPS_HAB*pdff$IT


pdff$lPPS_HAB_EU_DK = pdff$lPPS_HAB_EU*pdff$DK
pdff$lPPS_HAB_EU_DE = pdff$lPPS_HAB_EU*pdff$DE
pdff$lPPS_HAB_EU_ES = pdff$lPPS_HAB_EU*pdff$ES
pdff$lPPS_HAB_EU_IT = pdff$lPPS_HAB_EU*pdff$IT







x11()
plotmeans(THS ~ Year, main="Vækstraten i Brutoværditilvækst Per Beskæftiget Time (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i Produktiviteten per Beskæftiget time (pct.)", bars=TRUE, barcol="blue", data=pdff)

x11()
plotmeans(PC_ACT ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdff)


x11()
plotmeans(PC_NE ~ Year, main="Vækstraten i Beskæftigelsen (per Time, fordelt per EUs Regioner)", ylab="Tilvækst i Beskæftigelsen per Time (pct.)", bars=TRUE, barcol="blue",data=pdff)




library(pglm)



# LTU ACT -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:2) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)



# LTU ACT -  LAG1 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_ACT,1) +
                    lag(PC_ACT_DK, 1) + lag(PC_ACT_DE, 1) + lag(PC_ACT_ES, 1) + lag(PC_ACT_IT, 1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,2:3) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)


# LTU ACT -  LAG2 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_ACT,2) +
                    lag(PC_ACT_DK, 2) + lag(PC_ACT_DE, 2) + lag(PC_ACT_ES, 2) + lag(PC_ACT_IT, 2) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,3:4) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)

# modelgmmT0X

sargan(modelgmmT0)


# LTU ACT -  LAG3 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_ACT,3) +
                    lag(PC_ACT_DK, 3) + lag(PC_ACT_DE, 3) + lag(PC_ACT_ES, 3) + lag(PC_ACT_IT, 3) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,4:5) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)




# LTU NE -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,1:2) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)




# LTU NE -  LAG1 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_NE,1) +
                    lag(PC_NE_DK, 1) + lag(PC_NE_DE, 1) + lag(PC_NE_ES, 1) + lag(PC_NE_IT, 1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,2:3) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)


# LTU NE -  LAG2 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_NE,2) +
                    lag(PC_NE_DK, 2) + lag(PC_NE_DE, 2) + lag(PC_NE_ES, 2) + lag(PC_NE_IT, 2) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,3:4) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)

# modelgmmT0X

sargan(modelgmmT0)


# LTU NE -  LAG3 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_NE,3) +
                    lag(PC_NE_DK, 3) + lag(PC_NE_DE, 3) + lag(PC_NE_ES, 3) + lag(PC_NE_IT, 3) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,4:5) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)





# LTU THS -  LAG1 is significant!
# # FULL
# modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + lag(GDP_PPS_HAB,1) + lag(THS,1) +
#                     
#                     lag(THS_DK, 1) + lag(THS_DE, 1) + lag(THS_ES, 1) + lag(THS_IT, 1) +
#                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(THS,1) + lag(THS,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmEMP, robust = TRUE)
# 
# 
# # LTU THS -  LAG2 is significant! 
# # FULL
# modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + lag(GDP_PPS_HAB,1) + lag(THS,2) +
#                     
#                     lag(THS_DK, 2) + lag(THS_DE, 2) + lag(THS_ES, 2) + lag(THS_IT, 2) +
#                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(THS,1) + lag(THS,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmEMP, robust = TRUE)
# 
# modelgmmT0X



# LTU ACT -  LAG0 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:2) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)




# LTU ACT -  LAG1 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    lag(PC_ACT,1) +
                    lag(PC_ACT_DK, 1) + lag(PC_ACT_DE, 1) + lag(PC_ACT_ES, 1) + lag(PC_ACT_IT, 1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,2:3) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)


# LTU ACT -  LAG2 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    lag(PC_ACT,2) +
                    lag(PC_ACT_DK, 2) + lag(PC_ACT_DE, 2) + lag(PC_ACT_ES, 2) + lag(PC_ACT_IT, 2) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,3:4) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)

# modelgmmT0X

sargan(modelgmmT0)


# LTU ACT -  LAG3 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    lag(PC_ACT,3) +
                    lag(PC_ACT_DK, 3) + lag(PC_ACT_DE, 3) + lag(PC_ACT_ES, 3) + lag(PC_ACT_IT, 3) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,4:5) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)


# LTU NE -  LAG0 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    PC_NE, +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,1:2) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)



# LTU NE -  LAG1 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    lag(PC_NE,1) +
                    lag(PC_NE_DK, 1) + lag(PC_NE_DE, 1) + lag(PC_NE_ES, 1) + lag(PC_NE_IT, 1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,2:3) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)


# LTU NE -  LAG2 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    lag(PC_NE,2) +
                    lag(PC_NE_DK, 2) + lag(PC_NE_DE, 2) + lag(PC_NE_ES, 2) + lag(PC_NE_IT, 2) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,3:4) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)

# modelgmmT0X

sargan(modelgmmT0)


# LTU NE -  LAG3 is significant! 
# FULL
modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + 
                    lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
                    lag(PC_NE,3) +
                    lag(PC_NE_DK, 3) + lag(PC_NE_DE, 3) + lag(PC_NE_ES, 3) + lag(PC_NE_IT, 3) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_SAL, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,4:5) + lag(lPPS_HAB,1:2), data=pdff, transformation = "ld")

summary(modelgmmSAL, robust = TRUE)




# # FULL
# modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + lag(GDP_PPS_HAB,1) + lag(PC_NE,1) + lag(PC_NE,2) + 
#                     lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_SAL, 3:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_NE,1) + lag(PC_NE,2), data=pdff, transformation = "ld")
# 
# summary(modelgmmSAL, robust = TRUE)
# 





###########################################
# ADDING Gross Fixed Capital Formation
###########################################


#### Additional COVARIATES for GMM Productivity 
## Long Term Unemployment in 3 Units ( 2 pct. change per Active pop/ Unemployed and a real growth)
## Gross Capital Formation MAYBE


GFC_EUR <- read.csv("GFC_EUR.csv", header = T)
names(GFC_EUR)
str(GFC_EUR)


GFC_LCU <- read.csv("GFC_LCU.csv", header = T)
names(GFC_LCU)
str(GFC_LCU)


####################################
## Subsetting GFC_EUR with NUTS2 ONLY!!!
####################################


# nodup = read.csv("EU_NO_DUP.csv", header = T)
# head(nodup)

nodup_reg = read.csv("EU_NO_DUP_reg.csv", header = T)
head(nodup_reg[,1])


# merge = merge(nodup[,c(1,2)], nodup_reg[,c(1,2)], by = c("Region"), all.x = T)
# head(merge)


D = as.character(GFC_EUR[,3])  %in% as.character(nodup_reg[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

df_nuts2 = GFC_EUR[nuts2,3]
str(df_nuts2)
summary(df_nuts2)

c=as.character(df_nuts2)

# pd1 = subset(pd, !(pd$Region %in% c))


GFC_EUR_nuts2 = subset(GFC_EUR, GFC_EUR$Region %in% c)
str(GFC_EUR_nuts2)
head(GFC_EUR_nuts2)


duplicated(GFC_EUR_nuts2)
GFC_EUR_nuts2[duplicated(GFC_EUR_nuts2), ]



# # Split data per GFC_EUR_UNITS (ROWS)
# GFC_EUR_nuts2_split <- split(GFC_EUR_nuts2, GFC_EUR_nuts2$GFC_EUR_Unit)
# new_names <- c("PC_ACT","PC_NE","THS")    
# 
# for (i in 1:length(GFC_EUR_nuts2_split)) {
#   assign(new_names[i], GFC_EUR_nuts2_split[[i]])
# }


write.csv(GFC_EUR_nuts2,"GFC_EUR_nuts2.csv")




GFC_EUR<- read.csv("GFC_EUR_nuts2.csv", header = T)
str(GFC_EUR)
names(GFC_EUR)


GFC_EUR_long = reshape(data = GFC_EUR,
                      idvar = "Region",
                      varying=list(names(GFC_EUR)[3:ncol(GFC_EUR)]),
                      sep = ".",
                      timevar = "Year",
                      times = c(2001, 2002, 2003, 2004,2005,2006,2007,      # NO 2000
                                2008,2009,2010,2011,2012, 2013),
                      direction = "long")
#
head(GFC_EUR_long)


write.csv(GFC_EUR_long,"GFC_EUR_long.csv")





#################
## A faire GFC_LCU ... (COPIER et COLLER)
##################


####################################
## Subsetting GFC_EUR with NUTS2 ONLY!!!
####################################


# nodup = read.csv("EU_NO_DUP.csv", header = T)
# head(nodup)

nodup_reg = read.csv("EU_NO_DUP_reg.csv", header = T)
head(nodup_reg[,1])


# merge = merge(nodup[,c(1,2)], nodup_reg[,c(1,2)], by = c("Region"), all.x = T)
# head(merge)


D = as.character(GFC_LCU[,3])  %in% as.character(nodup_reg[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

df_nuts2 = GFC_LCU[nuts2,3]
str(df_nuts2)
summary(df_nuts2)

c=as.character(df_nuts2)

# pd1 = subset(pd, !(pd$Region %in% c))


GFC_LCU_nuts2 = subset(GFC_LCU, GFC_LCU$Region %in% c)
str(GFC_LCU_nuts2)
head(GFC_LCU_nuts2)

duplicated(GFC_LCU_nuts2)
GFC_LCU_nuts2[duplicated(GFC_LCU_nuts2), ]

# # Split data per GFC_LCU_UNITS (ROWS)
# GFC_LCU_nuts2_split <- split(GFC_LCU_nuts2, GFC_LCU_nuts2$GFC_LCU_Unit)
# new_names <- c("PC_ACT","PC_NE","THS")    
# 
# for (i in 1:length(GFC_LCU_nuts2_split)) {
#   assign(new_names[i], GFC_LCU_nuts2_split[[i]])
# }


write.csv(GFC_LCU_nuts2,"GFC_LCU_nuts2.csv")




GFC_LCU<- read.csv("GFC_LCU_nuts2.csv", header = T)
str(GFC_LCU)
names(GFC_LCU)


GFC_LCU_long = reshape(data = GFC_LCU,
                       idvar = "Region",
                       varying=list(names(GFC_LCU)[3:ncol(GFC_LCU)]),
                       sep = ".",
                       timevar = "Year",
                       times = c(2001, 2002, 2003, 2004,2005,2006,2007,      # NO 2000
                                 2008,2009,2010,2011,2012, 2013),
                       direction = "long")
#
head(GFC_LCU_long)


write.csv(GFC_LCU_long,"GFC_LCU_long.csv")





# Merging LTU data


GFC_EUR_long<- read.csv("GFC_EUR_long.csv", header = T)
GFC_LCU_long<- read.csv("GFC_LCU_long.csv", header = T)

head(GFC_EUR_long)

merge1 = merge(GFC_EUR_long, GFC_LCU_long, by = c("Region","Year"), all = T)
head(merge1)


write.csv(merge1,"GFC_long.csv")


### Merging with Productivity Panel Data - NO LONG TERM UNEMPLOYMENT YET!!!

pdf = read.csv("Panel_Data_NUTS2_dummies.csv", header = T)
head(pdf)
str(pdf)
summary(pdf)


pdGFC = read.csv("GFC_long.csv", header = T)
head(pdGFC)
str(pdGFC)
summary(pdGFC)



mergeA = merge(pdf, pdGFC, by = c("Region","Year"), all.x = T)
head(mergeA)



write.csv(mergeA,"Panel_Data_Prod_GFC_sh.csv")


# GFC = INV
# ####################################################################
# pdfff = read.csv("Panel_Data_Prod_GFC_sh.csv", header = T)
# head(pdfff)
# str(pdfff)
# summary(pdfff)
# colnames(pdfff)



##################################################################
# NOUVEAU - COMBINER PANEL DATA OBSERVATION ET NUTS2 SCREENING!!!

unique(pdfff[,1])

NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

# Post
D = as.character(NUTS2[,1]) %in%  as.character(unique(pdfff[,1])) 
head(D)

nuts2 = which(D==TRUE)
nuts2

NUTS2[nuts2,1]

subset3 = pdfff[pdfff$Region %in% as.character(NUTS2[nuts2,1]), ]
unique(subset3[,1])


write.csv(subset3,"Panel_Data_Prod_GFC_CORRECT.csv")



###########################################################################
### CORRECTED NUTS2 Observations (AND DK-productivity)!!!
###########################################################################

pdfff = read.csv("Panel_Data_Prod_GFC_CORRECT.csv", header = T)
head(pdfff)
str(pdfff)
summary(pdfff)




pdfff$Y_EMP_DK = pdfff$Y_EMP*pdfff$DK
pdfff$Y_EMP_DE = pdfff$Y_EMP*pdfff$DE
pdfff$Y_EMP_ES = pdfff$Y_EMP*pdfff$ES
pdfff$Y_EMP_IT = pdfff$Y_EMP*pdfff$IT


pdfff$Y_SAL_DK = pdfff$Y_SAL*pdfff$DK
pdfff$Y_SAL_DE = pdfff$Y_SAL*pdfff$DE
pdfff$Y_SAL_ES = pdfff$Y_SAL*pdfff$ES
pdfff$Y_SAL_IT = pdfff$Y_SAL*pdfff$IT


pdfff$rGVA_DK = pdfff$rGVA*pdfff$DK
pdfff$rGVA_DE = pdfff$rGVA*pdfff$DE
pdfff$rGVA_ES = pdfff$rGVA*pdfff$ES
pdfff$rGVA_IT = pdfff$rGVA*pdfff$IT


pdfff$GFC_EUR_DK = pdfff$GFC_EUR*pdfff$DK
pdfff$GFC_EUR_DE = pdfff$GFC_EUR*pdfff$DE
pdfff$GFC_EUR_ES = pdfff$GFC_EUR*pdfff$ES
pdfff$GFC_EUR_IT = pdfff$GFC_EUR*pdfff$IT


pdfff$GFC_LCU_DK = pdfff$GFC_LCU*pdfff$DK
pdfff$GFC_LCU_DE = pdfff$GFC_LCU*pdfff$DE
pdfff$GFC_LCU_ES = pdfff$GFC_LCU*pdfff$ES
pdfff$GFC_LCU_IT = pdfff$GFC_LCU*pdfff$IT



pdfff$GDP_PPS_HAB_DK = pdfff$GDP_PPS_HAB*pdfff$DK
pdfff$GDP_PPS_HAB_DE = pdfff$GDP_PPS_HAB*pdfff$DE
pdfff$GDP_PPS_HAB_ES = pdfff$GDP_PPS_HAB*pdfff$ES
pdfff$GDP_PPS_HAB_IT = pdfff$GDP_PPS_HAB*pdfff$IT




pdfff$lPPS_HAB = log(pdfff$GDP_PPS_HABn)
pdfff$lPPS_HAB_EU = log(pdfff$GDP_PPS_HAB_EU.1)


pdfff$lPPS_HAB_DK = pdfff$lPPS_HAB*pdfff$DK
pdfff$lPPS_HAB_DE = pdfff$lPPS_HAB*pdfff$DE
pdfff$lPPS_HAB_ES = pdfff$lPPS_HAB*pdfff$ES
pdfff$lPPS_HAB_IT = pdfff$lPPS_HAB*pdfff$IT


pdfff$lPPS_HAB_EU_DK = pdfff$lPPS_HAB_EU*pdfff$DK
pdfff$lPPS_HAB_EU_DE = pdfff$lPPS_HAB_EU*pdfff$DE
pdfff$lPPS_HAB_EU_ES = pdfff$lPPS_HAB_EU*pdfff$ES
pdfff$lPPS_HAB_EU_IT = pdfff$lPPS_HAB_EU*pdfff$IT






x11()
plotmeans(GFC_EUR ~ Year, main="Vækstraten i Brutoværditilvækst Per Beskæftiget Time (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i Produktiviteten per Beskæftiget time (pct.)", bars=TRUE, barcol="blue", data=pdfff)

x11()
plotmeans(GFC_LCU ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdfff)


colnames(pdfff)
head(pdfff, 20)

pdfff_LCU = pdfff[,-c(16,30:33, 36)]
# pdfff_EUR = subset(pdfff_EUR, pdfff_EUR$Region)
pdfff_LCU = subset(pdfff_LCU, !(pdfff_LCU$Year==2009 | pdfff_LCU$Year==2010  |
                                pdfff_LCU$Year==2011 | pdfff_LCU$Year==2012  |
                                pdfff_LCU$Year==2013 | pdfff_LCU$Year==2014  |
                                pdfff_LCU$Year==2015 ))


                   
# pdfff_EUR = pdfff[,-c(17,32,34:37)]


pdfff_EUR = subset(pdfff, !(pdfff$Year==2009 | pdfff$Year==2010  |
                                  pdfff$Year==2011 | pdfff$Year==2012  |
                                  pdfff$Year==2013 | pdfff$Year==2014  |
                                  pdfff$Year==2015 ))

summary(pdfff_EUR)

pdfff_EURp = subset(pdfff, !(pdfff$Year==2001 | pdfff$Year==2002  |
                                  pdfff$Year==2003 | pdfff$Year==2004  |
                                  pdfff$Year==2005 | pdfff$Year==2006  |
                                  pdfff$Year==2007 | pdfff$Year==2008 | pdfff$Year==2009 ))

summary(pdfff_EURp)

library(pglm)


# GMM Ne mArche PAS! - Peut etre static!!!

# # LTU ACT -  LAG1 is significant! 
# # FULL
# modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
#                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
#                     lag(lPPS_HAB,1) +
#                     lag(lPPS_HAB_DK,1) + lag(lPPS_HAB_DE,1) + lag(lPPS_HAB_ES,1) + lag(lPPS_HAB_IT,1) +
#                     lag(GFC_EUR,2) +
#                     lag(GFC_EUR_DK, 2) + lag(GFC_EUR_DE, 2) +  lag(GFC_EUR_IT, 2) +
# 
#                     lag(rGVA, 1) +
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
#                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) , data=pdfff, transformation = "ld")
# 
# summary(modelgmmEMP, robust = TRUE)
# # 
# 
# # LTU ACT -  LAG2 is significant! 
# # FULL
# modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + lag(GDP_PPS_HAB,1) + lag(GFC_EUR,2) +
#                     
#                     lag(GFC_EUR_DK, 2) + lag(GFC_EUR_DE, 2) + lag(GFC_EUR_ES, 2) + lag(GFC_EUR_IT, 2) +
#                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(GFC_EUR,1) + lag(GFC_EUR,2) , data=pdfff, transformation = "ld")
# 
# summary(modelgmmEMP, robust = TRUE)
# 
# # modelgmmT0X
# 
# sargan(modelgmmT0)
# 
# 
# 
# 
# 
# # LTU NE -  LAG1 is significant! 
# # FULL
# modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1)  + lag(PC_NE,1) +
#                     
#                     
#                     lag(PC_NE_DK, 1) + lag(PC_NE_DE, 1) + lag(PC_NE_ES, 1) + lag(PC_NE_IT, 1) +
#                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_NE,1) + lag(PC_NE,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmEMP, robust = TRUE)
# 
# 
# # LTU NE -  LAG2 is significant! 
# # FULL
# modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + lag(GDP_PPS_HAB,1) + lag(PC_NE,2) +
#                     
#                     lag(PC_NE_DK, 2) + lag(PC_NE_DE, 2) + lag(PC_NE_ES, 2) + lag(PC_NE_IT, 2) +
#                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_NE,1) + lag(PC_NE,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmEMP, robust = TRUE)
# 
# # modelgmmT0X
# 
# 
# 
# 
# # LTU THS -  LAG1 is significant!
# # # FULL
# # modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + lag(GDP_PPS_HAB,1) + lag(THS,1) +
# #                     
# #                     lag(THS_DK, 1) + lag(THS_DE, 1) + lag(THS_ES, 1) + lag(THS_IT, 1) +
# #                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
# #                     lag(rGVA, 1) + 
# #                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
# #                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(THS,1) + lag(THS,2) , data=pdff, transformation = "ld")
# # 
# # summary(modelgmmEMP, robust = TRUE)
# # 
# # 
# # # LTU THS -  LAG2 is significant! 
# # # FULL
# # modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + lag(GDP_PPS_HAB,1) + lag(THS,2) +
# #                     
# #                     lag(THS_DK, 2) + lag(THS_DE, 2) + lag(THS_ES, 2) + lag(THS_IT, 2) +
# #                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
# #                     lag(rGVA, 1) + 
# #                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
# #                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(THS,1) + lag(THS,2) , data=pdff, transformation = "ld")
# # 
# # summary(modelgmmEMP, robust = TRUE)
# # 
# # modelgmmT0X
# 
# 
# 
# 
# # LTU ACT -  LAG1 is significant! 
# # FULL
# modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + lag(GDP_PPS_HAB,1) + lag(PC_ACT,1) +
#                     
#                     lag(PC_ACT_DK, 1) + lag(PC_ACT_DE, 1) + lag(PC_ACT_ES, 1) + lag(PC_ACT_IT, 1) +
#                     lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_SAL, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_ACT,1) + lag(PC_ACT,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmSAL, robust = TRUE)
# 
# 
# # LTU ACT -  LAG2 is significant! 
# # FULL
# modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + lag(GDP_PPS_HAB,1) + lag(PC_ACT,2) +
#                     
#                     lag(PC_ACT_DK, 2) + lag(PC_ACT_DE, 2) + lag(PC_ACT_ES, 2) + lag(PC_ACT_IT, 2) +
#                     lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_SAL, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_ACT,1) + lag(PC_ACT,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmSAL, robust = TRUE)
# 
# # modelgmmT0X
# 
# sargan(modelgmmT0)
# 
# 
# 
# 
# 
# # LTU NE -  LAG1 is significant! 
# # FULL
# modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + lag(GDP_PPS_HAB,1) + lag(PC_NE,1) +
#                     
#                     lag(PC_NE_DK, 1) + lag(PC_NE_DE, 1) + lag(PC_NE_ES, 1) + lag(PC_NE_IT, 1) +
#                     lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_SAL, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_NE,1) + lag(PC_NE,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmSAL, robust = TRUE)
# 
# 
# # LTU NE -  LAG2 is significant! 
# # FULL
# modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + lag(GDP_PPS_HAB,1) + lag(PC_NE,2) +
#                     
#                     lag(PC_NE_DK, 2) + lag(PC_NE_DE, 2) + lag(PC_NE_ES, 2) + lag(PC_NE_IT, 2) +
#                     lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_SAL, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_NE,1) + lag(PC_NE,2) , data=pdff, transformation = "ld")
# 
# summary(modelgmmSAL, robust = TRUE)
# 
# # modelgmmT0X
# 
# 
# 
# 
# # # FULL
# # modelgmmSAL<-pgmm(Y_SAL ~ lag(Y_SAL, 1) + lag(GDP_PPS_HAB,1) + lag(PC_NE,1) + lag(PC_NE,2) + 
# #                     lag(Y_SAL_DK, 1) + lag(Y_SAL_DE, 1) + lag(Y_SAL_ES, 1) + lag(Y_SAL_IT, 1) +
# #                     lag(rGVA, 1) + 
# #                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
# #                   | lag(Y_SAL, 3:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) + lag(PC_NE,1) + lag(PC_NE,2), data=pdff, transformation = "ld")
# # 
# # summary(modelgmmSAL, robust = TRUE)
# # 
# 
# 
# 
# modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + lag(GDP_PPS_HAB,1) + lag(GFC_EUR,1) + 
#                     
#                     #                    lag(GFC_EUR_DK, 1) + lag(GFC_EUR_DE, 1) + lag(GFC_EUR_IT, 1) + # lag(GFC_EUR_ES, 1) + 
#                     lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
#                     lag(rGVA, 1) + 
#                     lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
#                   | lag(Y_EMP, 2:10) + lag(rGVA, 2:10) + lag(GDP_PPS_HAB,1:2) , data=pdfff_EUR, transformation = "ld")
# 
# summary(modelgmmEMP, robust = TRUE)
# 
# 
# summary(pdfff)


############################################
# Y_L - POST ##############################
############################################

# NO SIGNIFICANCE Regional Effects of GVA on productivity POST (marginal effects of GFC formation = INV),


head(pdfff)

# Y_EMP - PRE


  
# MOST CONVENIENT MODEL -FULL
modelOLS_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                  lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                  GFC_EUR +
                  GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                  lag(GFC_EUR,2) +
                  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                  rGVA + 
                  rGVA_DK + rGVA_DE + rGVA_IT +
                    factor(Year),
                data=pdfff_EUR, 
                family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_EMP)



# MOST CONVENIENT MODEL - PARSI
modelOLS_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                    lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                    GFC_EUR +
                    GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                    lag(GFC_EUR,2) +
                    # lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                    rGVA + 
                    rGVA_DK + rGVA_DE + rGVA_IT +
                    factor(Year),
                  data=pdfff_EUR, 
                  family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_EMP)



modelRE_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT +
                   factor(Year),
               data=pdfff_EUR, 
               family = gaussian,model="random",index=c("Region","Year"))
summary(modelRE_EMP)


# Within

modelwithin_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                       lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                       GFC_EUR +
                       GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                       lag(GFC_EUR,2) +
                       lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                       rGVA + 
                       rGVA_DK + rGVA_DE + rGVA_IT,
                   data=pdfff_EUR, effects = "twoways",
                   family = gaussian,model="within",index=c("Region","Year"))
summary(modelwithin_EMP)


# Fixed effects

fix.ef_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                  lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                  GFC_EUR +
                  GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                  lag(GFC_EUR,2) +
                  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                  rGVA + 
                  rGVA_DK + rGVA_DE + rGVA_IT +
                  factor(Year),
                     data=pdfff_EUR, effects = "twoways",
                     family = gaussian,model="within",index=c("Region","Year"))
summary(fix.ef_EMP)


# FD
modelFD_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   # lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT,
                 data=pdfff_EUR, effects = "twoways",
                 family = gaussian,model="fd",index=c("Region","Year"))
summary(modelFD_EMP)


# Y_SAL-FULL

modelOLS_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                    lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                    GFC_EUR +
                    GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                    lag(GFC_EUR,2) +
                    lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                    rGVA + 
                    rGVA_DK + rGVA_DE + rGVA_IT +
                    factor (Year),
                data=pdfff_EUR, 
                family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_SAL)


# Y_SAL-PARSI

modelOLS_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                    lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                    GFC_EUR +
                    GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                    lag(GFC_EUR,2) +
                    # lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                    rGVA + 
                    rGVA_DK + rGVA_DE + rGVA_IT +
                    factor (Year),
                  data=pdfff_EUR, 
                  family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_SAL)


modelRE_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT +
                   factor (Year),
               data=pdfff_EUR, 
               family = gaussian,model="random",index=c("Region","Year"))
summary(modelRE_SAL)


# Within

modelwithin_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                       lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                       GFC_EUR +
                       GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                       lag(GFC_EUR,2) +
                       lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                       rGVA + 
                       rGVA_DK + rGVA_DE + rGVA_IT,
                   data=pdfff_EUR, effects = "twoways",
                   family = gaussian,model="within",index=c("Region","Year"))
summary(modelwithin_SAL)


# fixed effects

fix.ef_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                  lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                  GFC_EUR +
                  GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                  lag(GFC_EUR,2) +
                  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                  rGVA + 
                  rGVA_DK + rGVA_DE + rGVA_IT +
                  factor (Year),
                     data=pdfff_EUR, effects = "twoways",
                     family = gaussian,model="within",index=c("Region","Year"))
summary(fix.ef_SAL)


# FD - FULL
modelFD_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   # lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT ,
                 data=pdfff_EUR, effects = "twoways",
                 family = gaussian,model="fd",index=c("Region","Year"))
summary(modelFD_SAL)



# FD - PARSI
modelFD_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                  #  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT,
                 data=pdfff_EUR, effects = "twoways",
                 family = gaussian,model="fd",index=c("Region","Year"))
summary(modelFD_SAL)

library(lmtest)
library(sandwich)

fixef(modelwithin_EMP)



x11()
plotmeans(Y_EMP ~ Region, main="Vækstraten i Brutoværditilvækst Per Beskæftiget Time (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i Produktiviteten per Beskæftiget time (pct.)", bars=TRUE, barcol="blue", data=pdff)

x11()
plotmeans(Y_EMP ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdff)


# No fixed effects
# No random big variance(ai)
# No heterosk
# No autocorrel

# =>OLS fine - need of correction for cross section dependence!!!

# Trial => Check factor models in longer samples!
# ccepmod <- pcce(Y_EMP ~ lPPS_HAB +  GFC_EUR + rGVA, data = pdfff_EUR, model="mg")
# summary(ccepmod)





plmtest(modelOLS_EMP, type = c("bp"))  # Random vs pooling => variance across countries big! 

plmtest(modelOLS_EMP, c("individual"), type=("bp"))
plmtest(modelFD_EMP, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO
plmtest(modelwithin_EMP, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO
plmtest(fix.ef_EMP, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO



# Individual effects
pFtest(modelRE_EMP, modelwithin_EMP)  # significant time effects => Yes!
pFtest(modelRE_EMP, fix.ef_EMP)  # significant time effects => Yes!
pFtest(modelRE_EMP, modelFD_EMP)  # significant time effects => Yes!


# FE or RE - Hausmann test
# No! one model is inconsistent

phtest(modelFD_EMP, modelOLS_EMP)
phtest(modelFD_EMP, modelRE_EMP)
phtest(modelwithin_EMP, modelRE_EMP)
phtest(fix.ef_EMP, modelRE_EMP)


pFtest(fix.ef_EMP, modelwithin_EMP)  # significant time effects => Yes!

# Time effects

plmtest(modelOLS_EMP, c("time"), type=("bp")) 
plmtest(modelRE_EMP, c("time"), type=("bp"))  # significant time effects => YES!


plmtest(modelwithin_EMP, c("time"), type=("bp"))  # significant time effects => YES!
plmtest(modelFD_EMP, c("time"), type=("bp"))  # significant time effects => YES!
plmtest(fix.ef_EMP, c("time"), type = ("bp")) # alternative!  # significant time effects => Check since controlled for!




# No serial corelation

pwfdtest(modelFD_EMP)

# Cross sectional dependence for macro situation (leads to BIAS) - SLIDES

pcdtest(fix.ef_EMP, test = c("lm"))   # yes, there is!

pcdtest(fix.ef_EMP, test = c("cd"))   # 

pcdtest(modelOLS_EMP)

# heteroskedasticity in differenced errors => robust (NO NEED but autocorrrlation)

bptest(modelOLS_EMP) 
bptest(modelRE_EMP) 




# residual autocorrelation in both cases (within controlled for time effects and FD)

pbgtest(modelOLS_EMP)

pbgtest(modelwithin_EMP)

pbgtest(fix.ef_EMP)

pbgtest(modelFD_EMP)  # Call for robust s.e. or dynamic modelling !!!
# pbgtest(modelFDd)  # Call for robust s.e. or dynamic modelling !!!





# test for autocorrelations - Again!

pbgtest(modelRE_EMP)

pbgtest(modelwithin_EMP)
pbgtest(fix.ef_EMP)

# pwfdtest(modelFD_EMP)



# Test Unit roots in Y_L

library(tseries)

adf.test(pdfff_EUR$Y_EMP, k = 4)



######################
# NON INTERESSANT
######################
###### 2009-2013 and FULL SAMPLE
#################


head(pdfff)

# Y_EMP - PRE

# lag(GFC_EUR,1) +
#   lag(GFC_EUR_DK,1) + lag(GFC_EUR_DE,1) + lag(GFC_EUR_IT,1) +
#   
   
modelOLS_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                      lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                      GFC_EUR +
                      GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                      lag(GFC_EUR,2) +
                      lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                      rGVA + 
                      rGVA_DK + rGVA_DE + rGVA_IT +
                      factor(Year),
                    data=pdfff, 
                    family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_EMP)

modelRE_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT +
                   factor(Year),
                 data=pdfff, 
                 family = gaussian,model="random",index=c("Region","Year"))
summary(modelRE_EMP)


# Within

modelwithin_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                       lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                       GFC_EUR +
                       GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                       lag(GFC_EUR,2) +
                       lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                       rGVA + 
                       rGVA_DK + rGVA_DE + rGVA_IT,
                     data=pdfff, effects = "twoways",
                     family = gaussian,model="within",index=c("Region","Year"))
summary(modelwithin_EMP)


# Fixed effects

fix.ef_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                  lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                  GFC_EUR +
                  GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                  lag(GFC_EUR,2) +
                  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                  rGVA + 
                  rGVA_DK + rGVA_DE + rGVA_IT +
                  factor(Year),
                data=pdfff, effects = "twoways",
                family = gaussian,model="within",index=c("Region","Year"))
summary(fix.ef_EMP)


# FD
modelFD_EMP<-plm(Y_EMP ~   lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT,
                 data=pdfff, effects = "twoways",
                 family = gaussian,model="fd",index=c("Region","Year"))
summary(modelFD_EMP)


# Y_SAL

modelOLS_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                    lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                    GFC_EUR +
                    GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                    lag(GFC_EUR,2) +
                    lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                    rGVA + 
                    rGVA_DK + rGVA_DE + rGVA_IT +
                    factor (Year),
                  data=pdfff, 
                  family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_SAL)

modelRE_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT +
                   factor (Year),
                 data=pdfff, 
                 family = gaussian,model="random",index=c("Region","Year"))
summary(modelRE_SAL)


# Within

modelwithin_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                       lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                       GFC_EUR +
                       GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                       lag(GFC_EUR,2) +
                       lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                       rGVA + 
                       rGVA_DK + rGVA_DE + rGVA_IT,
                     data=pdfff, effects = "twoways",
                     family = gaussian,model="within",index=c("Region","Year"))
summary(modelwithin_SAL)


# fixed effects

fix.ef_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                  lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                  GFC_EUR +
                  GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                  lag(GFC_EUR,2) +
                  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                  rGVA + 
                  rGVA_DK + rGVA_DE + rGVA_IT +
                  factor (Year),
                data=pdfff, effects = "twoways",
                family = gaussian,model="within",index=c("Region","Year"))
summary(fix.ef_SAL)


# FD
modelFD_SAL<-plm(Y_SAL ~ lPPS_HAB +  
                   lPPS_HAB_DK + lPPS_HAB_DE + lPPS_HAB_IT + 
                   GFC_EUR +
                   GFC_EUR_DK + GFC_EUR_DE + GFC_EUR_IT + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT ,
                 data=pdfff, effects = "twoways",
                 family = gaussian,model="fd",index=c("Region","Year"))
summary(modelFD_SAL)


library(lmtest)
library(sandwich)

fixef(modelwithin_EMP)



x11()
plotmeans(Y_EMP ~ Region, main="Vækstraten i Brutoværditilvækst Per Beskæftiget Time (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i Produktiviteten per Beskæftiget time (pct.)", bars=TRUE, barcol="blue", data=pdff)

x11()
plotmeans(Y_EMP ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdff)


# No fixed effects
# No random big variance(ai)
# No heterosk
# No autocorrel

# =>OLS fine - need of correction for CROSS SECTION DEPENDENCE!!!

# Trial => Check factor models in longer samples!
# ccepmod <- pcce(Y_EMP ~ lPPS_HAB +  GFC_EUR + rGVA, data = pdfff, model="p")
# summary(ccepmod)




#########################################################
# Simple OLS - FD estimates and cluster robust estimates
#########################################################

# Y_EMP
coeftest(modelOLS_EMP, vcov=vcovHC(modelOLS_EMP,type="HC0",cluster="group"))

OLS_EMP = cbind(coeftest(modelOLS_EMP),coeftest(modelOLS_EMP, vcov=vcovHC(modelOLS_EMP,cluster="group")))
head(OLS, 20)

?coeftest

coeftest(modelFD_EMP, vcov=vcovHC(modelFD_EMP,type="HC0",cluster="group"))

FD_EMP = cbind(coeftest(modelFD_EMP),coeftest(modelFD_EMP, vcov=vcovHC(modelFD_EMP,type="HC0",cluster="group")))
head(FD_EMP, 20)


# BINDING OLS - FD results for Y EMP
library(broom)

bOLS = tidy(coeftest(modelOLS_EMP, vcov=vcovHC(modelOLS_EMP,cluster="group")))
bFD = tidy(coeftest(modelFD_EMP, vcov=vcovHC(modelFD_EMP,type="HC0",cluster="group")))
dim(bOLS)
dim(bFD)

?data.frame

init=(nrow(bOLS)-4):nrow(bOLS)
centres= bOLS[init,]
centres

n=dim(bFD)[1]
n

indsup= n + 1:5
indsup
bFD[indsup,]=centres


# testing
test = bFD

test[15:19,2:5]=rep(c("NA","NA","NA","NA"),5)

OLS_FD <- cbind(bOLS,test)
OLS_FD

write.csv(OLS_FD,"OLS_FD_Y_EMP.csv")



# MUCH SIMPLER

write.csv(coeftest(modelOLS_SAL, vcov=vcovHC(modelOLS_SAL,type="HC0",cluster="group")),"test1.csv")
write.csv(coeftest(modelFD_SAL, vcov=vcovHC(modelOLS_SAL,type="HC0",cluster="group")),"test2.csv")



# Y_SAL
coeftest(modelOLS_SAL, vcov=vcovHC(modelOLS_SAL,type="HC0",cluster="group"))

OLS_SAL = cbind(coeftest(modelOLS_SAL),coeftest(modelOLS_SAL, vcov=vcovHC(modelOLS_SAL,type="HC0",cluster="group")))
head(OLS, 20)


coeftest(modelFD_SAL, vcov=vcovHC(modelFD_SAL,type="HC0",cluster="group"))

FD_SAL = cbind(coeftest(modelFD_SAL),coeftest(modelFD_SAL, vcov=vcovHC(modelFD_SAL,type="HC0",cluster="group")))
head(FD_SAL, 20)


# BINDING OLS - FD results for Y SAL
library(broom)

bOLS = tidy(coeftest(modelOLS_SAL, vcov=vcovHC(modelOLS_SAL,type="HC0",cluster="group")))
bFD = tidy(coeftest(modelFD_SAL, vcov=vcovHC(modelFD_SAL,type="HC0",cluster="group")))
dim(bOLS)
dim(bFD)

?data.frame

init=(nrow(bOLS)-4):nrow(bOLS)
centres= bOLS[init,]
centres

n=dim(bFD)[1]
n

indsup= n + 1:5
indsup
bFD[indsup,]=centres


# testing
test = bFD

test[15:19,2:5]=rep(c("NA","NA","NA","NA"),5)

OLS_FD <- cbind(bOLS,test)
OLS_FD


write.csv(OLS_FD,"OLS_FD_Y_SAL.csv")



# Y_EMP tests

plmtest(modelOLS_EMP, type = c("bp"))  # Random vs pooling => variance across countries big! 

plmtest(modelFD_EMP, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO
plmtest(modelwithin_EMP, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO
plmtest(fix.ef_EMP, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO



# Individual effects
pFtest(modelRE_EMP, modelwithin_EMP)  # significant time effects => Yes!
pFtest(modelRE_EMP, fix.ef_EMP)  # significant time effects => Yes!
pFtest(modelRE_EMP, modelFD_EMP)  # significant time effects => Yes!


# FE or RE - Hausmann test
# No! one model is inconsistent

phtest(modelFD_EMP, modelRE_EMP)
phtest(modelwithin_EMP, modelRE_EMP)
phtest(fix.ef_EMP, modelRE_EMP)


pFtest(fix.ef_EMP, modelwithin_EMP)  # significant time effects => Yes!

# Time effects


plmtest(modelRE_EMP, c("time"), type=("bp"))  # significant time effects => YES!


plmtest(modelwithin_EMP, c("time"), type=("bp"))  # significant time effects => YES!
plmtest(modelFD_EMP, c("time"), type=("bp"))  # significant time effects => YES!
plmtest(fix.ef_EMP, c("time"), type = ("bp")) # alternative!  # significant time effects => Check since controlled for!




# No serial corelation
pwfdtest(modelFD_EMP)

# Cross sectional dependence for macro situation (leads to BIAS) - SLIDES

pcdtest(fix.ef_EMP, test = c("lm"))   # yes, there is!

pcdtest(fix.ef_EMP, test = c("cd"))   # 



# heteroskedasticity in differenced errors => robust (NO NEED but autocorrrlation)

bptest(modelRE_EMP) 
bptest(modelOLS_EMP) 



# residual autocorrelation in both cases (within controlled for time effects and FD)

pbgtest(modelOLS_EMP)

pbgtest(modelwithin_EMP)

pbgtest(fix.ef_EMP)

pbgtest(modelFD_EMP)  # Call for robust s.e. or dynamic modelling !!!
# pbgtest(modelFDd)  # Call for robust s.e. or dynamic modelling !!!





# test for autocorrelations - Again!

pbgtest(modelRE_EMP)

pbgtest(modelwithin_EMP)
pbgtest(fix.ef_EMP)

# pwfdtest(modelFD_EMP)



# Test Unit roots in Y_L

library(tseries)

adf.test(pdfff_EURp$Y_EMP, k = 4)




# Y_SAL tests

plmtest(modelOLS_SAL, type = c("bp"))  # Random vs pooling => variance across countries big! 

plmtest(modelFD_SAL, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO
plmtest(modelwithin_SAL, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO
plmtest(fix.ef_SAL, c("individual"), type=("bp")) # significant individual cross-sectional effects => NO



# Individual effects
pFtest(modelRE_SAL, modelwithin_SAL)  # significant time effects => Yes!
pFtest(modelRE_SAL, fix.ef_SAL)  # significant time effects => Yes!
pFtest(modelRE_SAL, modelFD_SAL)  # significant time effects => Yes!


# FE or RE - Hausmann test
# No! one model is inconsistent

phtest(modelFD_SAL, modelRE_SAL)
phtest(modelwithin_SAL, modelRE_SAL)
phtest(fix.ef_SAL, modelRE_SAL)


pFtest(fix.ef_SAL, modelwithin_SAL)  # significant time effects => Yes!

# Time effects


plmtest(modelRE_SAL, c("time"), type=("bp"))  # significant time effects => YES!


plmtest(modelwithin_SAL, c("time"), type=("bp"))  # significant time effects => YES!
plmtest(modelFD_SAL, c("time"), type=("bp"))  # significant time effects => YES!
plmtest(fix.ef_SAL, c("time"), type = ("bp")) # alternative!  # significant time effects => Check since controlled for!




# No serial corelation
pwfdtest(modelFD_SAL)

# Cross sectional dependence for macro situation (leads to BIAS) - SLIDES

pcdtest(fix.ef_SAL, test = c("lm"))   # yes, there is!

pcdtest(fix.ef_SAL, test = c("cd"))   # 



# heteroskedasticity in differenced errors => robust (NO NEED but autocorrrlation)

bptest(modelRE_SAL) 
bptest(modelOLS_SAL) 



# residual autocorrelation in both cases (within controlled for time effects and FD)

pbgtest(modelOLS_SAL)

pbgtest(modelwithin_SAL)

pbgtest(fix.ef_SAL)

pbgtest(modelFD_SAL)  # Call for robust s.e. or dynamic modelling !!!
# pbgtest(modelFDd)  # Call for robust s.e. or dynamic modelling !!!





# test for autocorrelations - Again!

pbgtest(modelRE_SAL)

pbgtest(modelwithin_SAL)
pbgtest(fix.ef_SAL)

# pwfdtest(modelFD_SAL)



# Test Unit roots in Y_L

library(tseries)

adf.test(pdfff_EURp$Y_SAL, k = 4)



### Tests from external SLIDES #######

# pFtest(modelwithin, modelOLS)

pFtest(modelwithin_EMP, modelRE_EMP)  # F test for individual effects => NO!

# pFtest(modelwithin, modelOLS)

pFtest(modelFD_EMP, modelRE_EMP)  # F test for individual effects => NO!

# significant differences across units? RE Vs OLS

plmtest(modelOLS_EMP, type=c("bp"))  # clearly random effects, that is Var(i) <> 0!

pFtest(fix.ef_EMP, modelwithin_EMP)  # significant time effects => Yes!

pFtest(fix.ef_EMP, modelFD_EMP)  # significant time effects => No - FD controlled for time effects!!!




############################################
# Y_L - POST ##############################
############################################

# NO SIGNIFICANCE Regional Effects of GVA on productivity POST (marginal effects on cap formation)


modelOLS_EMP<-plm(Y_EMP ~  GDP_PPS_HAB + 
                    lag(GFC_EUR,2) +
                    lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                    rGVA + 
                    rGVA_DK + rGVA_DE + rGVA_IT ,
                  data=pdfff_EURp, 
                  family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_EMP)

modelRE_EMP<-plm(Y_EMP ~  GDP_PPS_HAB + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT ,
                 data=pdfff_EURp, 
                 family = gaussian,model="random",index=c("Region","Year"))
summary(modelRE_EMP)


# Within

modelwithin_EMP<-plm(Y_EMP ~  GDP_PPS_HAB + 
                       lag(GFC_EUR,2) +
                       lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                       rGVA + 
                       rGVA_DK + rGVA_DE + rGVA_IT ,
                     data=pdfff_EURp, effects = "twoways",
                     family = gaussian,model="within",index=c("Region","Year"))
summary(modelwithin_EMP)


# Fixed effects

fix.ef_EMP<-plm(Y_EMP ~  GDP_PPS_HAB + 
                  lag(GFC_EUR,2) +
                  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                  rGVA + 
                  rGVA_DK + rGVA_DE + rGVA_IT +
                  factor(Year),
                data=pdfff_EURp, effects = "twoways",
                family = gaussian,model="within",index=c("Region","Year"))
summary(fix.ef_EMP)


# FD
modelFD_EMP<-plm(Y_EMP ~  GDP_PPS_HAB + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT,
                 data=pdfff_EURp, effects = "twoways",
                 family = gaussian,model="fd",index=c("Region","Year"))
summary(modelFD_EMP)


# Y_SAL

modelOLS_SAL<-plm(Y_SAL ~  GDP_PPS_HAB + 
                    lag(GFC_EUR,2) +
                    lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                    rGVA + 
                    rGVA_DK + rGVA_DE + rGVA_IT ,
                  data=pdfff_EURp, 
                  family = gaussian,model="pooling",index=c("Region","Year"))
summary(modelOLS_SAL)

modelRE_SAL<-plm(Y_SAL ~  GDP_PPS_HAB + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT ,
                 data=pdfff_EURp, 
                 family = gaussian,model="random",index=c("Region","Year"))
summary(modelRE_SAL)


# Within

modelwithin_SAL<-plm(Y_SAL ~  GDP_PPS_HAB + 
                       lag(GFC_EUR,2) +
                       lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                       rGVA + 
                       rGVA_DK + rGVA_DE + rGVA_IT ,
                     data=pdfff_EURp, effects = "twoways",
                     family = gaussian,model="within",index=c("Region","Year"))
summary(modelwithin_SAL)


# fixed effects

fix.ef_SAL<-plm(Y_SAL ~  GDP_PPS_HAB + 
                  lag(GFC_EUR,2) +
                  lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                  rGVA + 
                  rGVA_DK + rGVA_DE + rGVA_IT +
                  factor(Year),
                data=pdfff_EURp, effects = "twoways",
                family = gaussian,model="within",index=c("Region","Year"))
summary(fix.ef_SAL)


# FD
modelFD_SAL<-plm(Y_SAL ~  GDP_PPS_HAB + 
                   lag(GFC_EUR,2) +
                   lag(GFC_EUR_DK,2) + lag(GFC_EUR_DE,2) + lag(GFC_EUR_IT,2) +
                   rGVA + 
                   rGVA_DK + rGVA_DE + rGVA_IT ,
                 data=pdfff_EURp, effects = "twoways",
                 family = gaussian,model="fd",index=c("Region","Year"))
summary(modelFD_SAL)


library(lmtest)
library(sandwich)

fixef(modelwithin_EMP)



x11()
plotmeans(Y_EMP ~ Region, main="Vækstraten i Brutoværditilvækst Per Beskæftiget Time (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i Produktiviteten per Beskæftiget time (pct.)", bars=TRUE, barcol="blue", data=pdff)

x11()
plotmeans(Y_EMP ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdff)





pFtest(modelOLS_EMP, modelFD_EMP)  # significant time effects => Yes!

# No serial corelation
pwfdtest(modelFD_EMP)

# FE or RE - Hausmann test
# No! one model is inconsistent

phtest(modelFD_EMP, modelRE_EMP)


# FE or RE - Hausmann test
# No! one model is inconsistent

phtest(modelwithin_EMP, modelRE_EMP)


pFtest(fix.ef_EMP, modelwithin_EMP)  # significant time effects => Yes!

plmtest(fix.ef_EMP, c("time"), type = ("bp")) # alternative!  # significant time effects => Yes!


plmtest(modelOLS_EMP, type = c("bp"))  # Random vs pooling => variance across countries big! 


# Cross sectional dependence for macro situation (leads to BIAS) - SLIDES

pcdtest(fix.ef_EMP, test = c("lm"))   # yes, there is!

pcdtest(fix.ef_EMP, test = c("cd"))   # no, there is not! Better size (type I error)



