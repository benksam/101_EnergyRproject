# CONTENU #


# RAW DATA MANIPULATION - FIRST PROGRAM FOR KL-DATA
# DATA SEPARATION BY VARIABLE/CATEGORY FROM HUGE DATASET


# Kommunale Data

KL <- read.csv("data_sheet.csv", header = T)

head(KL)
str(KL)

summary(KL$prenh)

# Subsetting rows per group and columns per variable

KL_B=subset(KL,KL$prenh=="B")
head(KL_B)
str(KL_B)


# Split data per category (ROWS)
KL_B_split <- split(KL_B, KL_B$dk)
new_names <- c("KL_B_B","KL_B_D","KL_B_K","KL_B_X")    

for (i in 1:length(KL_B_split)) {
  assign(new_names[i], KL_B_split[[i]])
}


str(KL_B_X)

head(KL_B_X)


KL_B_X$trans = factor(KL_B_X$trans)
levels(KL_B_X$trans)

summary(KL_B_X$trans)


# Split data per category (ROWS)
KL_B_X_split <- split(KL_B_X, KL_B_X$trans)
new_names <- c("KL_B_X_EMP","KL_B_X_POP","KL_B_X_SAL","KL_B_X_SELF")    

for (i in 1:length(KL_B_X_split)) {
  assign(new_names[i], KL_B_X_split[[i]])
}


head(KL_B_X_EMP)
head(KL_B_X_POP)


write.csv(KL_B_X,"KL_Persons_X.csv")

write.csv(KL_B_X_EMP,"KL_Persons_X_EMP.csv")
write.csv(KL_B_X_POP,"KL_Persons_X_POP.csv")
write.csv(KL_B_X_SAL,"KL_Persons_X_SAL.csv")
write.csv(KL_B_X_SELF,"KL_Persons_X_SELF.csv")




KL_D=subset(KL,KL$prenh=="D")
head(KL_D)
str(KL_D)


# Split data per category (ROWS)
KL_D_split <- split(KL_D, KL_D$dk)
new_names <- c("KL_D_B","KL_D_D","KL_D_K","KL_D_X")    

for (i in 1:length(KL_D_split)) {
  assign(new_names[i], KL_D_split[[i]])
}


write.csv(KL_D_K,"KL_Lag_Prices_K.csv")

str(KL_D_B)

unique(KL_D_B$trans)

KL_D_B$trans = factor(KL_D_B$trans)
levels(KL_D_B$trans)


summary(KL_D_B$trans)


# Split data per category (ROWS)
KL_D_B_split <- split(KL_D_B, KL_D_B$trans)
new_names <- c("KL_D_B_B1G","KL_D_B_B1GF","KL_D_B_B1GQ","KL_D_B_B2A3G")    

for (i in 1:length(KL_D_B_split)) {
  assign(new_names[i], KL_D_B_split[[i]])
}


head(KL_B_X_EMP)
head(KL_B_X_POP)


write.csv(KL_D_B,"KL_Lag_Prices_B.csv")

write.csv(KL_D_B_B1G,"KL_Lag_Prices_B_B1G.csv")
write.csv(KL_D_B_B1GF,"KL_Lag_Prices_B_B1GF.csv")
write.csv(KL_D_B_B1GQ,"KL_Lag_Prices_B_B1GQ.csv")
write.csv(KL_D_B_B2A3G,"KL_Lag_Prices_B_B2A3G.csv")



write.csv(KL_D,"KL_Lag_Prices.csv")


summary(KL_D_D$trans)

KL_D_D$trans = factor(KL_D_D$trans)
levels(KL_D_D$trans)


# Split data per category (ROWS)
KL_D_D_split <- split(KL_D_D, KL_D_D$trans)
new_names <- c("KL_D_D_D21X31","KL_D_D_P2","KL_D_D_P51G")    

for (i in 1:length(KL_D_D_split)) {
  assign(new_names[i], KL_D_D_split[[i]])
}


write.csv(KL_D_D,"KL_Lag_Prices_D.csv")

write.csv(KL_D_D_D21X31,"KL_Lag_Prices_D_D21X31.csv")
write.csv(KL_D_D_P2,"KL_Lag_Prices_D_P2.csv")
write.csv(KL_D_D_P51G,"KL_Lag_Prices_D_P51G.csv")




KL_H=subset(KL,KL$prenh=="H")
head(KL_H)
str(KL_H)

summary(KL_H)


unique(KL_H$trans)

KL_H$trans = factor(KL_H$trans)
levels(KL_H$trans)


# Split data per category (ROWS)
KL_H_split <- split(KL_H, KL_H$trans)
new_names <- c("KL_H_EMP","KL_H_SAL","KL_H_SELF")    

for (i in 1:length(KL_H_split)) {
  assign(new_names[i], KL_H_split[[i]])
}


write.csv(KL_H,"KL_Hours.csv")

write.csv(KL_H_EMP,"KL_Hours_EMP.csv")
write.csv(KL_H_SAL,"KL_Hours_SAL.csv")
write.csv(KL_H_SELF,"KL_Hours_SELF.csv")




KL_K=subset(KL,KL$prenh=="K")
head(KL_K)
str(KL_K)

summary(KL_K)


unique(KL_K$trans)

KL_K$trans = factor(KL_K$trans)
levels(KL_K$trans)


# Split data per category (ROWS)
KL_K_split <- split(KL_K, KL_K$trans)
new_names <- c("KL_K_B1G","KL_K_B1GQ","KL_K_D21X31","KL_K_P1","KL_K_P2","KL_K_P51G")    

for (i in 1:length(KL_K_split)) {
  assign(new_names[i], KL_K_split[[i]])
}


write.csv(KL_K,"KL_Chained.csv")

write.csv(KL_K_B1G,"KL_Chained_B1G.csv")
write.csv(KL_K_B1GQ,"KL_Chained_B1GQ.csv")
write.csv(KL_K_D21X31,"KL_Chained_D21X31.csv")
write.csv(KL_K_P1,"KL_Chained_P1.csv")
write.csv(KL_K_P2,"KL_Chained_P2.csv")
write.csv(KL_K_P51G,"KL_Chained_P51G.csv")



KL_L=subset(KL,KL$prenh=="L")
head(KL_L)
str(KL_L)


summary(KL_L)



# Split data per category (ROWS)
KL_L_split <- split(KL_L, KL_L$dk)
new_names <- c("KL_L_B","KL_L_D","KL_L_K")    

for (i in 1:length(KL_L_split)) {
  assign(new_names[i], KL_L_split[[i]])
}



str(KL_L_B)

summary(KL_L_B)


unique(KL_L_B$trans)

KL_L_B$trans = factor(KL_L_B$trans)
levels(KL_L_B$trans)




# Split data per category (ROWS)
KL_L_B_split <- split(KL_L_B, KL_L_B$trans)
new_names <- c("KL_L_B_B1G","KL_L_B_B1GF","KL_L_B_B1GQ","KL_L_B_B2A3G", "KL_L_B_B5G", "KL_L_B_B6G")    

for (i in 1:length(KL_L_B_split)) {
  assign(new_names[i], KL_L_B_split[[i]])
}


summary(KL_L_B_B2A3G)


# Split data per category (ROWS)
KL_L_B_B2A3G_split <- split(KL_L_B_B2A3G, KL_L_B_B2A3G$sektor)
new_names <- c("KL_L_B_B2A3G_S1", "KL_L_B_B2A3G_S14")    

for (i in 1:length(KL_L_B_B2A3G_split)) {
  assign(new_names[i], KL_L_B_B2A3G_split[[i]])
}


summary(KL_L_B_B2A3G)



head(KL_B_X_EMP)
head(KL_B_X_POP)


write.csv(KL_L_B,"KL_Current_B.csv")

 

write.csv(KL_L_B_B1G,"KL_Current_B_B1G.csv")
write.csv(KL_L_B_B1GF,"KL_Current_B_B1GF.csv")
write.csv(KL_L_B_B1GQ,"KL_Current_B_B1GQ.csv")
write.csv(KL_L_B_B2A3G,"KL_Current_B_B2A3G.csv")
write.csv(KL_L_B_B2A3G_S1,"KL_Current_B_B2A3G_S1.csv")
write.csv(KL_L_B_B2A3G_S14,"KL_Current_B_B2A3G_S14.csv")
write.csv(KL_L_B_B5G,"KL_Current_B_B5G.csv")
write.csv(KL_L_B_B6G,"KL_Current_B_B6G.csv")



str(KL_L_D)

summary(KL_L_D)


unique(KL_L_D$trans)

KL_L_D$trans = factor(KL_L_D$trans)
levels(KL_L_D$trans)




# Split data per category (ROWS)
KL_L_D_split <- split(KL_L_D, KL_L_D$trans)
new_names <- c("KL_L_D_D1","KL_L_D_D21X31","KL_L_D_D29X39","KL_L_D_D4", "KL_L_D_D5", "KL_L_D_D61", "KL_L_D_D7", 
               "KL_L_D_P2", "KL_L_D_P51C", "KL_L_D_P51G")    

for (i in 1:length(KL_L_D_split)) {
  assign(new_names[i], KL_L_D_split[[i]])
}




write.csv(KL_L_D,"KL_Current_D.csv")



write.csv(KL_L_D_D1,"KL_Current_D_D1.csv")
write.csv(KL_L_D_D21X31,"KL_Current_D_D21X31.csv")
write.csv(KL_L_D_D29X39,"KL_Current_D_D29X39.csv")
write.csv(KL_L_D_D4,"KL_Current_D_D4.csv")
write.csv(KL_L_D_D5,"KL_Current_D_D5.csv")
write.csv(KL_L_D_D61,"KL_Current_D_D61.csv")
write.csv(KL_L_D_D7,"KL_Current_D_D7.csv")
write.csv(KL_L_D_P2,"KL_Current_D_P2.csv")
write.csv(KL_L_D_P51C,"KL_Current_D_P51C.csv")
write.csv(KL_L_D_P51G,"KL_Current_D_P51G.csv")



str(KL_L_K)

summary(KL_L_K)


unique(KL_L_K$trans)

KL_L_K$trans = factor(KL_L_K$trans)
levels(KL_L_K$trans)




# Split data per category (ROWS)
KL_L_K_split <- split(KL_L_K, KL_L_K$trans)
new_names <- c("KL_L_K_D1", "KL_L_K_D4", "KL_L_K_D7", "KL_L_K_D62", "KL_L_K_P1")    

for (i in 1:length(KL_L_K_split)) {
  assign(new_names[i], KL_L_K_split[[i]])
}




write.csv(KL_L_K,"KL_Current_K.csv")



write.csv(KL_L_K_D1,"KL_Current_K_D1.csv")
write.csv(KL_L_K_D4,"KL_Current_K_D4.csv")
write.csv(KL_L_K_D62,"KL_Current_K_D62.csv")
write.csv(KL_L_K_D7,"KL_Current_K_D7.csv")
write.csv(KL_L_K_P1,"KL_Current_K_P1.csv")





# RESHAPING - WIDE TO LONG FOR GGPLOTS OR PANEL DATA!
# AND FOR PLOTMEANS!!!


#################################################################################################


## CHECKING the Konjektur Periods for BVT/BNP and INV for municipality analysis.
## Partitionning into 5 periods, Pre, Crissis and Post and earlier KJ periods.

## Collective evolution in BNP and related measures


KL <- read.csv("data_sheet.csv", header = T)

head(KL)
str(KL)



Chained_B1G <- read.csv("KL_Chained_B1G.csv", header = T)
names(Chained_B1G)



Chained_B1G_pd <- reshape(Chained_B1G, direction="long", 
                        varying=list(names(Chained_B1G)[2:ncol(Chained_B1G)]),
                        v.names="BVT", 
                        idvar=c("kom"), timevar="Year")


Chained_B1G_pd=Chained_B1G_pd[order(Chained_B1G_pd[,c("kom")]),]
head(Chained_B1G_pd)
tail(Chained_B1G_pd)

str(Chained_B1G_pd)


Chained_B1GQ <- read.csv("KL_Chained_B1GQ.csv", header = T)
names(Chained_B1GQ)



Chained_B1GQ_pd <- reshape(Chained_B1GQ, direction="long", 
                          varying=list(names(Chained_B1GQ)[2:ncol(Chained_B1GQ)]),
                          v.names="BNP", 
                          idvar=c("kom"), timevar="Year")


Chained_B1GQ_pd=Chained_B1GQ_pd[order(Chained_B1GQ_pd[,c("kom")]),]
head(Chained_B1GQ_pd)



Chained_P51G <- read.csv("KL_Chained_P51G.csv", header = T)
names(Chained_P51G)
head(Chained_P51G)



Chained_P51G_pd <- reshape(Chained_P51G, direction="long", 
                           varying=list(names(Chained_P51G)[2:ncol(Chained_P51G)]),
                           v.names="Inv", 
                           idvar=c("kom"), timevar="Year")


Chained_P51G_pd=Chained_P51G_pd[order(Chained_P51G_pd[,c("kom")]),]
tail(Chained_P51G_pd)




Persons_EMP <- read.csv("KL_Persons_X_EMP.csv", header = T)
names(Persons_EMP)
head(Persons_EMP)



Persons_EMP_pd <- reshape(Persons_EMP, direction="long", 
                           varying=list(names(Persons_EMP)[2:ncol(Persons_EMP)]),
                           v.names="Employees", 
                           idvar=c("kom"), timevar="Year")


Persons_EMP_pd = Persons_EMP_pd[order(Persons_EMP_pd[,c("kom")]),]
tail(Persons_EMP_pd)


Persons_POP <- read.csv("KL_Persons_X_POP.csv", header = T)
names(Persons_POP)
head(Persons_POP)



Persons_POP_pd <- reshape(Persons_POP, direction="long", 
                          varying=list(names(Persons_POP)[2:ncol(Persons_POP)]),
                          v.names="Population", 
                          idvar=c("kom"), timevar="Year")


Persons_POP_pd = Persons_POP_pd[order(Persons_POP_pd[,c("kom")]),]
tail(Persons_POP_pd)




merge1 = cbind(Chained_B1G_pd, Chained_B1GQ_pd)
head(merge1)
tail(merge1)

str(merge1)

merge2 = cbind(merge1, Chained_P51G_pd)
head(merge2)
tail(merge2)


merge2 = merge2[,-c(4,5,7,8)]
head(merge2)

class(merge2)


merge3 = merge(Persons_EMP_pd, Persons_POP_pd, by = c("kom","Year"), all.x = T)
head(merge3)



merge4 = cbind(merge2, merge3)
head(merge4)
tail(merge4)

merge4 = merge4[,-c(6,7)]
head(merge4)

pd=merge4

pd$BVT_C=pd$BVT/pd$Population
pd$INV_C=pd$Inv/pd$Population

pd$BVT_E=pd$BVT/pd$Employees
pd$INV_E=pd$Inv/pd$Employees


head(pd)



dat$l.Inc <- log(dat$ndi/dat$cpi)


# write.csv(merge2,"KL_pd.csv")



# pdf = merge(pd, merge3, by = c("kom","Year"), all.x = T)
# head(pdf)


pdf$BVT_C=pdf$BVT/pdf$Population
pdf$INV_C=pdf$Inv/pdf$Population

pdf$BVT_E=pdf$BVT/pdf$Employees
pdf$INV_E=pdf$Inv/pdf$Employees




# write.csv(pdf,"KL_panel_data.csv")


# heterogenetity across countries?

library(gplots)

# plotmeans(Producer_PI ~ AreaCode, main="Heterogeineity across countries", data=pd)


pd <- read.csv("KL_panel_data.csv", header = T)
head(pd)

2208/23
# 96

# Excluding Danmark - Already Excluded!!!
# pd_DK = subset(pd, pd$kom!=999)
# head(pd_DK)


y = seq(1993,2015)
y

År = rep(y,96)

pdf = cbind(pd,År)
head(pdf)


# heterogenetity across years?

x11()
plotmeans(BVT_E ~ År, main="Udvikling i Brutoværditilvækst Per Beskæftigede (fordelt per kommuner, 2010-Kædede Værdier )", ylab="Produktivitet per Beskæftigede (tusinde kr)", data=pdf)

# axis(2)
# ylab="Produktivitet per Beskæftigede (tusinde kr)"

x11()
plotmeans(BVT_C ~ År, main="Udvikling i Brutoværditilvækst Per Indbygger (fordelt per kommuner, 2010-Kædede Værdier )", ylab="BVT per Indbygger (tusinde kr)", data=pdf)

?plotmeans


dev.off()

# system GMM NOT ok! since no mean stationary!

coplot(BVT ~ Year|kom, type="l", data=merge2) # Lines
coplot(Producer_PI ~ Year|AreaCode, type="b", data=pd) # Points and lines



dev.off()

library(car)

x11()
scatterplot(BVT ~ Year|kom, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=merge2)

scatterplot(Producer_PI~Year|AreaCode, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=newdata.st)


scatterplot(Agrdefl_ratio~Year|AreaCode, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=pd)

scatterplot(GDPdefl_ratio~Year|AreaCode, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=pd)







###################





# sub2=data8[data8$sexhead==0 & data8$agehead==45, c("famsize","educhead")]
# 
# 
# 
# 
# monthly_b = grep("M", colnames(BOP_br))
# 
# quaterly_b = grep("Q", colnames(BOP_br))
# 
# BOP_br_q = BOP_br[,quaterly_b]
# 
# BOP_br_y = BOP_br[,-c(monthly_b,quaterly_b)]
# 
# 
# write.csv(BOP_br_q,"BOP_std_br_q.csv")
# write.csv(BOP_br_y,"BOP_std_br_y.csv")
# 

