# CONTENU


# ADDING WEIGHTS FOR WEIGHTED AVERAGES OF GROWTH RATES
# AGGREGATING WEIGHTS BY REGION/LANDSDEL
# SPLITTING BY REGION/LANDSDEL AND RECALCULATING REGIONAL WEIGHTS


library(ggplot2)
library(reshape2)
library(psych)




BVT <- read.csv("BVT_chained.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(BVT)
head(BVT)


data_ref<- read.csv("KL_3_Periods_NUM_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_ref)
tail(data_ref)

data_ref=data_ref[,c(1,2,3)]


merge_ref = merge(data_ref, BVT, by = c("kom"), all = T)
tail(merge_ref)

summary(merge_ref$Region_navn)


# write.csv(merge_ref,"BVT_For_Weight.csv")




# NO DK already AND NO "Uden REgion" ALSO!

BVT_W<- read.csv("BVT_For_Weight.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(BVT_W)
tail(BVT_W)

BVT_W_sh = subset(BVT_W, !(BVT_W$kom==961 | BVT_W$kom==999))
head(BVT_W_sh)
str(BVT_W_sh)
unique(BVT_W_sh$kom)

BVT_W_sh$Region_navn = factor(BVT_W_sh$Region_navn)
levels(BVT_W_sh$Region_navn)

BVT_W_sh$Landsdel_navn = factor(BVT_W_sh$Landsdel_navn)
levels(BVT_W_sh$Landsdel_navn)

BVT_W_sh$Omr�de = factor(BVT_W_sh$Omr�de)
levels(BVT_W_sh$Omr�de)


# DPLYR Data SUMMARY
# L for LEVEL!

# # LG_LX <- tbl_df(data_NIVEAU_long_DK) # 
# W_X <- tbl_df(BVT_W_sh[,-c(1,2,3,4)]) # To compare effects on SUMMARY STATISTICS

# 
# SumX = W_X %>%
#   group_by(Omr�de) %>%
#   summarise_each(funs(sum(., na.rm=TRUE)), contains("X"))
# 
# 
# data.frame(W_X)

# tapply(BVT_W_sh$Y_T_�r, BVT_W_sh$Omr�de, sum, na.rm=TRUE)

# aggregate(BVT_W_sh[,6:28],by=BVT_W_sh$Omr�de, sum)



by(BVT_W_sh[,6:28],BVT_W_sh$Omr�de, FUN=colSums)   #  liste au lie de dataframe!!!
by(BVT_W_sh[,6:28],BVT_W_sh$Region_navn, FUN=colSums)   #  liste au lie de dataframe!!!
by(BVT_W_sh[,6:28],BVT_W_sh$Landsdel_navn, FUN=colSums)   #  liste au lie de dataframe!!!


# SPLIT by Omr�de, Region and Landsdel

head(BVT_W_sh)
str(BVT_W_sh)

summary(BVT_W_sh$Omr�de)

# Subsetting rows per group and columns per variable

BVT_W_Hov=subset(BVT_W_sh,BVT_W_sh$Omr�de=="Hovedstaden")
head(BVT_W_Hov)
str(BVT_W_Hov)


# OMR�DE
# Split data per category (ROWS)
BVT_W_split <- split(BVT_W_sh, BVT_W_sh$Omr�de)
new_names <- c("BVT_W_Hoved","BVT_W_�vrige")    

for (i in 1:length(BVT_W_split)) {
  assign(new_names[i], BVT_W_split[[i]])
}


write.csv(BVT_W_Hoved,"BVT_Hoved.csv")
write.csv(BVT_W_�vrige,"BVT_�vrige.csv")



# REGION
summary(BVT_W_sh$Region_navn)

# Split data per category (ROWS)
BVT_W_split <- split(BVT_W_sh, BVT_W_sh$Region_navn)
new_names <- c("Hovedstaden","Midtjylland","Nordjylland","�-kommuner","Sj�lland","Syddanmark")    

for (i in 1:length(BVT_W_split)) {
  assign(new_names[i], BVT_W_split[[i]])
}


write.csv(Hovedstaden,"BVT_HovedX.csv")
write.csv(Midtjylland,"BVT_Midtjylland.csv")
write.csv(Nordjylland,"BVT_Nordjylland.csv")
write.csv(�-kommune,"BVT_�-kommune.csv")
write.csv(Sj�lland,"BVT_Sj�lland.csv")
write.csv(Syddanmark,"BVT_Syddanmark.csv")



# LANDSDEL
summary(BVT_W_sh$Landsdel_navn)

# Split data per category (ROWS)
BVT_W_split <- split(BVT_W_sh, BVT_W_sh$Landsdel_navn)
new_names <- c("Bornholm","Byen_K�benhavn","Fyn","K�benhavns_omegn","Nordjylland","Nordsj�lland","�_kommuner","�stjylland","�stsj�lland",
               "Sydjylland","Vest_og_Sydsj�lland","Vestjylland")    

for (i in 1:length(BVT_W_split)) {
  assign(new_names[i], BVT_W_split[[i]])
}


write.csv(Bornholm,"BVT_Bornholm.csv")

write.csv(Nordjylland,"BVT_Nordjylland.csv")
write.csv(�_kommuner,"BVT_�-kommune.csv")

write.csv(Byen_K�benhavn,"BVT_Byen K�benhavn.csv")
write.csv(Fyn,"BVT_Fyn.csv")
write.csv(K�benhavns_omegn,"BVT_K�benhavns_omegn.csv")
write.csv(Nordjylland,"BVT_Nordjylland.csv")
write.csv(Nordsj�lland,"BVT_Nordsj�lland.csv")
write.csv(�stjylland,"BVT_�stjylland.csv")
write.csv(�stsj�lland,"BVT_�stsj�lland.csv")
write.csv(Sydjylland,"BVT_Sydjylland.csv")
write.csv(Vest_og_Sydsj�lland,"BVT_Vest-og-Sydsj�lland.csv")
write.csv(Vestjylland,"BVT_Vestjyllandk.csv")


