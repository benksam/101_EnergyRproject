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

BVT_W_sh$Område = factor(BVT_W_sh$Område)
levels(BVT_W_sh$Område)


# DPLYR Data SUMMARY
# L for LEVEL!

# # LG_LX <- tbl_df(data_NIVEAU_long_DK) # 
# W_X <- tbl_df(BVT_W_sh[,-c(1,2,3,4)]) # To compare effects on SUMMARY STATISTICS

# 
# SumX = W_X %>%
#   group_by(Område) %>%
#   summarise_each(funs(sum(., na.rm=TRUE)), contains("X"))
# 
# 
# data.frame(W_X)

# tapply(BVT_W_sh$Y_T_År, BVT_W_sh$Område, sum, na.rm=TRUE)

# aggregate(BVT_W_sh[,6:28],by=BVT_W_sh$Område, sum)



by(BVT_W_sh[,6:28],BVT_W_sh$Område, FUN=colSums)   #  liste au lie de dataframe!!!
by(BVT_W_sh[,6:28],BVT_W_sh$Region_navn, FUN=colSums)   #  liste au lie de dataframe!!!
by(BVT_W_sh[,6:28],BVT_W_sh$Landsdel_navn, FUN=colSums)   #  liste au lie de dataframe!!!


# SPLIT by Område, Region and Landsdel

head(BVT_W_sh)
str(BVT_W_sh)

summary(BVT_W_sh$Område)

# Subsetting rows per group and columns per variable

BVT_W_Hov=subset(BVT_W_sh,BVT_W_sh$Område=="Hovedstaden")
head(BVT_W_Hov)
str(BVT_W_Hov)


# OMRÅDE
# Split data per category (ROWS)
BVT_W_split <- split(BVT_W_sh, BVT_W_sh$Område)
new_names <- c("BVT_W_Hoved","BVT_W_Øvrige")    

for (i in 1:length(BVT_W_split)) {
  assign(new_names[i], BVT_W_split[[i]])
}


write.csv(BVT_W_Hoved,"BVT_Hoved.csv")
write.csv(BVT_W_Øvrige,"BVT_Øvrige.csv")



# REGION
summary(BVT_W_sh$Region_navn)

# Split data per category (ROWS)
BVT_W_split <- split(BVT_W_sh, BVT_W_sh$Region_navn)
new_names <- c("Hovedstaden","Midtjylland","Nordjylland","Ø-kommuner","Sjælland","Syddanmark")    

for (i in 1:length(BVT_W_split)) {
  assign(new_names[i], BVT_W_split[[i]])
}


write.csv(Hovedstaden,"BVT_HovedX.csv")
write.csv(Midtjylland,"BVT_Midtjylland.csv")
write.csv(Nordjylland,"BVT_Nordjylland.csv")
write.csv(Ø-kommune,"BVT_Ø-kommune.csv")
write.csv(Sjælland,"BVT_Sjælland.csv")
write.csv(Syddanmark,"BVT_Syddanmark.csv")



# LANDSDEL
summary(BVT_W_sh$Landsdel_navn)

# Split data per category (ROWS)
BVT_W_split <- split(BVT_W_sh, BVT_W_sh$Landsdel_navn)
new_names <- c("Bornholm","Byen_København","Fyn","Københavns_omegn","Nordjylland","Nordsjælland","Ø_kommuner","Østjylland","Østsjælland",
               "Sydjylland","Vest_og_Sydsjælland","Vestjylland")    

for (i in 1:length(BVT_W_split)) {
  assign(new_names[i], BVT_W_split[[i]])
}


write.csv(Bornholm,"BVT_Bornholm.csv")

write.csv(Nordjylland,"BVT_Nordjylland.csv")
write.csv(Ø_kommuner,"BVT_Ø-kommune.csv")

write.csv(Byen_København,"BVT_Byen København.csv")
write.csv(Fyn,"BVT_Fyn.csv")
write.csv(Københavns_omegn,"BVT_Københavns_omegn.csv")
write.csv(Nordjylland,"BVT_Nordjylland.csv")
write.csv(Nordsjælland,"BVT_Nordsjælland.csv")
write.csv(Østjylland,"BVT_Østjylland.csv")
write.csv(Østsjælland,"BVT_Østsjælland.csv")
write.csv(Sydjylland,"BVT_Sydjylland.csv")
write.csv(Vest_og_Sydsjælland,"BVT_Vest-og-Sydsjælland.csv")
write.csv(Vestjylland,"BVT_Vestjyllandk.csv")


