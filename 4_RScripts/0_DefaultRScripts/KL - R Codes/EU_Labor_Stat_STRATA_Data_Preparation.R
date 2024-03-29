
##########################################################
# Merging Labour stat and productivity and keeping only NUTS 2 (77 regions out of 300)
# MISSING data => IMPUTATIONS for full productivity sample
# DONE for 3 labour stat categories (EDUC, UNEMP and Professional STATUS)
##########################################################


#################################
# From WIDE to LONG for PIVOT TABLE data MANIPULATION (Year time series for PRE/CRISIS/POST calculations
# From WIDE to LONG for SCATTER plots and FACETTING
#################################



library(FactoMineR)
library(ggplot2)
library("factoextra")
library("dplyr")
library(ggrepel)

library(reshape2)
library(psych)

library(missMDA)




######################################
######################################
#### ANALYSE AVEC DONNNES MANQUANTES - MISSMDA!!!
######################################
######################################




#################################################################
# NUTS2 data ALONE USEFUL for LABOR STATISTICS (NO PRODUCTIVITY data INCLUDED)
#################################################################


# Merging and keeping only NUTS 2

# EDUC by Age, Educ, Gender and period (NUTS2 and NUTS1)
Educ = read.csv("PCA-ALL-AGES-EDUC-GENDER.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column, , row.names = 1
str(Educ)
tail(Educ)
summary(Educ)

names(Educ)


# NUTS2 ONLY (Neat and Prepared)
NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

# Post
D = as.character(Educ[,2])  %in% as.character(NUTS2[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

Educ_nuts2 = Educ[nuts2,]
str(Educ_nuts2)
summary(Educ_nuts2)


write.csv(Educ_nuts2,"PCA-ALL-AGES-EDUC-GENDER_sh.csv")


Educ = read.csv("PCA-ALL-AGES-EDUC-GENDER_sh.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column, , 
str(Educ)
tail(Educ)
summary(Educ)



##################################################################################################
######################## IMPUTATIONS before merging with 77-productivity NUTS2 regions  ##########
##################################################################################################



# IMPUTING BOTH ACTIVE and PASSIVE variables - Productivity in MERGED in the end NOT IMPUTTED!!!
library(missMDA)

nb <- estim_ncpPCA(Educ[,2:58], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

# nbX <- estim_ncpPCA(Educ[,20:38], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
# nbX

comp <- imputePCA(Educ[,2:58], ncp=4, scale=TRUE) ## Compl�te le tableau -c(121,206)

# compX <- imputePCA(Educ[,20:38], ncp=2, scale=TRUE) ## Compl�te le tableau -c(121,206)


?imputePCA

dev.off()

mi <- MIPCA(Educ[,2:58], scale = TRUE, ncp=4) # -c(121,206)
plot(mi)

# mi <- MIPCA(Educ[,20:38], scale = TRUE, ncp=2) # -c(121,206)
# plot(mi)



head(comp$completeObs)
str(comp$completeObs)

cpt_obs = as.data.frame(comp$completeObs)
head(cpt_obs)
str(cpt_obs)
summary(cpt_obs)



# Create a `date` variable
cpt_obs$Region = rownames(cpt_obs)
# Reset the `rownames` of your original data
rownames(cpt_obs) = NULL


write.csv(cpt_obs,"PCA-ALL-AGES-EDUC-GENDER_sh_COMPLETE.csv")

Educ_cpt = read.csv("PCA-ALL-AGES-EDUC-GENDER_sh_COMPLETE.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(Educ_cpt)


# cptX_obs = as.data.frame(compX$completeObs)
# head(cptX_obs)
# str(cptX_obs)


# Educ_cpt = cbind(cpt_obs, cptX_obs, Educ[,39])
# str(Educ_cpt)
# summary(Educ_cpt)

# RENAME A COLUMN
# colnames(Educ_cpt)[39] <- "Geo"


# Preparing merge with PRODUCTIVITY
na = read.csv("EU_NO_DUP_reg.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(na)
head(na)
summary(na)


# NO OMITTED VARIABLES AND NO DUPLICATES
pdty = na.omit(na)
str(pdty)

rownames(pdty)
colnames(pdty)


# # Create a `date` variable
# Educ_cpt$Region = rownames(Educ_cpt)
# # Reset the `rownames` of your original data
# rownames(Educ_cpt) = NULL


merged = merge(Educ_cpt, pdty[,1:19], by = c("Region"), all.y = T)
head(merged)
summary(merged)
str(merged)

write.csv(merged,"FULL_Educ.csv")


############################################
# Unemployment by Age, Gender and period
############################################


Unemp = read.csv("PCA_AGES_GENDER_unemp_pct.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column, , row.names = 1
str(Unemp)
tail(Unemp)
summary(Unemp)

names(Unemp)


NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

# Post
D = as.character(Unemp[,1])  %in% as.character(NUTS2[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

Unemp_nuts2 = Unemp[nuts2,]
str(Unemp_nuts2)
summary(Unemp_nuts2)


write.csv(Unemp_nuts2,"PCA_AGES_GENDER_unemp_sh.csv")


Unemp = read.csv("PCA_AGES_GENDER_unemp_sh.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column, , 
str(Unemp)
tail(Unemp)
summary(Unemp)



# IMPUTING BOTH ACTIVE and PASSIVE variables - Productivity in MERGED in the end NOT IMPUTTED!!!
library(missMDA)

nb <- estim_ncpPCA(Unemp[,1:18], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

# nbX <- estim_ncpPCA(Unemp[,20:38], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
# nbX

comp <- imputePCA(Unemp[,1:18], ncp=4, scale=TRUE) ## Compl�te le tableau -c(121,206)

# compX <- imputePCA(Unemp[,20:38], ncp=2, scale=TRUE) ## Compl�te le tableau -c(121,206)


?imputePCA

dev.off()

mi <- MIPCA(Unemp[,1:18], scale = TRUE, ncp=4) # -c(121,206)
plot(mi)

# mi <- MIPCA(Unemp[,20:38], scale = TRUE, ncp=2) # -c(121,206)
# plot(mi)



head(comp$completeObs)
str(comp$completeObs)

cpt_obs = as.data.frame(comp$completeObs)
head(cpt_obs)
str(cpt_obs)
summary(cpt_obs)





# Create a `date` variable
cpt_obs$Region = rownames(cpt_obs)
# Reset the `rownames` of your original data
rownames(cpt_obs) = NULL


write.csv(cpt_obs,"PCA_AGES_GENDER_unemp_sh_COMPLETE.csv")

Unemp_cpt = read.csv("PCA_AGES_GENDER_unemp_sh_COMPLETE.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(Unemp_cpt)


# cptX_obs = as.data.frame(compX$completeObs)
# head(cptX_obs)
# str(cptX_obs)


# Unemp_cpt = cbind(cpt_obs, cptX_obs, Unemp[,39])
# str(Unemp_cpt)
# summary(Unemp_cpt)

# RENAME A COLUMN
# colnames(Unemp_cpt)[39] <- "Geo"


# Preparing merge with PRODUCTIVITY
na = read.csv("EU_NO_DUP_reg.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(na)
head(na)
summary(na)


# NO OMITTED VARIABLES AND NO DUPLICATES
pdty = na.omit(na)
str(pdty)

rownames(pdty)
colnames(pdty)


# # Create a `date` variable
# Unemp_cpt$Region = rownames(Unemp_cpt)
# # Reset the `rownames` of your original data
# rownames(Unemp_cpt) = NULL


merged = merge(Unemp_cpt, pdty[,1:19], by = c("Region"), all.y = T)
head(merged)
summary(merged)
str(merged)

write.csv(merged,"FULL_Unemp.csv")




# EMP by Age, PROF STATUS and period

Prof = read.csv("PCA-ALL-AGES-PROF.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column, , row.names = 1
str(Prof)
tail(Prof)
summary(Prof)

names(Prof)


NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

# Post
D = as.character(Prof[,1])  %in% as.character(NUTS2[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

Prof_nuts2 = Prof[nuts2,]
str(Prof_nuts2)
summary(Prof_nuts2)


write.csv(Prof_nuts2,"PCA_AGES_GENDER_PROF_sh.csv")


Prof = read.csv("PCA_AGES_GENDER_PROF_sh.csv",header=TRUE,sep=",", dec=".", row.names = 1)  # ROW 1 NAME as 1st column, , 
str(Prof)
tail(Prof)
summary(Prof)



# IMPUTING BOTH ACTIVE and PASSIVE variables - Productivity in MERGED in the end NOT IMPUTTED!!!
library(missMDA)

nb <- estim_ncpPCA(Prof, scale=TRUE) ## Estime le nb de dimensions -c(121,206)
nb

# nbX <- estim_ncpPCA(Prof[,20:38], scale=TRUE) ## Estime le nb de dimensions -c(121,206)
# nbX

comp <- imputePCA(Prof, ncp=4, scale=TRUE) ## Compl�te le tableau -c(121,206)

# compX <- imputePCA(Prof[,20:38], ncp=2, scale=TRUE) ## Compl�te le tableau -c(121,206)


?imputePCA

dev.off()

mi <- MIPCA(Prof, scale = TRUE, ncp=4) # -c(121,206)
plot(mi)

# mi <- MIPCA(Prof[,20:38], scale = TRUE, ncp=2) # -c(121,206)
# plot(mi)



head(comp$completeObs)
str(comp$completeObs)

cpt_obs = as.data.frame(comp$completeObs)
head(cpt_obs)
str(cpt_obs)
summary(cpt_obs)





# Create a `date` variable
cpt_obs$Region = rownames(cpt_obs)
# Reset the `rownames` of your original data
rownames(cpt_obs) = NULL


write.csv(cpt_obs,"PCA_AGES_GENDER_PROF_sh_COMPLETE.csv")

Prof_cpt = read.csv("PCA_AGES_GENDER_PROF_sh_COMPLETE.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(Prof_cpt)


# cptX_obs = as.data.frame(compX$completeObs)
# head(cptX_obs)
# str(cptX_obs)


# Prof_cpt = cbind(cpt_obs, cptX_obs, Prof[,39])
# str(Prof_cpt)
# summary(Prof_cpt)

# RENAME A COLUMN
# colnames(Prof_cpt)[39] <- "Geo"


# Preparing merge with PRODUCTIVITY
na = read.csv("EU_NO_DUP_reg.csv",header=TRUE,sep=",", dec=".")  # ROW 1 NAME as 1st column , row.names = 1
str(na)
head(na)
summary(na)


# NO OMITTED VARIABLES AND NO DUPLICATES
pdty = na.omit(na)
str(pdty)

rownames(pdty)
colnames(pdty)


# # Create a `date` variable
# Prof_cpt$Region = rownames(Prof_cpt)
# # Reset the `rownames` of your original data
# rownames(Prof_cpt) = NULL


merged = merge(Prof_cpt, pdty[,1:19], by = c("Region"), all.y = T)
head(merged)
summary(merged)
str(merged)

write.csv(merged,"FULL_PROF.csv")



#################################
# From WIDE to LONG for PIVOT TABLE data MANIPULATION (Year time series for PRE/CRISIS/POST calculations
# From WIDE to LONG for SCATTER plots and FACETTING
# WIDE to LONG for categories (2/3) - USUALLY DONE FOR YEARS (Time Series) for PlotMEANS and PANEL DATA ANALYSIS
#################################


# Professional STATUS Data


Prof<- read.csv("FULL_PROF_STRATA_Productivity_COMPLETE.csv", header = T)
str(Prof)
names(Prof)

Prof = Prof[,-c(33:35)]


# Pre-crisis

Prof_Pre = Prof[,c(1,grep("Pre", colnames(Prof)))]
str(Prof_Pre)
head(Prof_Pre)


# # Create a `date` variable
Prof_Pre$Period = "Pre"


# Post-crisis

Prof_Post = Prof[,c(1,grep("Post", colnames(Prof)))]
str(Prof_Post)
head(Prof_Post)


# # Create a `date` variable
Prof_Post$Period = "Post"



# Crisis

Prof_Crisis = Prof[,c(1,grep("Crisis", colnames(Prof)))]
str(Prof_Crisis)
head(Prof_Crisis)


# # Create a `date` variable
Prof_Crisis$Period = "Crisis"

write.csv(Prof_Pre,"Prof_Pre.csv")
write.csv(Prof_Crisis,"Prof_Crisis.csv")
write.csv(Prof_Post,"Prof_Post.csv")


# Prof_Pre <- unname(Prof_Pre)
# Prof_Crisis <- unname(Prof_Crisis)
# Prof_Post <- unname(Prof_Post)


# data.frame(rbind(as.matrix(df),as.matrix(df[,2:1])))
# 
# Prof_Period = rbind(as.matrix(Prof_Pre),as.matrix(Prof_Crisis),as.matrix(Prof_Post))
# head(Prof_Period)
# 
# str(Prof_Period)



Prof_Age<- read.csv("Prof_Delperiod_Alder.csv", header = T)
str(Prof_Age)
names(Prof_Age)


Prof_long = reshape(data = Prof_Age,
                       idvar = "Region",
                       varying=list(names(Prof_Age)[8:ncol(Prof_Age)]),
                       sep = "_",
                       timevar = "Age",
                    
                    times = c("Y15.24","Y25.64","Y65.74"),
                           
                       direction = "long")
#
head(Prof_long)
dim(Prof_long)

write.csv(Prof_long,"Prof_long_Age.csv")




Prof_Young<- read.csv("Prof_Delperiod_Young.csv", header = T)
str(Prof_Young)
names(Prof_Young)


Prof_long = reshape(data = Prof_Young,
                    idvar = "Region",
                    varying=list(names(Prof_Young)[8:ncol(Prof_Young)]),
                    sep = "_",
                    timevar = "Young",
                    
                    times = c("SAL","SELF"),
                    
                    direction = "long")
#
head(Prof_long)
dim(Prof_long)

write.csv(Prof_long,"Prof_long_Young.csv")



Prof_Adult<- read.csv("Prof_Delperiod_Adult.csv", header = T)
str(Prof_Adult)
names(Prof_Adult)


Prof_long = reshape(data = Prof_Adult,
                    idvar = "Region",
                    varying=list(names(Prof_Adult)[8:ncol(Prof_Adult)]),
                    sep = "_",
                    timevar = "Adult",
                    
                    times = c("SAL","SELF"),
                    
                    direction = "long")
#
head(Prof_long)
dim(Prof_long)

write.csv(Prof_long,"Prof_long_Adult.csv")




#######
# REady for Scatter plot Analysis for Prof Status!!!




#################################
# From WIDE to LONG for PIVOT TABLE data MANIPULATION (Year time series for PRE/CRISIS/POST calculations
# From WIDE to LONG for SCATTER plots and FACETTING
# WIDE to LONG for categories (2/3) - USUALLY DONE FOR YEARS (Time Series) for PlotMEANS and PANEL DATA ANALYSIS
#################################


# UNEMP data
# From WIDE to LONG


Unemp<- read.csv("FULL_Unemp_Strata_Productivity.csv", header = T)
str(Unemp)
names(Unemp)

Unemp = Unemp[,-c(30:32)]


# Pre-crisis

Unemp_Pre = Unemp[,c(1,grep("Pre", colnames(Unemp)))]
str(Unemp_Pre)
head(Unemp_Pre)


# # Create a `date` variable
Unemp_Pre$Period = "Pre"


# Post-crisis

Unemp_Post = Unemp[,c(1,grep("Post", colnames(Unemp)))]
str(Unemp_Post)
head(Unemp_Post)


# # Create a `date` variable
Unemp_Post$Period = "Post"



# Crisis

Unemp_Crisis = Unemp[,c(1,grep("Crisis", colnames(Unemp)))]
str(Unemp_Crisis)
head(Unemp_Crisis)


# # Create a `date` variable
Unemp_Crisis$Period = "Crisis"

write.csv(Unemp_Pre,"Unemp_Pre.csv")
write.csv(Unemp_Crisis,"Unemp_Crisis.csv")
write.csv(Unemp_Post,"Unemp_Post.csv")


# Unemp_Pre <- unname(Unemp_Pre)
# Unemp_Crisis <- unname(Unemp_Crisis)
# Unemp_Post <- unname(Unemp_Post)


# data.frame(rbind(as.matrix(df),as.matrix(df[,2:1])))
# 
# Unemp_Period = rbind(as.matrix(Unemp_Pre),as.matrix(Unemp_Crisis),as.matrix(Unemp_Post))
# head(Unemp_Period)
# 
# str(Unemp_Period)



Unemp_Young_G<- read.csv("Unemp_Delperiode_Young.csv", header = T)
str(Unemp_Young_G)
names(Unemp_Young_G)


Unemp_long = reshape(data = Unemp_Young_G,
                    idvar = "Region",
                    varying=list(names(Unemp_Young_G)[8:ncol(Unemp_Young_G)]),
                    sep = "_",
                    timevar = "Gender",
                    
                    times = c("T","M","F"),
                    
                    direction = "long")
#
head(Unemp_long)
dim(Unemp_long)

write.csv(Unemp_long,"Unemp_long_Young_G.csv")




Unemp_Adults<- read.csv("Unemp_Delperiode_Adults.csv", header = T)
str(Unemp_Adults)
names(Unemp_Adults)


Unemp_long = reshape(data = Unemp_Adults,
                    idvar = "Region",
                    varying=list(names(Unemp_Adults)[8:ncol(Unemp_Adults)]),
                    sep = "_",
                    timevar = "Gender",
                    
                    times = c("T","M","F"),
                    
                    direction = "long")
#
head(Unemp_long)
dim(Unemp_long)

write.csv(Unemp_long,"Unemp_long_Adults.csv")


#######
# REady for Scatter plot Analysis for Unemp Status!!!



#################################
# From WIDE to LONG for PIVOT TABLE data MANIPULATION (Year time series for PRE/CRISIS/POST calculations
# From WIDE to LONG for SCATTER plots and FACETTING
# WIDE to LONG for categories (2/3) - USUALLY DONE FOR YEARS (Time Series) for PlotMEANS and PANEL DATA ANALYSIS
#################################


#################################
# EDUC datawith MORE STRATA!!!
#################################

# From WIDE to LONG


Educ<- read.csv("FULL_Educ_Strata_Productivity.csv", header = T)
str(Educ)
names(Educ)

Educ = Educ[,-c(69:71)]


# Pre-crisis

Educ_Pre = Educ[,c(1,grep("Pre", colnames(Educ)))]
str(Educ_Pre)
head(Educ_Pre)


# # Create a `date` variable
Educ_Pre$Period = "Pre"


# Post-crisis

Educ_Post = Educ[,c(1,grep("Post", colnames(Educ)))]
str(Educ_Post)
head(Educ_Post)


# # Create a `date` variable
Educ_Post$Period = "Post"



# Crisis

Educ_Crisis = Educ[,c(1,grep("Crisis", colnames(Educ)))]
str(Educ_Crisis)
head(Educ_Crisis)


# # Create a `date` variable
Educ_Crisis$Period = "Crisis"

write.csv(Educ_Pre,"Educ_Pre.csv")
write.csv(Educ_Crisis,"Educ_Crisis.csv")
write.csv(Educ_Post,"Educ_Post.csv")


# Educ_Pre <- unname(Educ_Pre)
# Educ_Crisis <- unname(Educ_Crisis)
# Educ_Post <- unname(Educ_Post)


# data.frame(rbind(as.matrix(df),as.matrix(df[,2:1])))
# 
# Educ_Period = rbind(as.matrix(Educ_Pre),as.matrix(Educ_Crisis),as.matrix(Educ_Post))
# head(Educ_Period)
# 
# str(Educ_Period)




############ YOUNG ... 15-24


Educ_Young_G_ED0<- read.csv("Educ_STRATA_Young_ED0_G.csv", header = T)
str(Educ_Young_G_ED0)
names(Educ_Young_G_ED0)


Educ_long = reshape(data = Educ_Young_G_ED0,
                     idvar = "Region",
                     varying=list(names(Educ_Young_G_ED0)[8:ncol(Educ_Young_G_ED0)]),
                     sep = "_",
                     timevar = "Gender",
                     
                     times = c("T","M","F"),
                     
                     direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Young_G_ED0.csv")



Educ_Young_G_ED3<- read.csv("Educ_STRATA_Young_ED3_G.csv", header = T)
str(Educ_Young_G_ED3)
names(Educ_Young_G_ED3)


Educ_long = reshape(data = Educ_Young_G_ED3,
                    idvar = "Region",
                    varying=list(names(Educ_Young_G_ED3)[8:ncol(Educ_Young_G_ED3)]),
                    sep = "_",
                    timevar = "Gender",
                    
                    times = c("T","M","F"),
                    
                    direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Young_G_ED3.csv")



Educ_Young_G_ED5<- read.csv("Educ_STRATA_Young_ED5_G.csv", header = T)
str(Educ_Young_G_ED5)
names(Educ_Young_G_ED5)


Educ_long = reshape(data = Educ_Young_G_ED5,
                    idvar = "Region",
                    varying=list(names(Educ_Young_G_ED5)[8:ncol(Educ_Young_G_ED5)]),
                    sep = "_",
                    timevar = "Gender",
                    
                    times = c("T","M","F"),
                    
                    direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Young_G_ED5.csv")




Educ_Young_ED<- read.csv("Educ_STRATA_Young_ED.csv", header = T)
str(Educ_Young_ED)
names(Educ_Young_ED)


Educ_long = reshape(data = Educ_Young_ED,
                    idvar = "Region",
                    varying=list(names(Educ_Young_ED)[8:ncol(Educ_Young_ED)]),
                    sep = "_",
                    timevar = "Educ",
                    
                    times = c("ED0","ED3","ED5"),
                    
                    direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Young_ED.csv")




############ ADULTS ... 25-64


Educ_Adults_G_ED0<- read.csv("Educ_STRATA_Adults_ED0_G.csv", header = T)
str(Educ_Adults_G_ED0)
names(Educ_Adults_G_ED0)


Educ_long = reshape(data = Educ_Adults_G_ED0,
                    idvar = "Region",
                    varying=list(names(Educ_Adults_G_ED0)[8:ncol(Educ_Adults_G_ED0)]),
                    sep = "_",
                    timevar = "Gender",
                    
                    times = c("T","M","F"),
                    
                    direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Adults_G_ED0.csv")



Educ_Adults_G_ED3<- read.csv("Educ_STRATA_Adults_ED3_G.csv", header = T)
str(Educ_Adults_G_ED3)
names(Educ_Adults_G_ED3)


Educ_long = reshape(data = Educ_Adults_G_ED3,
                    idvar = "Region",
                    varying=list(names(Educ_Adults_G_ED3)[8:ncol(Educ_Adults_G_ED3)]),
                    sep = "_",
                    timevar = "Gender",
                    
                    times = c("T","M","F"),
                    
                    direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Adults_G_ED3.csv")



Educ_Adults_G_ED5<- read.csv("Educ_STRATA_Adults_ED5_G.csv", header = T)
str(Educ_Adults_G_ED5)
names(Educ_Adults_G_ED5)


Educ_long = reshape(data = Educ_Adults_G_ED5,
                    idvar = "Region",
                    varying=list(names(Educ_Adults_G_ED5)[8:ncol(Educ_Adults_G_ED5)]),
                    sep = "_",
                    timevar = "Gender",
                    
                    times = c("T","M","F"),
                    
                    direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Adults_G_ED5.csv")



Educ_Adults_ED<- read.csv("Educ_STRATA_Adults_ED.csv", header = T)
str(Educ_Adults_ED)
names(Educ_Adults_ED)

Educ_long = reshape(data = Educ_Adults_ED,
                    idvar = "Region",
                    varying=list(names(Educ_Adults_ED)[8:ncol(Educ_Adults_ED)]),
                    sep = "_",
                    timevar = "Educ",
                    
                    times = c("ED0","ED3","ED5"),
                    
                    direction = "long")
#
head(Educ_long)
dim(Educ_long)

write.csv(Educ_long,"Educ_long_Adults_ED.csv")




