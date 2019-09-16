# CONTENU

# DATA MERGING OF DELPERIODE AGREGATES BY DELPERIOD - PRE -CRISIS - POST
# WIDE FORMAT STILL, NO LONG CONVERSION


# Kommunale Data

dKom_Reg <- read.csv("Kommuner_Regioner.csv", header = T)

head(dKom_Reg)
str(dKom_Reg)


# Ordering NON-Factors

# test=dBVT_L[,c(1,3)]
# head(test)
# 
# tab1<-dKom_Reg[order(-dKom_Reg[,1]),] 
# head(tab1)


dBVT_K <- read.csv("KL_Chained_B1G_Konjunktur.csv", header = T)
dBVT_L <- read.csv("KL_Current_B_B1G_Konjunktur_AV_Levels.csv", header = T)

head(dBVT_L)
str(dBVT_L)

# dBVT_K$kom<-as.numeric(dBVT_K$kom)
# dBVT_L$kom<-as.numeric(dBVT_L$kom)



# Ordering before Merging ...

dBVT_K<-dBVT_K[order(dBVT_K[,1]),] 
head(dBVT_K)



dBVT_K = dBVT_K[,c(1,3,5,6)]
head(dBVT_K)

dBVT_L = dBVT_L[,c(1,3,5,6)]
head(dBVT_L)




# Ordering before Merging ...

dBVT_L<-dBVT_L[order(dBVT_L[,1]),] 
head(dBVT_L)


dINV_K <- read.csv("KL_Chained_P51G_Konjunktur.csv", header = T)
dINV_L <- read.csv("KL_Current_D_P51G_Konjunktur_AV_Levels.csv", header = T)


head(dINV_K)
str(dINV_L)

dINV_K = dINV_K[,c(1,3,4,5)]
head(dINV_K)


dINV_L = dINV_L[,c(1,3,5,6)]
head(dINV_L)


# Ordering before Merging ...

dINV_K<-dINV_K[order(dINV_K[,1]),] 
head(dINV_K)


dINV_L<-dINV_L[order(dINV_L[,1]),] 
head(dINV_L)



dDBI_L <- read.csv("KL_Current_B_B6G_Konjunktur_AV_Levels.csv", header = T)

head(dDBI_L)
str(dDBI_L)

dDBI_L = dDBI_L[,c(1,3,5,6)]
head(dDBI_L)


# Ordering before Merging ...

dDBI_L<-dDBI_L[order(dDBI_L[,1]),] 
head(dDBI_L)



dFISk_L <- read.csv("KL_Current_D_D5_Konjunktur_AV_Levels.csv", header = T)

head(dFISk_L)
str(dFISk_L)

dFISk_L = dFISk_L[,c(1,3,5,6)]
head(dFISk_L)


# Ordering before Merging ...

dFISk_L<-dFISk_L[order(dFISk_L[,1]),] 
head(dFISk_L)



dSY_L <- read.csv("KL_Current_K_D62_Konjunktur_AV_Levels.csv", header = T)

head(dSY_L)
str(dSY_L)

dSY_L = dSY_L[,c(1,3,5,6)]
head(dSY_L)


# Ordering before Merging ...

dSY_L<-dSY_L[order(dSY_L[,1]),] 
head(dSY_L)




dEMP_H_g <- read.csv("KL_Hours_EMP_Konjunktur_AV_Levels.csv", header = T)
dEMP_H_v <- read.csv("KL_Hours_EMP_Konjunktur_Growth.csv", header = T)


head(dEMP_H_v)
str(dEMP_H_v)

dEMP_H_v = dEMP_H_v[,c(1,3,5,6)]
head(dEMP_H_v)

head(dEMP_H_g)
str(dEMP_H_g)

dEMP_H_g = dEMP_H_g[,c(1,3,5,6)]
head(dEMP_H_g)



# Ordering before Merging ...

dEMP_H_v<-dEMP_H_v[order(dEMP_H_v[,1]),] 
head(dEMP_H_v)


dEMP_H_g<-dEMP_H_g[order(dEMP_H_g[,1]),] 
head(dEMP_H_g)


dPerson_g <- read.csv("KL_Persons_X_EMP_Konjunktur_av_Levels.csv", header = T)
dPerson_v <- read.csv("KL_Persons_X_EMP_Konjunktur_Growth.csv", header = T)

head(dPerson_g)
str(dPerson_g)


dPerson_v = dPerson_v[,c(1,3,5,6)]
head(dPerson_v)


dPerson_g = dPerson_g[,c(1,3,5,6)]
head(dPerson_g)


# Ordering before Merging ...

dPerson_v<-dPerson_v[order(dPerson_v[,1]),] 
head(dPerson_v)


dPerson_g<-dPerson_g[order(dPerson_g[,1]),] 
head(dPerson_g)



### ATTENTION LABELLING of POP !!!


dPOP_g <- read.csv("KL_Persons_X_Konjunktur_POP_g.csv", header = T)
dPOP_v <- read.csv("KL_Persons_X_Konjunktur_POP_v.csv", header = T)

head(dPOP_v)
str(dPOP_v)


dPOP_g = dPOP_g[,c(1,3,5,6)]
head(dPOP_g)


dPOP_v = dPOP_v[,c(1,3,5,6)]
head(dPOP_v)



# Ordering before Merging ...

dPOP_v<-dPOP_v[order(dPOP_v[,1]),] 
head(dPOP_v)


dPOP_g<-dPOP_g[order(dPOP_g[,1]),] 
head(dPOP_g)




dYLP_g <- read.csv("KL_Chained_YL_P_Konjunktur.csv", header = T)
dYLH_g <- read.csv("KL_Chained_YL_H_Konjunktur.csv", header = T)

tail(dYLH_g)
str(dYLP_g)


dYLP_g = dYLP_g[,c(1,4,5,6)]
head(dYLP_g)


dYLH_g = dYLH_g[,c(1,4,5,6)]
head(dYLH_g)



# Ordering before Merging ...

dYLP_g<-dYLP_g[order(dYLP_g[,1]),] 
head(dYLP_g)


ddYLH_g<-dYLH_g[order(dYLH_g[,1]),] 
head(dYLH_g)







# MERGING


merge0 = merge(dKom_Reg, dBVT_K, by = c("kom"), all = T)
head(merge0)

merge1 = merge(merge0, dBVT_L, by = c("kom"), all = T)
head(merge1)

merge2 = merge(merge1, dINV_K, by = c("kom"), all = T)
head(merge2)

merge3 = merge(merge2, dINV_L, by = c("kom"), all = T)
head(merge3)

merge4 = merge(merge3, dDBI_L, by = c("kom"), all = T)
head(merge4)

merge5 = merge(merge4, dFISk_L, by = c("kom"), all = T)
head(merge5)

merge6 = merge(merge5, dSY_L, by = c("kom"), all = T)
head(merge6)

merge7 = merge(merge6, dEMP_H_g, by = c("kom"), all = T)
head(merge7)

merge8 = merge(merge7, dEMP_H_v, by = c("kom"), all = T)
head(merge8)

merge9 = merge(merge8, dPerson_g, by = c("kom"), all = T)
head(merge9)

merge10 = merge(merge9, dPerson_v, by = c("kom"), all = T)
head(merge10)





### ATTENTION LABELLING of POP !!!

merge11 = merge(merge10, dPOP_g, by = c("kom"), all = T)
head(merge11)

merge12 = merge(merge11, dPOP_v, by = c("kom"), all = T)
head(merge12)


merge13 = merge(merge12, dYLP_g, by = c("kom"), all = T)
head(merge13)


merge14 = merge(merge13, dYLH_g, by = c("kom"), all = T)
head(merge14)



# Variables from Pre (2004-2006), Crisis (2007-2009) and Post periods (2010-2015)


data_crisis = merge14[,c(1:3,grep("Crisis", colnames(merge14)))]
head(data_crisis)

data_pre = merge14[,c(1:3,grep("Pre", colnames(merge14)))]
head(data_pre)

data_post = merge14[,c(1:3,grep("Post", colnames(merge14)))]
head(data_post)


write.csv(merge14,"KL_3_Periods_LAST.csv")
# #
write.csv(data_crisis,"KL_Crisis_LAST.csv")
write.csv(data_pre,"KL_Pre_LAST.csv")
write.csv(data_post,"KL_Post_LAST.csv")




# Data Cleaned and Prepared (NO MISSINGS)


# POST

dat_Post = read.csv("KL_Post_LAST.csv", header = T)
head(dat_Post)

summary(dat_Post)
tail(dat_Post)

names(dat_Post)


dat_Post$Ypc = 1000*dat_Post$BVT_L_Post/dat_Post$POP_g_Post
dat_Post$YDpc = 1000*dat_Post$DBI_L_Post/dat_Post$POP_g_Post
dat_Post$SKpc = 1000*dat_Post$FISk_L_Post/dat_Post$POP_g_Post
dat_Post$SYpc = 1000*dat_Post$SY_L_Post/dat_Post$POP_g_Post

dat_Post$YL_T = 1000*dat_Post$BVT_L_Post/dat_Post$EMP_T_g_Post
dat_Post$YL_P = 1000*dat_Post$BVT_L_Post/dat_Post$EMP_P_g_Post

dat_Post$INVpc = 1000*dat_Post$INV_L_Post/dat_Post$POP_g_Post

dat_Post$Besk_R = dat_Post$EMP_P_g_Post/dat_Post$POP_g_Post




head(dat_Post)
names(dat_Post)


# Produktivitet NIVEAU / VÆKST

dat_Post = dat_Post[,-c(5,7:10,13)]
head(dat_Post)
names(dat_Post)


dat_Post$kom = as.factor(dat_Post$kom)
str(dat_Post)

write.csv(dat_Post,"KL_Post_LAST.csv")



# PRE

dat_Pre = read.csv("KL_Pre_LAST.csv", header = T)


summary(dat_Pre)
tail(dat_Pre)

names(dat_Pre)


dat_Pre$Ypc = 1000*dat_Pre$BVT_L_Pre/dat_Pre$POP_g_Pre
dat_Pre$YDpc = 1000*dat_Pre$DBI_L_Pre/dat_Pre$POP_g_Pre
dat_Pre$SKpc = 1000*dat_Pre$FISk_L_Pre/dat_Pre$POP_g_Pre
dat_Pre$SYpc = 1000*dat_Pre$SY_L_Pre/dat_Pre$POP_g_Pre

dat_Pre$YL_T = 1000*dat_Pre$BVT_L_Pre/dat_Pre$EMP_T_g_Pre
dat_Pre$YL_P = 1000*dat_Pre$BVT_L_Pre/dat_Pre$EMP_P_g_Pre

dat_Pre$INVpc = 1000*dat_Pre$INV_L_Pre/dat_Pre$POP_g_Pre

dat_Pre$Besk_R = dat_Pre$EMP_P_g_Pre/dat_Pre$POP_g_Pre


head(dat_Pre)
names(dat_Pre)


dat_Pre = dat_Pre[,-c(5,7:10,13)]
head(dat_Pre)
names(dat_Pre)


write.csv(dat_Pre,"KL_Pre_LAST.csv")



# Crisis

dat_Crisis = read.csv("KL_Crisis_LAST.csv", header = T)


summary(dat_Crisis)
tail(dat_Crisis)

names(dat_Crisis)


dat_Crisis$Ypc = 1000*dat_Crisis$BVT_L_Crisis/dat_Crisis$POP_g_Crisis
dat_Crisis$YDpc = 1000*dat_Crisis$DBI_L_Crisis/dat_Crisis$POP_g_Crisis
dat_Crisis$SKpc = 1000*dat_Crisis$FISk_L_Crisis/dat_Crisis$POP_g_Crisis
dat_Crisis$SYpc = 1000*dat_Crisis$SY_L_Crisis/dat_Crisis$POP_g_Crisis

dat_Crisis$YL_T = 1000*dat_Crisis$BVT_L_Crisis/dat_Crisis$EMP_T_g_Crisis
dat_Crisis$YL_P = 1000*dat_Crisis$BVT_L_Crisis/dat_Crisis$EMP_P_g_Crisis

dat_Crisis$INVpc = 1000*dat_Crisis$INV_L_Crisis/dat_Crisis$POP_g_Crisis

dat_Crisis$Besk_R = dat_Crisis$EMP_P_g_Crisis/dat_Crisis$POP_g_Crisis

head(dat_Crisis)
names(dat_Crisis)


dat_Crisis = dat_Crisis[,-c(5,7:10,13)]
head(dat_Crisis)
names(dat_Crisis)


write.csv(dat_Crisis,"KL_Crisis_LAST.csv")




# # FULL Data Set Cleaned and Prepared (NO MISSINGS)
# 
# 
# 
# dat_FULL = read.csv("KL_FULL_PCA.csv", header = T)
# head(dat_FULL)
# 
# summary(dat_FULL)
# tail(dat_FULL)
# 
# names(dat_FULL)
# 
# 
# dat_FULL$Ypc_Post = 1000*dat_FULL$BVT_L_Post/dat_FULL$POP_g_Post
# dat_FULL$YDpc_Post = 1000*dat_FULL$DBI_L_Post/dat_FULL$POP_g_Post
# dat_FULL$SKpc_Post = 1000*dat_FULL$FISk_L_Post/dat_FULL$POP_g_Post
# dat_FULL$SYpc_Post = 1000*dat_FULL$SY_L_Post/dat_FULL$POP_g_Post
# 
# dat_FULL$YL_T_Post = 1000*dat_FULL$BVT_L_Post/dat_FULL$EMP_T_g_Post
# dat_FULL$YL_P_Post = 1000*dat_FULL$BVT_L_Post/dat_FULL$EMP_P_g_Post
# 
# dat_FULL$INVpc_Post = 1000*dat_FULL$INV_L_Post/dat_FULL$POP_g_Post
# 
# dat_FULL$Besk_R_Post = dat_FULL$EMP_P_g_Post/dat_FULL$POP_g_Post
# 
# 
# 
# 
# dat_FULL$Ypc_Pre = 1000*dat_FULL$BVT_L_Pre/dat_FULL$POP_g_Pre
# dat_FULL$YDpc_Pre = 1000*dat_FULL$DBI_L_Pre/dat_FULL$POP_g_Pre
# dat_FULL$SKpc_Pre = 1000*dat_FULL$FISk_L_Pre/dat_FULL$POP_g_Pre
# dat_FULL$SYpc_Pre = 1000*dat_FULL$SY_L_Pre/dat_FULL$POP_g_Pre
# 
# dat_FULL$YL_T_Pre = 1000*dat_FULL$BVT_L_Pre/dat_FULL$EMP_T_g_Pre
# dat_FULL$YL_P_Pre = 1000*dat_FULL$BVT_L_Pre/dat_FULL$EMP_P_g_Pre
# 
# dat_FULL$INVpc_Pre = 1000*dat_FULL$INV_L_Pre/dat_FULL$POP_g_Pre
# 
# dat_FULL$Besk_R_Pre = dat_FULL$EMP_P_g_Pre/dat_FULL$POP_g_Pre
# 
# 
# 
# 
# dat_FULL$Ypc_Crisis = 1000*dat_FULL$BVT_L_Crisis/dat_FULL$POP_g_Crisis
# dat_FULL$YDpc_Crisis = 1000*dat_FULL$DBI_L_Crisis/dat_FULL$POP_g_Crisis
# dat_FULL$SKpc_Crisis = 1000*dat_FULL$FISk_L_Crisis/dat_FULL$POP_g_Crisis
# dat_FULL$SYpc_Crisis = 1000*dat_FULL$SY_L_Crisis/dat_FULL$POP_g_Crisis
# 
# dat_FULL$YL_T_Crisis = 1000*dat_FULL$BVT_L_Crisis/dat_FULL$EMP_T_g_Crisis
# dat_FULL$YL_P_Crisis = 1000*dat_FULL$BVT_L_Crisis/dat_FULL$EMP_P_g_Crisis
# 
# dat_FULL$INVpc_Crisis = 1000*dat_FULL$INV_L_Crisis/dat_FULL$POP_g_Crisis
# 
# dat_FULL$Besk_R_Crisis = dat_FULL$EMP_P_g_Crisis/dat_FULL$POP_g_Crisis
# 
# 
# colnames(dat_FULL)
# 
# 
# dat_FULL = dat_FULL[,-c(1,8,9,10,14:25)]
# 
# 
# head(dat_FULL)
# 
# 
# write.csv(dat_FULL,"KL_PCA_FULL.csv")
# 
# 
# 
# 
# 
# 
# 
# dat_Post = dat_Post[,-c(5,7:11,13)]
# head(dat_Post)
# names(dat_Post)
# 
# 
# dat_Post$kom = as.factor(dat_Post$kom)
# str(dat_Post)
# 
# write.csv(dat_Post,"KL_Post_PCA.csv")
# 







dKom_Reg <- read.csv("Kommuner_Regioner.csv", header = T)


# Subsetting rows per group and columns per variable

KL_B=subset(KL,KL$prenh=="B")
head(KL_B)
str(KL_B)


