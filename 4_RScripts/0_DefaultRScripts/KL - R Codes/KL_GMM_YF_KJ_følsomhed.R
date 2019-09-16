



library(ggplot2)
library(reshape2)
library(psych)




data_ref<- read.csv("KL_3_Periods_NUM_PCA.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_ref)
head(data_ref)

data_ref=data_ref[,c(1,2,3)]


data_NUM_YF_T<- read.csv("YF_T_GMM.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM_YF_T)
head(data_NUM_YF_T)

summary(data_NUM_YF_T[,3])


data_NUM_YF_P<- read.csv("YF_P_GMM.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM_YF_P)
head(data_NUM_YF_P)

summary(data_NUM_YF_P[,3])



data_NUM_BVT_P<- read.csv("BVT_P_GMM.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM_BVT_P)
tail(data_NUM_BVT_P)


data_NUM_EMP_T<- read.csv("EMP_T_GMM.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM_EMP_T)
head(data_NUM_EMP_T)


data_NUM_EMP_P<- read.csv("EMP_P_GMM.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM_EMP_P)
head(data_NUM_EMP_P)


# SHORTER SAMPLE
data_NUM_INV_F<- read.csv("GMM_INV_F.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_NUM_INV_F)
head(data_NUM_INV_F)

summary(data_NUM_YF_P[,3])



merge_ref = merge(data_ref, data_NUM_YF_T, by = c("kom"), all = T)
tail(merge_ref)

summary(merge_ref$Region_navn)


# write.csv(merge_ref,"GMM_FASTE_YF_T.csv")


data_ref<- read.csv("GMM_FASTE_YF_T.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
str(data_ref)


# ?reshape
# 
long_ref = reshape(data = data_ref,
               idvar = "kom",
               varying = colnames(data_ref)[-c(1,2,3,4)],
               sep = ".",
               timevar = "Year",
               times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
               direction = "long")
# 
head(long_ref)

# write.csv(long_ref,"GMM_FASTE_YF_T_LONG.csv")


# # ADDING Y_L_T Components (Originally FULL Time Series)
# data_Comp_wide<- read.csv("YL_Components_År_WIDE.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
# str(data_Comp_wide)
# head(data_Comp_wide)
# tail(data_Comp_wide)



# ?reshape
# 
data_YF_P_long = reshape(data = data_NUM_YF_P,
                         idvar = "kom",
                         varying = colnames(data_NUM_YF_P[-c(1)]),
                         sep = ".",
                         timevar = "Year",
                         times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
                         direction = "long")
#
head(data_YF_P_long)
tail(data_YF_P_long)
str(data_YF_P_long)


# write.csv(data_YF_P_long,"GMM_YF_P_long.csv")



data_BVT_F_long = reshape(data = data_NUM_BVT_P,
                         idvar = "kom",
                         varying = colnames(data_NUM_BVT_P[-c(1)]),
                         sep = ".",
                         timevar = "Year",
                         times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
                         direction = "long")
#
head(data_BVT_F_long)
tail(data_BVT_F_long)
str(data_BVT_F_long)


# write.csv(data_BVT_F_long,"GMM_BVT_F_long.csv")



data_EMP_T_long = reshape(data = data_NUM_EMP_T,
                          idvar = "kom",
                          varying = colnames(data_NUM_EMP_T[-c(1)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
                          direction = "long")
#
head(data_EMP_T_long)
tail(data_EMP_T_long)
str(data_EMP_T_long)


# write.csv(data_EMP_T_long,"GMM_EMP_T_long.csv")


data_EMP_P_long = reshape(data = data_NUM_EMP_P,
                          idvar = "kom",
                          varying = colnames(data_NUM_EMP_P[-c(1)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
                          direction = "long")
#
head(data_EMP_P_long)
tail(data_EMP_P_long)
str(data_EMP_P_long)


# write.csv(data_EMP_P_long,"GMM_EMP_P_long.csv")



data_INV_F_long = reshape(data = data_NUM_INV_F,
                          idvar = "kom",
                          varying = colnames(data_NUM_INV_F[-c(1)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007, 2008,2009,2010,2011,2012, 2013, 2014, 2015),
                          direction = "long")
#
head(data_INV_F_long)
tail(data_INV_F_long)
str(data_INV_F_long)


# write.csv(data_INV_F_long,"GMM_INV_F_long.csv")





# READ ALL NEAT LONG Variables!!!

data_ref_long<- read.csv("GMM_FASTE_YF_T_LONG_ref.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_ref_long)


data_YF_P_long<- read.csv("GMM_YF_P_long.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_YF_P_long)


data_BVT_F_long<- read.csv("GMM_BVT_F_long.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_BVT_F_long)


data_EMP_T_long<- read.csv("GMM_EMP_T_long.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_EMP_T_long)


data_EMP_P_long<- read.csv("GMM_EMP_P_long.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_EMP_P_long)

data_INV_F_long<- read.csv("GMM_INV_F_long.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_INV_F_long)


# NEW FULLER SAMPLE - BUT INVESTMENTS shorter ...

merge1 = merge(data_ref_long, data_YF_P_long, by = c("kom",("Year")), all = T)
tail(merge1)

merge2 = merge(merge1, data_BVT_F_long, by = c("kom",("Year")), all = T)
tail(merge2)

merge3 = merge(merge2, data_EMP_T_long, by = c("kom",("Year")), all = T)
tail(merge3)

merge4 = merge(merge3, data_EMP_P_long, by = c("kom",("Year")), all = T)
tail(merge4)

merge5 = merge(merge4, data_INV_F_long, by = c("kom",("Year")), all = T)
tail(merge5)

write.csv(merge5,"GMM_LONG_FULL_INV.csv")


##############################################
######### END of DATA Manipulation and Merging!
##############################################


data_GMM<- read.csv("GMM_LONG_FULL_dummies.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_GMM)

summary(data_GMM$Landsdel_navn)

638/22
374/22

# NO DK AND NO "Uden REgion" ALSO!
data_GMM_DK = subset(data_GMM, data_GMM$kom==999)
head(data_GMM_DK)

# write.csv(data_GMM_DK,"GMM_data_DK.csv")

# NO DK AND NO "Uden REgion" ALSO!
data_GMM_sh = subset(data_GMM, !(data_GMM$kom==961 | data_GMM$kom==999))
head(data_GMM_sh, 50)
str(data_GMM_sh)
unique(data_GMM_sh$kom)
unique(data_GMM_sh$Region_navn)

levels(data_GMM_sh$Region_navn)

data_GMM_sh$Region_navn = factor(data_GMM_sh$Region_navn)

# r1= Hovedstaden, r2= Midtjylland, r3= Nordjylland, r4= Ø-kommuner, r5= Nordsjælland, r6= Syddanmark.

summary(data_GMM$Landsdel_navn)

# l1= Byen København, l2= Fyn, l3= Københavns omegn, l4= Nordjylland, l5= Sjælland, l6= Østjylland.
# l7= Østsjælland, l8= Sydjylland, l9= Vest- og Sydsjælland, l10= Vestjylland


data_GMM_sh$BVT_o1 = data_GMM_sh$BVT_F*data_GMM_sh$o1

data_GMM_sh$YF_T_o1 = data_GMM_sh$YF_T*data_GMM_sh$o1
data_GMM_sh$YF_P_o1 = data_GMM_sh$YF_P*data_GMM_sh$o1

data_GMM_sh$YF_T_l1 = data_GMM_sh$YF_T*data_GMM_sh$l1
data_GMM_sh$YF_P_l1 = data_GMM_sh$YF_P*data_GMM_sh$l1

data_GMM_sh$YF_T_l2 = data_GMM_sh$YF_T*data_GMM_sh$l2
data_GMM_sh$YF_P_l2 = data_GMM_sh$YF_P*data_GMM_sh$l2

data_GMM_sh$YF_T_l3 = data_GMM_sh$YF_T*data_GMM_sh$l3
data_GMM_sh$YF_P_l3 = data_GMM_sh$YF_P*data_GMM_sh$l3

data_GMM_sh$YF_T_l4 = data_GMM_sh$YF_T*data_GMM_sh$l4
data_GMM_sh$YF_P_l4 = data_GMM_sh$YF_P*data_GMM_sh$l4

data_GMM_sh$YF_T_l5 = data_GMM_sh$YF_T*data_GMM_sh$l5
data_GMM_sh$YF_P_l5 = data_GMM_sh$YF_P*data_GMM_sh$l5

data_GMM_sh$YF_T_l6 = data_GMM_sh$YF_T*data_GMM_sh$l6
data_GMM_sh$YF_P_l6 = data_GMM_sh$YF_P*data_GMM_sh$l6

data_GMM_sh$YF_T_l7 = data_GMM_sh$YF_T*data_GMM_sh$l7
data_GMM_sh$YF_P_l7 = data_GMM_sh$YF_P*data_GMM_sh$l7

data_GMM_sh$YF_T_l8 = data_GMM_sh$YF_T*data_GMM_sh$l8
data_GMM_sh$YF_P_l8 = data_GMM_sh$YF_P*data_GMM_sh$l8

data_GMM_sh$YF_T_l9 = data_GMM_sh$YF_T*data_GMM_sh$l9
data_GMM_sh$YF_P_l9 = data_GMM_sh$YF_P*data_GMM_sh$l9

data_GMM_sh$YF_T_l10 = data_GMM_sh$YF_T*data_GMM_sh$l10
data_GMM_sh$YF_P_l10 = data_GMM_sh$YF_P*data_GMM_sh$l10




data_GMM_sh$BVT_o1_03 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d03
data_GMM_sh$BVT_o1_05 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d05

data_GMM_sh$BVT_o1_04 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d04
data_GMM_sh$BVT_o1_07 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d07
data_GMM_sh$BVT_o1_06 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d06

data_GMM_sh$BVT_o1_08 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d08
data_GMM_sh$BVT_o1_09 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d09
data_GMM_sh$BVT_o1_10 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d10
data_GMM_sh$BVT_o1_11 = data_GMM_sh$BVT_F*data_GMM_sh$o1*data_GMM_sh$d11


data_GMM_sh$BVT_r1 = data_GMM_sh$BVT_F*data_GMM_sh$r1
data_GMM_sh$BVT_r2 = data_GMM_sh$BVT_F*data_GMM_sh$r2
data_GMM_sh$BVT_r3 = data_GMM_sh$BVT_F*data_GMM_sh$r3
data_GMM_sh$BVT_r4 = data_GMM_sh$BVT_F*data_GMM_sh$r4
data_GMM_sh$BVT_r5 = data_GMM_sh$BVT_F*data_GMM_sh$r5
data_GMM_sh$BVT_r6 = data_GMM_sh$BVT_F*data_GMM_sh$r6


data_GMM_sh$YF_T_r1 = data_GMM_sh$YF_T*data_GMM_sh$r1
data_GMM_sh$YF_P_r1 = data_GMM_sh$YF_P*data_GMM_sh$r1

data_GMM_sh$YF_T_r2 = data_GMM_sh$YF_T*data_GMM_sh$r2
data_GMM_sh$YF_P_r2 = data_GMM_sh$YF_P*data_GMM_sh$r2

data_GMM_sh$YF_T_r3 = data_GMM_sh$YF_T*data_GMM_sh$r3
data_GMM_sh$YF_P_r3 = data_GMM_sh$YF_P*data_GMM_sh$r3

data_GMM_sh$YF_T_r4 = data_GMM_sh$YF_T*data_GMM_sh$r4
data_GMM_sh$YF_P_r4 = data_GMM_sh$YF_P*data_GMM_sh$r4

data_GMM_sh$YF_T_r5 = data_GMM_sh$YF_T*data_GMM_sh$r5
data_GMM_sh$YF_P_r5 = data_GMM_sh$YF_P*data_GMM_sh$r5

data_GMM_sh$YF_T_r6 = data_GMM_sh$YF_T*data_GMM_sh$r6
data_GMM_sh$YF_P_r6 = data_GMM_sh$YF_P*data_GMM_sh$r6



data_GMM_sh$BVT_r1_03 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d03
data_GMM_sh$BVT_r1_05 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d05

data_GMM_sh$BVT_r1_04 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d04
data_GMM_sh$BVT_r1_07 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d07
data_GMM_sh$BVT_r1_06 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d06


data_GMM_sh$BVT_r1_08 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d08
data_GMM_sh$BVT_r1_09 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d09
data_GMM_sh$BVT_r1_10 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d10
data_GMM_sh$BVT_r1_11 = data_GMM_sh$BVT_F*data_GMM_sh$r1*data_GMM_sh$d11


data_GMM_sh$BVT_r2_03 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d03
data_GMM_sh$BVT_r2_05 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d05

data_GMM_sh$BVT_r2_04 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d04
data_GMM_sh$BVT_r2_07 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d07
data_GMM_sh$BVT_r2_06 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d06

data_GMM_sh$BVT_r2_08 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d08
data_GMM_sh$BVT_r2_09 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d09
data_GMM_sh$BVT_r2_10 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d10
data_GMM_sh$BVT_r2_11 = data_GMM_sh$BVT_F*data_GMM_sh$r2*data_GMM_sh$d11


data_GMM_sh$BVT_r3_03 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d03
data_GMM_sh$BVT_r3_05 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d05

data_GMM_sh$BVT_r3_04 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d04
data_GMM_sh$BVT_r3_07 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d07
data_GMM_sh$BVT_r3_06 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d06

data_GMM_sh$BVT_r3_08 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d08
data_GMM_sh$BVT_r3_09 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d09
data_GMM_sh$BVT_r3_10 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d10
data_GMM_sh$BVT_r3_11 = data_GMM_sh$BVT_F*data_GMM_sh$r3*data_GMM_sh$d11


data_GMM_sh$BVT_r4_03 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d03
data_GMM_sh$BVT_r4_05 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d05

data_GMM_sh$BVT_r4_04 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d04
data_GMM_sh$BVT_r4_07 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d07
data_GMM_sh$BVT_r4_06 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d06

data_GMM_sh$BVT_r4_08 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d08
data_GMM_sh$BVT_r4_09 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d09
data_GMM_sh$BVT_r4_10 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d10
data_GMM_sh$BVT_r4_11 = data_GMM_sh$BVT_F*data_GMM_sh$r4*data_GMM_sh$d11


data_GMM_sh$BVT_r5_03 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d03
data_GMM_sh$BVT_r5_05 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d05

data_GMM_sh$BVT_r5_04 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d04
data_GMM_sh$BVT_r5_07 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d07
data_GMM_sh$BVT_r5_06 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d06

data_GMM_sh$BVT_r5_08 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d08
data_GMM_sh$BVT_r5_09 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d09
data_GMM_sh$BVT_r5_10 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d10
data_GMM_sh$BVT_r5_11 = data_GMM_sh$BVT_F*data_GMM_sh$r5*data_GMM_sh$d11


data_GMM_sh$BVT_r6_03 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d03
data_GMM_sh$BVT_r6_05 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d05

data_GMM_sh$BVT_r6_04 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d04
data_GMM_sh$BVT_r6_07 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d07
data_GMM_sh$BVT_r6_06 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d06

data_GMM_sh$BVT_r6_08 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d08
data_GMM_sh$BVT_r6_09 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d09
data_GMM_sh$BVT_r6_10 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d10
data_GMM_sh$BVT_r6_11 = data_GMM_sh$BVT_F*data_GMM_sh$r6*data_GMM_sh$d11



# data_GMM_sh$EMP_T_o1 = data_GMM_sh$EMP_T*data_GMM_sh$o1
# 
# data_GMM_sh$EMP_T_o1_04 = data_GMM_sh$EMP_T*data_GMM_sh$o1*data_GMM_sh$d04
# data_GMM_sh$EMP_T_o1_06 = data_GMM_sh$EMP_T*data_GMM_sh$o1*data_GMM_sh$d06
# data_GMM_sh$EMP_T_o1_07 = data_GMM_sh$EMP_T*data_GMM_sh$o1*data_GMM_sh$d07
# 
# 
# data_GMM_sh$EMP_T_o1_08 = data_GMM_sh$EMP_T*data_GMM_sh$o1*data_GMM_sh$d08
# data_GMM_sh$EMP_T_o1_09 = data_GMM_sh$EMP_T*data_GMM_sh$o1*data_GMM_sh$d09
# data_GMM_sh$EMP_T_o1_10 = data_GMM_sh$EMP_T*data_GMM_sh$o1*data_GMM_sh$d10
# data_GMM_sh$EMP_T_o1_11 = data_GMM_sh$EMP_T*data_GMM_sh$o1*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_T_r1 = data_GMM_sh$EMP_T*data_GMM_sh$r1
# data_GMM_sh$EMP_T_r2 = data_GMM_sh$EMP_T*data_GMM_sh$r2
# data_GMM_sh$EMP_T_r3 = data_GMM_sh$EMP_T*data_GMM_sh$r3
# data_GMM_sh$EMP_T_r4 = data_GMM_sh$EMP_T*data_GMM_sh$r4
# data_GMM_sh$EMP_T_r5 = data_GMM_sh$EMP_T*data_GMM_sh$r5
# data_GMM_sh$EMP_T_r6 = data_GMM_sh$EMP_T*data_GMM_sh$r6
# 
# 
# data_GMM_sh$EMP_T_r1_04 = data_GMM_sh$EMP_T*data_GMM_sh$r1*data_GMM_sh$d04
# data_GMM_sh$EMP_T_r1_06 = data_GMM_sh$EMP_T*data_GMM_sh$r1*data_GMM_sh$d06
# data_GMM_sh$EMP_T_r1_07 = data_GMM_sh$EMP_T*data_GMM_sh$r1*data_GMM_sh$d07
# 
# data_GMM_sh$EMP_T_r1_08 = data_GMM_sh$EMP_T*data_GMM_sh$r1*data_GMM_sh$d08
# data_GMM_sh$EMP_T_r1_09 = data_GMM_sh$EMP_T*data_GMM_sh$r1*data_GMM_sh$d09
# data_GMM_sh$EMP_T_r1_10 = data_GMM_sh$EMP_T*data_GMM_sh$r1*data_GMM_sh$d10
# data_GMM_sh$EMP_T_r1_11 = data_GMM_sh$EMP_T*data_GMM_sh$r1*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_T_r2_04 = data_GMM_sh$EMP_T*data_GMM_sh$r2*data_GMM_sh$d04
# data_GMM_sh$EMP_T_r2_06 = data_GMM_sh$EMP_T*data_GMM_sh$r2*data_GMM_sh$d06
# data_GMM_sh$EMP_T_r2_07 = data_GMM_sh$EMP_T*data_GMM_sh$r2*data_GMM_sh$d07
# 
# data_GMM_sh$EMP_T_r2_08 = data_GMM_sh$EMP_T*data_GMM_sh$r2*data_GMM_sh$d08
# data_GMM_sh$EMP_T_r2_09 = data_GMM_sh$EMP_T*data_GMM_sh$r2*data_GMM_sh$d09
# data_GMM_sh$EMP_T_r2_10 = data_GMM_sh$EMP_T*data_GMM_sh$r2*data_GMM_sh$d10
# data_GMM_sh$EMP_T_r2_11 = data_GMM_sh$EMP_T*data_GMM_sh$r2*data_GMM_sh$d11
# 
# 
# 
# data_GMM_sh$EMP_T_r3_04 = data_GMM_sh$EMP_T*data_GMM_sh$r3*data_GMM_sh$d04
# data_GMM_sh$EMP_T_r3_06 = data_GMM_sh$EMP_T*data_GMM_sh$r3*data_GMM_sh$d06
# data_GMM_sh$EMP_T_r3_07 = data_GMM_sh$EMP_T*data_GMM_sh$r3*data_GMM_sh$d07
# 
# data_GMM_sh$EMP_T_r3_08 = data_GMM_sh$EMP_T*data_GMM_sh$r3*data_GMM_sh$d08
# data_GMM_sh$EMP_T_r3_09 = data_GMM_sh$EMP_T*data_GMM_sh$r3*data_GMM_sh$d09
# data_GMM_sh$EMP_T_r3_10 = data_GMM_sh$EMP_T*data_GMM_sh$r3*data_GMM_sh$d10
# data_GMM_sh$EMP_T_r3_11 = data_GMM_sh$EMP_T*data_GMM_sh$r3*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_T_r4_04 = data_GMM_sh$EMP_T*data_GMM_sh$r4*data_GMM_sh$d04
# data_GMM_sh$EMP_T_r4_06 = data_GMM_sh$EMP_T*data_GMM_sh$r4*data_GMM_sh$d06
# data_GMM_sh$EMP_T_r4_07 = data_GMM_sh$EMP_T*data_GMM_sh$r4*data_GMM_sh$d07
# 
# data_GMM_sh$EMP_T_r4_08 = data_GMM_sh$EMP_T*data_GMM_sh$r4*data_GMM_sh$d08
# data_GMM_sh$EMP_T_r4_09 = data_GMM_sh$EMP_T*data_GMM_sh$r4*data_GMM_sh$d09
# data_GMM_sh$EMP_T_r4_10 = data_GMM_sh$EMP_T*data_GMM_sh$r4*data_GMM_sh$d10
# data_GMM_sh$EMP_T_r4_11 = data_GMM_sh$EMP_T*data_GMM_sh$r4*data_GMM_sh$d11
# 
# 
# 
# data_GMM_sh$EMP_T_r5_04 = data_GMM_sh$EMP_T*data_GMM_sh$r5*data_GMM_sh$d04
# data_GMM_sh$EMP_T_r5_06 = data_GMM_sh$EMP_T*data_GMM_sh$r5*data_GMM_sh$d06
# data_GMM_sh$EMP_T_r5_07 = data_GMM_sh$EMP_T*data_GMM_sh$r5*data_GMM_sh$d07
# 
# data_GMM_sh$EMP_T_r5_08 = data_GMM_sh$EMP_T*data_GMM_sh$r5*data_GMM_sh$d08
# data_GMM_sh$EMP_T_r5_09 = data_GMM_sh$EMP_T*data_GMM_sh$r5*data_GMM_sh$d09
# data_GMM_sh$EMP_T_r5_10 = data_GMM_sh$EMP_T*data_GMM_sh$r5*data_GMM_sh$d10
# data_GMM_sh$EMP_T_r5_11 = data_GMM_sh$EMP_T*data_GMM_sh$r5*data_GMM_sh$d11
# 
# 
# 
# 
# 
# data_GMM_sh$EMP_P_o1 = data_GMM_sh$EMP_P*data_GMM_sh$o1
# 
# data_GMM_sh$EMP_P_o1_04 = data_GMM_sh$EMP_P*data_GMM_sh$o1*data_GMM_sh$d04
# data_GMM_sh$EMP_P_o1_07 = data_GMM_sh$EMP_P*data_GMM_sh$o1*data_GMM_sh$d07
# data_GMM_sh$EMP_P_o1_06 = data_GMM_sh$EMP_P*data_GMM_sh$o1*data_GMM_sh$d06
# 
# 
# data_GMM_sh$EMP_P_o1_08 = data_GMM_sh$EMP_P*data_GMM_sh$o1*data_GMM_sh$d08
# data_GMM_sh$EMP_P_o1_09 = data_GMM_sh$EMP_P*data_GMM_sh$o1*data_GMM_sh$d09
# data_GMM_sh$EMP_P_o1_10 = data_GMM_sh$EMP_P*data_GMM_sh$o1*data_GMM_sh$d10
# data_GMM_sh$EMP_P_o1_11 = data_GMM_sh$EMP_P*data_GMM_sh$o1*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_P_r1 = data_GMM_sh$EMP_P*data_GMM_sh$r1
# data_GMM_sh$EMP_P_r2 = data_GMM_sh$EMP_P*data_GMM_sh$r2
# data_GMM_sh$EMP_P_r3 = data_GMM_sh$EMP_P*data_GMM_sh$r3
# data_GMM_sh$EMP_P_r4 = data_GMM_sh$EMP_P*data_GMM_sh$r4
# data_GMM_sh$EMP_P_r5 = data_GMM_sh$EMP_P*data_GMM_sh$r5
# data_GMM_sh$EMP_P_r6 = data_GMM_sh$EMP_P*data_GMM_sh$r6
# 
# 
# data_GMM_sh$EMP_P_r1_04 = data_GMM_sh$EMP_P*data_GMM_sh$r1*data_GMM_sh$d04
# data_GMM_sh$EMP_P_r1_07 = data_GMM_sh$EMP_P*data_GMM_sh$r1*data_GMM_sh$d07
# data_GMM_sh$EMP_P_r1_06 = data_GMM_sh$EMP_P*data_GMM_sh$r1*data_GMM_sh$d06
# 
# 
# data_GMM_sh$EMP_P_r1_08 = data_GMM_sh$EMP_P*data_GMM_sh$r1*data_GMM_sh$d08
# data_GMM_sh$EMP_P_r1_09 = data_GMM_sh$EMP_P*data_GMM_sh$r1*data_GMM_sh$d09
# data_GMM_sh$EMP_P_r1_10 = data_GMM_sh$EMP_P*data_GMM_sh$r1*data_GMM_sh$d10
# data_GMM_sh$EMP_P_r1_11 = data_GMM_sh$EMP_P*data_GMM_sh$r1*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_P_r2_04 = data_GMM_sh$EMP_P*data_GMM_sh$r2*data_GMM_sh$d04
# data_GMM_sh$EMP_P_r2_07 = data_GMM_sh$EMP_P*data_GMM_sh$r2*data_GMM_sh$d07
# data_GMM_sh$EMP_P_r2_06 = data_GMM_sh$EMP_P*data_GMM_sh$r2*data_GMM_sh$d06
# 
# data_GMM_sh$EMP_P_r2_08 = data_GMM_sh$EMP_P*data_GMM_sh$r2*data_GMM_sh$d08
# data_GMM_sh$EMP_P_r2_09 = data_GMM_sh$EMP_P*data_GMM_sh$r2*data_GMM_sh$d09
# data_GMM_sh$EMP_P_r2_10 = data_GMM_sh$EMP_P*data_GMM_sh$r2*data_GMM_sh$d10
# data_GMM_sh$EMP_P_r2_11 = data_GMM_sh$EMP_P*data_GMM_sh$r2*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_P_r3_04 = data_GMM_sh$EMP_P*data_GMM_sh$r3*data_GMM_sh$d04
# data_GMM_sh$EMP_P_r3_07 = data_GMM_sh$EMP_P*data_GMM_sh$r3*data_GMM_sh$d07
# data_GMM_sh$EMP_P_r3_06 = data_GMM_sh$EMP_P*data_GMM_sh$r3*data_GMM_sh$d06
# 
# data_GMM_sh$EMP_P_r3_08 = data_GMM_sh$EMP_P*data_GMM_sh$r3*data_GMM_sh$d08
# data_GMM_sh$EMP_P_r3_09 = data_GMM_sh$EMP_P*data_GMM_sh$r3*data_GMM_sh$d09
# data_GMM_sh$EMP_P_r3_10 = data_GMM_sh$EMP_P*data_GMM_sh$r3*data_GMM_sh$d10
# data_GMM_sh$EMP_P_r3_11 = data_GMM_sh$EMP_P*data_GMM_sh$r3*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_P_r4_04 = data_GMM_sh$EMP_P*data_GMM_sh$r4*data_GMM_sh$d04
# data_GMM_sh$EMP_P_r4_07 = data_GMM_sh$EMP_P*data_GMM_sh$r4*data_GMM_sh$d07
# data_GMM_sh$EMP_P_r4_06 = data_GMM_sh$EMP_P*data_GMM_sh$r4*data_GMM_sh$d06
# 
# data_GMM_sh$EMP_P_r4_08 = data_GMM_sh$EMP_P*data_GMM_sh$r4*data_GMM_sh$d08
# data_GMM_sh$EMP_P_r4_09 = data_GMM_sh$EMP_P*data_GMM_sh$r4*data_GMM_sh$d09
# data_GMM_sh$EMP_P_r4_10 = data_GMM_sh$EMP_P*data_GMM_sh$r4*data_GMM_sh$d10
# data_GMM_sh$EMP_P_r4_11 = data_GMM_sh$EMP_P*data_GMM_sh$r4*data_GMM_sh$d11
# 
# 
# data_GMM_sh$EMP_P_r5_04 = data_GMM_sh$EMP_P*data_GMM_sh$r5*data_GMM_sh$d04
# data_GMM_sh$EMP_P_r5_07 = data_GMM_sh$EMP_P*data_GMM_sh$r5*data_GMM_sh$d07
# data_GMM_sh$EMP_P_r5_06 = data_GMM_sh$EMP_P*data_GMM_sh$r5*data_GMM_sh$d06
# 
# data_GMM_sh$EMP_P_r5_08 = data_GMM_sh$EMP_P*data_GMM_sh$r5*data_GMM_sh$d08
# data_GMM_sh$EMP_P_r5_09 = data_GMM_sh$EMP_P*data_GMM_sh$r5*data_GMM_sh$d09
# data_GMM_sh$EMP_P_r5_10 = data_GMM_sh$EMP_P*data_GMM_sh$r5*data_GMM_sh$d10
# data_GMM_sh$EMP_P_r5_11 = data_GMM_sh$EMP_P*data_GMM_sh$r5*data_GMM_sh$d11


data_GMM_sh$BVT_03 = data_GMM_sh$BVT_F*data_GMM_sh$d03
data_GMM_sh$BVT_04 = data_GMM_sh$BVT_F*data_GMM_sh$d04
data_GMM_sh$BVT_05 = data_GMM_sh$BVT_F*data_GMM_sh$d05
data_GMM_sh$BVT_06 = data_GMM_sh$BVT_F*data_GMM_sh$d06
data_GMM_sh$BVT_07 = data_GMM_sh$BVT_F*data_GMM_sh$d07

data_GMM_sh$BVT_08 = data_GMM_sh$BVT_F*data_GMM_sh$d08
data_GMM_sh$BVT_09 = data_GMM_sh$BVT_F*data_GMM_sh$d09
data_GMM_sh$BVT_10 = data_GMM_sh$BVT_F*data_GMM_sh$d10
data_GMM_sh$BVT_11 = data_GMM_sh$BVT_F*data_GMM_sh$d11
# data_GMM_sh$BVT_12 = data_GMM_sh$BVT_F*data_GMM_sh$d12
# data_GMM_sh$BVT_13 = data_GMM_sh$BVT_F*data_GMM_sh$d13
# data_GMM_sh$BVT_14 = data_GMM_sh$BVT_F*data_GMM_sh$d14
# 
# 
# 
# # data_GMM_sh$EMP_T02 = data_GMM_sh$EMP_T*data_GMM_sh$d02
# data_GMM_sh$EMP_T03 = data_GMM_sh$EMP_T*data_GMM_sh$d03
# data_GMM_sh$EMP_T04 = data_GMM_sh$EMP_T*data_GMM_sh$d04
# # data_GMM_sh$EMP_T05 = data_GMM_sh$EMP_T*data_GMM_sh$d05
# data_GMM_sh$EMP_T06 = data_GMM_sh$EMP_T*data_GMM_sh$d06
# data_GMM_sh$EMP_T07 = data_GMM_sh$EMP_T*data_GMM_sh$d07
# 
# data_GMM_sh$EMP_T08 = data_GMM_sh$EMP_T*data_GMM_sh$d08
# data_GMM_sh$EMP_T09 = data_GMM_sh$EMP_T*data_GMM_sh$d09
# data_GMM_sh$EMP_T10 = data_GMM_sh$EMP_T*data_GMM_sh$d10
# data_GMM_sh$EMP_T11 = data_GMM_sh$EMP_T*data_GMM_sh$d11
# # data_GMM_sh$EMP_T12 = data_GMM_sh$EMP_T*data_GMM_sh$d12
# # data_GMM_sh$EMP_T13 = data_GMM_sh$EMP_T*data_GMM_sh$d13
# # data_GMM_sh$EMP_T14 = data_GMM_sh$EMP_T*data_GMM_sh$d14
# # 
# # 
# # data_GMM_sh$EMP_P02 = data_GMM_sh$EMP_P*data_GMM_sh$d02
# data_GMM_sh$EMP_P03 = data_GMM_sh$EMP_P*data_GMM_sh$d03
# data_GMM_sh$EMP_P04 = data_GMM_sh$EMP_P*data_GMM_sh$d04
# # data_GMM_sh$EMP_P05 = data_GMM_sh$EMP_P*data_GMM_sh$d05
# data_GMM_sh$EMP_P06 = data_GMM_sh$EMP_P*data_GMM_sh$d06
# data_GMM_sh$EMP_P07 = data_GMM_sh$EMP_P*data_GMM_sh$d07
# 
# data_GMM_sh$EMP_P08 = data_GMM_sh$EMP_P*data_GMM_sh$d08
# data_GMM_sh$EMP_P09 = data_GMM_sh$EMP_P*data_GMM_sh$d09
# data_GMM_sh$EMP_P10 = data_GMM_sh$EMP_P*data_GMM_sh$d10
# data_GMM_sh$EMP_P11 = data_GMM_sh$EMP_P*data_GMM_sh$d11
# # data_GMM_sh$EMP_P12 = data_GMM_sh$EMP_P*data_GMM_sh$d12
# # data_GMM_sh$EMP_P13 = data_GMM_sh$EMP_P*data_GMM_sh$d13
# # data_GMM_sh$EMP_P14 = data_GMM_sh$EMP_P*data_GMM_sh$d14

head(data_GMM_sh)

# Summarisk rater - BVT pour interpretation des parametres estimees!

# tapply(data_GMM_sh$BVT_F, data_GMM_sh$Område, mean, na.rm = T)
# tapply(data_GMM_sh$BVT_F, data_GMM_sh$Region_navn, mean, na.rm = T)

with(data_GMM_sh, tapply(BVT_F, list(Område, Year), mean))

with(data_GMM_sh, tapply(BVT_F, list(Region_navn, Year), mean))



with(data_GMM_sh, tapply(YF_T, list(Område, Year), mean))

with(data_GMM_sh, tapply(YF_T, list(Region_navn, Year), mean))



with(data_GMM_sh, tapply(YF_P, list(Område, Year), mean))

with(data_GMM_sh, tapply(YF_P, list(Region_navn, Year), mean))


library(plm)
library(lmtest)
library(sandwich)


bptest(modelFD)     # heteroskedasticity in differenced errors => robust
bptest(modelFDd)    # heteroskedasticity in differenced errors => robust
bptest(modelRE)

?plm

?pgmm

head(data_GMM_sh)
str(data_GMM_sh)
2090/22



library(gplots)

dev.off()

x11()
plotmeans(YF_T ~ Year, main="Vækstraten i produktivitet (per time)",
          xlab = "År",
          ylab = "Pct.", bars=TRUE, barcol="blue",
          data=data_GMM_sh)

x11()
plotmeans(YF_P ~ Year, main="Vækstraten i produktivitet (per person)",
          xlab = "År",
          ylab = "Pct.", bars=TRUE, barcol="blue",
          data=data_GMM_sh)

x11()
plotmeans(BVT_F ~ Year, main="BVTs vækstrate", 
          xlab = "År",
          ylab = "Pct.", bars=TRUE, barcol="blue",
          data=data_GMM_sh)

x11()
plotmeans(EMP_T ~ Year, main="Vækstraten i beskæftigelsen (per time)", 
          xlab = "År",
          ylab = "Pct.", bars=TRUE, barcol="blue",
          data=data_GMM_sh)

x11()
plotmeans(EMP_P ~ Year, main="Vækstraten i beskæftigelsen (per person)", 
          xlab = "År",
          ylab = "Pct.", bars=TRUE, barcol="blue",
          data=data_GMM_sh)





# lay1=cbind(c(1,1,2,2,3,3),c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(4,4,5,5,6,6))
lay1=cbind(c(1,1,2,2,3,3),c(1,1,2,2,3,3),c(1,1,2,2,3,3))
lay1
layout(lay1) 


require(gridExtra)

grid.arrange(box_ALL_g, box_ALL_v, ncol=2)
?grid.arrange


# grid.arrange(t1,bvt,empt, layout_matrix = rbind(c(1,1),c(2,3)))
# grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
#              ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

?plotmeans

# Unit root tests

yt <- data.frame(split(data_GMM_sh$YF_T, data_GMM_sh$kom))

head(yt)

purtest(yt, pmax = 3, exo = "intercept", test = "madwu")
purtest(yt, pmax = 3, exo = "intercept", test = "ips")
purtest(yt, pmax = 3, exo = "intercept", test = "levinlin")


yp <- data.frame(split(data_GMM_sh$YF_P, data_GMM_sh$kom))

purtest(yp, pmax = 3, exo = "intercept", test = "madwu")
purtest(yp, pmax = 3, exo = "intercept", test = "ips")
purtest(yp, pmax = 3, exo = "intercept", test = "levinlin")


# Time dummies - 2003. 2004 - 2006, 2007
# lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(BVT_F, 2) + lag(EMP_T, 2)


# OLD MODEL!!!
# modelgmmT0<-pgmm(YF_T ~ lag(YF_T, 1) + lag(YF_T, 2) + lag(BVT_F, 1) + lag(EMP_T, 1) 
#                 + lag(data_GMM_sh$BVT_03, 1) + lag(data_GMM_sh$BVT_04, 1) +
#                 + lag(data_GMM_sh$EMP_T03, 1) + lag(data_GMM_sh$EMP_T04, 1) +
#                   + lag(data_GMM_sh$BVT_06, 1) + lag(data_GMM_sh$BVT_07, 1) +
#                   + lag(data_GMM_sh$EMP_T06, 1) + lag(data_GMM_sh$EMP_T07, 1) +
#                 lag(BVT_o1, 1) + lag(EMP_T_o1, 1) +
#                 lag(data_GMM_sh$BVT_o1_04, 1) + lag(data_GMM_sh$BVT_o1_06, 1) + lag(data_GMM_sh$BVT_o1_07, 1) +
#                 lag(data_GMM_sh$EMP_T_o1_04, 1) + lag(data_GMM_sh$EMP_T_o1_06, 1) + lag(data_GMM_sh$EMP_T_o1_07, 1)
#                 | lag(YF_T, 3:5), data=data_GMM_sh,
#                 transformation = "ld")
# summary(modelgmmT0, robust = TRUE)
# 
# modelgmmT0
# 
# sargan(modelgmmT0)

# FULL
modelgmmT0<-pgmm(YF_T ~ lag(YF_T, 1) + lag(YF_T_o1, 1) + lag(BVT_F, 1) + 
                    
                    lag(BVT_o1, 1) + 
                   
                   # lag(data_GMM_sh$BVT_03, 1) + lag(data_GMM_sh$BVT_04, 1) + 
                   # lag(data_GMM_sh$BVT_05, 1) + lag(data_GMM_sh$BVT_06, 1) + 
                   # lag(data_GMM_sh$BVT_07, 1) +
                   # lag(data_GMM_sh$BVT_08, 1) +
                    
                    lag(data_GMM_sh$BVT_o1_03, 1) + lag(data_GMM_sh$BVT_o1_04, 1) + 
                    lag(data_GMM_sh$BVT_o1_05, 1) + lag(data_GMM_sh$BVT_o1_06, 1) + 
                   lag(data_GMM_sh$BVT_o1_07, 1) +
                    lag(data_GMM_sh$BVT_o1_08, 1) 
                  
                  | lag(YF_T, 2:3) + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")
summary(modelgmmT0, robust = TRUE)

# modelgmmT0X

sargan(modelgmmT0)


# Parsimonious
modelgmmT0<-pgmm(YF_T ~ lag(YF_T, 1) + lag(YF_T_o1,1) + lag(BVT_F, 1) + 
                   
                   lag(BVT_o1, 1) + 
                   
                   lag(data_GMM_sh$BVT_03, 1) +  
                   lag(data_GMM_sh$BVT_07, 1) +

                   
                   lag(data_GMM_sh$BVT_o1_03, 1) + lag(data_GMM_sh$BVT_o1_04, 1) + 
                   lag(data_GMM_sh$BVT_o1_05, 1) + lag(data_GMM_sh$BVT_o1_07, 1)


                 
                 | lag(YF_T, 2:3) + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")
summary(modelgmmT0, robust = TRUE, time.dummies=T)

?pgmm


head(modelgmmT0$fitted.values)
head(modelgmmT0$residuals)



test_res = as.data.frame(modelgmmT0$residuals)
head(test_res)  


# Plot Res


x11()
plotmeans(modelgmmT0$residuals ~ Year, main="Vækstraten i beskæftigelsen (per person)", 
          xlab = "År",
          ylab = "Pct.", bars=TRUE, barcol="blue",
          data=data_GMM_sh)



test_fit = as.data.frame(modelgmmT0$fitted.values)
head(test_fit)

# write.csv(test_fit,"fit_modelgmmT0.csv")



# Plot fit

x11()
plotmeans(modelgmmT0$fitted.values ~ Year, main="Vækstraten i beskæftigelsen (per person)", 
          xlab = "År",
          ylab = "Pct.", bars=TRUE, barcol="blue",
          data=data_GMM_sh)



# r1 - hovedstaden - comme reference!!!

# data_sub = subset(data_GMM_sh, data_GMM_sh$Region_navn!="Ø-kommuner")

# FULL
modelgmmT1<-pgmm(YF_T ~ lag(YF_T, 1)  + 
                    lag(BVT_F, 1) + 
                   
                    lag(BVT_r6, 1) +  lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) + # lag(BVT_r4, 1) +
                    
                    lag(data_GMM_sh$BVT_r2_03, 1) + lag(data_GMM_sh$BVT_r2_04, 1) + 
                    lag(data_GMM_sh$BVT_r2_05, 1)  + lag(data_GMM_sh$BVT_r2_06, 1)  +
                    lag(data_GMM_sh$BVT_r2_07, 1)  + lag(data_GMM_sh$BVT_r2_08, 1)  +
                    
                    lag(data_GMM_sh$BVT_r3_03, 1) + lag(data_GMM_sh$BVT_r3_04, 1) + 
                    lag(data_GMM_sh$BVT_r3_05, 1) + lag(data_GMM_sh$BVT_r3_06, 1)  +
                    lag(data_GMM_sh$BVT_r3_07, 1) + lag(data_GMM_sh$BVT_r3_08, 1)  +
                    
                    # lag(data_GMM_sh$BVT_r4_03, 1) + lag(data_GMM_sh$BVT_r4_04, 1) + 
                    # lag(data_GMM_sh$BVT_r4_05, 1) + lag(data_GMM_sh$BVT_r4_06, 1)  +
                    # lag(data_GMM_sh$BVT_r4_07, 1) + lag(data_GMM_sh$BVT_r4_08, 1) +
                    
                    lag(data_GMM_sh$BVT_r5_03, 1) + lag(data_GMM_sh$BVT_r5_04, 1) + 
                    lag(data_GMM_sh$BVT_r5_05, 1) + lag(data_GMM_sh$BVT_r5_06, 1)  +
                    lag(data_GMM_sh$BVT_r5_07, 1) + lag(data_GMM_sh$BVT_r5_08, 1) +
                   
                   lag(data_GMM_sh$BVT_r6_03, 1) + lag(data_GMM_sh$BVT_r6_04, 1) + 
                   lag(data_GMM_sh$BVT_r6_05, 1)  + lag(data_GMM_sh$BVT_r6_06, 1)  +
                   lag(data_GMM_sh$BVT_r6_07, 1)  + lag(data_GMM_sh$BVT_r6_08, 1) 
                  
                  | lag(YF_T, 2) + lag(BVT_F, 2) , data=data_GMM_sh,
                 transformation = "ld")

summary(modelgmmT1)

sargan(modelgmmT1)

summary(data_GMM_sh$Region_navn)
summary(data_GMM_sh$Landsdel_navn)



# PARSI avec r1 lag
modelgmmT1<-pgmm(YF_T ~ lag(YF_T, 1)  + lag(YF_T_r2,1) + lag(YF_T_r3,1) + lag(YF_T_r4,1) + lag(YF_T_r5,1) + lag(YF_T_r6,1) +
                   lag(BVT_F, 1) + 
                   
                   lag(BVT_r3, 1) +  
                   lag(BVT_r5, 1) + # lag(BVT_r4, 1) +
                   
                   # lag(BVT_03, 1) +  lag(BVT_04, 1) + lag(BVT_05, 1) +  
                   # lag(BVT_06, 1) + 
                   
                   
                   lag(data_GMM_sh$BVT_r3_04, 1) + 
                   lag(data_GMM_sh$BVT_r3_06, 1)  +
                   
                   
                   # lag(data_GMM_sh$BVT_r4_03, 1) + lag(data_GMM_sh$BVT_r4_04, 1) + 
                   # lag(data_GMM_sh$BVT_r4_05, 1) + lag(data_GMM_sh$BVT_r4_06, 1)  +
                   # lag(data_GMM_sh$BVT_r4_07, 1) + lag(data_GMM_sh$BVT_r4_08, 1) +
                   
                   
                   lag(data_GMM_sh$BVT_r5_07, 1) + 
                   
                   lag(data_GMM_sh$BVT_r6_03, 1) +  
                   lag(data_GMM_sh$BVT_r6_07, 1)
                 
                 | lag(YF_T, 2:3) + lag(BVT_F, 2) , data=data_GMM_sh,
                 transformation = "ld")

summary(modelgmmT1, robust = TRUE, time.dummies=T)

sargan(modelgmmT1)


# PARSI avec l1 lag - Remove small insignifikant kommuner, 4 (Bornholm) and 890 (Økommuner)

summary(data_GMM_sh$Landsdel_navn)

data_GMM_sh_l = subset(data_GMM_sh, !(data_GMM$kom==4 | data_GMM$kom==890))

modelgmmT1<-pgmm(YF_T ~ lag(YF_T, 1)  + lag(YF_T_l2,1) + lag(YF_T_l3,1) + lag(YF_T_l4,1) + lag(YF_T_l5,1) + 
                   lag(YF_T_l6,1) +
                  
                   lag(YF_T_l7,1) + lag(YF_T_l8,1) + lag(YF_T_l10,1) + lag(YF_T_l1,1) + # l9
                   
                   lag(BVT_F, 1) + 
                   
                   lag(BVT_r3, 1) +  
                   lag(BVT_r5, 1) + # lag(BVT_r4, 1) +
                   
                   # lag(BVT_03, 1) +  lag(BVT_04, 1) + lag(BVT_05, 1) +  
                   # lag(BVT_06, 1) + 
                   
                   
                   lag(data_GMM_sh$BVT_r3_04, 1) + 
                   lag(data_GMM_sh$BVT_r3_06, 1)  +
                   
                   
                   # lag(data_GMM_sh$BVT_r4_03, 1) + lag(data_GMM_sh$BVT_r4_04, 1) + 
                   # lag(data_GMM_sh$BVT_r4_05, 1) + lag(data_GMM_sh$BVT_r4_06, 1)  +
                   # lag(data_GMM_sh$BVT_r4_07, 1) + lag(data_GMM_sh$BVT_r4_08, 1) +
                   
                   
                   lag(data_GMM_sh$BVT_r5_07, 1) + 
                   
                   lag(data_GMM_sh$BVT_r6_03, 1) +  
                   lag(data_GMM_sh$BVT_r6_07, 1)
                 
                 | lag(YF_T, 2:3) + lag(BVT_F, 2) , data=data_GMM_sh_l,
                 transformation = "ld")

summary(modelgmmT1, robust = TRUE, time.dummies=T)





# 
# modelgmmT1<-pgmm(YF_T ~ lag(YF_T, 1) + lag(YF_T, 2) + lag(BVT_F, 1) + lag(EMP_T, 1)  
#                 + lag(data_GMM_sh$BVT_03, 1) + lag(data_GMM_sh$BVT_04, 1) +
#                 + lag(data_GMM_sh$EMP_T03, 1) + lag(data_GMM_sh$EMP_T04, 1) +
#                   + lag(data_GMM_sh$BVT_06, 1) + lag(data_GMM_sh$BVT_07, 1) +
#                   + lag(data_GMM_sh$EMP_T06, 1) + lag(data_GMM_sh$EMP_T07, 1) +
#                   lag(BVT_r1, 1) + lag(EMP_T_r1, 1) + lag(BVT_r2, 1) + lag(EMP_T_r2, 1) + lag(BVT_r3, 1) + 
#                   lag(EMP_T_r3, 1) + lag(BVT_r4, 1) + lag(EMP_T_r4, 1) + 
#                   lag(BVT_r5, 1) + lag(EMP_T_r5, 1) + 
#                   lag(BVT_r1_07, 1) + lag(EMP_T_r1_07, 1) + lag(BVT_r2_07, 1) + lag(EMP_T_r2_07, 1) + 
#                   lag(BVT_r3_07, 1) + lag(EMP_T_r3_07, 1) 
#                 | lag(YF_T, 3:5) , data=data_GMM_sh, 
#                 transformation = "ld")
# 
# summary(modelgmmT1)
# 
# sargan(modelgmmT1)



# lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) 

# lag(data_GMM_sh$BVT_o1_07, 1) +
# lag(data_GMM_sh$EMP_P_o1_07, 1)

# FULL
modelgmmP0<-pgmm(YF_P ~ lag(YF_P, 1) + lag(BVT_F, 1)  +
                    
                    lag(BVT_o1, 1) +
                    
                    lag(data_GMM_sh$BVT_o1_03, 1) + lag(data_GMM_sh$BVT_o1_04, 1) + 
                    lag(data_GMM_sh$BVT_o1_05, 1) + lag(data_GMM_sh$BVT_o1_06, 1) +
                    lag(data_GMM_sh$BVT_o1_07, 1) + lag(data_GMM_sh$BVT_o1_08, 1) 
                  | lag(YF_P, 2:3) + lag(BVT_F, 2), data=data_GMM_sh,
                  transformation = "ld")
summary(modelgmmP0, robust = TRUE)

sargan(modelgmmP0)


# Parsi
modelgmmP0<-pgmm(YF_P ~ lag(YF_P, 1) + lag(YF_P_o1,1) + lag(BVT_F, 1)  +
                   
                   lag(BVT_o1, 1) +
                   
                   lag(data_GMM_sh$BVT_03, 1) + 
                   
                   
                   
                   lag(data_GMM_sh$BVT_o1_03, 1) + lag(data_GMM_sh$BVT_o1_04, 1) + 
                   lag(data_GMM_sh$BVT_o1_05, 1) 
                 | lag(YF_P, 2:3) + lag(BVT_F, 2), data=data_GMM_sh,
                 transformation = "ld")
summary(modelgmmP0, robust = TRUE, time.dummies=T)

sargan(modelgmmP0)



# modelgmmP0<-pgmm (YF_P ~ lag(YF_P, 1) + lag(YF_P, 2) + lag(BVT_F, 1) + lag(EMP_P, 1) +
#                  lag(data_GMM_sh$BVT_03, 1) + lag(data_GMM_sh$BVT_04, 1) +
#                   lag(data_GMM_sh$EMP_P03, 1) + lag(data_GMM_sh$EMP_P04, 1) +
#                   lag(data_GMM_sh$BVT_06, 1) + lag(data_GMM_sh$BVT_07, 1) +
#                   lag(data_GMM_sh$EMP_P06, 1) + lag(data_GMM_sh$EMP_P07, 1) +
#                   lag(BVT_o1, 1) + lag(EMP_P_o1, 1) +
#                   lag(data_GMM_sh$BVT_o1_04, 1) + lag(data_GMM_sh$BVT_o1_06, 1) + lag(data_GMM_sh$BVT_o1_07, 1) + 
#                   lag(data_GMM_sh$EMP_P_o1_04) + lag(data_GMM_sh$EMP_P_o1_06, 1) + lag(data_GMM_sh$EMP_P_o1_07, 1)  
#                 | lag(YF_P, 3:5) , data=data_GMM_sh, transformation = "ld")
# summary(modelgmmP0)
# 
# sargan(modelgmmP0)



# FULL
modelgmmP1<-pgmm(YF_P ~ lag(YF_P, 1)  + lag(YF_P, 2)  + 
                    
                    lag(BVT_F, 1) + 
                    
                    lag(BVT_r1, 1) +  lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r6, 1) + lag(BVT_r5, 1) +  
                    
                    lag(data_GMM_sh$BVT_r1_03, 1) + lag(data_GMM_sh$BVT_r1_04, 1) + 
                    lag(data_GMM_sh$BVT_r1_05, 1)  + lag(data_GMM_sh$BVT_r1_06, 1)  +
                    lag(data_GMM_sh$BVT_r1_07, 1)  + lag(data_GMM_sh$BVT_r1_08, 1)  +
                    
                    lag(data_GMM_sh$BVT_r2_03, 1) + lag(data_GMM_sh$BVT_r2_04, 1) + 
                    lag(data_GMM_sh$BVT_r2_05, 1)  + lag(data_GMM_sh$BVT_r2_06, 1)  +
                    lag(data_GMM_sh$BVT_r2_07, 1)  + lag(data_GMM_sh$BVT_r2_08, 1)  +
                    
                    lag(data_GMM_sh$BVT_r3_03, 1) + lag(data_GMM_sh$BVT_r3_04, 1) + 
                    lag(data_GMM_sh$BVT_r3_05, 1) + lag(data_GMM_sh$BVT_r3_06, 1) +
                    lag(data_GMM_sh$BVT_r3_07, 1)  + lag(data_GMM_sh$BVT_r3_08, 1)  +
                    
                    lag(data_GMM_sh$BVT_r6_03, 1) + lag(data_GMM_sh$BVT_r6_04, 1) + 
                    lag(data_GMM_sh$BVT_r6_05, 1) + lag(data_GMM_sh$BVT_r6_06, 1) +
                    lag(data_GMM_sh$BVT_r6_07, 1)  + lag(data_GMM_sh$BVT_r6_08, 1)  +
                    
                    lag(data_GMM_sh$BVT_r5_03, 1) + lag(data_GMM_sh$BVT_r5_04, 1) + 
                    lag(data_GMM_sh$BVT_r5_05, 1) + lag(data_GMM_sh$BVT_r5_06, 1) +
                    lag(data_GMM_sh$BVT_r5_07, 1)  + lag(data_GMM_sh$BVT_r5_08, 1)
                  
                  | lag(YF_P, 3) + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")

summary(modelgmmP1)

sargan(modelgmmP1)


# PARSI with r1

modelgmmP1<-pgmm(YF_P ~ lag(YF_P, 1)  + lag(YF_P_r2,1) + lag(YF_P_r3,1) + lag(YF_P_r4,1) + lag(YF_P_r5,1) + lag(YF_P_r6,1) +
                   
                   lag(BVT_F, 1) + 
                   
                   lag(BVT_r6, 1) +  lag(BVT_r2, 1) + lag(BVT_r3, 1) +  # r1
                   lag(BVT_r5, 1) +  
                   
                  # lag(BVT_05, 1) +  
                   
                   
#                   lag(data_GMM_sh$BVT_r2_04, 1) + # r1
#                   lag(data_GMM_sh$BVT_r2_05, 1)  + # r1
                   
                  
                  
                   
                   lag(data_GMM_sh$BVT_r6_03, 1) + 
                   lag(data_GMM_sh$BVT_r6_05, 1) + 
                   
              
                   lag(data_GMM_sh$BVT_r5_03, 1) + lag(data_GMM_sh$BVT_r5_04, 1) + 
                   
                   lag(data_GMM_sh$BVT_r5_07, 1)
                 
                 | lag(YF_P, 2:3) + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")

summary(modelgmmP1, robust = TRUE, time.dummies=T)




# PARSI with l1

modelgmmP1<-pgmm(YF_P ~ lag(YF_P, 1)  + lag(YF_P_l2,1) + lag(YF_P_l3,1) + lag(YF_P_l4,1) + lag(YF_P_l5,1) + 
                   lag(YF_P_l6,1) +
                   
                   
                   
                   lag(YF_P_l7,1) + lag(YF_P_l8,1) + lag(YF_P_l10,1) + lag(YF_P_l1,1) + # l9
                   
                   lag(BVT_F, 1) + 
                   
                   lag(BVT_r6, 1) +  lag(BVT_r2, 1) + lag(BVT_r3, 1) +  # r1
                   lag(BVT_r5, 1) +  
                   
                   # lag(BVT_05, 1) +  
                   
                   
                   #                   lag(data_GMM_sh$BVT_r2_04, 1) + # r1
                   #                   lag(data_GMM_sh$BVT_r2_05, 1)  + # r1
                   
                   
                   
                   
                   lag(data_GMM_sh$BVT_r6_03, 1) + 
                   lag(data_GMM_sh$BVT_r6_05, 1) + 
                   
                   
                   lag(data_GMM_sh$BVT_r5_03, 1) + lag(data_GMM_sh$BVT_r5_04, 1) + 
                   
                   lag(data_GMM_sh$BVT_r5_07, 1)
                 
                 | lag(YF_P,2:3) + lag(BVT_F, 2), data=data_GMM_sh_l, transformation = "ld")

summary(modelgmmP1, robust = TRUE, time.dummies=T)





# modelgmmP1<-pgmm(YF_P ~ lag(YF_P, 1) + lag(YF_P, 2) + lag(BVT_F, 1) + lag(EMP_P, 1)  
#                 + lag(data_GMM_sh$BVT_03, 1) + lag(data_GMM_sh$BVT_04, 1) +
#                   + lag(data_GMM_sh$EMP_P03, 1) + lag(data_GMM_sh$EMP_P04, 1) +
#                   + lag(data_GMM_sh$BVT_06, 1) + lag(data_GMM_sh$BVT_07, 1) +
#                   + lag(data_GMM_sh$EMP_P06, 1) + lag(data_GMM_sh$EMP_P07, 1) +
#                   lag(BVT_r1, 1) + lag(EMP_P_r1, 1) + lag(BVT_r2, 1) + lag(EMP_P_r2, 1) + 
#                   lag(BVT_r3, 1) + lag(EMP_P_r3, 1) + lag(BVT_r4, 1) + lag(EMP_P_r4, 1) + 
#                   lag(BVT_r5, 1) + lag(EMP_P_r5, 1) + 
#                   lag(BVT_r1_04, 1) + lag(EMP_P_r1_04, 1) + lag(BVT_r2_04, 1) + lag(EMP_P_r2_04, 1) + 
#                   lag(BVT_r3_04, 1) + lag(EMP_P_r3_04, 1) 
#                 | lag(YF_P, 3:5) , data=data_GMM_sh, 
#                 transformation = "ld")
# 
# summary(modelgmmP1)
# 
# sargan(modelgmmP1)





####################
# Time Dummies: 2006, 2007, 2008, 2009, 2010, 2011
####################






# ATTENTION DEFINITION PRODUCTIVIY ET LAGS!!! EMP excluded!!!
# Multicol - Non-Identification
# lag(EMP_T, 1) + lag(EMP_T_o1, 1) +

# Time dummies - 2003. 2004 - 2006, 2007
# lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(BVT_F, 2) + lag(EMP_T, 2)

# FULL
modelgmmT0X<-pgmm(YF_T ~ lag(YF_T, 1) + lag(BVT_F, 1) + 
                    
                   lag(BVT_o1, 1) + 
                   
                   lag(data_GMM_sh$BVT_o1_06, 1) + lag(data_GMM_sh$BVT_o1_07, 1) + 
                   lag(data_GMM_sh$BVT_o1_08, 1) + lag(data_GMM_sh$BVT_o1_09, 1) +
                   lag(data_GMM_sh$BVT_o1_10, 1) + lag(data_GMM_sh$BVT_o1_11, 1) 
  
                 | lag(YF_T, 2:3)  + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")
summary(modelgmmT0X, robust = TRUE)

# modelgmmT0X

sargan(modelgmmT0X)


# lag(data_GMM_sh$BVT_06, 1) + lag(data_GMM_sh$BVT_07, 1) +
# lag(data_GMM_sh$BVT_08, 1) + lag(data_GMM_sh$BVT_09, 1) +

# PARSI
modelgmmT0X<-pgmm(YF_T ~ lag(YF_T, 1) + lag(BVT_F, 1) + 
                    
                    lag(BVT_o1, 1) + 
                    
                    lag(data_GMM_sh$BVT_07, 1) +

                    
                    
                    lag(data_GMM_sh$BVT_o1_08, 1) 
                  
                  | lag(YF_T, 2:3)  + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")
summary(modelgmmT0X, robust = TRUE, time.dummies=T )


# FULL
modelgmmT1X<-pgmm(YF_T ~ lag(YF_T, 1)  + 
                    
                    lag(BVT_F, 1) + 
                    
                    lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) +  lag(BVT_r6, 1) + #  lag(BVT_r4, 1) + 
                     
                    lag(data_GMM_sh$BVT_r2_06, 1) + lag(data_GMM_sh$BVT_r2_07, 1) + 
                    lag(data_GMM_sh$BVT_r2_08, 1)  + lag(data_GMM_sh$BVT_r2_09, 1)  +
                    lag(data_GMM_sh$BVT_r2_10, 1)  + lag(data_GMM_sh$BVT_r2_11, 1)  +
                    
                    lag(data_GMM_sh$BVT_r3_06, 1) + lag(data_GMM_sh$BVT_r3_07, 1) + 
                    lag(data_GMM_sh$BVT_r3_08, 1) + lag(data_GMM_sh$BVT_r3_09, 1)  +
                    lag(data_GMM_sh$BVT_r3_10, 1) + lag(data_GMM_sh$BVT_r3_11, 1)  +
                    
                    # lag(data_GMM_sh$BVT_r4_06, 1) + lag(data_GMM_sh$BVT_r4_07, 1) + 
                    # lag(data_GMM_sh$BVT_r4_08, 1) + lag(data_GMM_sh$BVT_r4_09, 1)  +
                    # lag(data_GMM_sh$BVT_r4_10, 1) + lag(data_GMM_sh$BVT_r4_11, 1) +
                    
                    lag(data_GMM_sh$BVT_r5_06, 1) + lag(data_GMM_sh$BVT_r5_07, 1) + 
                    lag(data_GMM_sh$BVT_r5_08, 1) + lag(data_GMM_sh$BVT_r5_09, 1)  +
                    lag(data_GMM_sh$BVT_r5_10, 1) + lag(data_GMM_sh$BVT_r5_11, 1) +

                    lag(data_GMM_sh$BVT_r6_06, 1) + lag(data_GMM_sh$BVT_r6_07, 1) + 
                    lag(data_GMM_sh$BVT_r6_08, 1)  + lag(data_GMM_sh$BVT_r6_09, 1)  +
                    lag(data_GMM_sh$BVT_r6_10, 1)  + lag(data_GMM_sh$BVT_r6_11, 1) | 
                    lag(YF_T, 2:3) + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")

summary(modelgmmT1X)

sargan(modelgmmT1X)


# PARSI

modelgmmT1X<-pgmm(YF_T ~ lag(YF_T, 1)  + 
                    
                    lag(BVT_F, 1) + 
                    
                    lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) +  lag(BVT_r6, 1) + #  lag(BVT_r4, 1) + 
                    
                    lag(data_GMM_sh$BVT_08, 1)  +
                    
                    lag(data_GMM_sh$BVT_11, 1) + 
                    lag(data_GMM_sh$BVT_r2_08, 1)  + 
                    
                    lag(data_GMM_sh$BVT_r3_06, 1) + 
                    
                    lag(data_GMM_sh$BVT_r3_10, 1) + 
                    
                    # lag(data_GMM_sh$BVT_r4_06, 1) + lag(data_GMM_sh$BVT_r4_07, 1) + 
                    # lag(data_GMM_sh$BVT_r4_08, 1) + lag(data_GMM_sh$BVT_r4_09, 1)  +
                    # lag(data_GMM_sh$BVT_r4_10, 1) + lag(data_GMM_sh$BVT_r4_11, 1) +
                    
                    lag(data_GMM_sh$BVT_r5_07, 1) + 
                    
                    lag(data_GMM_sh$BVT_r5_10, 1) + 
                    
                    lag(data_GMM_sh$BVT_r6_07, 1) + 
                    lag(data_GMM_sh$BVT_r6_09, 1)  +
                    lag(data_GMM_sh$BVT_r6_10, 1)  | 
                    lag(YF_T, 2:3) + lag(BVT_F, 2), data=data_GMM_sh, transformation = "ld")

summary(modelgmmT1X, robust = TRUE,  time.dummies=T)



summary(data_GMM_sh$Region_navn)

                                         
#                   lag(EMP_T_r1, 1) + lag(EMP_T_r2, 1) + lag(EMP_T_r3, 1)  + lag(EMP_T_r4, 1) + lag(EMP_T_r5, 1) 
                    # lag(data_GMM_sh$EMP_T_r1_06, 1) + lag(data_GMM_sh$EMP_T_r1_07, 1) + 
                     # lag(data_GMM_sh$EMP_T_r1_08, 1)  +
                    
                     # lag(data_GMM_sh$EMP_T_r2_06, 1) + lag(data_GMM_sh$EMP_T_r2_07, 1) + 
                     # lag(data_GMM_sh$EMP_T_r2_08, 1)  +   
                     
   
   
                    #  lag(data_GMM_sh$EMP_T_r3_06, 1) + lag(data_GMM_sh$EMP_T_r3_07, 1) + 
                    #  lag(data_GMM_sh$EMP_T_r3_08, 1) +
                    # lag(data_GMM_sh$EMP_T_r4_06, 1) + lag(data_GMM_sh$EMP_T_r4_07, 1) + 
                    # lag(data_GMM_sh$EMP_T_r4_08, 1) | 
                    #  




# lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) 

# lag(data_GMM_sh$BVT_o1_07, 1) +
# lag(data_GMM_sh$EMP_P_o1_07, 1)

# FULL
modelgmmP0X<-pgmm(YF_P ~ lag(YF_P, 1) + lag(BVT_F, 1)  +
                    
                    lag(BVT_o1, 1) +
                    
                    lag(data_GMM_sh$BVT_o1_06, 1) + lag(data_GMM_sh$BVT_o1_07, 1) + 
                    lag(data_GMM_sh$BVT_o1_08, 1) +
                    lag(data_GMM_sh$BVT_o1_11, 1) 
                    | lag(YF_P, 2:3) + lag(BVT_F, 2), data=data_GMM_sh,
                    transformation = "ld")
summary(modelgmmP0X, robust = TRUE, time.dummies=T )

sargan(modelgmmP0X)


# Parsi

modelgmmP0X<-pgmm(YF_P ~ lag(YF_P, 1) + lag(BVT_F, 1)  +
                    
                    lag(BVT_o1, 1) +
                    
                    lag(data_GMM_sh$BVT_07, 1) +
                    lag(data_GMM_sh$BVT_09, 1) +
                    lag(data_GMM_sh$BVT_10, 1) + lag(data_GMM_sh$BVT_11, 1) +
                    
                    lag(data_GMM_sh$BVT_o1_06, 1) + lag(data_GMM_sh$BVT_o1_07, 1) + 
                    lag(data_GMM_sh$BVT_o1_08, 1) + lag(data_GMM_sh$BVT_o1_09, 1) 
                  | lag(YF_P, 2:3) + lag(BVT_F, 2), data=data_GMM_sh,
                  transformation = "ld")
summary(modelgmmP0X, robust = TRUE, time.dummies=T )

           
+
  # lag(data_GMM_sh$BVT_06, 1) + lag(data_GMM_sh$BVT_07, 1) +
  # lag(data_GMM_sh$BVT_08, 1) + lag(data_GMM_sh$BVT_09, 1) +
  # lag(data_GMM_sh$BVT_10, 1) + lag(data_GMM_sh$BVT_11, 1) +
  
  # lag(data_GMM_sh$EMP_P06, 1) + lag(data_GMM_sh$EMP_P07, 1) +
  # lag(data_GMM_sh$EMP_P08, 1) + lag(data_GMM_sh$EMP_P09, 1) +
  #                   lag(data_GMM_sh$EMP_P10, 1) + lag(data_GMM_sh$EMP_P11, 1) +
  
                    
                    # lag(data_GMM_sh$EMP_P_o1_06, 1) + lag(data_GMM_sh$EMP_P_o1_07, 1) + 
                    # lag(data_GMM_sh$EMP_P_o1_08, 1) + lag(data_GMM_sh$EMP_P_o1_09, 1)
                    # 

# Full
modelgmmP1X<-pgmm(YF_P ~ lag(YF_P, 1)  + 
                    
                    lag(BVT_F, 1) + 
                    
                    lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) + lag(BVT_r6, 1) +  
                    
                    lag(data_GMM_sh$BVT_r1_06, 1) + lag(data_GMM_sh$BVT_r1_07, 1) + 
                    lag(data_GMM_sh$BVT_r1_08, 1)  + lag(data_GMM_sh$BVT_r1_09, 1)  +
                    lag(data_GMM_sh$BVT_r1_10, 1)  + lag(data_GMM_sh$BVT_r1_11, 1)  +
                    
                    lag(data_GMM_sh$BVT_r2_06, 1) + lag(data_GMM_sh$BVT_r2_07, 1) + 
                    lag(data_GMM_sh$BVT_r2_08, 1)  + lag(data_GMM_sh$BVT_r2_09, 1)  +
                    lag(data_GMM_sh$BVT_r2_10, 1)  + lag(data_GMM_sh$BVT_r2_11, 1)  +
                    
                    lag(data_GMM_sh$BVT_r3_06, 1) + lag(data_GMM_sh$BVT_r3_07, 1) + 
                    lag(data_GMM_sh$BVT_r3_08, 1) + lag(data_GMM_sh$BVT_r3_09, 1) +
                    lag(data_GMM_sh$BVT_r3_10, 1)  + lag(data_GMM_sh$BVT_r3_11, 1)  +
                    
                    lag(data_GMM_sh$BVT_r5_06, 1) + lag(data_GMM_sh$BVT_r5_07, 1) + 
                    lag(data_GMM_sh$BVT_r5_08, 1) + lag(data_GMM_sh$BVT_r5_09, 1) +
                    lag(data_GMM_sh$BVT_r5_10, 1)  + lag(data_GMM_sh$BVT_r5_11, 1) +
                    
                    lag(data_GMM_sh$BVT_r6_06, 1) + lag(data_GMM_sh$BVT_r6_07, 1) + 
                    lag(data_GMM_sh$BVT_r6_08, 1) + lag(data_GMM_sh$BVT_r6_09, 1) +
                    lag(data_GMM_sh$BVT_r6_10, 1)  + lag(data_GMM_sh$BVT_r6_11, 1)
                  
                  | lag(YF_P, 2:3) + lag(BVT_F, 2) , data=data_GMM_sh, transformation = "ld")

summary(modelgmmP1X)

sargan(modelgmmP1X)



# PARSI
modelgmmP1X<-pgmm(YF_P ~ lag(YF_P, 1)  + 
                    
                    lag(BVT_F, 1) + 
                    
                    lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) + lag(BVT_r6, 1) +  
                    

                    lag(data_GMM_sh$BVT_r1_06, 1) + 
                    lag(data_GMM_sh$BVT_r1_08, 1)  + 
                    lag(data_GMM_sh$BVT_r1_11, 1)  +
                    
                  
                    lag(data_GMM_sh$BVT_r5_07, 1) + 
                    lag(data_GMM_sh$BVT_r5_08, 1) + 
                    lag(data_GMM_sh$BVT_r5_10, 1)  +
                    
                    
                    lag(data_GMM_sh$BVT_r6_09, 1)
                    
                  
                  | lag(YF_P, 2:3) + lag(BVT_F, 2) , data=data_GMM_sh, transformation = "ld")

summary(modelgmmP1X, robust = TRUE, time.dummies=T )




# modelgmmP1<-pgmm(YF_P ~ lag(YF_P, 1) + lag(YF_P, 2) + lag(BVT_F, 1) + lag(EMP_P, 1)  
#                  + lag(data_GMM_sh$BVT_03, 1) + lag(data_GMM_sh$BVT_04, 1) +
#                    + lag(data_GMM_sh$EMP_P03, 1) + lag(data_GMM_sh$EMP_P04, 1) +
#                    + lag(data_GMM_sh$BVT_06, 1) + lag(data_GMM_sh$BVT_07, 1) +
#                    + lag(data_GMM_sh$EMP_P06, 1) + lag(data_GMM_sh$EMP_P07, 1) +
#                    lag(BVT_r1, 1) + lag(EMP_P_r1, 1) + lag(BVT_r2, 1) + lag(EMP_P_r2, 1) + 
#                    lag(BVT_r3, 1) + lag(EMP_P_r3, 1) + lag(BVT_r4, 1) + lag(EMP_P_r4, 1) + 
#                    lag(BVT_r5, 1) + lag(EMP_P_r5, 1) + 
#                    lag(BVT_r1_04, 1) + lag(EMP_P_r1_04, 1) + lag(BVT_r2_04, 1) + lag(EMP_P_r2_04, 1) + 
#                    lag(BVT_r3_04, 1) + lag(EMP_P_r3_04, 1) 
#                  | lag(YF_P, 3:5) , data=data_GMM_sh, 
#                  transformation = "ld")
# 
# summary(modelgmmP1)
# 
# sargan(modelgmmP1)



?pgmm




#################################
# With INV and SHORTER SAMPLE YEAR
#################################


data_GMM_I<- read.csv("GMM_LONG_FULL_INV_dummies.csv",header=TRUE,sep=",", dec=".")  # , row.names=1
head(data_GMM_I)
str(data_GMM_I)

summary(data_GMM_I)

682/22
1452/22
1261/13

data_GMM_I = subset(data_GMM_I, !(data_GMM_I$Year==1994 | data_GMM_I$Year==1995 | data_GMM_I$Year==1996 
                                  | data_GMM_I$Year==1997 | data_GMM_I$Year==1998 | data_GMM_I$Year==1999 
                                  | data_GMM_I$Year==2000 | data_GMM_I$Year==2014
                                  | data_GMM_I$Year==2015))



# NO DK AND NO "Uden REgion" ALSO!
data_GMM_I_sh = subset(data_GMM_I, !(data_GMM_I$kom==961 | data_GMM_I$kom==999))

tail(data_GMM_I_sh)

str(data_GMM_I_sh)
unique(data_GMM_I_sh$kom)
unique(data_GMM_I_sh$Region_navn)



x11()
plotmeans(INV_F ~ Year, main="Vækstraten i bruttoinvesteringer",
          xlab = "År",
          ylab = "Pct.",
          data=data_GMM_I_sh)



data_GMM_I_sh$Region_navn = factor(data_GMM_I_sh$Region_navn)
levels(data_GMM_I_sh$Region_navn)

data_GMM_I_sh$BVT_o1 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1

# data_GMM_I_sh$BVT_o1_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d04
# data_GMM_I_sh$BVT_o1_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d07

data_GMM_I_sh$BVT_r1 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1
data_GMM_I_sh$BVT_r2 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2
data_GMM_I_sh$BVT_r3 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3
data_GMM_I_sh$BVT_r4 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4
data_GMM_I_sh$BVT_r5 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5
data_GMM_I_sh$BVT_r6 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6



data_GMM_I_sh$BVT_o1_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d03
data_GMM_I_sh$BVT_o1_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d05

data_GMM_I_sh$BVT_o1_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d04
data_GMM_I_sh$BVT_o1_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d07
data_GMM_I_sh$BVT_o1_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d06

data_GMM_I_sh$BVT_o1_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d08
data_GMM_I_sh$BVT_o1_09 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d09
data_GMM_I_sh$BVT_o1_10 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d10
data_GMM_I_sh$BVT_o1_11 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$o1*data_GMM_I_sh$d11



data_GMM_I_sh$INV_o1_03 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d03
data_GMM_I_sh$INV_o1_05 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d05

data_GMM_I_sh$INV_o1_04 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d04
data_GMM_I_sh$INV_o1_07 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d07
data_GMM_I_sh$INV_o1_06 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d06

data_GMM_I_sh$INV_o1_08 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d08
data_GMM_I_sh$INV_o1_09 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d09
data_GMM_I_sh$INV_o1_10 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d10
data_GMM_I_sh$INV_o1_11 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d11


data_GMM_I_sh$BVT_r1 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1
data_GMM_I_sh$BVT_r2 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2
data_GMM_I_sh$BVT_r3 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3
data_GMM_I_sh$BVT_r4 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4
data_GMM_I_sh$BVT_r5 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5
# data_GMM_I_sh$BVT_r6 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6


data_GMM_I_sh$BVT_r1_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d03
data_GMM_I_sh$BVT_r1_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d05

data_GMM_I_sh$BVT_r1_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d04
data_GMM_I_sh$BVT_r1_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d07
data_GMM_I_sh$BVT_r1_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d06


data_GMM_I_sh$BVT_r1_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d08
data_GMM_I_sh$BVT_r1_09 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d09
data_GMM_I_sh$BVT_r1_10 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d10
data_GMM_I_sh$BVT_r1_11 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r1*data_GMM_I_sh$d11


data_GMM_I_sh$BVT_r2_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d03
data_GMM_I_sh$BVT_r2_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d05

data_GMM_I_sh$BVT_r2_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d04
data_GMM_I_sh$BVT_r2_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d07
data_GMM_I_sh$BVT_r2_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d06

data_GMM_I_sh$BVT_r2_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d08
data_GMM_I_sh$BVT_r2_09 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d09
data_GMM_I_sh$BVT_r2_10 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d10
data_GMM_I_sh$BVT_r2_11 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r2*data_GMM_I_sh$d11


data_GMM_I_sh$BVT_r3_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d03
data_GMM_I_sh$BVT_r3_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d05

data_GMM_I_sh$BVT_r3_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d04
data_GMM_I_sh$BVT_r3_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d07
data_GMM_I_sh$BVT_r3_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d06

data_GMM_I_sh$BVT_r3_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d08
data_GMM_I_sh$BVT_r3_09 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d09
data_GMM_I_sh$BVT_r3_10 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d10
data_GMM_I_sh$BVT_r3_11 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r3*data_GMM_I_sh$d11


data_GMM_I_sh$BVT_r4_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d03
data_GMM_I_sh$BVT_r4_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d05

data_GMM_I_sh$BVT_r4_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d04
data_GMM_I_sh$BVT_r4_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d07
data_GMM_I_sh$BVT_r4_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d06

data_GMM_I_sh$BVT_r4_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d08
data_GMM_I_sh$BVT_r4_09 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d09
data_GMM_I_sh$BVT_r4_10 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d10
data_GMM_I_sh$BVT_r4_11 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r4*data_GMM_I_sh$d11


data_GMM_I_sh$BVT_r5_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d03
data_GMM_I_sh$BVT_r5_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d05

data_GMM_I_sh$BVT_r5_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d04
data_GMM_I_sh$BVT_r5_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d07
data_GMM_I_sh$BVT_r5_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d06

data_GMM_I_sh$BVT_r5_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d08
data_GMM_I_sh$BVT_r5_09 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d09
data_GMM_I_sh$BVT_r5_10 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d10
data_GMM_I_sh$BVT_r5_11 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r5*data_GMM_I_sh$d11


data_GMM_I_sh$BVT_r6_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d03
data_GMM_I_sh$BVT_r6_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d05

data_GMM_I_sh$BVT_r6_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d04
data_GMM_I_sh$BVT_r6_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d07
data_GMM_I_sh$BVT_r6_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d06

data_GMM_I_sh$BVT_r6_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d08
data_GMM_I_sh$BVT_r6_09 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d09
data_GMM_I_sh$BVT_r6_10 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d10
data_GMM_I_sh$BVT_r6_11 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$r6*data_GMM_I_sh$d11



################################################
###############   MODEL with INVESTMENTS!! 2003-2008
################################################


modelgmmTI0<-pgmm(YF_T ~ lag(YF_T, 1) + lag(BVT_F, 1) + lag(INV_F, 1) +
                   
                   lag(BVT_o1, 1) + lag(INV_F_o1, 1) +
                    
                    lag(data_GMM_I_sh$INV_o1_03, 1) + lag(data_GMM_I_sh$INV_o1_04, 1) + 
                    lag(data_GMM_I_sh$INV_o1_05, 1) + lag(data_GMM_I_sh$INV_o1_06, 1) +
                    lag(data_GMM_I_sh$INV_o1_07, 1) + lag(data_GMM_I_sh$INV_o1_08, 1) +
                   
                   lag(data_GMM_I_sh$BVT_o1_03, 1) + lag(data_GMM_I_sh$BVT_o1_04, 1) + 
                   lag(data_GMM_I_sh$BVT_o1_05, 1) + lag(data_GMM_I_sh$BVT_o1_06, 1) +
                   lag(data_GMM_I_sh$BVT_o1_06, 1) + lag(data_GMM_I_sh$BVT_o1_08, 1) 
                 
                 | lag(YF_T, 2:4) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh, transformation = "ld")
summary(modelgmmTI0, robust = TRUE)

# modelgmmT0X

sargan(modelgmmTI0)

# r1 - hovedstaden - comme reference!!!

# data_sub = subset(data_GMM_I_sh, data_GMM_I_sh$Region_navn!="Ø-kommuner")

modelgmmTI1<-pgmm(YF_T ~ lag(YF_T, 1)  + 
                   lag(BVT_F, 1) + lag(INV_F, 1) +
                    
                   lag(INV_F_r2, 1) + lag(INV_F_r3, 1) +
                    lag(INV_F_r5, 1) + lag(INV_F_r6, 1) +
                   
                    lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                   lag(BVT_r5, 1) + lag(BVT_r6, 1) + # lag(BVT_r4, 1) +
                   
                   lag(data_GMM_I_sh$BVT_r2_03, 1) + lag(data_GMM_I_sh$BVT_r2_04, 1) + 
                   lag(data_GMM_I_sh$BVT_r2_05, 1)  + lag(data_GMM_I_sh$BVT_r2_06, 1)  +
                   lag(data_GMM_I_sh$BVT_r2_07, 1)  + lag(data_GMM_I_sh$BVT_r2_08, 1)  +
                   
                   lag(data_GMM_I_sh$BVT_r3_03, 1) + lag(data_GMM_I_sh$BVT_r3_04, 1) + 
                   lag(data_GMM_I_sh$BVT_r3_05, 1) + lag(data_GMM_I_sh$BVT_r3_06, 1)  +
                   lag(data_GMM_I_sh$BVT_r3_07, 1) + lag(data_GMM_I_sh$BVT_r3_08, 1)  +
                   
                   # lag(data_GMM_I_sh$BVT_r4_03, 1) + lag(data_GMM_I_sh$BVT_r4_04, 1) + 
                   # lag(data_GMM_I_sh$BVT_r4_05, 1) + lag(data_GMM_I_sh$BVT_r4_06, 1)  +
                   # lag(data_GMM_I_sh$BVT_r4_07, 1) + lag(data_GMM_I_sh$BVT_r4_08, 1) +
                   
                   lag(data_GMM_I_sh$BVT_r5_03, 1) + lag(data_GMM_I_sh$BVT_r5_04, 1) + 
                   lag(data_GMM_I_sh$BVT_r5_05, 1) + lag(data_GMM_I_sh$BVT_r5_06, 1)  +
                   lag(data_GMM_I_sh$BVT_r5_07, 1) + lag(data_GMM_I_sh$BVT_r5_08, 1) +
                   
                   lag(data_GMM_I_sh$BVT_r6_03, 1) + lag(data_GMM_I_sh$BVT_r6_04, 1) + 
                   lag(data_GMM_I_sh$BVT_r6_05, 1)  + lag(data_GMM_I_sh$BVT_r6_06, 1)  +
                   lag(data_GMM_I_sh$BVT_r6_07, 1)  + lag(data_GMM_I_sh$BVT_r6_08, 1) 
                 
                 | lag(YF_T, 2:3) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh,
                 transformation = "ld")

summary(modelgmmTI1)

sargan(modelgmmT1)

summary(data_GMM_I_sh$Region_navn)


# 
# modelgmmT1<-pgmm(YF_T ~ lag(YF_T, 1) + lag(YF_T, 2) + lag(BVT_F, 1) + lag(EMP_T, 1)  
#                 + lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
#                 + lag(data_GMM_I_sh$EMP_T03, 1) + lag(data_GMM_I_sh$EMP_T04, 1) +
#                   + lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
#                   + lag(data_GMM_I_sh$EMP_T06, 1) + lag(data_GMM_I_sh$EMP_T07, 1) +
#                   lag(BVT_r1, 1) + lag(EMP_T_r1, 1) + lag(BVT_r2, 1) + lag(EMP_T_r2, 1) + lag(BVT_r3, 1) + 
#                   lag(EMP_T_r3, 1) + lag(BVT_r4, 1) + lag(EMP_T_r4, 1) + 
#                   lag(BVT_r5, 1) + lag(EMP_T_r5, 1) + 
#                   lag(BVT_r1_07, 1) + lag(EMP_T_r1_07, 1) + lag(BVT_r2_07, 1) + lag(EMP_T_r2_07, 1) + 
#                   lag(BVT_r3_07, 1) + lag(EMP_T_r3_07, 1) 
#                 | lag(YF_T, 3:5) , data=data_GMM_I_sh, 
#                 transformation = "ld")
# 
# summary(modelgmmT1)
# 
# sargan(modelgmmT1)



# lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) 

# lag(data_GMM_I_sh$BVT_o1_07, 1) +
# lag(data_GMM_I_sh$EMP_P_o1_07, 1)


modelgmmPI0<-pgmm(YF_P ~ lag(YF_P, 1) + lag(BVT_F, 1)  + lag(INV_F, 1) +
                   
                   lag(BVT_o1, 1) + lag(INV_F_o1, 1) +
                   
                   lag(data_GMM_I_sh$INV_o1_03, 1) + lag(data_GMM_I_sh$INV_o1_04, 1) + 
                   lag(data_GMM_I_sh$INV_o1_05, 1) + lag(data_GMM_I_sh$INV_o1_06, 1) +
                   lag(data_GMM_I_sh$INV_o1_07, 1) + lag(data_GMM_I_sh$INV_o1_08, 1) +
                   
                   lag(data_GMM_I_sh$BVT_o1_03, 1) + lag(data_GMM_I_sh$BVT_o1_04, 1) + 
                   lag(data_GMM_I_sh$BVT_o1_05, 1) + lag(data_GMM_I_sh$BVT_o1_06, 1) +
                   lag(data_GMM_I_sh$BVT_o1_07, 1) + lag(data_GMM_I_sh$BVT_o1_08, 1) 
                 
                 | lag(YF_P, 2:3) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh,
                 transformation = "ld")
summary(modelgmmPI0, robust = TRUE)

sargan(modelgmmPI0)





# modelgmmP0<-pgmm (YF_P ~ lag(YF_P, 1) + lag(YF_P, 2) + lag(BVT_F, 1) + lag(EMP_P, 1) +
#                  lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
#                   lag(data_GMM_I_sh$EMP_P03, 1) + lag(data_GMM_I_sh$EMP_P04, 1) +
#                   lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
#                   lag(data_GMM_I_sh$EMP_P06, 1) + lag(data_GMM_I_sh$EMP_P07, 1) +
#                   lag(BVT_o1, 1) + lag(EMP_P_o1, 1) +
#                   lag(data_GMM_I_sh$BVT_o1_04, 1) + lag(data_GMM_I_sh$BVT_o1_06, 1) + lag(data_GMM_I_sh$BVT_o1_07, 1) + 
#                   lag(data_GMM_I_sh$EMP_P_o1_04) + lag(data_GMM_I_sh$EMP_P_o1_06, 1) + lag(data_GMM_I_sh$EMP_P_o1_07, 1)  
#                 | lag(YF_P, 3:5) , data=data_GMM_I_sh, transformation = "ld")
# summary(modelgmmP0)
# 
# sargan(modelgmmP0)




modelgmmPI1<-pgmm(YF_P ~ lag(YF_P, 1)  + 
                   
                   lag(BVT_F, 1) + lag(INV_F, 1) +
                    
                    lag(INV_F_r2, 1) + lag(INV_F_r3, 1) + 
                    lag(INV_F_r5, 1) +lag(INV_F_r6, 1) + 
                   
                   lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) +  lag(BVT_r6, 1) +
                   

                   
                   lag(data_GMM_I_sh$BVT_r2_03, 1) + lag(data_GMM_I_sh$BVT_r2_04, 1) + 
                   lag(data_GMM_I_sh$BVT_r2_05, 1)  + lag(data_GMM_I_sh$BVT_r2_06, 1)  +
                   lag(data_GMM_I_sh$BVT_r2_07, 1)  + lag(data_GMM_I_sh$BVT_r2_08, 1)  +
                   
                   lag(data_GMM_I_sh$BVT_r3_03, 1) + lag(data_GMM_I_sh$BVT_r3_04, 1) + 
                   lag(data_GMM_I_sh$BVT_r3_05, 1) + lag(data_GMM_I_sh$BVT_r3_06, 1) +
                   lag(data_GMM_I_sh$BVT_r3_07, 1)  + lag(data_GMM_I_sh$BVT_r3_08, 1) 
                   

                   

                 
                 | lag(YF_P, 2:3) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh, transformation = "ld")

summary(modelgmmPI1)

sargan(modelgmmP1)

summary(data_GMM_I_sh$Region_navn)

# lag(data_GMM_I_sh$BVT_r1_03, 1) + lag(data_GMM_I_sh$BVT_r1_04, 1) + 
#   lag(data_GMM_I_sh$BVT_r1_05, 1)  + lag(data_GMM_I_sh$BVT_r1_06, 1)  +
#   lag(data_GMM_I_sh$BVT_r1_07, 1)  + lag(data_GMM_I_sh$BVT_r1_08, 1)  +

# lag(data_GMM_I_sh$BVT_r5_03, 1) + lag(data_GMM_I_sh$BVT_r5_04, 1) + 
#   lag(data_GMM_I_sh$BVT_r5_05, 1) + lag(data_GMM_I_sh$BVT_r5_06, 1) +
#   lag(data_GMM_I_sh$BVT_r5_07, 1)  + lag(data_GMM_I_sh$BVT_r5_08, 1)
# 

# lag(data_GMM_I_sh$BVT_r6_03, 1) + lag(data_GMM_I_sh$BVT_r6_04, 1) + 
#   lag(data_GMM_I_sh$BVT_r6_05, 1) + lag(data_GMM_I_sh$BVT_r6_06, 1) +
#   lag(data_GMM_I_sh$BVT_r6_07, 1)  + lag(data_GMM_I_sh$BVT_r6_08, 1) 



# 
# data_GMM_I_sh$EMP_T_o1 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$o1
# 
# data_GMM_I_sh$EMP_T_o1_04 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$o1*data_GMM_I_sh$d04
# data_GMM_I_sh$EMP_T_o1_07 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$o1*data_GMM_I_sh$d07
# 
# data_GMM_I_sh$EMP_T_r1 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$r1
# data_GMM_I_sh$EMP_T_r2 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$r2
# data_GMM_I_sh$EMP_T_r3 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$r3
# data_GMM_I_sh$EMP_T_r4 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$r4
# data_GMM_I_sh$EMP_T_r5 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$r5
# data_GMM_I_sh$EMP_T_r6 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$r6
# 
# 
# data_GMM_I_sh$EMP_P_o1 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$o1
# 
# data_GMM_I_sh$INV_F_o1 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1
# 
# data_GMM_I_sh$EMP_P_o1_04 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$o1*data_GMM_I_sh$d04
# data_GMM_I_sh$EMP_P_o1_07 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$o1*data_GMM_I_sh$d07
# 
# data_GMM_I_sh$INV_F_o1_04 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d04
# data_GMM_I_sh$INV_F_o1_07 = data_GMM_I_sh$INV_F*data_GMM_I_sh$o1*data_GMM_I_sh$d07
# 
# 
# data_GMM_I_sh$EMP_P_r1 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$r1
# data_GMM_I_sh$EMP_P_r2 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$r2
# data_GMM_I_sh$EMP_P_r3 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$r3
# data_GMM_I_sh$EMP_P_r4 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$r4
# data_GMM_I_sh$EMP_P_r5 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$r5
# data_GMM_I_sh$EMP_P_r6 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$r6
# 
# data_GMM_I_sh$INV_F_r1 = data_GMM_I_sh$INV_F*data_GMM_I_sh$r1
# data_GMM_I_sh$INV_F_r2 = data_GMM_I_sh$INV_F*data_GMM_I_sh$r2
# data_GMM_I_sh$INV_F_r3 = data_GMM_I_sh$INV_F*data_GMM_I_sh$r3
# data_GMM_I_sh$INV_F_r4 = data_GMM_I_sh$INV_F*data_GMM_I_sh$r4
# data_GMM_I_sh$INV_F_r5 = data_GMM_I_sh$INV_F*data_GMM_I_sh$r5
# data_GMM_I_sh$INV_F_r6 = data_GMM_I_sh$INV_F*data_GMM_I_sh$r6
# 
# 
# data_GMM_I_sh$BVT_03 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$d03
# data_GMM_I_sh$BVT_04 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$d04
# # data_GMM_I_sh$BVT_05 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$d05
# data_GMM_I_sh$BVT_06 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$d06
# data_GMM_I_sh$BVT_07 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$d07
# # data_GMM_I_sh$BVT_08 = data_GMM_I_sh$BVT_F*data_GMM_I_sh$d08
# # data_GMM_sh$BVT_09 = data_GMM_sh$BVT_F*data_GMM_sh$d09
# # data_GMM_sh$BVT_10 = data_GMM_sh$BVT_F*data_GMM_sh$d10
# # data_GMM_sh$BVT_11 = data_GMM_sh$BVT_F*data_GMM_sh$d11
# # data_GMM_sh$BVT_12 = data_GMM_sh$BVT_F*data_GMM_sh$d12
# # data_GMM_sh$BVT_13 = data_GMM_sh$BVT_F*data_GMM_sh$d13
# # data_GMM_sh$BVT_14 = data_GMM_sh$BVT_F*data_GMM_sh$d14
# 
# 
# 
# 
# # data_GMM_I_sh$EMP_T02 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d02
# data_GMM_I_sh$EMP_T03 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d03
# data_GMM_I_sh$EMP_T04 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d04
# # data_GMM_I_sh$EMP_T05 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d05
# data_GMM_I_sh$EMP_T06 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d06
# data_GMM_I_sh$EMP_T07 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d07
# # data_GMM_I_sh$EMP_T08 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d08
# # data_GMM_I_sh$EMP_T09 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d09
# # data_GMM_I_sh$EMP_T10 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d10
# # data_GMM_I_sh$EMP_T11 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d11
# # data_GMM_I_sh$EMP_T12 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d12
# # data_GMM_I_sh$EMP_T13 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d13
# # data_GMM_I_sh$EMP_T14 = data_GMM_I_sh$EMP_T*data_GMM_I_sh$d14
# # 
# 
# # data_GMM_I_sh$EMP_P02 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d02
# data_GMM_I_sh$EMP_P03 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d03
# data_GMM_I_sh$EMP_P04 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d04
# # data_GMM_I_sh$EMP_P05 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d05
# data_GMM_I_sh$EMP_P06 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d06
# data_GMM_I_sh$EMP_P07 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d07
# # data_GMM_I_sh$EMP_P08 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d08
# # data_GMM_I_sh$EMP_P09 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d09
# # data_GMM_I_sh$EMP_P10 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d10
# # data_GMM_I_sh$EMP_P11 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d11
# # data_GMM_I_sh$EMP_P12 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d12
# # data_GMM_I_sh$EMP_P13 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d13
# # data_GMM_I_sh$EMP_P14 = data_GMM_I_sh$EMP_P*data_GMM_I_sh$d14





################################################
###############   MODEL with INVESTMENTS!! 2006-2011
################################################


modelgmmTI0X<-pgmm(YF_T ~ lag(YF_T, 1) + lag(BVT_F, 1) + lag(INV_F, 1) +
                    
                    lag(BVT_o1, 1) + lag(INV_F_o1, 1) +
                    
                    lag(data_GMM_I_sh$INV_o1_06, 1) + lag(data_GMM_I_sh$INV_o1_07, 1) + 
                    lag(data_GMM_I_sh$INV_o1_08, 1) + lag(data_GMM_I_sh$INV_o1_09, 1) +
                    lag(data_GMM_I_sh$INV_o1_10, 1) + lag(data_GMM_I_sh$INV_o1_11, 1) +
                    
                    lag(data_GMM_I_sh$BVT_o1_06, 1) + lag(data_GMM_I_sh$BVT_o1_07, 1) + 
                    lag(data_GMM_I_sh$BVT_o1_08, 1) + lag(data_GMM_I_sh$BVT_o1_09, 1) +
                    lag(data_GMM_I_sh$BVT_o1_10, 1) + lag(data_GMM_I_sh$BVT_o1_11, 1) 
                  
                  | lag(YF_T, 2:3) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh, transformation = "ld")
summary(modelgmmTI0X, robust = TRUE)

# modelgmmT0X

sargan(modelgmmTI0X)

# r1 - hovedstaden - comme reference!!!

# data_sub = subset(data_GMM_I_sh, data_GMM_I_sh$Region_navn!="Ø-kommuner")

modelgmmTI1X<-pgmm(YF_T ~ lag(YF_T, 1)  + 
                    lag(BVT_F, 1) + lag(INV_F, 1) +
                    
                    lag(INV_F_r2, 1) + lag(INV_F_r3, 1) +
                     lag(INV_F_r5, 1) + lag(INV_F_r6, 1) +
                    
                     lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) + lag(BVT_r6, 1) + # lag(BVT_r4, 1) + 
                    
                    lag(data_GMM_I_sh$BVT_r2_06, 1) + lag(data_GMM_I_sh$BVT_r2_07, 1) + 
                    lag(data_GMM_I_sh$BVT_r2_08, 1)  + lag(data_GMM_I_sh$BVT_r2_09, 1)  +
                    lag(data_GMM_I_sh$BVT_r2_10, 1)  + lag(data_GMM_I_sh$BVT_r2_11, 1)  +
                    
                    lag(data_GMM_I_sh$BVT_r3_06, 1) + lag(data_GMM_I_sh$BVT_r3_07, 1) + 
                    lag(data_GMM_I_sh$BVT_r3_08, 1) + lag(data_GMM_I_sh$BVT_r3_09, 1)  +
                    lag(data_GMM_I_sh$BVT_r3_10, 1) + lag(data_GMM_I_sh$BVT_r3_11, 1)  +
                    
                    # lag(data_GMM_I_sh$BVT_r4_06, 1) + lag(data_GMM_I_sh$BVT_r4_06, 1) + 
                    # lag(data_GMM_I_sh$BVT_r4_08, 1) + lag(data_GMM_I_sh$BVT_r4_06, 1)  +
                    # lag(data_GMM_I_sh$BVT_r4_07, 1) + lag(data_GMM_I_sh$BVT_r4_08, 1) +
                    
                    lag(data_GMM_I_sh$BVT_r5_06, 1) + lag(data_GMM_I_sh$BVT_r5_07, 1) + 
                    lag(data_GMM_I_sh$BVT_r5_08, 1) + lag(data_GMM_I_sh$BVT_r5_09, 1)  +
                    lag(data_GMM_I_sh$BVT_r5_10, 1) + lag(data_GMM_I_sh$BVT_r5_11, 1) +
                    
                    lag(data_GMM_I_sh$BVT_r6_06, 1) + lag(data_GMM_I_sh$BVT_r6_07, 1) + 
                    lag(data_GMM_I_sh$BVT_r6_08, 1)  + lag(data_GMM_I_sh$BVT_r6_09, 1)  +
                    lag(data_GMM_I_sh$BVT_r6_10, 1)  + lag(data_GMM_I_sh$BVT_r6_11, 1) 
                  
                  | lag(YF_T, 2:3) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh,
                  transformation = "ld")

summary(modelgmmTI1X)

sargan(modelgmmT1X)

summary(data_GMM_I_sh$Region_navn)

# 
# modelgmmT1<-pgmm(YF_T ~ lag(YF_T, 1) + lag(YF_T, 2) + lag(BVT_F, 1) + lag(EMP_T, 1)  
#                 + lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
#                 + lag(data_GMM_I_sh$EMP_T03, 1) + lag(data_GMM_I_sh$EMP_T04, 1) +
#                   + lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
#                   + lag(data_GMM_I_sh$EMP_T06, 1) + lag(data_GMM_I_sh$EMP_T07, 1) +
#                   lag(BVT_r1, 1) + lag(EMP_T_r1, 1) + lag(BVT_r2, 1) + lag(EMP_T_r2, 1) + lag(BVT_r3, 1) + 
#                   lag(EMP_T_r3, 1) + lag(BVT_r4, 1) + lag(EMP_T_r4, 1) + 
#                   lag(BVT_r5, 1) + lag(EMP_T_r5, 1) + 
#                   lag(BVT_r1_07, 1) + lag(EMP_T_r1_07, 1) + lag(BVT_r2_07, 1) + lag(EMP_T_r2_07, 1) + 
#                   lag(BVT_r3_07, 1) + lag(EMP_T_r3_07, 1) 
#                 | lag(YF_T, 3:5) , data=data_GMM_I_sh, 
#                 transformation = "ld")
# 
# summary(modelgmmT1)
# 
# sargan(modelgmmT1)



# lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) 

# lag(data_GMM_I_sh$BVT_o1_07, 1) +
# lag(data_GMM_I_sh$EMP_P_o1_07, 1)


modelgmmPI0X<-pgmm(YF_P ~ lag(YF_P, 1) + lag(BVT_F, 1)  + lag(INV_F, 1) +
                    
                    lag(BVT_o1, 1) + lag(INV_F_o1, 1) +
                    
                    lag(data_GMM_I_sh$INV_o1_06, 1) + lag(data_GMM_I_sh$INV_o1_07, 1) + 
                    lag(data_GMM_I_sh$INV_o1_08, 1) + lag(data_GMM_I_sh$INV_o1_09, 1) +
                    lag(data_GMM_I_sh$INV_o1_10, 1) + lag(data_GMM_I_sh$INV_o1_11, 1) +
                    
                    lag(data_GMM_I_sh$BVT_o1_06, 1) + lag(data_GMM_I_sh$BVT_o1_07, 1) + 
                    lag(data_GMM_I_sh$BVT_o1_08, 1) + lag(data_GMM_I_sh$BVT_o1_09, 1) +
                    lag(data_GMM_I_sh$BVT_o1_10, 1) + lag(data_GMM_I_sh$BVT_o1_11, 1) 
                  
                  | lag(YF_P, 2:4) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh,
                  transformation = "ld")
summary(modelgmmPI0X, robust = TRUE)

sargan(modelgmmPI0)





# modelgmmP0<-pgmm (YF_P ~ lag(YF_P, 1) + lag(YF_P, 2) + lag(BVT_F, 1) + lag(EMP_P, 1) +
#                  lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
#                   lag(data_GMM_I_sh$EMP_P03, 1) + lag(data_GMM_I_sh$EMP_P04, 1) +
#                   lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
#                   lag(data_GMM_I_sh$EMP_P06, 1) + lag(data_GMM_I_sh$EMP_P07, 1) +
#                   lag(BVT_o1, 1) + lag(EMP_P_o1, 1) +
#                   lag(data_GMM_I_sh$BVT_o1_04, 1) + lag(data_GMM_I_sh$BVT_o1_06, 1) + lag(data_GMM_I_sh$BVT_o1_07, 1) + 
#                   lag(data_GMM_I_sh$EMP_P_o1_04) + lag(data_GMM_I_sh$EMP_P_o1_06, 1) + lag(data_GMM_I_sh$EMP_P_o1_07, 1)  
#                 | lag(YF_P, 3:5) , data=data_GMM_I_sh, transformation = "ld")
# summary(modelgmmP0)
# 
# sargan(modelgmmP0)




modelgmmPI1X<-pgmm(YF_P ~ lag(YF_P, 1)  + 
                    
                    lag(BVT_F, 1) + lag(INV_F, 1) +
                    
                    lag(INV_F_r2, 1) + 
                     lag(INV_F_r3, 1) + lag(INV_F_r5, 1) + lag(INV_F_r6, 1) +
                    
                    lag(BVT_r2, 1) + lag(BVT_r3, 1) +  
                    lag(BVT_r5, 1) +  lag(BVT_r6, 1) + 
                    
                    
                    
                    lag(data_GMM_I_sh$BVT_r2_06, 1) + lag(data_GMM_I_sh$BVT_r2_07, 1) + 
                    lag(data_GMM_I_sh$BVT_r2_08, 1)  + lag(data_GMM_I_sh$BVT_r2_09, 1)  +
                    lag(data_GMM_I_sh$BVT_r2_10, 1)  + lag(data_GMM_I_sh$BVT_r2_11, 1)  +
                    
                    lag(data_GMM_I_sh$BVT_r3_06, 1) + lag(data_GMM_I_sh$BVT_r3_07, 1) + 
                    lag(data_GMM_I_sh$BVT_r3_08, 1) + lag(data_GMM_I_sh$BVT_r3_09, 1) +
                    lag(data_GMM_I_sh$BVT_r3_10, 1)  + lag(data_GMM_I_sh$BVT_r3_11, 1) 
                  
                  
                  
                  
                  
                  | lag(YF_P, 2:3) + lag(BVT_F, 2) + lag(INV_F, 2), data=data_GMM_I_sh, transformation = "ld")

summary(modelgmmPI1X)





























############################
# OLD MULTICOLINER PART
############################

library(plm)
library(lmtest)
library(sandwich)


bptest(modelFD)     # heteroskedasticity in differenced errors => robust
bptest(modelFDd)    # heteroskedasticity in differenced errors => robust
bptest(modelRE)

?plm

?pgmm

head(data_GMM_I_sh)

modelgmmT0<-pgmm(YF_T ~ lag(YF_T, 1) + lag(BVT_F, 1) + lag(EMP_T, 1) + lag(INV_F, 1) 
                   + lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
                   + lag(data_GMM_I_sh$EMP_T03, 1) + lag(data_GMM_I_sh$EMP_T04, 1) +
                   + lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
                   + lag(data_GMM_I_sh$EMP_T06, 1) + lag(data_GMM_I_sh$EMP_T07, 1) +
                   lag(data_GMM_I_sh$BVT_o1, 1) + lag(data_GMM_I_sh$EMP_T_o1, 1) +
                   lag(data_GMM_I_sh$INV_F_o1, 1) +
                   lag(data_GMM_I_sh$BVT_o1_04, 1) + lag(data_GMM_I_sh$BVT_o1_07, 1) +
                  lag(data_GMM_I_sh$EMP_T_o1_04, 1) + lag(data_GMM_I_sh$EMP_T_o1_07, 1) +
                   lag(data_GMM_I_sh$INV_F_o1_04, 1) + lag(data_GMM_I_sh$INV_F_o1_07, 1)

                 | lag(YF_T, 2) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) , data=data_GMM_I_sh, 
                 transformation = "ld")
summary(modelgmmT0)

sargan(modelgmmT0)


# + lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
#   + lag(data_GMM_I_sh$EMP_T03, 1) + lag(data_GMM_I_sh$EMP_T04, 1) +
#   + lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
#   + lag(data_GMM_I_sh$EMP_T06, 1) + lag(data_GMM_I_sh$EMP_T07, 1) +
# #   lag(data_GMM_I_sh$BVT_o1, 1) + lag(data_GMM_I_sh$EMP_T_o1, 1) +
#   data_GMM_I_sh$BVT_o1_04 + data_GMM_I_sh$BVT_o1_07 +
#   data_GMM_I_sh$EMP_T_o1_04 + data_GMM_I_sh$EMP_T_o1_07


# lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) 

modelgmmT1<-pgmm(YF_T ~ lag(YF_T, 1) + lag(YF_T, 2) + lag(BVT_F, 1) + lag(EMP_T, 1) + lag(INV_F, 1)  
                 + lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
                   + lag(data_GMM_I_sh$EMP_T03, 1) + lag(data_GMM_I_sh$EMP_T04, 1) +
                   + lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
                   + lag(data_GMM_I_sh$EMP_T06, 1) + lag(data_GMM_I_sh$EMP_T07, 1)  +
                   lag(data_GMM_I_sh$BVT_r1, 1) + lag(data_GMM_I_sh$EMP_T_r1, 1) + lag(data_GMM_I_sh$BVT_r2, 1) + lag(data_GMM_I_sh$EMP_T_r2, 1) + 
                   lag(data_GMM_I_sh$BVT_r3, 1) + lag(data_GMM_I_sh$EMP_T_r3, 1) + lag(data_GMM_I_sh$BVT_r4, 1) + lag(data_GMM_I_sh$EMP_T_r4, 1) + 
                   lag(data_GMM_I_sh$BVT_r5, 1) + lag(data_GMM_I_sh$EMP_T_r5, 1) + lag(data_GMM_I_sh$BVT_r6, 1) + lag(data_GMM_I_sh$EMP_T_r6, 1) +
                   lag(data_GMM_I_sh$INV_F_r1, 1) + lag(data_GMM_I_sh$INV_F_r2, 1) + lag(data_GMM_I_sh$INV_F_r3, 1) + lag(data_GMM_I_sh$INV_F_r4, 1) +
                   lag(data_GMM_I_sh$INV_F_r5, 1) + lag(data_GMM_I_sh$INV_F_r6, 1)
                 | lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2)  , data=data_GMM_I_sh, 
                 transformation = "ld")

summary(modelgmmT1)





sargan(modelgmmT1)


# 

# # lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) 

modelgmmP0<-pgmm(YF_P ~ lag(YF_P, 1) + lag(YF_P, 2) + lag(BVT_F, 1) + lag(EMP_P, 1) + lag(INV_F, 1)
                 + lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
                   + lag(data_GMM_I_sh$EMP_P03, 1) + lag(data_GMM_I_sh$EMP_P04, 1) +
                   + lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
                   + lag(data_GMM_I_sh$EMP_P06, 1) + lag(data_GMM_I_sh$EMP_P07, 1) +
                    lag(data_GMM_I_sh$BVT_o1, 1) + lag(data_GMM_I_sh$EMP_P_o1, 1) +
                   lag(data_GMM_I_sh$INV_F_o1, 1) +
                   lag(data_GMM_I_sh$BVT_o1_04, 1) + lag(data_GMM_I_sh$BVT_o1_07, 1) +
                  lag(data_GMM_I_sh$EMP_P_o1_04, 1) + lag(data_GMM_I_sh$EMP_P_o1_07, 1) +
                   lag(data_GMM_I_sh$INV_F_o1_04, 1) + lag(data_GMM_I_sh$INV_F_o1_07, 1)

                 | lag(YF_P, 3) + lag(BVT_F, 2) + lag(EMP_P, 2) + lag(INV_F, 2) , data=data_GMM_I_sh, 
                 transformation = "ld")
summary(modelgmmP0)

sargan(modelgmmP0)

# lag(data_GMM_I_sh$BVT_o1, 1) + lag(data_GMM_I_sh$EMP_P_o1, 1)
# data_GMM_I_sh$BVT_o1_04 + data_GMM_I_sh$BVT_o1_07 +
# data_GMM_I_sh$EMP_P_o1_04 + data_GMM_I_sh$EMP_P_o1_07


# # lag(YF_T, 3) + lag(BVT_F, 2) + lag(EMP_T, 2) + lag(INV_F, 2) 

modelgmmP1<-pgmm(YF_P ~ lag(YF_P, 1) + lag(YF_P, 2) + lag(BVT_F, 1) + lag(EMP_P, 1) + lag(INV_F, 1)  
                 + lag(data_GMM_I_sh$BVT_03, 1) + lag(data_GMM_I_sh$BVT_04, 1) +
                   + lag(data_GMM_I_sh$EMP_P03, 1) + lag(data_GMM_I_sh$EMP_P04, 1) +
                   + lag(data_GMM_I_sh$BVT_06, 1) + lag(data_GMM_I_sh$BVT_07, 1) +
                   + lag(data_GMM_I_sh$EMP_P06, 1) + lag(data_GMM_I_sh$EMP_P07, 1) +
                   lag(BVT_r1, 1) + lag(EMP_P_r1, 1) + lag(BVT_r2, 1) + lag(EMP_P_r2, 1) + lag(BVT_r3, 1) + lag(EMP_P_r3, 1) + lag(BVT_r4, 1) + lag(EMP_P_r4, 1) + 
                   lag(BVT_r5, 1) + lag(EMP_P_r5, 1) + lag(data_GMM_I_sh$INV_F_r1, 1) + lag(data_GMM_I_sh$INV_F_r2, 1) + lag(data_GMM_I_sh$INV_F_r3, 1) + 
                   lag(data_GMM_I_sh$INV_F_r4, 1) + lag(data_GMM_I_sh$INV_F_r5, 1)
                 | lag(YF_P, 3) + lag(BVT_F, 2) + lag(EMP_P, 2) + lag(INV_F, 2) , data=data_GMM_I_sh, 
                 transformation = "ld")

summary(modelgmmP1)

sargan(modelgmmP1)

# +
# lag(data_GMM_I_sh$INV_F_r1, 1) + lag(data_GMM_I_sh$INV_F_r2, 1) + lag(data_GMM_I_sh$INV_F_r3, 1) + lag(data_GMM_I_sh$INV_F_r4, 1) +
#  lag(data_GMM_I_sh$INV_F_r5, 1) + lag(data_GMM_I_sh$INV_F_r6, 1)


