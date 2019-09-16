
############################
# TO BE USED FOR GMM !!!
############################

# For EXCEL PIVOT TABLES and Filtering Versus R coding! 

# DATA LONG pour PLOTMEANS
# DATA LONG pour GMM peut etre (MERGED)!!!
# DATA LONG back to WIDE to construct PRE, CRISIS, POST and PRODUCTIVITY VARIABLES / RATES

###############################
# Eliminating DUPLICATES"""
# NUTS1 - NUTS2 separations and merging with PRODUCTIVITY data 
###############################



library(gplots)
library(reshape2)


###############
# EMP_EDUC data (WIDE to LONG for EXCEL PIVOT TABLE and PCA - Faster than R coding with DPLYR - Filter and Summarize ...)
###############

# EMP_ED0-2
emp_educ <- read.csv("EMP_ED0-2.csv", header = T)

head(emp_educ)
str(emp_educ)
attributes(emp_educ)

write.csv(emp_educ,"emp_educ_ED0-2.csv")

emp_educ <- read.csv("emp_educ_ED0-2.csv", header = T)
names(emp_educ)

anyDuplicated(emp_educ)
which(!duplicated(data))

emp_educ_long = reshape(data = emp_educ,
                   idvar = "Region",
                   varying=list(names(emp_educ)[5:ncol(emp_educ)]),
                   sep = ".",
                   timevar = "Year",
                   times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                             2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                   direction = "long")
#
head(emp_educ_long)


write.csv(emp_educ_long,"emp_educ_ED0-2_long.csv")


# EMP_ED3-4
emp_educ <- read.csv("EMP_ED3-4.csv", header = T)


head(emp_educ)
str(emp_educ)
attributes(emp_educ)


write.csv(emp_educ,"emp_educ_ED3-4.csv")



emp_educ <- read.csv("emp_educ_ED3-4.csv", header = T)
names(emp_educ)
head(emp_educ)

anyDuplicated(emp_educ)
which(!duplicated(data))

emp_educ_long = reshape(data = emp_educ,
                        idvar = "Region",
                        varying=list(names(emp_educ)[4:ncol(emp_educ)]),
                        sep = ".",
                        timevar = "Year",
                        times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                        direction = "long")
#
head(emp_educ_long)


write.csv(emp_educ_long,"emp_educ_ED3-4_long.csv")





# EMP_ED5-8
emp_educ <- read.csv("EMP_EDUC_ED5-8.csv", header = T)


head(emp_educ)
str(emp_educ)
attributes(emp_educ)


write.csv(emp_educ,"emp_educ_ED5-8.csv")



emp_educ <- read.csv("emp_educ_ED5-8.csv", header = T)
names(emp_educ)
head(emp_educ)

anyDuplicated(emp_educ)
which(!duplicated(data))

emp_educ_long = reshape(data = emp_educ,
                        idvar = "Region",
                        varying=list(names(emp_educ)[5:ncol(emp_educ)]),
                        sep = ".",
                        timevar = "Year",
                        times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                        direction = "long")
#
head(emp_educ_long)


write.csv(emp_educ_long,"emp_educ_ED5-8_long.csv")



###############
# EMP_BRANCHE data (WIDE to LONG for PIVOT TABLE and PCA - Faster than DPLYR - Filter and Summarize ...)
###############


# A


emp_branch <- read.csv("EMP_branch_A.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_A.csv")



emp_branch <- read.csv("emp_branch_A.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                        idvar = "Region",
                        varying=list(names(emp_branch)[4:ncol(emp_branch)]),
                        sep = ".",
                        timevar = "Year",
                        times = c(
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                        direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_A_long.csv")



# B-E


emp_branch <- read.csv("EMP_branch_B-E.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_B-E.csv")



emp_branch <- read.csv("emp_branch_B-E.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_B-E_long.csv")


# F


emp_branch <- read.csv("EMP_branch_F.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_F.csv")



emp_branch <- read.csv("emp_branch_F.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_F_long.csv")


# G-I



emp_branch <- read.csv("EMP_branch_G-I.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_G-I.csv")



emp_branch <- read.csv("emp_branch_G-I.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_G-I_long.csv")


# J

emp_branch <- read.csv("EMP_branch_J.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_J.csv")



emp_branch <- read.csv("emp_branch_J.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_J_long.csv")


# K



emp_branch <- read.csv("EMP_branch_K.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_K.csv")



emp_branch <- read.csv("emp_branch_K.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_K_long.csv")


# L

emp_branch <- read.csv("EMP_branch_L.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_L.csv")



emp_branch <- read.csv("emp_branch_L.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_L_long.csv")



# M_N


emp_branch <- read.csv("EMP_branch_M-N.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_M-N.csv")



emp_branch <- read.csv("emp_branch_M-N.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_M-N_long.csv")



# O-Q


emp_branch <- read.csv("EMP_branch_O-Q.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_O-Q.csv")



emp_branch <- read.csv("emp_branch_O-Q.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_O-Q_long.csv")



# R-U


emp_branch <- read.csv("EMP_branch_R-U.csv", header = T)


head(emp_branch)
str(emp_branch)
attributes(emp_branch)


write.csv(emp_branch,"emp_branch_R-U.csv")



emp_branch <- read.csv("emp_branch_R-U.csv", header = T)
names(emp_branch)
head(emp_branch)

anyDuplicated(emp_branch)
which(!duplicated(data))

emp_branch_long = reshape(data = emp_branch,
                          idvar = "Region",
                          varying=list(names(emp_branch)[5:ncol(emp_branch)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(
                            2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(emp_branch_long)


write.csv(emp_branch_long,"emp_branch_R-U_long.csv")



###############
# EMP_Prof data (WIDE to LONG for PIVOT TABLE and PCA - Faster than DPLYR - Filter and Summarize ...)
###############

# EMP_CFAM
emp_prof <- read.csv("emp_Prof_CFAM.csv", header = T)

head(emp_prof)
str(emp_prof)
attributes(emp_prof)

write.csv(emp_prof,"emp_CFAM.csv")

emp_prof <- read.csv("emp_CFAM.csv", header = T)
names(emp_prof)

anyDuplicated(emp_prof)
which(!duplicated(data))

emp_prof_long = reshape(data = emp_prof,
                        idvar = "Region",
                        varying=list(names(emp_prof)[5:ncol(emp_prof)]),
                        sep = ".",
                        timevar = "Year",
                        times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                        direction = "long")
#
head(emp_prof_long)


write.csv(emp_prof_long,"emp_CFAM_long.csv")




# EMP_SAL
emp_prof <- read.csv("emp_SAL.csv", header = T)

head(emp_prof)
str(emp_prof)
attributes(emp_prof)

write.csv(emp_prof,"emp_SAL.csv")

emp_prof <- read.csv("emp_SAL.csv", header = T)
names(emp_prof)

anyDuplicated(emp_prof)
which(!duplicated(data))

emp_prof_long = reshape(data = emp_prof,
                        idvar = "Region",
                        varying=list(names(emp_prof)[5:ncol(emp_prof)]),
                        sep = ".",
                        timevar = "Year",
                        times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                        direction = "long")
#
head(emp_prof_long)


write.csv(emp_prof_long,"emp_SAL_long.csv")



# EMP_SELF
emp_prof <- read.csv("emp_SELF.csv", header = T)

head(emp_prof)
str(emp_prof)
attributes(emp_prof)

write.csv(emp_prof,"emp_SELF.csv")

emp_prof <- read.csv("emp_SELF.csv", header = T)
names(emp_prof)

anyDuplicated(emp_prof)
which(!duplicated(data))

emp_prof_long = reshape(data = emp_prof,
                        idvar = "Region",
                        varying=list(names(emp_prof)[5:ncol(emp_prof)]),
                        sep = ".",
                        timevar = "Year",
                        times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                        direction = "long")
#
head(emp_prof_long)


write.csv(emp_prof_long,"emp_SELF_long.csv")





# unemp_pct
unemp_pct <- read.csv("unemp_pct.csv", header = T)

head(unemp_pct)
str(unemp_pct)
attributes(unemp_pct)

write.csv(unemp_pct,"unemp_pct.csv")

unemp_pct <- read.csv("unemp_pct.csv", header = T)
names(unemp_pct)

anyDuplicated(unemp_pct)
which(!duplicated(data))

unemp_pct_long = reshape(data = unemp_pct,
                        idvar = "Region",
                        varying=list(names(unemp_pct)[5:ncol(unemp_pct)]),
                        sep = ".",
                        timevar = "Year",
                        times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                  2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                        direction = "long")
#
head(unemp_pct_long)


write.csv(unemp_pct_long,"unemp_pct_long.csv")



# act_ED0-2
act_ED0_2 <- read.csv("act_ED0-2.csv", header = T)

head(act_ED0_2)
str(act_ED0_2)
attributes(act_ED0_2)

write.csv(act_ED0_2,"act_ED0_2.csv")

act_ED0_2 <- read.csv("act_ED0_2.csv", header = T)
names(act_ED0_2)

anyDuplicated(act_ED0_2)
which(!duplicated(data))

act_ED0_2_long = reshape(data = act_ED0_2,
                         idvar = "Region",
                         varying=list(names(act_ED0_2)[6:ncol(act_ED0_2)]),
                         sep = ".",
                         timevar = "Year",
                         times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                   2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                         direction = "long")
#
head(act_ED0_2_long)


write.csv(act_ED0_2_long,"act_ED0_2_long.csv")




# act_ED3-4
act_ED3_4 <- read.csv("act_ED3_4.csv", header = T)

head(act_ED3_4)
str(act_ED3_4)
attributes(act_ED3_4)

write.csv(act_ED3_4,"act_ED3_4.csv")

act_ED3_4 <- read.csv("act_ED3_4.csv", header = T)
names(act_ED3_4)

anyDuplicated(act_ED3_4)
which(!duplicated(data))

act_ED3_4_long = reshape(data = act_ED3_4,
                         idvar = "Region",
                         varying=list(names(act_ED3_4)[6:ncol(act_ED3_4)]),
                         sep = ".",
                         timevar = "Year",
                         times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                   2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                         direction = "long")
#
head(act_ED3_4_long)


write.csv(act_ED3_4_long,"act_ED3_4_long.csv")



# act_ED5-8
act_ED5_8 <- read.csv("act_ED5_8.csv", header = T)

head(act_ED5_8)
str(act_ED5_8)
attributes(act_ED5_8)

write.csv(act_ED5_8,"act_ED5_8.csv")

act_ED5_8 <- read.csv("act_ED5_8.csv", header = T)
names(act_ED5_8)

anyDuplicated(act_ED5_8)
which(!duplicated(data))

act_ED5_8_long = reshape(data = act_ED5_8,
                         idvar = "Region",
                         varying=list(names(act_ED5_8)[6:ncol(act_ED5_8)]),
                         sep = ".",
                         timevar = "Year",
                         times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                   2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                         direction = "long")
#
head(act_ED5_8_long)


write.csv(act_ED5_8_long,"act_ED5_8_long.csv")



## Long Term Unemployed

# LTU_PC_ACT
ltu_PC_ACT <- read.csv("ltu_PC_ACT_last.csv", header = T)

head(ltu_PC_ACT)
str(ltu_PC_ACT)
attributes(ltu_PC_ACT)

write.csv(ltu_PC_ACT,"ltu_PC_ACT.csv")

ltu_PC_ACT <- read.csv("ltu_PC_ACT.csv", header = T)
names(ltu_PC_ACT)

anyDuplicated(ltu_PC_ACT)
which(!duplicated(data))

ltu_PC_ACT_long = reshape(data = ltu_PC_ACT,
                         idvar = "Region",
                         varying=list(names(ltu_PC_ACT)[4:ncol(ltu_PC_ACT)]),
                         sep = ".",
                         timevar = "Year",
                         times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                   2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                         direction = "long")
#
head(ltu_PC_ACT_long)


write.csv(ltu_PC_ACT_long,"ltu_PC_ACT_long.csv")



# LTU_PC_NE
ltu_PC_NE <- read.csv("ltu_PC_NE_last.csv", header = T)

head(ltu_PC_NE)
str(ltu_PC_NE)
attributes(ltu_PC_NE)

write.csv(ltu_PC_NE,"ltu_PC_NE.csv")

ltu_PC_NE <- read.csv("ltu_PC_NE.csv", header = T)
names(ltu_PC_NE)

anyDuplicated(ltu_PC_NE)
which(!duplicated(data))

ltu_PC_NE_long = reshape(data = ltu_PC_NE,
                          idvar = "Region",
                          varying=list(names(ltu_PC_NE)[4:ncol(ltu_PC_NE)]),
                          sep = ".",
                          timevar = "Year",
                          times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                    2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                          direction = "long")
#
head(ltu_PC_NE_long)


write.csv(ltu_PC_NE_long,"ltu_PC_NE_long.csv")




######################
# Merging for PCA  ###  No merges yet for GMM
######################

# READING CSV

UNEMP <- read.csv("Long_UNEMP_Data_Pivot_Tables.csv", header = T)
LTU <- read.csv("Long_LTU_Data_Pivot_Tables.csv", header = T)
EDUC <- read.csv("Long_EDUC_Data_Pivot_Tables.csv", header = T)
BRANCH <- read.csv("Long_BRANCH_Data_Pivot_Tables.csv", header = T)
PROF <- read.csv("Long_PROF_Data_Pivot_Tables.csv", header = T)



merge1 = merge(UNEMP, LTU, by = c("Region"), all = T)
tail(merge1)
str(merge1)

merge2 = merge(merge1, EDUC, by = c("Region"), all = T)
tail(merge2)
str(merge2)

merge3 = merge(merge2, PROF, by = c("Region"), all = T)
tail(merge3)
str(merge3)



###############################
# Eliminating DUPLICATES
# For NUTS1 - NUTS2 separations - Vector Formation
###############################


write.csv(merge3,"FULL_Data_Regions.csv")

# Done

merge4 = merge(merge3, BRANCH, by = c("Region"), all = T)
tail(merge4)
str(merge4)


duplicated(merge4[,-1])
merge4 = merge4[!duplicated(merge4[,-1]), ]


write.csv(merge4,"FULL_merge_Labor_Stat.csv")



# Averages

data_Av = merge4[,c(1,grep("Av", colnames(merge4)))]
str(data_Av)

# Pre-crisis

data_Pre = merge4[,c(1,grep("Pre", colnames(merge4)))]
str(data_Pre)

data_Pre = cbind(data_Pre, data_Av)
names(data_Pre)

duplicated(data_Pre[,-c(1,11)])
data_Pre = data_Pre[!duplicated(data_Pre[,-c(1,11)]), ]

write.csv(data_Pre,"Labor_Stat_Pre.csv")


# crisis

data_Cris = merge4[,c(1,grep("Cris", colnames(merge4)))]
str(data_Cris)

data_Cris= cbind(data_Cris, data_Av)
names(data_Cris)

duplicated(data_Cris[,-c(1,21)])
data_Cris = data_Cris[!duplicated(data_Cris[,-c(1,21)]), ]


write.csv(data_Cris,"Labor_Stat_Crisis.csv")


# Post-crisis

data_Post = merge4[,c(1,grep("Post", colnames(merge4)))]
str(data_Post)

data_Post = cbind(data_Post, data_Av)
names(data_Post)

duplicated(data_Post[,-c(1,21)])
data_Post = data_Post[!duplicated(data_Post[,-c(1,21)]), ]

write.csv(data_Post,"Labor_Stat_Post.csv")



# Subsetting with NUTS2 ONLY!


Post <- read.csv("Labor_Stat_Post_Neat.csv", header = T)
Pre <- read.csv("Labor_Stat_Pre_Neat.csv", header = T)
Crisis <- read.csv("Labor_Stat_Crisis_Neat.csv", header = T)

head(Post)


# Neat NUTS2 Regions already prepared
NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)


# Post
D = as.character(Post[,1])  %in% as.character(NUTS2[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

Post_nuts2 = Post[nuts2,]
str(Post_nuts2)
summary(Post_nuts2)


write.csv(Post_nuts2,"PCA_Labor_Stat_Post.csv")



# Pre
D = as.character(Pre[,1])  %in% as.character(NUTS2[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

Pre_nuts2 = Pre[nuts2,]
str(Pre_nuts2)
summary(Pre_nuts2)


write.csv(Pre_nuts2,"PCA_Labor_Stat_Pre.csv")



# Crisis
D = as.character(Crisis[,1])  %in% as.character(NUTS2[,1]) 
head(D)

nuts2 = which(D==TRUE)
nuts2

Crisis_nuts2 = Crisis[nuts2,]
str(Crisis_nuts2)
summary(Crisis_nuts2)


write.csv(Crisis_nuts2,"PCA_Labor_Stat_Crisis.csv")


