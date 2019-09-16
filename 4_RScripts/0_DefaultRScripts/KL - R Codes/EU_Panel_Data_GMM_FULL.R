




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


# 
# pdff = read.csv("Panel_Data_Prod_LTU_sh.csv", header = T)
# head(pdff)
# str(pdff)
# summary(pdff)




##################################################
#### IMPORTING NEW AFM-significant variables
##################################################



# ADULT unemployment

# unemp_20_64_T
unemp_20_64_T <- read.csv("Y20-64_T_unemp_pct.csv", header = T)

head(unemp_20_64_T)
str(unemp_20_64_T)


write.csv(unemp_20_64_T,"unemp_20_64_T_wide.csv")

unemp_20_64_T <- read.csv("unemp_20_64_T_wide.csv", header = T)
names(unemp_20_64_T)


unemp_20_64_T_long = reshape(data = unemp_20_64_T,
                   idvar = "Region",
                   varying=list(names(unemp_20_64_T)[5:ncol(unemp_20_64_T)]),
                   sep = ".",
                   timevar = "Year",
                   times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                             2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                   direction = "long")
#
head(unemp_20_64_T_long)


write.csv(unemp_20_64_T_long,"unemp_20_64_T_long.csv")





# unemp_20_64_M
unemp_20_64_M <- read.csv("Y20-64_M_unemp_pct.csv", header = T)

head(unemp_20_64_M)
str(unemp_20_64_M)


write.csv(unemp_20_64_M,"unemp_20_64_M_wide.csv")

unemp_20_64_M <- read.csv("unemp_20_64_M_wide.csv", header = T)
names(unemp_20_64_M)


unemp_20_64_M_long = reshape(data = unemp_20_64_M,
                             idvar = "Region",
                             varying=list(names(unemp_20_64_M)[5:ncol(unemp_20_64_M)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(unemp_20_64_M_long)


write.csv(unemp_20_64_M_long,"unemp_20_64_M_long.csv")





# unemp_20_64_F
unemp_20_64_F <- read.csv("Y20-64_F_unemp_pct.csv", header = T)

head(unemp_20_64_F)
str(unemp_20_64_F)


write.csv(unemp_20_64_F,"unemp_20_64_F_wide.csv")

unemp_20_64_F <- read.csv("unemp_20_64_F_wide.csv", header = T)
names(unemp_20_64_F)


unemp_20_64_F_long = reshape(data = unemp_20_64_F,
                             idvar = "Region",
                             varying=list(names(unemp_20_64_F)[5:ncol(unemp_20_64_F)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(unemp_20_64_F_long)


write.csv(unemp_20_64_F_long,"unemp_20_64_F_long.csv")






### YOUNG unemployment



# unemp_15_24_T
unemp_15_24_T <- read.csv("Y15-24_T_unemp_pct.csv", header = T)

head(unemp_15_24_T)
str(unemp_15_24_T)


write.csv(unemp_15_24_T,"unemp_15_24_T_wide.csv")

unemp_15_24_T <- read.csv("unemp_15_24_T_wide.csv", header = T)
names(unemp_15_24_T)


unemp_15_24_T_long = reshape(data = unemp_15_24_T,
                             idvar = "Region",
                             varying=list(names(unemp_15_24_T)[5:ncol(unemp_15_24_T)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(unemp_15_24_T_long)


write.csv(unemp_15_24_T_long,"unemp_15_24_T_long.csv")





# unemp_15_24_M
unemp_15_24_M <- read.csv("Y15-24_M_unemp_pct.csv", header = T)

head(unemp_15_24_M)
str(unemp_15_24_M)


write.csv(unemp_15_24_M,"unemp_15_24_M_wide.csv")

unemp_15_24_M <- read.csv("unemp_15_24_M_wide.csv", header = T)
names(unemp_15_24_M)


unemp_15_24_M_long = reshape(data = unemp_15_24_M,
                             idvar = "Region",
                             varying=list(names(unemp_15_24_M)[5:ncol(unemp_15_24_M)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(unemp_15_24_M_long)


write.csv(unemp_15_24_M_long,"unemp_15_24_M_long.csv")





# unemp_15_24_F
unemp_15_24_F <- read.csv("Y15-24_F_unemp_pct_COR.csv", header = T)

head(unemp_15_24_F)
str(unemp_15_24_F)


write.csv(unemp_15_24_F,"unemp_15_24_F_wide.csv")

unemp_15_24_F <- read.csv("unemp_15_24_F_wide.csv", header = T)
names(unemp_15_24_F)


unemp_15_24_F_long = reshape(data = unemp_15_24_F,
                             idvar = "Region",
                             varying=list(names(unemp_15_24_F)[5:ncol(unemp_15_24_F)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(unemp_15_24_F_long)


write.csv(unemp_15_24_F_long,"unemp_15_24_F_long.csv")



#############
### MERGES of UNEMP ##
#############



unemp_20_64_T_long <- read.csv("unemp_20_64_T_long.csv", header = T)
unemp_20_64_M_long <- read.csv("unemp_20_64_M_long.csv", header = T)
unemp_20_64_F_long <- read.csv("unemp_20_64_F_long.csv", header = T)
unemp_15_24_T_long <- read.csv("unemp_15_24_T_long.csv", header = T)
unemp_15_24_M_long <- read.csv("unemp_15_24_M_long.csv", header = T)
unemp_15_24_F_long <- read.csv("unemp_15_24_F_long.csv", header = T)


merge1 = merge(unemp_20_64_T_long, unemp_20_64_M_long , by = c("Region","Year"), all= T)
head(merge1)


merge2 = merge(merge1, unemp_20_64_F_long, by = c("Region","Year"), all= T)
head(merge2)



merge3 = merge(merge2, unemp_15_24_T_long, by = c("Region","Year"), all= T)
head(merge3)



merge4 = merge(merge3, unemp_15_24_M_long, by = c("Region","Year"), all= T)
head(merge4)



merge5 = merge(merge4,unemp_15_24_F_long, by = c("Region","Year"), all= T)
head(merge5)



# FULL UNEMPLOYMENT for GMM

write.csv(merge5,"unemp_FULL_long.csv")


# 
# 
# # RENAME A COLUMN
# colnames(merge2)[5] <- "GVA"
# colnames(merge2)[9] <- "EMP"
# colnames(merge2)[13] <- "SAL"
# 
# write.csv(merge2,"GVA_LAB.csv")






# ADULT SAL

# SAL_20_64
SAL_20_64 <- read.csv("Y20-64_SAL_pct.csv", header = T)

head(SAL_20_64)
str(SAL_20_64)


write.csv(SAL_20_64,"SAL_20_64_wide.csv")

SAL_20_64 <- read.csv("SAL_20_64_wide.csv", header = T)
names(SAL_20_64)


SAL_20_64_long = reshape(data = SAL_20_64,
                             idvar = "Region",
                             varying=list(names(SAL_20_64)[5:ncol(SAL_20_64)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(SAL_20_64_long)


write.csv(SAL_20_64_long,"SAL_20_64_long.csv")



# ADULT SELF

# SELF_20_64
SELF_20_64 <- read.csv("Y20-64_SELF_pct.csv", header = T)

head(SELF_20_64)
str(SELF_20_64)


write.csv(SELF_20_64,"SELF_20_64_wide.csv")

SELF_20_64 <- read.csv("SELF_20_64_wide.csv", header = T)
names(SELF_20_64)


SELF_20_64_long = reshape(data = SELF_20_64,
                         idvar = "Region",
                         varying=list(names(SELF_20_64)[5:ncol(SELF_20_64)]),
                         sep = ".",
                         timevar = "Year",
                         times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                   2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                         direction = "long")
#
head(SELF_20_64_long)


write.csv(SELF_20_64_long,"SELF_20_64_long.csv")





#############
### MERGES of PROF ##
#############



SAL_20_64_long <- read.csv("SAL_20_64_long.csv", header = T)
SELF_20_64_long <- read.csv("SELF_20_64_long.csv", header = T)


mergeA = merge(SAL_20_64_long , SELF_20_64_long , by = c("Region","Year"), all= T)
head(mergeA)


# FULL UNEMPLOYMENT for GMM

write.csv(mergeA,"Prof_FULL_long.csv")






# EDUCATION

# ADULT education

# educ_20_64_T - ED0
educ_20_64_T <- read.csv("Y20-64_ED0_pct.csv", header = T)

head(educ_20_64_T)
str(educ_20_64_T)


write.csv(educ_20_64_T,"educ_20_64_T_wide.csv")

educ_20_64_T <- read.csv("educ_20_64_T_wide.csv", header = T)
names(educ_20_64_T)


educ_20_64_T_long = reshape(data = educ_20_64_T,
                             idvar = "Region",
                             varying=list(names(educ_20_64_T)[6:ncol(educ_20_64_T)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ_20_64_T_long)


write.csv(educ_20_64_T_long,"educ_20_64_T_long.csv")





# educ_20_64_T - ED3
educ3_20_64_T <- read.csv("Y20-64_ED3_pct.csv", header = T)

head(educ3_20_64_T)
str(educ3_20_64_T)


write.csv(educ3_20_64_T,"educ3_20_64_T_wide.csv")

educ3_20_64_T <- read.csv("educ3_20_64_T_wide.csv", header = T)
names(educ3_20_64_T)


educ3_20_64_T_long = reshape(data = educ3_20_64_T,
                            idvar = "Region",
                            varying=list(names(educ3_20_64_T)[6:ncol(educ3_20_64_T)]),
                            sep = ".",
                            timevar = "Year",
                            times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                      2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                            direction = "long")
#
head(educ3_20_64_T_long)


write.csv(educ3_20_64_T_long,"educ3_20_64_T_long.csv")



# educ_20_64_T - ED5
educ5_20_64_T <- read.csv("Y20-64_ED5_pct.csv", header = T)

head(educ5_20_64_T)
str(educ5_20_64_T)


write.csv(educ5_20_64_T,"educ5_20_64_T_wide.csv")

educ5_20_64_T <- read.csv("educ5_20_64_T_wide.csv", header = T)
names(educ5_20_64_T)


educ5_20_64_T_long = reshape(data = educ5_20_64_T,
                             idvar = "Region",
                             varying=list(names(educ5_20_64_T)[6:ncol(educ5_20_64_T)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ5_20_64_T_long)


write.csv(educ5_20_64_T_long,"educ5_20_64_T_long.csv")





# educ_20_64_M - ED0
educ_20_64_M <- read.csv("Y20-64_ED0_M_pct.csv", header = T)

head(educ_20_64_M)
str(educ_20_64_M)


write.csv(educ_20_64_M,"educ0_20_64_M_wide.csv")

educ_20_64_M <- read.csv("educ_20_64_M_wide.csv", header = T)
names(educ_20_64_M)


educ_20_64_M_long = reshape(data = educ_20_64_M,
                            idvar = "Region",
                            varying=list(names(educ_20_64_M)[6:ncol(educ_20_64_M)]),
                            sep = ".",
                            timevar = "Year",
                            times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                      2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                            direction = "long")
#
head(educ_20_64_M_long)


write.csv(educ_20_64_M_long,"educ0_20_64_M_long.csv")





# educ_20_64_M - ED3
educ3_20_64_M <- read.csv("Y20-64_ED3_M_pct.csv", header = T)

head(educ3_20_64_M)
str(educ3_20_64_M)


write.csv(educ3_20_64_M,"educ3_20_64_M_wide.csv")

educ3_20_64_M <- read.csv("educ3_20_64_M_wide.csv", header = T)
names(educ3_20_64_M)


educ3_20_64_M_long = reshape(data = educ3_20_64_M,
                             idvar = "Region",
                             varying=list(names(educ3_20_64_M)[6:ncol(educ3_20_64_M)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ3_20_64_M_long)


write.csv(educ3_20_64_M_long,"educ3_20_64_M_long.csv")



# educ_20_64_M - ED5
educ5_20_64_M <- read.csv("Y20-64_ED5_M_pct.csv", header = T)

head(educ5_20_64_M)
str(educ5_20_64_M)


write.csv(educ5_20_64_M,"educ5_20_64_M_wide.csv")

educ5_20_64_M <- read.csv("educ5_20_64_M_wide.csv", header = T)
names(educ5_20_64_M)


educ5_20_64_M_long = reshape(data = educ5_20_64_M,
                             idvar = "Region",
                             varying=list(names(educ5_20_64_M)[6:ncol(educ5_20_64_M)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ5_20_64_M_long)


write.csv(educ5_20_64_M_long,"educ5_20_64_M_long.csv")





# educ_20_64_F - ED0
educ_20_64_F <- read.csv("Y20-64_ED0_F_pct.csv", header = T)

head(educ_20_64_F)
str(educ_20_64_F)


write.csv(educ_20_64_F,"educ0_20_64_F_wide.csv")

educ_20_64_F <- read.csv("educ0_20_64_F_wide.csv", header = T)
names(educ_20_64_F)


educ_20_64_F_long = reshape(data = educ_20_64_F,
                            idvar = "Region",
                            varying=list(names(educ_20_64_F)[6:ncol(educ_20_64_F)]),
                            sep = ".",
                            timevar = "Year",
                            times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                      2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                            direction = "long")
#
head(educ_20_64_F_long)


write.csv(educ_20_64_F_long,"educ0_20_64_F_long.csv")





# educ_20_64_F - ED3
educ3_20_64_F <- read.csv("Y20-64_ED3_F_pct.csv", header = T)

head(educ3_20_64_F)
str(educ3_20_64_F)


write.csv(educ3_20_64_F,"educ3_20_64_F_wide.csv")

educ3_20_64_F <- read.csv("educ3_20_64_F_wide.csv", header = T)
names(educ3_20_64_F)


educ3_20_64_F_long = reshape(data = educ3_20_64_F,
                             idvar = "Region",
                             varying=list(names(educ3_20_64_F)[6:ncol(educ3_20_64_F)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ3_20_64_F_long)


write.csv(educ3_20_64_F_long,"educ3_20_64_F_long.csv")



# educ_20_64_F - ED5
educ5_20_64_F <- read.csv("Y20-64_ED5_F_pct.csv", header = T)

head(educ5_20_64_F)
str(educ5_20_64_F)


write.csv(educ5_20_64_F,"educ5_20_64_F_wide.csv")

educ5_20_64_F <- read.csv("educ5_20_64_F_wide.csv", header = T)
names(educ5_20_64_F)


educ5_20_64_F_long = reshape(data = educ5_20_64_F,
                             idvar = "Region",
                             varying=list(names(educ5_20_64_F)[6:ncol(educ5_20_64_F)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ5_20_64_F_long)


write.csv(educ5_20_64_F_long,"educ5_20_64_F_long.csv")





### YOUNG unemployment



# educ_15_24_T - ED0
educ_15_24_T <- read.csv("educ0_15_24_T.csv", header = T)

head(educ_15_24_T)
str(educ_15_24_T)


write.csv(educ_15_24_T,"educ_15_24_T_wide.csv")

educ_15_24_T <- read.csv("educ_15_24_T_wide.csv", header = T)
names(educ_15_24_T)


educ_15_24_T_long = reshape(data = educ_15_24_T,
                            idvar = "Region",
                            varying=list(names(educ_15_24_T)[6:ncol(educ_15_24_T)]),
                            sep = ".",
                            timevar = "Year",
                            times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                      2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                            direction = "long")
#
head(educ_15_24_T_long)


write.csv(educ_15_24_T_long,"educ0_15_24_T_long.csv")





# educ_15_24_T - ED3
educ3_15_24_T <- read.csv("educ3_15_24_T.csv", header = T)

head(educ3_15_24_T)
str(educ3_15_24_T)


write.csv(educ3_15_24_T,"educ3_15_24_T_wide.csv")

educ3_15_24_T <- read.csv("educ3_15_24_T_wide.csv", header = T)
names(educ3_15_24_T)


educ3_15_24_T_long = reshape(data = educ3_15_24_T,
                             idvar = "Region",
                             varying=list(names(educ3_15_24_T)[6:ncol(educ3_15_24_T)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ3_15_24_T_long)


write.csv(educ3_15_24_T_long,"educ3_15_24_T_long.csv")



# educ_15_24_M - ED0
educ_15_24_M <- read.csv("Y15-24_ED0_M_pct.csv", header = T)

head(educ_15_24_M)
str(educ_15_24_M)


write.csv(educ_15_24_M,"educ0_15_24_M_wide.csv")

educ_15_24_M <- read.csv("educ0_15_24_M_wide.csv", header = T)
names(educ_15_24_M)


educ_15_24_M_long = reshape(data = educ_15_24_M,
                            idvar = "Region",
                            varying=list(names(educ_15_24_M)[6:ncol(educ_15_24_M)]),
                            sep = ".",
                            timevar = "Year",
                            times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                      2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                            direction = "long")
#
head(educ_15_24_M_long)


write.csv(educ_15_24_M_long,"educ0_15_24_M_long.csv")





# educ_15_24_M - ED3
educ3_15_24_M <- read.csv("Y15-24_ED3_M_pct.csv", header = T)

head(educ3_15_24_M)
str(educ3_15_24_M)


write.csv(educ3_15_24_M,"educ3_15_24_M_wide.csv")

educ3_15_24_M <- read.csv("educ3_15_24_M_wide.csv", header = T)
names(educ3_15_24_M)


educ3_15_24_M_long = reshape(data = educ3_15_24_M,
                             idvar = "Region",
                             varying=list(names(educ3_15_24_M)[6:ncol(educ3_15_24_M)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ3_15_24_M_long)


write.csv(educ3_15_24_M_long,"educ3_15_24_M_long.csv")



# educ_15_24_F - ED0
educ_15_24_F <- read.csv("Y15-24_ED0_F_pct.csv", header = T)

head(educ_15_24_F)
str(educ_15_24_F)


write.csv(educ_15_24_F,"educ0_15_24_F_wide.csv")

educ_15_24_F <- read.csv("educ0_15_24_F_wide.csv", header = T)
names(educ_15_24_F)


educ_15_24_F_long = reshape(data = educ_15_24_F,
                            idvar = "Region",
                            varying=list(names(educ_15_24_F)[6:ncol(educ_15_24_F)]),
                            sep = ".",
                            timevar = "Year",
                            times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                      2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                            direction = "long")
#
head(educ_15_24_F_long)


write.csv(educ_15_24_F_long,"educ0_15_24_F_long.csv")





# educ_15_24_F - ED3
educ3_15_24_F <- read.csv("Y15-24_ED3_F_pct.csv", header = T)

head(educ3_15_24_F)
str(educ3_15_24_F)


write.csv(educ3_15_24_F,"educ3_15_24_F_wide.csv")

educ3_15_24_F <- read.csv("educ3_15_24_F_wide.csv", header = T)
names(educ3_15_24_F)


educ3_15_24_F_long = reshape(data = educ3_15_24_F,
                             idvar = "Region",
                             varying=list(names(educ3_15_24_F)[6:ncol(educ3_15_24_F)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(1999, 2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015, 2016),
                             direction = "long")
#
head(educ3_15_24_F_long)


write.csv(educ3_15_24_F_long,"educ3_15_24_F_long.csv")




#############
### MERGES of UNEMP ##
#############



educ0_20_64_T_long <- read.csv("educ0_20_64_T_long.csv", header = T)
educ0_20_64_M_long <- read.csv("educ0_20_64_M_long.csv", header = T)
educ0_20_64_F_long <- read.csv("educ0_20_64_F_long.csv", header = T)

educ3_20_64_T_long <- read.csv("educ3_20_64_T_long.csv", header = T)
educ3_20_64_M_long <- read.csv("educ3_20_64_M_long.csv", header = T)
educ3_20_64_F_long <- read.csv("educ3_20_64_F_long.csv", header = T)

educ5_20_64_T_long <- read.csv("educ5_20_64_T_long.csv", header = T)
educ5_20_64_M_long <- read.csv("educ5_20_64_M_long.csv", header = T)
educ5_20_64_F_long <- read.csv("educ5_20_64_F_long.csv", header = T)


educ0_15_24_T_long <- read.csv("educ0_15_24_T_long.csv", header = T)
educ0_15_24_M_long <- read.csv("educ0_15_24_M_long.csv", header = T)
educ0_15_24_F_long <- read.csv("educ0_15_24_F_long.csv", header = T)

educ3_15_24_T_long <- read.csv("educ3_15_24_T_long.csv", header = T)
educ3_15_24_M_long <- read.csv("educ3_15_24_M_long.csv", header = T)
educ3_15_24_F_long <- read.csv("educ3_15_24_F_long.csv", header = T)




merge1 = merge(educ0_20_64_T_long, educ0_20_64_M_long , by = c("Region","Year"), all= T)
head(merge1)


merge2 = merge(merge1, educ0_20_64_F_long, by = c("Region","Year"), all= T)
head(merge2)

#

merge3 = merge(educ3_20_64_T_long,educ3_20_64_M_long, by = c("Region","Year"), all= T)
head(merge3)



merge4 = merge(merge3, educ3_20_64_F_long, by = c("Region","Year"), all= T)
head(merge4)

# 

merge5 = merge(educ5_20_64_T_long, educ5_20_64_M_long, by = c("Region","Year"), all= T)
head(merge5)


merge6 = merge(merge5, educ5_20_64_F_long, by = c("Region","Year"), all= T)
head(merge6)


# 

merge7 = merge(educ0_15_24_T_long , educ0_15_24_M_long  , by = c("Region","Year"), all= T)
head(merge7)


merge8 = merge(merge7, educ0_15_24_F_long , by = c("Region","Year"), all= T)
head(merge8)

# 

merge9 = merge(educ3_15_24_T_long , educ3_15_24_M_long  , by = c("Region","Year"), all= T)
head(merge9)


merge10 = merge(merge9, educ3_15_24_F_long , by = c("Region","Year"), all= T)
head(merge10)

#


mergeB = merge(merge2, merge4, by = c("Region","Year"), all= T)
head(mergeB)

mergeC = merge(mergeB, merge6, by = c("Region","Year"), all= T)
head(mergeC)


mergeD = merge(merge8, merge10, by = c("Region","Year"), all= T)
head(mergeD)


mergeE = merge(mergeC, mergeD, by = c("Region","Year"), all= T)
head(mergeE)



# FULL UNEMPLOYMENT for GMM

write.csv(mergeE,"educ_FULL_long.csv")




# FULL merge of GMM-labour statistics



educ_FULL_long <- read.csv("educ_FULL_long.csv", header = T)
Prof_FULL_long <- read.csv("Prof_FULL_long.csv", header = T)
unemp_FULL_long <- read.csv("unemp_FULL_long.csv", header = T)


mergeF = merge(unemp_FULL_long, educ_FULL_long, by = c("Region","Year"), all= T)
head(mergeF)

mergeG = merge(mergeF, Prof_FULL_long , by = c("Region","Year"), all= T)
head(mergeG)



# FULL Labor Stat for GMM

write.csv(mergeG,"Labor_Stat_FULL_long.csv")



##################################################################
# COMBINER PANEL DATA OBSERVATION ET NUTS2 SCREENING!!!


NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

Labor_Stat_FULL_long = read.csv("Labor_Stat_FULL_long.csv", header = T)
head(Labor_Stat_FULL_long)

# Post
D = as.character(NUTS2[,1]) %in%  as.character(unique(Labor_Stat_FULL_long[,1])) 
head(D)

nuts2 = which(D==TRUE)
nuts2

NUTS2[nuts2,1]

subset2 = Labor_Stat_FULL_long[Labor_Stat_FULL_long$Region %in% as.character(NUTS2[nuts2,1]), ]
unique(subset2[,1])


write.csv(subset2,"Labor_Stat_FULL_long_sh.csv")


4518/18
#251







#######################
# PRODUCTIVITY refaite
#######################


# GVA
GVA <- read.csv("GVA_prod.csv", header = T)

head(GVA)
str(GVA)



GVA_long = reshape(data = GVA,
                             idvar = "Region",
                             varying=list(names(GVA)[2:ncol(GVA)]),
                             sep = ".",
                             timevar = "Year",
                             times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                                       2008,2009,2010,2011,2012, 2013, 2014, 2015),
                             direction = "long")
#
head(GVA_long)


write.csv(GVA_long,"GVA_long.csv")



# EMP

EMP <- read.csv("EMP_gr.csv", header = T)

head(EMP)
str(EMP)



EMP_long = reshape(data = EMP,
                   idvar = "Region",
                   varying=list(names(EMP)[2:ncol(EMP)]),
                   sep = ".",
                   timevar = "Year",
                   times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                             2008,2009,2010,2011,2012, 2013, 2014, 2015),
                   direction = "long")
#
head(EMP_long)


write.csv(EMP_long,"EMP_long.csv")



# SAL

SAL <- read.csv("SAL_gr.csv", header = T)

head(SAL)
str(SAL)



SAL_long = reshape(data = SAL,
                   idvar = "Region",
                   varying=list(names(SAL)[2:ncol(SAL)]),
                   sep = ".",
                   timevar = "Year",
                   times = c(2000, 2001, 2002, 2003, 2004,2005,2006,2007,
                             2008,2009,2010,2011,2012, 2013, 2014, 2015),
                   direction = "long")
#
head(SAL_long)


write.csv(SAL_long,"SAL_long.csv")



#############
### MERGES of Productivity ##
#############



GVA_long <- read.csv("GVA_long.csv", header = T)
EMP_long <- read.csv("EMP_long.csv", header = T)
SAL_long <- read.csv("SAL_long.csv", header = T)



merge1 = merge(GVA_long, EMP_long , by = c("Region","Year"), all.x= T)
head(merge1)


merge2 = merge(merge1, SAL_long, by = c("Region","Year"), all.x= T)
head(merge2)


write.csv(merge2,"Productivity_long.csv")



##################################################################
# COMBINER PANEL DATA OBSERVATION ET NUTS2 SCREENING!!!


NUTS2 = read.csv("NUTS2_Regions.csv", header = T)
head(NUTS2)

Productivity_long = read.csv("Productivity_long.csv", header = T)
head(Productivity_long)

# Post
D = as.character(NUTS2[,1]) %in%  as.character(unique(Productivity_long[,1])) 
head(D)

nuts2 = which(D==TRUE)
nuts2

NUTS2[nuts2,1]

subset1 = Productivity_long[Productivity_long$Region %in% as.character(NUTS2[nuts2,1]), ]
unique(subset1[,1])


write.csv(subset1,"Productivity_long_sh.csv")


3200/16
#200



###########################################################################
### MERGE Productivity NEW with OLD and KEEP Controls (GDP/capita ...)
###########################################################################

Productivity_long_sh = read.csv("Productivity_long_sh.csv", header = T)
head(Productivity_long_sh)
str(Productivity_long_sh)


Panel_Data_Prod_LTU_CORRECT = read.csv("Panel_Data_Prod_LTU_CORRECT.csv", header = T)
head(Panel_Data_Prod_LTU_CORRECT)
str(Panel_Data_Prod_LTU_CORRECT)



mergeA = merge(Productivity_long_sh , Panel_Data_Prod_LTU_CORRECT[,-c(3:7)], by = c("Region","Year"), all.x= T)
head(mergeA)



write.csv(mergeA,"Productivity_Controls_long_sh.csv")



###########################################################################
### MERGE Productivity COntrols with FULL LABOR STAT
###########################################################################


Productivity_Controls_long_sh = read.csv("Productivity_Controls_long_sh.csv", header = T)
head(Productivity_Controls_long_sh)
str(Productivity_Controls_long_sh)



Labor_Stat_FULL_long_sh = read.csv("Labor_Stat_FULL_long_sh.csv", header = T)
head(Labor_Stat_FULL_long_sh)
str(Labor_Stat_FULL_long_sh)



mergeB = merge(Productivity_Controls_long_sh , Labor_Stat_FULL_long_sh, by = c("Region","Year"), all.x= T)
head(mergeB)



write.csv(mergeB,"Panel_Data_Product_Labour.csv")


###########################################################################
### FULL Panel DATA Productivity Controls with FULL LABOR STAT
###########################################################################


pdf = read.csv("Panel_Data_Product_Labour.csv", header = T)
head(pdf, 15)
str(pdf)

3200/16


# NO produt DATA for BE, FR and SE

pdf = subset(pdf, !(pdf$Country %in% c("BE", "FR", "SE")))
head(pdf)
str(pdf)


2544/16


# write.csv(pdf,"Panel_Data_Product_Labour_NONA.csv")


############################################
# Filtering away more Y_EMP countries
############################################


summary(pdf$Y_EMP)

table(is.na(pdf$Y_EMP), pdf$Country)


# MOre filtering

pdf = subset(pdf, !(pdf$Country %in% c("CZ", "FI", "NL", "PL", "UK")))
head(pdf)
str(pdf)


# MOre filtering YEARS

pdf = subset(pdf, !(pdf$Year %in% c(2000, 2016)))
head(pdf)
str(pdf)


library(Amelia)

dev.off()

# create a missing map
x11()
missmap(pdf, col=c("black", "grey"), legend=T, rank.order=F)

?missmap




x11()
plotmeans(Y_EMP ~ Year, main="Vækstraten i Brutoværditilvækst Per Beskæftiget Time (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i Produktiviteten per Beskæftiget time (pct.)", bars=TRUE, barcol="blue", data=pdf)

x11()
plotmeans(PC_ACT ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdf)


x11()
plotmeans(PC_NE ~ Year, main="Vækstraten i Beskæftigelsen (per Time, fordelt per EUs Regioner)", ylab="Tilvækst i Beskæftigelsen per Time (pct.)", bars=TRUE, barcol="blue",data=pdf)


# no educ0_25_64_T, educ0_25_64_M, educ0_25_64_F


x11()
plotmeans(Y_EMP ~ Year, main="Vækstraten i Brutoværditilvæksten (Fordeling af EUs Regioner, faste priser)", ylab="Tilvækst i BVT (pct.)", bars=TRUE, barcol="blue", data=pdf)


x11()
plotmeans(GVA_pr  ~ Year, main="Vækstraten i Beskæftigelsen (per Time, fordelt per EUs Regioner)", ylab="Tilvækst i Beskæftigelsen per Time (pct.)", bars=TRUE, barcol="blue",data=pdf)




###################################################
# OK Labor COVARIATES:  
###################################################
# unemp_20_64_T , educ0_15_24_M, unemp_20_64_M, unemp_15_24_M , unemp_20_64_F, unemp_15_24_F, 
# educ0_15_24_F , educ3_25_64_T, 
# "educ3_15_24_T", "educ3_15_24_M" , 
# "SAL_25_64", SELF_25_64
###########################################################################################






head(pdf, 20)

3000/15


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



pdf$PC_NE_DK = pdf$PC_NE*pdf$DK
pdf$PC_NE_DE = pdf$PC_NE*pdf$DE
pdf$PC_NE_ES = pdf$PC_NE*pdf$ES
pdf$PC_NE_IT = pdf$PC_NE*pdf$IT


pdf$PC_ACT_DK = pdf$PC_ACT*pdf$DK
pdf$PC_ACT_DE = pdf$PC_ACT*pdf$DE
pdf$PC_ACT_ES = pdf$PC_ACT*pdf$ES
pdf$PC_ACT_IT = pdf$PC_ACT*pdf$IT

str(pdf)

pdf$lPPS_HAB = log(pdf$GDP_PPS_HABn)
pdf$lPPS_HAB_EU = log(pdf$GDP_PPS_HAB_Eun)


pdf$lPPS_HAB_DK = pdf$lPPS_HAB*pdf$DK
pdf$lPPS_HAB_DE = pdf$lPPS_HAB*pdf$DE
pdf$lPPS_HAB_ES = pdf$lPPS_HAB*pdf$ES
pdf$lPPS_HAB_IT = pdf$lPPS_HAB*pdf$IT


pdf$lPPS_HAB_EU_DK = pdf$lPPS_HAB_EU*pdf$DK
pdf$lPPS_HAB_EU_DE = pdf$lPPS_HAB_EU*pdf$DE
pdf$lPPS_HAB_EU_ES = pdf$lPPS_HAB_EU*pdf$ES
pdf$lPPS_HAB_EU_IT = pdf$lPPS_HAB_EU*pdf$IT


pdf$unemp_20_64_T_DK = pdf$unemp_20_64_T*pdf$DK
pdf$unemp_20_64_T_DE = pdf$unemp_20_64_T*pdf$DE
pdf$unemp_20_64_T_ES = pdf$unemp_20_64_T*pdf$ES
pdf$unemp_20_64_T_IT = pdf$unemp_20_64_T*pdf$IT


pdf$unemp_20_64_M_DK = pdf$unemp_20_64_M*pdf$DK
pdf$unemp_20_64_M_DE = pdf$unemp_20_64_M*pdf$DE
pdf$unemp_20_64_M_ES = pdf$unemp_20_64_M*pdf$ES
pdf$unemp_20_64_M_IT = pdf$unemp_20_64_M*pdf$IT


pdf$unemp_20_64_F_DK = pdf$unemp_20_64_F*pdf$DK
pdf$unemp_20_64_F_DE = pdf$unemp_20_64_F*pdf$DE
pdf$unemp_20_64_F_ES = pdf$unemp_20_64_F*pdf$ES
pdf$unemp_20_64_F_IT = pdf$unemp_20_64_F*pdf$IT



pdf$educ3_25_64_T_DK = pdf$educ3_25_64_T*pdf$DK
pdf$educ3_25_64_T_DE = pdf$educ3_25_64_T*pdf$DE
pdf$educ3_25_64_T_ES = pdf$educ3_25_64_T*pdf$ES
pdf$educ3_25_64_T_IT = pdf$educ3_25_64_T*pdf$IT


pdf$SAL_25_64_DK = pdf$SAL_25_64*pdf$DK
pdf$SAL_25_64_DE = pdf$SAL_25_64*pdf$DE
pdf$SAL_25_64_ES = pdf$SAL_25_64*pdf$ES
pdf$SAL_25_64_IT = pdf$SAL_25_64*pdf$IT



pdf$SELF_25_64_DK = pdf$SELF_25_64*pdf$DK
pdf$SELF_25_64_DE = pdf$SELF_25_64*pdf$DE
pdf$SELF_25_64_ES = pdf$SELF_25_64*pdf$ES
pdf$SELF_25_64_IT = pdf$SELF_25_64*pdf$IT


pdf$Y_EMP_Pr = pdf$Y_EMP*pdf$Pre
pdf$Y_EMP_Po = pdf$Y_EMP*pdf$Post

pdf$rGVA_Pr = pdf$rGVA*pdf$Pre
pdf$rGVA_Po = pdf$rGVA*pdf$Post



str(pdf)



library(pglm)

detach(pglm)



# LTU ACT -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)


# ORIGINAL MODELS

# LTU ACT -  LAG1 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_ACT,1) +
                    lag(PC_ACT_DK, 1) + lag(PC_ACT_DE, 1) + lag(PC_ACT_ES, 1) + lag(PC_ACT_IT, 1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,2:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)


# LTU ACT -  LAG2 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_ACT,2) +
                    lag(PC_ACT_DK, 2) + lag(PC_ACT_DE, 2) + lag(PC_ACT_ES, 2) + lag(PC_ACT_IT, 2) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,3:4) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

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
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,4:5) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)




# LTU NE -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,1:2) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)




# LTU NE -  LAG1 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_NE,1) +
                    lag(PC_NE_DK, 1) + lag(PC_NE_DE, 1) + lag(PC_NE_ES, 1) + lag(PC_NE_IT, 1) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,2:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)


# LTU NE -  LAG2 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(PC_NE,2) +
                    lag(PC_NE_DK, 2) + lag(PC_NE_DE, 2) + lag(PC_NE_ES, 2) + lag(PC_NE_IT, 2) +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,3:4) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

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
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,4:5) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)





# NEW EXTENSIONS WITH LABOUR STATISTICS



###################################################
# OK Labor COVARIATES:  
###################################################
# unemp_20_64_T , educ0_15_24_M, unemp_20_64_M, unemp_15_24_M , unemp_20_64_F, unemp_15_24_F, 
# educ0_15_24_F , educ3_25_64_T, 
# "educ3_15_24_T", "educ3_15_24_M" , 
# "SAL_25_64", SELF_25_64
###########################################################################################


################
# With LTU  ACT 
################


# OLD ORIGINAL MODEL
# LTU ACT -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)


# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)



# AVEC SAL uniquement 
# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    SAL_25_64 + 
                    SAL_25_64_ES + 
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)



# AVEC SELF uniquement - CONTRARIANT SAL sauf si S.E. est pris en compte
# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    SELF_25_64 + 
                    SELF_25_64_ES + 
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)

confint(modelgmmEMP)

# OLD ORIGINAL MODEL with TIME DUMMIES
# LTU ACT -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) +
                    lag(rGVA_Pr, 1) + lag(rGVA_Po, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)


# + unemp_20_64_T + SAL_25_64 + educ3_25_64_T +
# SELF_25_64 opposite effects of SAL


# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    lag(Y_EMP_DK, 1) + 
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    unemp_20_64_T + 
                    unemp_20_64_T_DE + 
                    educ3_25_64_T +
                    SAL_25_64 + 
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                    
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)




# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    
                    lag(Y_EMP_DK, 1) + 
                    PC_ACT +
                    PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    unemp_20_64_T + 
                    unemp_20_64_T_DE + 
                    educ3_25_64_T +
                    SAL_25_64 + 
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)


# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    unemp_20_64_M + 
                    unemp_20_64_M_DE +
                    educ3_25_64_T +
                    educ3_25_64_T_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)



# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) +
                    PC_ACT +
                    PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    unemp_20_64_M + 
                    unemp_20_64_M_DE +
                    educ3_25_64_T +
                    educ3_25_64_T_IT +
                    
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)


# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_ACT +
                    PC_ACT_DK + PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    unemp_20_64_F + 
                    unemp_20_64_F_DK + unemp_20_64_F_DE + unemp_20_64_F_ES +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)



# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) + 
                    PC_ACT +
                    PC_ACT_DE + PC_ACT_ES + PC_ACT_IT +
                    unemp_20_64_F + 
                    unemp_20_64_F_DK + unemp_20_64_F_DE + unemp_20_64_F_ES +
                    
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)






################
# With LTU NE 
################

# LTU NE -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,1:2) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)



# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) + 
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_IT, 1) +
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    lag(rGVA, 1) + 
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,1:2) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = TRUE)




# AVEC SAL uniquement 
# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) +
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    SAL_25_64 + 
                    SAL_25_64_DK + SAL_25_64_ES + SAL_25_64_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)



# AVEC SELF uniquement - CONTRARIANT SAL sauf si S.E. est pris en compte
# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) +
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    SELF_25_64 + 
                    SELF_25_64_DK + SELF_25_64_ES + SELF_25_64_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_NE,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)




# OLD ORIGINAL MODEL with TIME DUMMIES
# LTU ACT -  LAG0 is significant! 
# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + 
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1) 
                    
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)


# + unemp_20_64_T + SAL_25_64 + educ3_25_64_T +
# SELF_25_64 opposite effects of SAL


# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    
                    lag(Y_EMP_DK, 1) + 
                    PC_NE +
                    PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    unemp_20_64_T + 
                    
                    educ3_25_64_T +
                    
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)




# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_NE +
                    PC_NE_DK + PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    unemp_20_64_M + 
                    unemp_20_64_M_DE +
                    educ3_25_64_T +
                    educ3_25_64_T_IT +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)



# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    
                    lag(Y_EMP_DK, 1) +
                    PC_NE +
                    PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    unemp_20_64_M + 
                    unemp_20_64_M_DE + 
                    educ3_25_64_T +
                    educ3_25_64_T_DE +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)


# FULL
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_DK, 1) + lag(Y_EMP_DE, 1) + lag(Y_EMP_ES, 1) + lag(Y_EMP_IT, 1) +
                    PC_NE +
                    PC_NE_DE + PC_NE_ES + PC_NE_IT +
                    unemp_20_64_F + 
                    unemp_20_64_F_DK + unemp_20_64_F_DE + unemp_20_64_F_ES +
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP)



# PARSI
modelgmmEMP<-pgmm(Y_EMP ~ lag(Y_EMP, 1) +
                    lag(Y_EMP_Pr,1) + lag(Y_EMP_Po,1) +
                    lag(Y_EMP_DK, 1) + 
                    PC_NE +
                    PC_NE_ES + PC_NE_IT +
                    unemp_20_64_F + 
                    unemp_20_64_M +
                    educ3_25_64_T +
                    educ3_25_64_T_DE + 
                    lag(rGVA, 1) +
                    lag(rGVA_DK, 1) + lag(rGVA_DE, 1) + lag(rGVA_ES, 1) + lag(rGVA_IT, 1)
                  | lag(Y_EMP, 2:3) + lag(rGVA, 2:3) + lag(PC_ACT,1:3) + lag(lPPS_HAB,1:2), data=pdf, transformation = "ld")

summary(modelgmmEMP, robust = T)



