# CONTENU #


# RAW DATA MANIPULATION - FIRST PROGRAM FOR CBS-DATA
# DATA SEPARATION BY VARIABLE/CATEGORY FROM HUGE DATASET


# General Data WB

gdp <- read.csv("GDP per Capita.csv", header = T)

head(gdp)
str(gdp)


Disclos <- read.csv("Business Disclosure.csv", header = T)

head(Disclos)
str(Disclos)


EasBus <- read.csv("Ease of Doing Business.csv", header = T)

head(EasBus)
str(EasBus)

CredPrSect <- read.csv("Credit to Private Sector.csv", header = T)

head(CredPrSect)
str(CredPrSect)

LegRight <- read.csv("Strength legal rights.csv", header = T)

head(LegRight)
str(LegRight)

CredInfo <- read.csv("Depth Credit Information.csv", header = T)

head(CredInfo)
str(CredInfo)


PatApp <- read.csv("Patent Application.csv", header = T)

head(PatApp)
str(PatApp)

merge0 = merge(Pop, Google, by = c("Country.Code"), all = T)

merge1 = merge(Pop, GDP_Rank, by = c("Country.Code"), all = T)

merge2 = merge(merge1, GDP_cap_PPP, by = c("Country.Code"), all.x = T)

merge3 = merge(merge2, GDP_cap_gr, by = c("Country.Code"), all.x = T)

merge4 = merge(merge3, GDP_gr, by = c("Country.Code"), all.x = T)


write.csv(merge4,"data1.csv")



# Energy Data

Pop <- read.csv("Population and Ranking.csv", header = T)

head(Pop)
str(Pop)


El_Cons_p_cap <- read.csv("kWh per capita.csv", header = T)

head(Pop)
str(Pop)


Alt_Nucl_tot <- read.csv("Alternative and nuclear energy pct of total .csv", header = T)

head(Alt_Nucl_tot)
str(Alt_Nucl_tot)



CombRen_Wast_tot <- read.csv("Combustible renewables and waste pct of total energy.csv", header = T)

head(CombRen_Wast_tot)
str(CombRen_Wast_tot)


Elprod_Hydr_tot <- read.csv("Electricity production from hydroelectric sources pct of total.csv", header = T)

head(Elprod_Hydr_tot)
str(Elprod_Hydr_tot)



Elprod_O_G_C_tot <- read.csv("Electricity production from oil gas and coal sources pct of total.csv", header = T)

head(Elprod_O_G_C_tot)
str(Elprod_O_G_C_tot)


Elprod_O_tot <- read.csv("Electricity production from oil sources pct of total.csv", header = T)

head(Elprod_O_tot)
str(Elprod_O_tot)



Elprod_Renew_Excl_Hydr <- read.csv("Electricity production from renewable sources excl hydroelectric kWh.csv", header = T)

head(Elprod_Renew_Excl_Hydr)
str(Elprod_Renew_Excl_Hydr)



Elprod_Renew_Excl_Hydr_tot <- read.csv("Electricity production from renewable sources excl hydroelectric pct of total.csv", header = T)

head(Elprod_Renew_Excl_Hydr_tot)
str(Elprod_Renew_Excl_Hydr_tot)



En_Use_kg_Oeq_p_GDP <- read.csv("Energy use kg of oil equivalent per $1000 GDP constant 2011 PPP.csv", header = T)

head(En_Use_kg_Oeq_p_GDP)
str(En_Use_kg_Oeq_p_GDP)


En_Use_kg_Oeq_p_cap <- read.csv("Energy use kg oil equivalent per capita.csv", header = T)

head(En_Use_kg_Oeq_p_cap)
str(En_Use_kg_Oeq_p_cap)


FosFuel_En_Cons_tot <- read.csv("Fossil fuel energy consumption pct of total.csv", header = T)

head(FosFuel_En_Cons_tot)
str(FosFuel_En_Cons_tot)


merge1 = merge(Pop, El_Cons_p_cap, by = c("Country.Code"), all.x = T)

merge2 = merge(merge1, En_Use_kg_Oeq_p_cap, by = c("Country.Code"), all.x = T)

merge3 = merge(merge2, En_Use_kg_Oeq_p_GDP, by = c("Country.Code"), all.x = T)

merge4 = merge(merge3, FosFuel_En_Cons_tot, by = c("Country.Code"), all.x = T)

merge5 = merge(merge4, Alt_Nucl_tot, by = c("Country.Code"), all.x = T)

merge6 = merge(merge5, Elprod_O_tot, by = c("Country.Code"), all.x = T)

merge7 = merge(merge6, Elprod_O_G_C_tot, by = c("Country.Code"), all.x = T)

merge8 = merge(merge7, Elprod_Renew_Excl_Hydr_tot, by = c("Country.Code"), all.x = T)


merge9 = merge(merge8, Elprod_Hydr_tot, by = c("Country.Code"), all.x = T)


merge10 = merge(merge9, Elprod_Renew_Excl_Hydr, by = c("Country.Code"), all.x = T)

merge11 = merge(merge10, CombRen_Wast_tot, by = c("Country.Code"), all.x = T)


write.csv(merge11,"data2.csv")


# Merging general Data and Energy Data



Data_General <- read.csv("data_General_LAST.csv", header = T)

Data_Energy <- read.csv("data_Energy_LAST.csv", header = T)

merge1 = merge(Data_General, Data_Energy, by = c("Country.Code"), all.x = T)


write.csv(merge1,"data12.csv")


# Scatter plots and Correlations

Data_General_Energy <- read.csv("data_General_Energy.csv", header = T)

names(Data_General_Energy)


# Vairables non-interessants General_Energy

# Growth variables 

# "Altertive.nuclear.energy.pct.tot.energy.use" 
# "Electricity.prod.from.oil.gas.and.coal.sources.pct.tot"

Data_General_Energy = Data_General_Energy[,-c(9,11)]
names(Data_General_Energy)

library(PerformanceAnalytics)

# Total General_Energy correlations
x11()
chart.Correlation(Data_General_Energy[,c(5:20)], histogram=TRUE, pch=19)



# Total Energy correlations
x11()
chart.Correlation(Data_General_Energy[,c(10:20)], histogram=TRUE, pch=19)



# Ordering correlations


library(Hmisc)

