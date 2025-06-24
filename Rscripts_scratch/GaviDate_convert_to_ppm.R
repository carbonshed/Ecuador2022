library(here)
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(cowplot)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dplyr)
library(lubridate)
library(readr)

df <- read.csv(here::here("ProcessedData/Drone_Gavi_sampling_GHG.csv"))%>%
  select(Site,Site2,Lat,Lon,DateTime,Watertemp_c,AirPress_kpa,air_pressure_atm,AirTemp_C,
         CO2_ppm,CO2_umol.L,CO2_sat,CH4_umol.L,CH4_pSat,KH_CH4_mol.L.atm)


#add henry's constant CH4
kH_STP_mol.L.atm = .0014182
dlnHcppersperK = 1600
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15

df$KH_CH4_mol.L.atm <- kH_STP_mol.L.atm*exp(dlnHcppersperK*(1/(df$Watertemp_c+273.15)-1/T_STP_K))

#Ambient CH4 concentration ppb	1910.97 	
#average 2021 from Moaa
CH4_air_ppb <- 1910.97 

df$CH4_air_atm <- df$air_pressure_atm * CH4_air_ppb * 10^-9

df$CH4_sat_umol.L <- df$CH4_air_atm*df$KH_CH4_mol.L.atm * 10^6

df$CH4_pSat <- df$CH4_umol.L/df$CH4_sat_umol.L *100

#calculate the partial pressure of ch4

df$pCH4_ppm <- df$CH4_umol.L /df$KH_CH4_mol.L.atm

#write out
#write.csv(df,here::here("ProcessedData/Drone_Gavi_sampling_GHG_2025.csv"))
