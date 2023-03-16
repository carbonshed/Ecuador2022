#Calc k600 

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

#step 1: Read in flux data

#step 2: read in water level

#step 3: read in vaisala data already adjusted and calibrated

#step 4: merge all data by DateTime columne (use full_join)

df <- read.csv(here::here("Kriddie/wetlands_df.csv"))
df <- df%>%select(Wetland,Location,Date,Watertemp_c,Waterlevel,
                      ppm_NOTcorrected,CO2_ppm,Flux_mean,Flux_stdev,
                   AirPress_kpa,AirTemp_C,Average.Wind.veolcity.per.day,
                  Solar.radiation..W.m.2.,  Time_Baro
                             )

#convert baro to hpa
#1 kPa = 10 hPa
#1 kPa = 0.101972 m
df$AirPress_hpa <- df$AirPress_kpa * 10



### Calculate concentration (gCO2-C per Liter) in air using ###

#concentration of air
#from www.esrl.noaa.gov
#average of July and August
#410.86 ppm
CO2_air_ppm <- 410*10^-6
R=0.08205736608096
gCO2asC <- 12

#convert hpa to atm; 1hPa = 0.0009869233 atm
df$AirPress_atm <- df$AirPress_hpa * 0.0009869233

df$VaporPressure_atm <- 10^(5.40221-(1838.675/((df$AirTemp_C + 273.15)-31.737)))

df$TotalAir_atm <- df$AirPress_atm - df$VaporPressure_atm
df$Total_air_MolperL <- df$TotalAir_atm/(R*(df$AirTemp_C + 273.15)) 
df$CO2_air_MolesPerLiter <- df$Total_air_MolperL * CO2_air_ppm
# 12 grams of C in 1 mole of CO2
df$CO2_air_gCO2asCPerLiter <- df$CO2_air_MolesPerLiter * gCO2asC


### Calculate concentration CO2 in water (gCO2-C per Liter) using Henry's law ###
#Equation below can be used to adjust Henry's constant to the temperature of the environment
# *NOTE: we may need to do an additional calculation to adjust for low pressure in our environment?*
#**KH = KH(STP) x exp(D(1/T-1/T(STP)))**
#* KH(STP) = Henry's law constant at STP (0.035 mol/L)
#*NOTE: the cited literature say that this in mol/(kg x bar)*
#  * D = Temperature dependence constant (2400 K)
#* T = Water Temperature (K)
#* T(STP) = Temp at STP (298.15 K)

#set constants
kH_STP_mol.L.atm = .035
D_K = 2400 
df$Watertemp_K <- df$Watertemp_c + 273.15
T_STP_K = 298.15

#calculate henry's law constant using 
df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/df$Watertemp_K - 1/T_STP_K))


UatmToatm <- 10^6

#calculate mass equivalence of CO2 in water
df$CO2_water_gCO2asCPerLiter <- (df$CO2_ppm / UatmToatm) * df$KH_mol.L.atm * gCO2asC

#calculate concntration difference between CO2 in the water and in the air
#for some reason, unclear to me, Mcdowell multiplies by henry's constant AGAIN. Let's try it
LiterToCubicMeter = 1000

df$deltaCO2_gCO2asCperM3 <-  (df$CO2_water_gCO2asCPerLiter - df$CO2_air_gCO2asCPerLiter)  *
  df$KH_mol.L.atm  * LiterToCubicMeter


#convert flux to mole/m2/d
#seconds per day (86,400)
SecToDay <- 86400
#umole to mole
UmoleToMole <- 10^6
#convert moles to grams of Carbon dioxide as Carbon
#gCO2asC <- 12

df$Flux_gCO2asCperM2perDay <- df$Flux_mean * SecToDay /  UmoleToMole *gCO2asC

#Fick' Law: calculate K

#**F(aq) = K[pCO2(aq) - pCO2(air)] x KH**
#  * k = F/(pCO2(aq)-pC02(air))/KH
#* F = Flux mole/m2/d
#* K = gas transfer rate m/d
#* KH = henry's constant for molar concentration of CO2 in water 

#*rearrange equation to solve for k*
#**k = F(aq) / ([pCO2(aq) - pCO2(air)] x KH)**

#convert Liters to cubic meters
#LiterToCubicMeter <- 1000
#df$deltaCO2_gCO2asCPerCubicMeter <- df$deltaCO2_gCO2asCPerLiter * LiterToCubicMeter

#calculate

df$k_m.d <- df$Flux_gCO2asCperM2perDay  / df$deltaCO2_gCO2asCperM3


#convert to k600
#k600 = kCO2 * (600/SC)^-0.5
#SC= schmidst constant, temperature dependent
#SC = 1911.1 - 118.11*T + 3.4527*T^2 - 0.04132*T^3
#T = temperature in c
df$Sc <- 1911.1 - 118.11*df$Watertemp_c + 3.4527*(df$Watertemp_c)^2 - 0.04132*(df$Watertemp_c)^3

df$K600.effective <- df$k_m.d * (600/df$Sc)^(-0.5)



df$Date <- as.Date(df$Date, format = "%m/%d/%y")

#df$uniqueID <- paste(df$Wetland,df$Date,sep="_")


summary_df <- df %>%
  group_by(Wetland,Date,Time_Baro)%>% 
  summarize(
    Watertemp_c = mean(Watertemp_c, na.rm = TRUE),
    Flux_umol_m2_s = mean(Flux_mean, na.rm = TRUE),
    AirTemp_c = mean(AirTemp_C, na.rm = TRUE),
    AirPress_kpa = mean(AirPress_kpa, na.rm = TRUE),
    CO2_ppm = mean(CO2_ppm, na.rm = TRUE),
    k_m.d = mean(k_m.d, na.rm = TRUE),
    K600 = mean(K600.effective, na.rm = TRUE),
    Wind_velocity_day = mean(Average.Wind.veolcity.per.day, na.rm = TRUE),
    SolarRad_Wm2 = mean(Solar.radiation..W.m.2., na.rm = TRUE)
  )


summary_df$Wetland <- paste("wetland",summary_df$Wetland,sep="_")


##write out
#write.csv(summary_df, here::here("Wetlands/Wetland_df_2023-03-14.csv"))

