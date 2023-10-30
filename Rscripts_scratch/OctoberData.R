#merge data collected in october
library(here)
library(dplyr) 
library(tidyr) 
library(ggplot2)

df <- read.csv(here::here("Kriddie/OctoberData.csv"))

#convert baro to hpa
#1 kPa = 10 hPa
#1 kPa = 0.101972 m
df$AirPress_hpa <- df$AirPress_kpa * 10


#these are all new viasalas

#The new viasalas are set to 700hPa and have an internal temperature, 
#so do not ned to be corrected for temp
#In 2021 the units for new vasialas are half of what they should be, but I believe I fixed that in 2022


df$ppm_NOTcalibrated <- (df$ppm_NOTcorrected)* (1 + (1013 - df$AirPress_hpa) * 0.0015) 

#calibration
# m*x+b
#we are considering k600 to be the real measurment and we will correct stn3 

#K600 old V
b_k600 <- -105.52898
m_k600 <- 1.069330

#station 3 new v
b_stn3 <- -231.66433
m_stn3 <- 1.082080

# m_stn3 * x_stn3 + b_stn3 = m_k600 * x_k600 + b_k600

# x_k600 = (x_stn3 * m_stn3 + b_stn3 - b_k600)/b_k600

df$CO2_ppm <- (df$ppm_NOTcalibrated * m_stn3 + b_stn3 - b_k600)/m_k600

###########
#calc k600#
###########


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

#no flux because I really didn't like that data

#but we did find a very consistent k, so I think we should use use the average k600 from previous months
df_summer <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-08-01.csv"))
df_summer$X <- NULL
res.aov2 <- aov(K600 ~ Wetland, data = df_summer)
summary(res.aov2)
mean(df_summer$K600)



df$K600 <- mean(df_summer$K600)
df$Sc <- 1911.1 - 118.11*df$Watertemp_c + 3.4527*(df$Watertemp_c)^2 - 0.04132*(df$Watertemp_c)^3
df$k_m.d <- df$K600 / (600/df$Sc)^(-0.5)
df$Flux_gCO2asCperM2perDay <- df$k_m.d * df$deltaCO2_gCO2asCperM3

#convert Flux to Flux_umol_m2_s

#convert flux to mole/m2/d
#seconds per day (86,400)
SecToDay <- 86400
#umole to mole
UmoleToMole <- 10^6
#convert moles to grams of Carbon dioxide as Carbon
gCO2asC <- 12

df$Flux_ave <- df$Flux_gCO2asCperM2perDay * UmoleToMole / gCO2asC /SecToDay

#calculate mass equivalence of CO2 in water
df$CO2_umol.L <- df$CO2_ppm * df$KH_mol.L

#summarize


#df$DateTime <- as.POSIXct(paste(df$Date,df$Time_Baro),format="%Y-%m-%d %H:%M")

#df$uniqueID <- paste(df$Wetland,df$Date,sep="_")

summary_df <- df %>%
  group_by(Wetland,Date,Time_Baro)%>% 
  summarize(
    Watertemp_c = mean(Watertemp_c, na.rm = TRUE),
    Waterlevel_m = mean(Waterlevel,na.rm = TRUE), 
    Flux_umol_m2_s = mean(Flux_ave, na.rm = TRUE),
    AirTemp_c = mean(AirTemp_C, na.rm = TRUE),
    AirPress_kpa = mean(AirPress_kpa, na.rm = TRUE),
    CO2_ppm = mean(CO2_ppm, na.rm = TRUE),
    CO2_umol.L = mean(CO2_umol.L, na.rm = TRUE),
    k_m.d = mean(k_m.d, na.rm = TRUE),
    K600 = mean(K600, na.rm = TRUE),
    KH_mol.L.atm = mean(KH_mol.L.atm, na.rm = TRUE)
  )

summary_df$CH4_umol.L <- NA
summary_df$CH4_sat <- NA

summary_df$Date <- as.Date(summary_df$Date, format = "%m/%d/%y")

#bind with previous data
df_summer_1 <- df_summer[c("Wetland","Date","Time_Baro","CO2_ppm","CO2_umol.L","CH4_umol.L","CH4_sat","Flux_umol_m2_s","K600","k_m.d",
                           "KH_mol.L.atm","Waterlevel_m","Watertemp_c","AirPress_kpa","AirTemp_c")]
df_summer_1$Date <- as.Date(df_summer_1$Date, format = "%Y-%m-%d")
summary_df_1 <- rbind(df_summer_1,summary_df)

summary_df_1$DateTime <- as.POSIXct(paste(summary_df_1$Date,summary_df_1$Time_Baro),format="%Y-%m-%d %H:%M")

##write out
#write.csv(summary_df_1, here::here("Wetlands/Wetland_df_2023-10-26.csv"))
