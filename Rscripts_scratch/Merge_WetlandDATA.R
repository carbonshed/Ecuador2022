#Wetland dataframe
#add methane
#add water
#add envir factors


library(dplyr)
library(tidyverse)
library(here)
library(lubridate)

###########
### CH4 ####
############
CH4_df <- read.csv(here::here("Methane/Methane_df_2023-07-20.csv"))
CH4_df$Date_collected <- as.Date.character(CH4_df$Date_collected,format="%m/%d/%y")
CH4_df <- CH4_df[,c("Site","Date_collected","CH4_umol_L.1","CH4_.sat","CH4_Gcoutput")]

#just wetlands
CH4_df <- CH4_df%>%
  filter(Site!="Colmillo")%>%
  filter(Site!="Gavi-main")%>%
  filter(Site!="Gavi-trib")

CH4_df <- CH4_df %>% 
  rename(
    Wetland = Site,
    Date = Date_collected
  )

#summarize

CH4_df <-CH4_df%>%group_by(Wetland,Date)%>% 
  summarize(
    CH4_umol.L = mean(CH4_umol_L.1,na.rm = TRUE),
    CH4_sat = mean(CH4_.sat,na.rm = TRUE)
  )

###########
### CO2 ####
############
CO2_df <- read.csv(here::here("Wetlands/Wetland_df_2023-08-01.csv"))
CO2_df$Date <- as.Date.character(CO2_df$Date,format="%Y-%m-%d")
CO2_df <- CO2_df%>%filter(Date < as.Date("2022-07-28"))
#CO2_df$CH4.ppm.NOT.CORRECTED <- NULL
CO2_df$X <- NULL

CO2_df$Wetland[CO2_df$Wetland == "wetland_1"] <- "Wetland_01"
CO2_df$Wetland[CO2_df$Wetland == "wetland_2"] <- "Wetland_02"
CO2_df$Wetland[CO2_df$Wetland == "wetland_3"] <- "Wetland_03"
CO2_df$Wetland[CO2_df$Wetland == "wetland_4"] <- "Wetland_04"
CO2_df$Wetland[CO2_df$Wetland == "wetland_5"] <- "Wetland_05"
CO2_df$Wetland[CO2_df$Wetland == "wetland_6"] <- "Wetland_06"
CO2_df$Wetland[CO2_df$Wetland == "wetland_7"] <- "Wetland_07"
CO2_df$Wetland[CO2_df$Wetland == "wetland_8"] <- "Wetland_08"
CO2_df$Wetland[CO2_df$Wetland == "wetland_9"] <- "Wetland_09"
CO2_df$Wetland[CO2_df$Wetland == "wetland_10"] <- "Wetland_10"
CO2_df$Wetland[CO2_df$Wetland == "wetland_11"] <- "Wetland_11"
CO2_df$Wetland[CO2_df$Wetland == "wetland_12"] <- "Wetland_12"

#calc diff in temp water/air
df$airwaterTemp_diff <-  df$AirTemp_c - df$Watertemp_c

#calc convert CO2 from ppm to umol/L

#set constants
kH_STP_mol.L = .035
D_K = 2400 
T_STP_K = 298.15
#calculate henry's law constant using 
#henrys law unit is mol/L
CO2_df$KH_mol.L <- kH_STP_mol.L * exp(D_K*(1/(CO2_df$Watertemp_c + 273.15) - 1/T_STP_K))

Temp=15+273.15
CO2_ppm = 1000

kH_1 <-  kH_STP_mol.L * exp(D_K*(1/(Temp + 273.15) - 1/T_STP_K))
kH_log = 108.3865 + 0.01985076*Temp - 6919.53/Temp - 40.45154*log(Temp) + 669365/Temp^2
kH_2 = log(kH_log)
Co2_mgL <- CO2_ppm * kH_1 * 10^-6 * 12
#UatmToatm <- 10^6

#calculate mass equivalence of CO2 in water
CO2_df$CO2_umol.L <- CO2_df$CO2_ppm * CO2_df$KH_mol.L #* 10^-6 * 10^6

kH_STP_mol.L * exp(D_K*(1/(Watertemp_c + 273.15) - 1/T_STP_K)) * CO2_ppm *12

############
#water samples
###############

DOC_df <- read.csv(here::here("WaterSamples/DOC_KW_08-25-2022.csv"))
DOC_df <- DOC_df[,c("DOC..mg.L.","TDN..mg.L.","Date","site")]
DOC_df <- DOC_df %>% 
  rename(
    Wetland = site,
    DOC_mg.L = DOC..mg.L.,
    TDN_mg.L = TDN..mg.L.
  )
DOC_df$Date <- as.Date.character(DOC_df$Date,format="%m/%d/%y")

DOC_df <- DOC_df%>%
  filter(Wetland!="Colm")%>%
  filter(Wetland!="gavi")%>%
  filter(Wetland!="gavi-trib")


#merge data
df <- full_join(CO2_df,CH4_df, by=c("Wetland","Date"))
df <- left_join(df,DOC_df [,c("Wetland","DOC_mg.L","TDN_mg.L")], by=c("Wetland"))


#########################
### add envirofactors ####
########################

df$Date <- as.Date(df$Date,format = "%Y-%m-%d")
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_Baro), format="%Y-%m-%d %H:%M",tz="UTC")
df$DateTime <- round_date(df$DateTime,unit = "5 minutes")

##read in enviro data from la virgen weather station
viento_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Dirección del viento_subhorario-validado.csv"))
colnames(viento_df) <- c("DateTime","windspeed_m_s","winddirecion") 
viento_df$DateTime <- as.POSIXct(viento_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

Solar_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Radiación Solar_subhorario-validado.csv"))
colnames(Solar_df) <- c("DateTime","solarrad_W_m2")
Solar_df$DateTime <- as.POSIXct(Solar_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

humidad_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Humedad del aire_subhorario-validado.csv"))
colnames(humidad_df) <- c("DateTime","humidity_%")
humidad_df$DateTime <- as.POSIXct(humidad_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

precip_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Precipitación_subhorario-validado.csv"))
colnames(precip_df) <- c("DateTime","precipt_mm")
precip_df$DateTime <- as.POSIXct(precip_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
precip_df$Date <- as.Date(precip_df$DateTime,format="%Y-%m-%d")
precip_summary <- precip_df%>%group_by(Date)%>%summarise(PrecipAccuDay_mm = sum(precipt_mm))
precip_summary$Date <- precip_summary$Date - 1


enviro_df <- full_join(viento_df,Solar_df,by="DateTime")
enviro_df <- full_join(enviro_df,humidad_df,by="DateTime")
#enviro_df <- full_join(enviro_df,precip_df,by="DateTime")

#join enviro data
df <- left_join(df,enviro_df,by="DateTime")
df <- left_join(df,precip_summary,by="Date")

#write
write.csv(df, here::here("Wetlands/Wetland_df_MERGE_2023-08-01.csv"))
 
