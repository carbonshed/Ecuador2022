#this script takes the methane data and summarizes it 

library(here)
library(lubridate)
library(dplyr)
library(ggplot2)

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

## read in CO2 data
CO2_df <-  read.csv(here::here("Wetlands/wetlands_df_2022-10-18.csv"))
CO2_df$Date <- as.Date.character(CO2_df$Date,format="%m/%d/%y")
#CO2_df$CH4.ppm.NOT.CORRECTED <- NULL
CO2_df$N20.ppm.NOT.CORRECTED <- NULL

CO2_df$Wetland[CO2_df$Wetland == "1"] <- "Wetland_01"
CO2_df$Wetland[CO2_df$Wetland == "2"] <- "Wetland_02"
CO2_df$Wetland[CO2_df$Wetland == "3"] <- "Wetland_03"
CO2_df$Wetland[CO2_df$Wetland == "4"] <- "Wetland_04"
CO2_df$Wetland[CO2_df$Wetland == "5"] <- "Wetland_05"
CO2_df$Wetland[CO2_df$Wetland == "6"] <- "Wetland_06"
CO2_df$Wetland[CO2_df$Wetland == "7"] <- "Wetland_07"
CO2_df$Wetland[CO2_df$Wetland == "8"] <- "Wetland_08"
CO2_df$Wetland[CO2_df$Wetland == "9"] <- "Wetland_09"
CO2_df$Wetland[CO2_df$Wetland == "10"] <- "Wetland_10"
CO2_df$Wetland[CO2_df$Wetland == "11"] <- "Wetland_11"
CO2_df$Wetland[CO2_df$Wetland == "12"] <- "Wetland_12"

#read in water samples
DOC_df <- read.csv(here::here("WaterSamples/DOC_KW_08-25-2022.csv"))
DOC_df <- DOC_df[,c("DOC..mg.L.","TDN..mg.L.","Date","site")]
DOC_df <- DOC_df %>% 
  rename(
    Wetland = site,
    DOC_mgL = DOC..mg.L.,
    TDN_mgL = TDN..mg.L.
  )
DOC_df$Date <- as.Date.character(DOC_df$Date,format="%m/%d/%y")

#summarize co2 and ch4

CH4_df <-CH4_df%>%group_by(Wetland,Date)%>% 
  summarize(
    CH4_umol_L = mean(CH4_umol_L.1,na.rm = TRUE),
    CH4_sat = mean(CH4_.sat,na.rm = TRUE)
  )

CO2_df <-CO2_df%>%group_by(Wetland,Date)%>% 
  summarize(
    Watertemp_c = mean(Watertemp_c,na.rm = TRUE),
    Waterlevel = mean(Waterlevel,na.rm = TRUE),
    CO2_NOTcorrected = mean(ppm_NOTcorrected,na.rm = TRUE),
    CH4_NOTcorrected = mean(CH4.ppm.NOT.CORRECTED,na.rm = TRUE),
    Flux_mean = mean(Flux_mean,na.rm = TRUE),
    CO2_ppm = mean(CO2_ppm,na.rm = TRUE),
    AirPress_kpa = mean(AirPress_kpa,na.rm = TRUE),
    VaporPressure_atm = mean(VaporPressure_atm,na.rm = TRUE),
    k_m.d = mean(k_m.d,na.rm = TRUE),
    K600 = mean(K600,na.rm = TRUE),
    
  )



#plot data real quick

ggplot(CH4_df, aes(x=Site, y=CH4_umol_L.1)) + 
  geom_point(#fill="red",
    aes(fill=Date_collected),
    shape=21, size = 3)

ggplot(CH4_df, aes(x=Date_collected, y=CH4_umol_L.1)) + 
  geom_point(#fill="red",
    aes(fill=Date_collected),
    shape=21, size = 3)+
  facet_wrap(~Site)



