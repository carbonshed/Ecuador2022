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

#summarize

CH4_df <-CH4_df%>%group_by(Wetland,Date)%>% 
  summarize(
    CH4_umol.L = mean(CH4_umol_L.1,na.rm = TRUE),
    CH4_sat = mean(CH4_.sat,na.rm = TRUE)
  )

## read in CO2 data
CO2_df <- read.csv(here::here("Wetlands/Wetland_df_2023-03-14.csv"))
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

#calc kh

#set constants
kH_STP_mol.L.atm = .035
D_K = 2400 
T_STP_K = 298.15
#calculate henry's law constant using 
CO2_df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/(CO2_df$Watertemp_c + 273.15) - 1/T_STP_K))

#UatmToatm <- 10^6

#calculate mass equivalence of CO2 in water
CO2_df$CO2_umol.L <- CO2_df$CO2_ppm * CO2_df$KH_mol.L.atm


#read in water samples
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
 

ggplot(df,aes(x=log(CH4_umol.L),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5) +
  theme(text=element_text(size=20))

ggplot(df ,aes(x=CO2_umol.L,y=DOC_mg.L)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=DOC_mg.L,y=TDN_mg.L)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=Watertemp_c,y=k_m.d)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=AirTemp_c,y=k_m.d)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=(Watertemp_c-AirTemp_c),y=k_m.d)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=AirPress_kpa,y=K600)) +
  geom_point(aes(color=Wetland),size=5)


#plot data real quick

ggplot(CH4_df, aes(x=Site, y=CH4_umol.L)) + 
  geom_point(#fill="red",
    aes(fill=Date_collected),
    shape=21, size = 3)

ggplot(CH4_df, aes(x=Date_collected, y=CH4_umol.L)) + 
  geom_point(#fill="red",
    aes(fill=Date_collected),
    shape=21, size = 3)+
  facet_wrap(~Site)



