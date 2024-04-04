library(ggforce)
library(lubridate)
library(dplyr)
library(tidyr)

#run FindWaterlevelData_2022-08-19 first


#import previous wetland data set, remove explanitory variables for now
#might need to add or subtract
df_saved <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-10.csv"))%>%
  select(Wetland,Date,Time_Baro,DateTime,CO2_ppm,CO2_umol.L,CH4_umol.L,CH4_sat,Flux_umol_m2_s,
         K600,k_m.d,KH_mol.L.atm,Waterlevel_m,Watertemp_c,AirPress_kpa,AirTemp_c,DOC_mg.L,TDN_mg.L,
         Elevation_m)
df_saved$DateTime <- as.POSIXct(df_saved$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#read in data from methane worksheet
df2 <- read.csv(here::here("Methane/Methane_fall2022_update.csv"))%>%select(Bottle_no1,Site,Location,CH4_umol.L,CH4_sat)%>%
  rename(Sample.Name=Bottle_no1,Site2=Location)
df2$CH4_umol.L <- as.numeric(df2$CH4_umol.L)
df2$CH4_sat <- as.numeric(df2$CH4_sat)

#bind with field data

#import wetland id
Drone_samples <- read.csv(here::here("DroneData/CH4andCO2_Sampling_Drone.csv"))%>%
  select(Site,Site2,Date,Time_End,Bottle_no1,Lat,Lon,ppm_NOTcorrected)%>%
  rename(time=Time_End,Sample.Name=Bottle_no1)%>%
  mutate_at(c('Sample.Name'), as.character)
Drone_samples$DateTime <- as.POSIXct(paste(Drone_samples$Date,Drone_samples$time),format="%m/%d/%y %H:%M",tz='UTC')
Drone_samples$DateTime <- round_date(Drone_samples$DateTime, "15 mins")
Drone_samples <- left_join(Drone_samples,WL_Wetland12%>%select(DateTime,WLTemp_c),by="DateTime")
Drone_samples <- Drone_samples%>%rename(Watertemp_c = WLTemp_c)
BaroSTATION <- BaroSTATION%>%rename(AirPress_kpa = Baro_kpa, AirTemp_C = BaroTemp_c)
Drone_samples <- left_join(Drone_samples,BaroSTATION,by="DateTime")
Drone_samples$viasala_type <- "NewV"
Drone_samples$Flux_mean <- NA

OctoberData <- read.csv(here::here("Methane/OctoberData.csv"))%>%
  select(Wetland,Location,Date,Time_CH4,Vial_no_ch4,AirTemp_C,AirPress_kpa,Watertemp_c,viasala_type,ppm_NOTcorrected,Flux_mean)%>%
  rename(time=Time_CH4,Site=Wetland,Sample.Name=Vial_no_ch4,Site2=Location)%>%
  mutate_at(c('Sample.Name'), as.character)
OctoberData$DateTime <- as.POSIXct(paste(OctoberData$Date,OctoberData$time),format="%m/%d/%y %H:%M",tz='UTC')
OctoberData$DateTime <- round_date(OctoberData$DateTime, "15 mins")
OctoberData$Lat <- NA
OctoberData$Lon <- NA

df <- rbind(Drone_samples,OctoberData)
df$Sample.Name <- as.integer(df$Sample.Name)

df <- full_join(df,df2,by=c("Sample.Name","Site","Site2"))
#correct CO2, all new
# Now correct the adjusted ppm 
#add 5cm water
water5cm = 0.490333
df$adjusted_ppm <- (df$ppm_NOTcorrected) * (1 + (1013 - df$AirPress_kpa+water5cm) * 0.0015)

#merge in results from methane workbook

df <- df%>%rename(CO2_ppm=adjusted_ppm)

#calc CO2_umol.L
kH_STP_mol.L.atm = .035
D_K = 2400 
T_STP_K = 298.15
#calculate henry's law constant using 
df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/(df$Watertemp_c + 273.15) - 1/T_STP_K))

#UatmToatm <- 10^6

#calculate mass equivalence of CO2 in water
df$CO2_umol.L <- df$CO2_ppm * df$KH_mol.L.atm

#now calculate k where possible
CO2_air_ppm <- 415.74*10^-6
R=0.08205736608096
gCO2asC <- 12

#convert hpa to atm; 1hPa = 0.00986923 atm
df$air_pressure_atm <- (df$AirPress_kpa+water5cm) * 0.00986923

df$VaporPressure_atm <- 10^(5.40221-(1838.675/((df$AirTemp_C + 273.15)-31.737)))

df$TotalAir_atm <- df$air_pressure_atm - df$VaporPressure_atm
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
df$WaterTemp_K <- df$Watertemp_c + 273.15
T_STP_K = 298.15

#calculate henry's law constant using 
df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/df$WaterTemp_K - 1/T_STP_K))

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

#calculate k
df$k_m.d <- df$Flux_gCO2asCperM2perDay  / df$deltaCO2_gCO2asCperM3

#convert to k600
#k600 = kCO2 * (600/SC)^-0.5
#SC= schmidst constant, temperature dependent
#SC = 1911.1 - 118.11*T + 3.4527*T^2 - 0.04132*T^3
#T = temperature in c
df$Sc <- 1911.1 - 118.11*df$Watertemp_c + 3.4527*(df$Watertemp_c)^2 - 0.04132*(df$Watertemp_c)^3

df$K600.effective <- df$k_m.d * (600/df$Sc)^(-0.5)

#write out all fall samples
#write.csv(df,here::here("Methane/methanedf_WetlandandDrone_fall2022.csv"))

#write out just dron3 data
drone_df <- df%>%filter(Site=="Gavilan")%>%select(Sample.Name,Site,Site2,Lat,Lon,Date,time,DateTime,Watertemp_c,AirPress_kpa,AirTemp_C,CO2_ppm,CO2_umol.L,CH4_umol.L,CH4_sat)
#write.csv(drone_df,here::here("DroneData/GasSamples_Drone_df.csv"))

#summarize ch4 data

##WETLAND 7!!!!
df_sum <- df%>%filter(Site!="Gavilan")%>%filter(Site!="Chakanas")%>%filter(Sample.Name!="179")%>%
  group_by(Site)%>%summarise(
    DateTime = mean(DateTime),
    CO2_ppm=mean(CO2_ppm,na.rm = TRUE),
    CO2_umol.L=mean(CO2_umol.L,na.rm = TRUE),
    CH4_umol.L=mean(CH4_umol.L,na.rm = TRUE),
    CH4_sat=mean(CH4_sat,na.rm = TRUE),
    Flux_umol_m2_s=mean(Flux_mean,na.rm = TRUE),
    k_m.d=mean(k_m.d,na.rm = TRUE),
    K600=mean(K600.effective,na.rm = TRUE),
    KH_mol.L.atm=mean(KH_mol.L.atm,na.rm = TRUE),
    Watertemp_c=mean(Watertemp_c,na.rm = TRUE),
    AirPress_kpa=mean(AirPress_kpa,na.rm = TRUE),
    AirTemp_c=mean(AirPress_kpa,na.rm = TRUE)
  )

df_sum$DateTime <- round_date(df_sum$DateTime, "15 mins")
df_sum$Site <-  gsub("_", "", df_sum$Site )

#bind in water level
WL_Wetland <- WL_Wetland%>%rename(Site=Station)
WL_Wetland$Site <-  gsub(".*?_", "", WL_Wetland$Site)

df_sum <- left_join(df_sum, WL_Wetland %>%select(DateTime,Site,Baro_kpa,BaroTemp_c,WaterLevel_m),by=c("DateTime","Site"))

#delete 15 min if wl column is empty
df_sum$DateTime_1 <- ifelse(is.na(df_sum$WaterLevel_m), 
                          as.POSIXct(df_sum$DateTime - minutes(15),format="%Y-%m-%d %H:%M:%S",tz="UTC"), 
                          df_sum$DateTime)
df_sum$DateTime <- as.POSIXct(df_sum$DateTime_1,format="%Y-%m-%d %H:%M:%S",tz="UTC")
df_sum <- left_join(df_sum%>%select(-c(Baro_kpa,BaroTemp_c,WaterLevel_m)), WL_Wetland %>%select(DateTime,Site,Baro_kpa,BaroTemp_c,WaterLevel_m),by=c("DateTime","Site"))
df_sum$DateTime_1 <- NULL
###BIND IN SUMMMER DATA
# now select the column we want to carry on to binding with the older data frame
df_saved_1 <- df_saved%>%select(Wetland,DateTime,CO2_ppm,CO2_umol.L,CH4_umol.L,CH4_sat,
                                Flux_umol_m2_s,k_m.d,K600,KH_mol.L.atm,
                                Waterlevel_m,Watertemp_c,AirPress_kpa,AirTemp_c,
                                DOC_mg.L,TDN_mg.L,Elevation_m)%>%rename(
                                  Site=Wetland,WaterLevel_m=Waterlevel_m
                                )
#also I want baro press and temp
df_saved_1 <- left_join(df_saved_1, BaroSTATION %>%select(DateTime,AirPress_kpa,BaroTemp_c),by=c("DateTime"))


#

df_sum$DOC_mg.L <- NA
df_sum$TDN_mg.L <- NA
df_sum$Elevation_m <- NA

df_4 <- rbind(df_saved_1,df_sum)

#write out
#write.csv(df_4,here::here("Wetlands/Wetland_df_2024-03-27.csv"))


ggplot(df_4%>%filter(Site=="Gavilan"),
       aes(x=CO2_umol.L,y=CH4_umol.L, color=Site2)) + 
  geom_point()

ggplot(df_4%>%filter(Site!="Gavilan")%>%filter(Site!="Chakanas"),
       aes(x=log(CO2_umol.L),y=log(CH4_umol.L), color=DateTime)) + geom_point()# +
#  geom_mark_ellipse(aes(color = Site), expand = unit(0.5,"mm"))

ggplot(df_4%>%filter(Site!="Gavilan")%>%filter(Site!="Chakanas"),
       aes(x=log1p(CO2_umol.L),y=log1p(CH4_umol.L), color=Site)) + geom_point() +
  geom_mark_ellipse(aes(color = Site), expand = unit(0.5,"mm"))

ggplot(df_4%>%filter(Site!="Gavilan")%>%filter(Site!="Chakanas"),
       aes(x=CO2_umol.L,y=CH4_umol.L, color=Site)) + geom_point() +
  geom_mark_ellipse(aes(color = Site), expand = unit(0.5,"mm"))

ggplot(df_4%>%filter(Site=="Gavilan"|Site=="Chakanas"),
       aes(x=log(CO2_umol.L),y=log(CH4_umol.L), color=Site)) + geom_point() +
  geom_mark_ellipse(aes(color = Site), expand = unit(0.5,"mm"))


#
df_4$Date <- as.Date(df_4$DateTime)
ggplot() + 
  geom_point(data=df_4%>%filter(Site!="Gavilan")%>%filter(Site!="Chakanas")%>%filter(Date>"2022-09-01"), 
             aes(x=CO2_umol.L, y=CH4_umol.L), color='black',size=3)+
  geom_point(data=df_4%>%filter(Site!="Gavilan")%>%filter(Site!="Chakanas"), 
             aes(x=CO2_umol.L, y=CH4_umol.L, color=Site)) + 
  geom_mark_ellipse(data=df_4%>%filter(Site!="Gavilan")%>%
                      filter(Site!="Chakanas"),
                    aes(x=CO2_umol.L, y=CH4_umol.L,color = Site), expand = unit(0.5,"mm"))+
  scale_x_log10() + scale_y_log10()


df_test <- df%>%filter(Site!="Gavilan")%>%filter(Site!="Chakanas")


df_test <- df_test%>%drop_na(Sample.Name)
df_test$Sample.Name <- as.numeric(df_test$Sample.Name)
df_test <- df_test%>%drop_na(Sample.Name)


df_dronedata <- df_4 %>%filter(Site=="Gavilan"|Site=="Chakanas")
ggplot(df_dronedata,
       aes(x=Site2,y=CH4_umol.L, color=Site)) + geom_point() +
  geom_mark_ellipse(aes(color = Site), expand = unit(0.5,"mm")) +
  scale_y_log10()
