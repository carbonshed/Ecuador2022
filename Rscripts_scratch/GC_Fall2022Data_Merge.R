#this script is to merge data collected in the fall of 2022 with results of GC
# and to adjust my ppms

#run FindWaterlevelData_2022-08-19 first

library(here)
library(dplyr)
library(lubridate)



#import notes for GC sample processing
sample_list <- read.csv(here::here("Methane/GCsamples_Fall2022.csv"))
vial_volume <- read.csv(here::here("Methane/Vial_volume.csv"))[c(1,4)]%>%
  rename(bottle_vol = Water_Weight, Bottle_no = Bottle_Number)%>%
  mutate_at(c('Bottle_no'), as.character)
sample_list <- left_join(sample_list, vial_volume, join_by(Bottle_no))
sample_list$DateTime <- as.POSIXct(paste(sample_list$Date_2, sample_list$Time_2), format="%d-%b %H:%M",tz='UTC')
sample_list$DateTime <- round_date(sample_list$DateTime, "15 mins")

PressureTemp_lab <- read.csv(here::here("Methane/PressureTemp_lab.csv"),skip=1)[c(2:4)]
colnames(PressureTemp_lab) <- c("DateTime","AirPres_kpa","Temp_c")
PressureTemp_lab$DateTime <- as.POSIXct(PressureTemp_lab$DateTime,format="%m/%d/%y %I:%M:%S %p",tz='UTC')
PressureTemp_lab$DateTime <- round_date(PressureTemp_lab$DateTime, "15 mins")
PressureTemp_summarise <- PressureTemp_lab%>%
  group_by(DateTime)%>%
  summarise(
    AirPres_kpa_lab = mean(AirPres_kpa, na.rm = TRUE),
    AirTemp_c_lab = mean(Temp_c, na.rm = TRUE)
  )

sample_list <- left_join(sample_list,PressureTemp_summarise,by="DateTime")

#import results
results1 <- read.csv(here::here("Methane/GCResults_Feb23.csv"))[c(4,6:9)]
results2 <- read.csv(here::here("Methane/GCResults_Feb03.csv"))[c(2,4,6:8)]%>% rename(N2O = nitrous.oxide)
results3 <- read.csv(here::here("Methane/GCResults_Jan26.csv"))[c(2,4:6)]
results3$N2O <- NA
results <- rbind(results1,results2,results3)
rm(results1,results2,results3)
results <- results%>%
  mutate(CO = ifelse(CO == "-----", 0, CO))%>%
  mutate(CH4 = ifelse(CH4 == "-----", 0, CH4))%>%
  mutate(CO2 = ifelse(CO2 == "----", 0, CO2))%>%
  mutate(N2O = ifelse(N2O == "-----", 0, N2O))%>%
  mutate_at(c('CO', 'CH4','CO2','N2O'), as.numeric)

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

#bind
FieldSample_df <- rbind(Drone_samples,OctoberData)
#FieldSample_df <- left_join(FieldSample_df,BaroSTATION,by="DateTime")

df <- right_join(results, sample_list, by=c('Sample.Name'='Bottle_no'))
df <- full_join(df#%>%select(Sample.Name,CH4,N2O)
                , FieldSample_df, by=c('Sample.Name'))


#wirte out df
#write.csv(df,here::here("Kriddie/methanesample_fall2022.csv"))

