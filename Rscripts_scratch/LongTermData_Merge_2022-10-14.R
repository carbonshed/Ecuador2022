#long term data
library(here)
library(lubridate)
library(dplyr)

##2019 

MergedData_2019 <- read.csv(here::here("LongTermData/2019/MergedData_2019.csv"))

##2020

#Solinst
WL_01 <- read.csv(here::here("LongTermData/2020/WL_01_enero2020_compensated.csv"), skip = 11, header = TRUE)
WL_01$DateTime <- paste(WL_01$Date, WL_01$Time)
WL_01$DateTime <- as.POSIXct(WL_01$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
WL_01$DateTime <- round_date(WL_01$DateTime, "5 mins")
WL_01 <- WL_01[,c("DateTime","LEVEL","TEMPERATURE")]
colnames(WL_01) <- c("DateTime","Stn01_WL_m","Stn01_WLTemp_C")

Baro <- read.csv(here::here("LongTermData/2020/Baro-enero-2020.csv"), skip = 10, header = TRUE)
Baro$DateTime <- paste(Baro$Date, Baro$Time)
Baro$DateTime <- as.POSIXct(Baro$DateTime, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
Baro$DateTime <- round_date(Baro$DateTime, "5 mins")
Baro <- Baro[,c("DateTime","LEVEL","TEMPERATURE")]
colnames(Baro) <- c("DateTime","AirPress_kpa","AirTemp_C")

WL_03 <- read.csv(here::here("LongTermData/2020/WL_03_enero2020_compensated.csv"), skip = 11, header = TRUE)
WL_03$DateTime <- paste(WL_03$Date, WL_03$Time)
WL_03$DateTime <- as.POSIXct(WL_03$DateTime, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
WL_03$DateTime <- round_date(WL_03$DateTime, "5 mins")
WL_03 <- WL_03[,c("DateTime","LEVEL","TEMPERATURE")]
colnames(WL_03) <- c("DateTime","Stn03_WL_m","Stn03_WLTemp_C")


#hobo
#Dissolved O2
DO_01 <- read.csv(here::here("LongTermData/2020/DO_01_January_2020.csv"), skip =1)
colnames(DO_01)=c("row","DateTime","Stn01_DO_mg.L","Stn01_DOTemp_C")
DO_01 <- DO_01[2:4]
DO_01$DateTime <- as.POSIXct(DO_01$DateTime, tz="UTC",
                              tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                             "%m/%d/%Y %H:%M"))

DO_02 <- read.csv(here::here("LongTermData/2020/DO_02_January_2020.csv"), skip =1)
colnames(DO_02)=c("row","DateTime","Stn02_DO_mg.L","Stn02_DOTemp_C")
DO_02 <- DO_02[2:4]
DO_02$DateTime <- as.POSIXct(DO_02$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))


DO_04 <- read.csv(here::here("LongTermData/2020/DO_04_January_2020.csv"), skip =1)
colnames(DO_04)=c("row","DateTime","Stn04_DO_mg.L","Stn04_DOTemp_C")
DO_04 <- DO_04[2:4]
DO_04$DateTime <- as.POSIXct(DO_04$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))

#conductivity
EC_01 <- read.csv(here::here("LongTermData/2020/EC_01_January_2020.csv"), skip =1)
colnames(EC_01)=c("row","DateTime","Stn01_EC_uS","Condus_highrange","Stn01_ECTemp_C")
EC_01 <- EC_01[c(2,3,5)]
EC_01$DateTime <- as.POSIXct(EC_01$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))

EC_02 <- read.csv(here::here("LongTermData/2020/EC_02_January_2020.csv"), skip =1)
colnames(EC_02)=c("row","DateTime","Stn02_EC_uS","Condus_highrange","Stn02_ECTemp_C")
EC_02 <- EC_02[c(2,3,5)]
EC_02$DateTime <- as.POSIXct(EC_02$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))

EC_04 <- read.csv(here::here("LongTermData/2020/EC_04_January_2020.csv"), skip =1)
colnames(EC_04)=c("row","DateTime","Stn04_EC_uS","Condus_highrange","Stn04_ECTemp_C")
EC_04 <- EC_04[c(2,3,5)]
EC_04$DateTime <- as.POSIXct(EC_04$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))

##Merge
MergedData_2020enero <- full_join(Baro,WL_01,by = "DateTime")
MergedData_2020enero <- full_join(MergedData_2020enero,EC_01,by = "DateTime")
MergedData_2020enero <- full_join(MergedData_2020enero,DO_01,by = "DateTime")
MergedData_2020enero <- full_join(MergedData_2020enero,EC_02,by = "DateTime")
MergedData_2020enero <- full_join(MergedData_2020enero,DO_02,by = "DateTime")
MergedData_2020enero <- full_join(MergedData_2020enero,WL_03,by = "DateTime")
MergedData_2020enero <- full_join(MergedData_2020enero,EC_04,by = "DateTime")
MergedData_2020enero <- full_join(MergedData_2020enero,DO_04,by = "DateTime")

MergedData_2020enero <- MergedData_2020enero[ order(MergedData_2020enero$DateTime , decreasing = FALSE ),]


write.csv(MergedData_2020enero, here::here("LongTermData/2020/MergedData_2020.csv"),
          row.names = FALSE)

MergedData_2019$Stn02_EC_uS <- NA
MergedData_2019$Stn02_ECTemp_C <- NA
MergedData_2019$Stn04_EC_uS <- NA
MergedData_2019$Stn04_ECTemp_C <- NA


MergedData_2020enero$Stn01_CO2_ppm <- NA
MergedData_2020enero$Stn02_CO2_ppm<- NA
MergedData_2020enero$Stn03_CO2_ppm<- NA
MergedData_2020enero$Stn04_CO2_ppm<- NA


df_2019to2020 <- rbind(MergedData_2020enero,MergedData_2019)

df_2019to2020 <- df_2019to2020[,c("DateTime", "AirPress_kpa", "AirTemp_C", 
                                      "Stn01_WL_m", "Stn01_WLTemp_C", "Stn01_EC_uS" , "Stn01_ECTemp_C", "Stn01_DO_mg.L" , "Stn01_DOTemp_C", "Stn01_CO2_ppm",
                                      "Stn02_EC_uS","Stn02_ECTemp_C","Stn02_DO_mg.L",  "Stn02_DOTemp_C","Stn02_CO2_ppm", 
                                      "Stn03_WL_m",     "Stn03_WLTemp_C", "Stn03_CO2_ppm",
                                      "Stn04_EC_uS" ,  "Stn04_ECTemp_C","Stn04_DO_mg.L",  "Stn04_DOTemp_C", "Stn04_CO2_ppm")]


#

EC_01 <- read.csv("LongTermData/2020/EC_01_2021-06-09.csv")
colnames(DO_04)=c("row","DateTime","WLPres_kpa","WLTemp_c")
DO_04 <- DO_04[2:4]
DO_04$DateTime <- as.POSIXct(DO_04$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))