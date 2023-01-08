#long term data
library(here)
library(lubridate)
library(dplyr)
library(plotly)
##2019 

MergedData_2019 <- read.csv(here::here("LongTermData/2019/MergedData_2019.csv"))
MergedData_2019$DateTime <- as.POSIXct(MergedData_2019$DateTime, tz="UTC",
                             tryFormats = c("%Y-%m-%d %H:%M:%S"))

####plot####
ggplot(MergedData_2019, aes(x=DateTime, y=AirTemp_C)) +
  geom_point() 

#cleaning forever

#AirPress_kpa - look good!

#AirTemp_C - clean during field season
AirTemp_C <- MergedData_2019[,c("DateTime","AirTemp_C")]

AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime < as.POSIXct("2019-07-12 19:15:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-07-16 13:00:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-07-16 19:45:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-07-18 14:30:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-07-18 16:45:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-07-19 16:30:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-07-19 21:30:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-07-23 12:30:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-07-23 16:30:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-07-25 15:15:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-07-25 18:15:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-07-29 12:45:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-07-29 14:45:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-08-02 14:00:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-08-02 17:15:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-08-05 17:15:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-08-05 19:45:00", tz="UTC"))] <- NA
AirTemp_C$AirTemp_C[which(AirTemp_C$DateTime > as.POSIXct("2019-08-09 10:00:00", tz="UTC") & AirTemp_C$DateTime < as.POSIXct("2019-08-09 12:30:00", tz="UTC"))] <- NA

MergedData_2019$AirTemp_C <- AirTemp_C$AirTemp_C
rm(AirTemp_C)

#Stn01_WL_m
plot_ly(data = MergedData_2019, x = ~DateTime, y = ~Stn01_WL_m)

Stn01_WL_m <- MergedData_2019[,c("DateTime","Stn01_WL_m","Stn01_WLTemp_C")]
Stn01_WL_m$Stn01_WL_m[which(Stn01_WL_m$Stn01_WL_m<0)] <- NA
Stn01_WL_m$Stn01_WL_m[which(Stn01_WL_m$DateTime < as.POSIXct("2019-07-12 12:45:00", tz="UTC"))] <- NA
Stn01_WL_m$Stn01_WL_m[which(Stn01_WL_m$DateTime > as.POSIXct("2019-07-16 12:45:00", tz="UTC") & Stn01_WL_m$DateTime < as.POSIXct("2019-07-17 13:15:00", tz="UTC"))] <- NA
Stn01_WL_m$Stn01_WL_m[which(Stn01_WL_m$DateTime > as.POSIXct("2019-07-23 11:45:00", tz="UTC") & Stn01_WL_m$DateTime < as.POSIXct("2019-07-25 16:30:00", tz="UTC"))] <- NA
Stn01_WL_m$Stn01_WLTemp_C[which(Stn01_WL_m$DateTime > as.POSIXct("2019-07-23 11:45:00", tz="UTC") & Stn01_WL_m$DateTime < as.POSIXct("2019-07-25 16:30:00", tz="UTC"))] <- NA
Stn01_WL_m$Stn01_WLTemp_C[which(Stn01_WL_m$DateTime == as.POSIXct("2019-07-19 14:30:00", tz="UTC"))] <- NA
Stn01_WL_m$Stn01_WLTemp_C[which(Stn01_WL_m$DateTime == as.POSIXct("2019-08-14 11:30:00", tz="UTC"))] <- NA

MergedData_2019$Stn01_WL_m <- Stn01_WL_m$Stn01_WL_m
MergedData_2019$Stn01_WLTemp_C <- Stn01_WL_m$Stn01_WLTemp_C
rm(Stn01_WL_m)

####Stn01_EC_uS
Stn01_EC_uS <- MergedData_2019[,c("DateTime","Stn01_EC_uS","Stn01_ECTemp_C")]
Stn01_EC_uS$Stn01_EC_uS[which(Stn01_EC_uS$Stn01_EC_uS<0)] <- NA
Stn01_EC_uS$Stn01_EC_uS[which(Stn01_EC_uS$DateTime < as.POSIXct("2019-07-18 18:30:00", tz="UTC"))] <- NA
Stn01_EC_uS$Stn01_EC_uS[which(Stn01_EC_uS$DateTime == as.POSIXct("2019-07-22 16:00:00", tz="UTC")|Stn01_EC_uS$DateTime == as.POSIXct("2019-08-01 16:00:00", tz="UTC")|Stn01_EC_uS$DateTime == as.POSIXct("2019-08-06 13:15:00", tz="UTC")|Stn01_EC_uS$DateTime == as.POSIXct("2019-08-05 16:15:00", tz="UTC")|Stn01_EC_uS$DateTime == as.POSIXct("2019-08-07 11:15:00", tz="UTC")|Stn01_EC_uS$DateTime == as.POSIXct("2019-08-07 17:45:00", tz="UTC")|Stn01_EC_uS$DateTime == as.POSIXct("2019-08-08 12:15:00", tz="UTC")|Stn01_EC_uS$DateTime == as.POSIXct("2019-08-08 12:30:00", tz="UTC"))] <- NA
Stn01_EC_uS$Stn01_EC_uS[which(Stn01_EC_uS$DateTime > as.POSIXct("2019-07-16 12:45:00", tz="UTC") & Stn01_EC_uS$DateTime < as.POSIXct("2019-07-17 13:15:00", tz="UTC"))] <- NA

MergedData_2019$Stn01_EC_uS <- Stn01_EC_uS$Stn01_EC_uS
MergedData_2019$Stn01_ECTemp_C <- Stn01_EC_uS$Stn01_ECTemp_C
rm(Stn01_EC_uS)

#Stn01_DO_mg.L
Stn01_DO_mg.L <- MergedData_2019[,c("DateTime","Stn01_DO_mg.L","Stn01_DOTemp_C")]
Stn01_DO_mg.L$Stn01_DO_mg.L[which(Stn01_DO_mg.L$DateTime == as.POSIXct("2019-07-17 13:15:00", tz="UTC")|Stn01_DO_mg.L$DateTime == as.POSIXct("2019-07-23 12:00:00", tz="UTC"))] <- NA

MergedData_2019$Stn01_DO_mg.L <- Stn01_DO_mg.L$Stn01_DO_mg.L
MergedData_2019$Stn01_DOTemp_C <- Stn01_DO_mg.L$Stn01_DOTemp_C
rm(Stn01_DO_mg.L)

#Stn02_DO_mg.L
Stn02_DO_mg.L <- MergedData_2019[,c("DateTime","Stn02_DO_mg.L","Stn02_DOTemp_C")]
Stn02_DO_mg.L$Stn02_DO_mg.L[which(Stn02_DO_mg.L$DateTime == as.POSIXct("2019-07-29 11:15:00", tz="UTC"))] <- NA

MergedData_2019$Stn02_DO_mg.L <- Stn02_DO_mg.L$Stn02_DO_mg.L
MergedData_2019$Stn02_DOTemp_C <- Stn02_DO_mg.L$Stn02_DOTemp_C
rm(Stn02_DO_mg.L)

#Stn03_WL_m
plot_ly(data = MergedData_2019, x = ~DateTime, y = ~Stn03_WLTemp_C)

Stn03_WL_m <- MergedData_2019[,c("DateTime","Stn03_WL_m","Stn03_WLTemp_C")]
Stn03_WL_m$Stn03_WL_m[which(Stn03_WL_m$Stn03_WL_m<0.05)] <- NA
Stn03_WL_m$Stn03_WLTemp_C[which(Stn03_WL_m$DateTime == as.POSIXct("2019-07-23 12:45:00", tz="UTC")|Stn03_WL_m$DateTime == as.POSIXct("2019-07-23 13:00:00", tz="UTC")|Stn03_WL_m$DateTime == as.POSIXct("2019-07-23 13:15:00", tz="UTC"))] <- NA
Stn03_WL_m$Stn03_WLTemp_C[which(Stn03_WL_m$DateTime == as.POSIXct("2019-07-25 16:00:00", tz="UTC")|Stn03_WL_m$DateTime == as.POSIXct("2019-08-02 14:30:00", tz="UTC")|Stn03_WL_m$DateTime == as.POSIXct("2019-08-09 10:30:00", tz="UTC")|Stn03_WL_m$DateTime == as.POSIXct("2019-08-09 10:45:00", tz="UTC"))] <- NA


MergedData_2019$Stn03_WL_m <- Stn03_WL_m$Stn03_WL_m
MergedData_2019$Stn03_WLTemp_C <- Stn03_WL_m$Stn03_WLTemp_C
rm(Stn03_WL_m)

#Stn04_DO_mg.L
#DO looks wierd for some time periods... but temp data looks fine, so I'm keeping all of it

#

CO2 <- MergedData_2019[,c("DateTime","Stn01_CO2_ppm","Stn02_CO2_ppm","Stn03_CO2_ppm","Stn04_CO2_ppm")]
CO2$Stn01_CO2_ppm[which(CO2$DateTime < as.POSIXct("2019-07-12 17:00:00", tz="UTC"))] <- NA
CO2$Stn01_CO2_ppm[which(CO2$DateTime == as.POSIXct("2019-08-14 12:00:00", tz="UTC"))] <- NA
CO2$Stn01_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-07-16 14:15:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-07-22 22:30:00", tz="UTC"))] <- NA
CO2$Stn01_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-07-26 11:15:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-07-26 14:15:00", tz="UTC"))] <- NA
CO2$Stn01_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-01 16:15:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-01 21:15:00", tz="UTC"))] <- NA
CO2$Stn01_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-05 16:00:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-05 20:45:00", tz="UTC"))] <- NA
CO2$Stn01_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-08 12:30:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-08 17:15:00", tz="UTC"))] <- NA

CO2$Stn02_CO2_ppm[which(CO2$DateTime < as.POSIXct("2019-07-12 17:00:00", tz="UTC"))] <- NA
CO2$Stn02_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-05 15:30:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-05 20:15:00", tz="UTC"))] <- NA

CO2$Stn03_CO2_ppm[which(CO2$DateTime < as.POSIXct("2019-07-12 17:15:00", tz="UTC"))] <- NA
CO2$Stn03_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-07-16 14:15:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-07-22 22:30:00", tz="UTC"))] <- NA
CO2$Stn03_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-07-26 11:45:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-07-26 14:45:00", tz="UTC"))] <- NA
CO2$Stn03_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-01 00:00:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-01 21:45:00", tz="UTC"))] <- NA
CO2$Stn03_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-05 00:00:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-05 21:30:00", tz="UTC"))] <- NA
CO2$Stn03_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-08 12:45:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-08 17:45:00", tz="UTC"))] <- NA
CO2$Stn03_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-13 10:45:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-13 12:30:00", tz="UTC"))] <- NA

CO2$Stn04_CO2_ppm[which(CO2$DateTime < as.POSIXct("2019-07-12 17:00:00", tz="UTC"))] <- NA
CO2$Stn04_CO2_ppm[which(CO2$DateTime > as.POSIXct("2019-08-13 05:45:00", tz="UTC") & CO2$DateTime < as.POSIXct("2019-08-13 12:30:00", tz="UTC"))] <- NA

MergedData_2019$Stn01_CO2_ppm <- CO2$Stn01_CO2_ppm
MergedData_2019$Stn02_CO2_ppm <- CO2$Stn02_CO2_ppm
MergedData_2019$Stn03_CO2_ppm <- CO2$Stn03_CO2_ppm
MergedData_2019$Stn04_CO2_ppm <- CO2$Stn04_CO2_ppm
rm(CO2)

plot_ly(data = MergedData_2019, x = ~DateTime, y = ~Stn03_CO2_ppm)
##############
####2020#####
##############

#Solinst
WL_01 <- read.csv(here::here("LongTermData/2020/WL_01_enero2020_compensated.csv"), skip = 11, header = TRUE)
WL_01$DateTime <- paste(WL_01$Date, WL_01$Time)
WL_01$DateTime <- as.POSIXct(WL_01$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
WL_01$DateTime <- round_date(WL_01$DateTime, "5 mins")
WL_01 <- WL_01[,c("DateTime","LEVEL","TEMPERATURE")]
WL_01$LEVEL <- WL_01$LEVEL/10
colnames(WL_01) <- c("DateTime","Stn01_WL_m","Stn01_WLTemp_C")


Baro <- read.csv(here::here("LongTermData/2020/Baro-enero-2020.csv"), skip = 10, header = TRUE)
Baro$DateTime <- paste(Baro$Date, Baro$Time)
Baro$DateTime <- as.POSIXct(Baro$DateTime, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
Baro$DateTime <- round_date(Baro$DateTime, "5 mins")
Baro <- Baro[,c("DateTime","LEVEL","TEMPERATURE")]
colnames(Baro) <- c("DateTime","AirPress_kpa","AirTemp_C")
Baro$AirTemp_C[which(Baro$DateTime < as.POSIXct("2019-08-14 15:00:00", tz="UTC"))] <- NA


WL_03 <- read.csv(here::here("LongTermData/2020/WL_03_enero2020_compensated.csv"), skip = 11, header = TRUE)
WL_03$DateTime <- paste(WL_03$Date, WL_03$Time)
WL_03$DateTime <- as.POSIXct(WL_03$DateTime, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
WL_03$DateTime <- round_date(WL_03$DateTime, "5 mins")
WL_03 <- WL_03[,c("DateTime","LEVEL","TEMPERATURE")]
WL_03$LEVEL <- WL_03$LEVEL/10
colnames(WL_03) <- c("DateTime","Stn03_WL_m","Stn03_WLTemp_C")

#hobo
#Dissolved O2
DO_01 <- read.csv(here::here("LongTermData/2020/DO_01_January_2020.csv"), skip =1)
colnames(DO_01)=c("row","DateTime","Stn01_DO_mg.L","Stn01_DOTemp_C")
DO_01 <- DO_01[2:4]
DO_01$DateTime <- as.POSIXct(DO_01$DateTime, tz="UTC",
                              tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                             "%m/%d/%Y %H:%M"))
DO_01$Stn01_DOTemp_C <- (DO_01$Stn01_DOTemp_C - 32) * 5/9
DO_01$Stn01_DO_mg.L[which(DO_01$Stn01_DO_mg.L < -800)] <- NA
DO_01$Stn01_DOTemp_C[which(DO_01$Stn01_DOTemp_C < -500)] <- NA


DO_02 <- read.csv(here::here("LongTermData/2020/DO_02_January_2020.csv"), skip =1)
colnames(DO_02)=c("row","DateTime","Stn02_DO_mg.L","Stn02_DOTemp_C")
DO_02 <- DO_02[2:4]
DO_02$DateTime <- as.POSIXct(DO_02$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))
DO_02$Stn02_DOTemp_C <- (DO_02$Stn02_DOTemp_C - 32) * 5/9
DO_02$Stn02_DO_mg.L[which(DO_02$Stn02_DO_mg.L < -800)] <- NA
DO_02$Stn02_DOTemp_C[which(DO_02$Stn02_DOTemp_C < -500)] <- NA


DO_04 <- read.csv(here::here("LongTermData/2020/DO_04_January_2020.csv"), skip =1)
colnames(DO_04)=c("row","DateTime","Stn04_DO_mg.L","Stn04_DOTemp_C")
DO_04 <- DO_04[2:4]
DO_04$DateTime <- as.POSIXct(DO_04$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))
DO_04$Stn04_DOTemp_C <- (DO_04$Stn04_DOTemp_C - 32) * 5/9
DO_04$Stn04_DO_mg.L[which(DO_04$Stn04_DO_mg.L < -800)] <- NA
DO_04$Stn04_DOTemp_C[which(DO_04$Stn04_DOTemp_C < -500)] <- NA


#conductivity
EC_01 <- read.csv(here::here("LongTermData/2020/EC_01_January_2020.csv"), skip =1)
colnames(EC_01)=c("row","DateTime","Stn01_EC_uS","Condus_highrange","Stn01_ECTemp_C")
EC_01 <- EC_01[c(2,3,5)]
EC_01$DateTime <- as.POSIXct(EC_01$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))
EC_01$Stn01_ECTemp_C <- (EC_01$Stn01_ECTemp_C - 32) * 5/9
EC_01$Stn01_EC_uS[which(EC_01$DateTime == as.POSIXct("2019-08-14 12:30:00", tz="UTC"))] <- NA
EC_01$Stn01_ECTemp_C[which(EC_01$DateTime == as.POSIXct("2019-08-14 12:30:00", tz="UTC"))] <- NA


EC_02 <- read.csv(here::here("LongTermData/2020/EC_02_January_2020.csv"), skip =1)
colnames(EC_02)=c("row","DateTime","Stn02_EC_uS","Condus_highrange","Stn02_ECTemp_C")
EC_02 <- EC_02[c(2,3,5)]
EC_02$DateTime <- as.POSIXct(EC_02$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))
EC_02$Stn02_ECTemp_C <- (EC_02$Stn02_ECTemp_C - 32) * 5/9
EC_02$Stn02_EC_uS[which(EC_02$DateTime == as.POSIXct("2019-08-14 13:15:00", tz="UTC"))] <- NA


EC_04 <- read.csv(here::here("LongTermData/2020/EC_04_January_2020.csv"), skip =1)
colnames(EC_04)=c("row","DateTime","Stn04_EC_uS","Condus_highrange","Stn04_ECTemp_C")
EC_04 <- EC_04[c(2,3,5)]
EC_04$DateTime <- as.POSIXct(EC_04$DateTime, tz="UTC",
                             tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                            "%m/%d/%Y %H:%M"))
EC_04$Stn04_ECTemp_C <- (EC_04$Stn04_ECTemp_C - 32) * 5/9
EC_04$Stn04_EC_uS[which(EC_04$DateTime == as.POSIXct("2019-08-14 14:15:00", tz="UTC"))] <- NA
EC_04$Stn04_EC_uS[which(EC_04$DateTime == as.POSIXct("2019-08-14 14:30:00", tz="UTC"))] <- NA

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

####plot####

#EC_01 us looks different in field seasonI think because field season data is high range (keep for now)

ggplot(df_2019to2020, aes(x=DateTime, y=Stn03_WL_m)) +
  geom_point() 

plot_ly(data = df_2019to2020, x = ~DateTime, y = ~Stn03_WL_m)


####

write.csv(MergedData_2020enero, here::here("LongTermData/MergedData_2019to2020.csv"),
          row.names = FALSE)

####

#2021

