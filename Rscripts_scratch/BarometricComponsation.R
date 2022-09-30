#$$compare baro pressure
#This skript is to varify that the barometric pressure station is appropriate to use to correct wetland water level data

library(lubridate) #package for our date parsing
library(here)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
library(sjmisc)
library(plotly)

#####
##STATION BAROMETRIC DATA###
######

#solinst website: 
#One Barologger can be used to compensate all Leveloggers in a 
  #30 km (20 mile) radius and/or 
  #with every 300 m (1000 ft.) change in elevation
#https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/levelogger/datasheet/barometric-compensation.php

###merge data with baro data###
setwd(here::here("Water_Level/BaroPress"))
all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
site_names="Baro" #creates list of site names for following loop

#rm old files, if they exsist
rm(BaroData)
rm(Temp_BaroData)

for (site in site_names){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("BaroData")){
      BaroData <- read.csv(file, skip=10, header = TRUE, sep = ",",
                           quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    }
    if (exists("BaroData")){
      Temp_BaroData <- read.csv(file, skip=10, header = TRUE, sep = ",",
                                quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      BaroData <- rbind(BaroData, Temp_BaroData)
      rm(Temp_BaroData)
    }
    
  }
  colnames(BaroData)=c("Date","Time","ms","Baro_kpa","BaroTemp_c")
  BaroData=unique(BaroData)
  BaroData$DateTime <- paste(BaroData$Date, BaroData$Time)
  BaroData$DateTime <- as.POSIXct(BaroData$DateTime, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
  BaroData$DateTime <- round_date(BaroData$DateTime, "15 mins")
  BaroData$ms <- NULL
  BaroData$Date <- NULL
  BaroData$Time <- NULL
  BaroData <- BaroData[,c(3,1,2)]
  BaroData=unique(BaroData)
  assign((paste(site,sep="_")),BaroData) #creates object with new appended data
  rm(BaroData) #removes WLdata so that multiple sites aren't appended together
}

BaroSTATION <- Baro 

#####
##NOMAD baro###
#####
 
BaroNOMAD <- read.csv(here::here("Wetlands/Wetland_Baro.csv"), skip=0, header = TRUE)
BaroNOMAD$DateTime <- as.POSIXct(BaroNOMAD$DateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
BaroNOMAD$DateTime <- round_date(BaroNOMAD$DateTime, "15 mins")
#bind
BaroDATA <- left_join(BaroSTATION,BaroNOMAD, by ="DateTime")

#
# Scatterplot
plot(BaroDATA$Baro_kpa, BaroDATA$AirPres_kpa, main = "Scatterplot")


#seperate into different wetlands

#wetlands 1-3
#7/7/22 @ 10:11-11:52,12:07 - 12:45, 1:01 - 1:27
#7/14 @ 10:01-10:50, 11:32-12:20, 12:36-1:05
#7/25/22 @ 9:56 - 10:32, 11:12-11:46, 12:04-12:40

Wetland010203_1 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-07 10:11:00",tz="UTC")&
                                        DateTime < as.POSIXct("2022-07-07 11:52:00",tz = "UTC"))
Wetland010203_2 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-14 10:10:01",tz="UTC")&
                                       DateTime < as.POSIXct("2022-07-14 13:05:00",tz = "UTC"))
Wetland010203_3 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-25 09:56:00",tz="UTC")&
                                       DateTime < as.POSIXct("2022-07-25 12:40:00",tz = "UTC"))

Wetland010203 <- rbind(Wetland010203_1,Wetland010203_2,Wetland010203_3)
rm(Wetland010203_1,Wetland010203_2,Wetland010203_3)

plot(Wetland010203$DateTime, Wetland010203$AirPres_kpa, main = "Wetland 1,2,3")

fit <- lm(AirPres_kpa ~ Baro_kpa, data = Wetland010203)
summary(fit)

plot(Wetland010203$Baro_kpa, Wetland010203$AirPres_kpa, main = "Wetland 1,2,3")
#abline(lm(Wetland010203$AirPres_kpa ~ Wetland010203$Baro_kpa))
abline(coef(fit)[1:2])
## rounded coefficients for better output
cf <- round(coef(fit), 2) 
## sign check to avoid having plus followed by minus for negative coefficients
eq <- paste0("BaroNOMAD = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " BaroSTATION ")#,
#             ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " hp")
## printing of the equation
mtext(eq, 3)

#wetland 4
#6/29 @9:20 to 11:03
#7/15/2022 @ 12:53 - 13:19
#7/22/2022 @ 10:50 - 11:18


Wetland04_1 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-06-29 09:20:00",tz="UTC")&
                                       DateTime < as.POSIXct("2022-06-29 11:03:00",tz = "UTC"))#%>%
#                                filter(AirPres_kpa > 64)
Wetland04_2 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-15 12:53:00",tz="UTC")&
                                       DateTime < as.POSIXct("2022-07-15 13:19:00",tz = "UTC"))
Wetland04_3 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-22 10:50:00",tz="UTC")&
                                       DateTime < as.POSIXct("2022-07-22 11:18:00",tz = "UTC"))

Wetland04 <- rbind(Wetland04_1,Wetland04_2,Wetland04_3)%>%drop_na(AirPres_kpa)
rm(Wetland04_1,Wetland04_2,Wetland04_3)

plot(Wetland04$DateTime, Wetland04$AirPres_kpa, main = "Wetland 4")

fit <- lm(AirPres_kpa ~ Baro_kpa, data = Wetland04)
summary(fit)

plot(Wetland04$Baro_kpa, Wetland04$AirPres_kpa, main = "Wetland 4")
abline(lm(Wetland04$AirPres_kpa ~ Wetland04$Baro_kpa))

## rounded coefficients for better output
cf <- round(coef(fit), 2) 
eq <- paste0("BaroNOMAD = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " BaroSTATION ")
mtext(eq, 3)


## Wetland 5
#6/28/22 @ 9:55 to 11:30
#7/19/22 @ 9:22 - 10:27
#7/27/22 @ 11:18-11:48

Wetland05_1 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-06-28 09:55:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-06-28 11:30:00",tz = "UTC"))
Wetland05_2 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-19 09:22:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-19 10:27:00",tz = "UTC"))
Wetland05_3 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-27 11:18:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-27 11:48:00",tz = "UTC"))

Wetland05 <- rbind(Wetland05_1,Wetland05_2,Wetland05_3)
rm(Wetland05_1,Wetland05_2,Wetland05_3)

plot(Wetland05$DateTime, Wetland05$AirPres_kpa, main = "Wetland 5")

fit <- lm(AirPres_kpa ~ Baro_kpa, data = Wetland05)
summary(fit)

plot(Wetland05$Baro_kpa, Wetland05$AirPres_kpa, main = "Wetland 5")
#abline(lm(Wetland05$AirPres_kpa ~ Wetland05$Baro_kpa))
abline(coef(fit)[1:2])
## rounded coefficients for better output
cf <- round(coef(fit), 2) 
eq <- paste0("BaroNOMAD = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " BaroSTATION ")
mtext(eq, 3)

#wetland 06, 07, 11
#launched 6/30 @ 9:10-10:10, 10:50 - 11:29
#7/11/2022 @10:15 - 13:50
#7/19/2022 @12:34 - 13:07, 11:38 - 12:05, 11:01 - 11:31
#7/22/22 12:02 - 12:32

Wetland060711_1 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-06-30 09:10:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-06-30 11:29:00",tz = "UTC"))
Wetland060711_2 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-11 10:15:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-11 13:15:00",tz = "UTC"))
Wetland060711_3 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-19 11:01:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-19 13:07:00",tz = "UTC"))
Wetland060711_4 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-22 12:02:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-22 12:32:00",tz = "UTC"))

Wetland060711 <- rbind(Wetland060711_1,Wetland060711_2,Wetland060711_3,Wetland060711_4)%>%drop_na(AirPres_kpa)
rm(Wetland060711_1,Wetland060711_2,Wetland060711_3,Wetland060711_4)

plot(Wetland060711$DateTime, Wetland060711$AirPres_kpa, main = "Wetland 6, 7, 11")

fit <- lm(AirPres_kpa ~ Baro_kpa, data = Wetland060711)
summary(fit)

plot(Wetland060711$Baro_kpa, Wetland060711$AirPres_kpa, main = "Wetland 6, 7, 11")
#abline(lm(Wetland060711$AirPres_kpa ~ Wetland060711$Baro_kpa))
abline(coef(fit)[1:2])

## rounded coefficients for better output
cf <- round(coef(fit), 2) 
eq <- paste0("BaroNOMAD = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " BaroSTATION ")
mtext(eq, 3)


## Wetland 8, 9 ,10
#7/5/22 @ 9:44 - 10:15, 10:37 - 11:07, 11:25 - 11:55
#7/8/22 @ 10:22 - 10:37, 10:57 - 11:17, 11:59 - 12:16
#7/18/22 @ 9:27-9:57. 10:08 - 11:19


Wetland080910_1 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-05 09:44:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-05 11:55:00",tz = "UTC"))
Wetland080910_2 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-08 10:22:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-08 12:16:00",tz = "UTC"))
Wetland080910_3 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-18 09:27:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-18 11:19:00",tz = "UTC"))

Wetland080910 <- rbind(Wetland080910_1,Wetland080910_2,Wetland080910_3)%>%drop_na(AirPres_kpa)
rm(Wetland080910_1,Wetland080910_2,Wetland080910_3)

fit <- lm(AirPres_kpa ~ Baro_kpa, data = Wetland080910)
summary(fit)

plot(Wetland080910$DateTime, Wetland080910$AirPres_kpa, main = "Wetland 8.9.10")

plot(Wetland080910$Baro_kpa, Wetland080910$AirPres_kpa, main = "Wetland 8.9.10")
#abline(lm(Wetland080910$AirPres_kpa ~ Wetland080910$Baro_kpa))
abline(coef(fit)[1:2])

## rounded coefficients for better output
cf <- round(coef(fit), 2) 
eq <- paste0("BaroNOMAD = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " BaroSTATION ")
mtext(eq, 3)




#Gavilan 
#7/6/222 @ 12:24 - 13:00
#7/22/22 9:30 - 10:01 
#7/27/22 @ 9:34 - 10:10


Wetland12_1 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-06 12:24:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-06 13:00:00",tz = "UTC"))

Wetland12_2 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-22 09:30:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-22 10:01:00",tz = "UTC"))

Wetland12_3 <- BaroDATA%>%filter(DateTime > as.POSIXct("2022-07-27 09:34:00",tz="UTC")&
                                   DateTime < as.POSIXct("2022-07-27 10:10:00",tz = "UTC"))


Wetland12 <- rbind(Wetland12_1,Wetland12_2,Wetland12_3)%>%drop_na(AirPres_kpa)
rm(Wetland12_1,Wetland12_2,Wetland12_3)

plot(Wetland12$DateTime, Wetland12$AirPres_kpa, main = "Wetland 12")

plot(Wetland12$Baro_kpa, Wetland12$AirPres_kpa, main = "Wetland 12")
abline(lm(Wetland12$AirPres_kpa ~ Wetland12$Baro_kpa))
