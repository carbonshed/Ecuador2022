# this script is for comparing readings across all vaisalas

library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)

#Temperature and baro
Temp <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Kmeasurement_Temp.csv"), skip=1, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Temp <- Temp[,2:4]
colnames(Temp) <- c("DateTime","WaterPressure_kpa","WaterTemp_c")
Temp$DateTime <- as.POSIXct(Temp$DateTime,  format="%m/%d/%y %I:%M:%S %p", tz = "UTC")

Baro <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Kmeasure_Baro.csv"), skip=1, header = TRUE, sep = ",",
                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Baro <- Baro[,2:4]
colnames(Baro) <- c("DateTime","AirPressure_kpa","AirTemp_c")
Baro$DateTime <- as.POSIXct(Baro$DateTime,  format="%m/%d/%y %I:%M:%S %p", tz = "UTC")


#Box1
Box1_NewV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box1_NewV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                            quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box1_NewV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box1_NewV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box1_NewV_03 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box1_NewV_03_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Box1_NewV <- rbind(Box1_NewV_01,Box1_NewV_02,Box1_NewV_03)
rm(Box1_NewV_01,Box1_NewV_02,Box1_NewV_03)

Box1_NewV$DateTime <- as.POSIXct(paste(Box1_NewV$Date, Box1_NewV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
Box1_NewV$Date <- as.Date(Box1_NewV$Date, format="%m/%d/%Y" )

Box1_NewV$VailsalaType <- "new"
Box1_NewV$Name <- "Box1_NewV"

#Box3
Box3_NewV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box3_NewV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box3_NewV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box3_NewV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")


Box3_NewV <- rbind(Box3_NewV_01,Box3_NewV_02)
rm(Box3_NewV_01,Box3_NewV_02)

Box3_NewV$DateTime <- as.POSIXct(paste(Box3_NewV$Date, Box3_NewV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
Box3_NewV$Date <- as.Date(Box3_NewV$Date, format="%m/%d/%Y" )

Box3_NewV$VailsalaType <- "new"

Box3_NewV$Name <- "Box3_NewV"

#Box4
Box4_NewV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box4_NewV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box4_NewV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box4_NewV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box4_NewV_03 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box4_NewV_03_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Box4_NewV <- rbind(Box4_NewV_01,Box4_NewV_02,Box4_NewV_03)
rm(Box4_NewV_01,Box4_NewV_02,Box4_NewV_03)

Box4_NewV$DateTime <- as.POSIXct(paste(Box4_NewV$Date, Box4_NewV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
Box4_NewV$Date <- as.Date(Box4_NewV$Date, format="%m/%d/%Y" )

Box4_NewV$VailsalaType <- "new"

Box4_NewV$Name <- "Box4_NewV"

#Box6
Box6_OldV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box6_OldV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box6_OldV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box6_OldV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box6_OldV_03 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box6_OldV_03_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Box6_OldV_04 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Box6_OldV_04_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Box6_OldV <- rbind(Box6_OldV_01,Box6_OldV_02,Box6_OldV_03,Box6_OldV_04)
rm(Box6_OldV_01,Box6_OldV_02,Box6_OldV_03,Box6_OldV_04)

Box6_OldV$DateTime <- as.POSIXct(paste(Box6_OldV$Date, Box6_OldV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
Box6_OldV$Date <- as.Date(Box6_OldV$Date, format="%m/%d/%Y" )

Box6_OldV$VailsalaType <- "old"

Box6_OldV$Name <- "Box6_OldV"

#Box K600
K600_OldV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/K600_OldV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
K600_OldV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/K600_OldV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")

K600_OldV <- rbind(K600_OldV_01,K600_OldV_02)
rm(K600_OldV_01,K600_OldV_02)

K600_OldV$DateTime <- as.POSIXct(paste(K600_OldV$Date, K600_OldV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
K600_OldV$Date <- as.Date(K600_OldV$Date, format="%m/%d/%Y" )

K600_OldV$VailsalaType <- "old"

K600_OldV$Name <- "K600_OldV"

#Station 1
Stn1_OldV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station1_OldV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn1_OldV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station1_OldV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Stn1_OldV <- rbind(Stn1_OldV_01,Stn1_OldV_02)
rm(Stn1_OldV_01,Stn1_OldV_02)

Stn1_OldV$DateTime <- as.POSIXct(paste(Stn1_OldV$Date, Stn1_OldV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
Stn1_OldV$Date <- as.Date(Stn1_OldV$Date, format="%m/%d/%Y" )

Stn1_OldV$VailsalaType <- "old"
Stn1_OldV$Name <- "Stn1_OldV"

#Station2
Stn2_OldV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_03 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_03_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_04 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_04_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_05 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_05_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_07 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_07_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_08 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_08_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_09 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_09_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn2_OldV_10 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station2_NewV_10_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")


Stn2_newV <- rbind(Stn2_OldV_01,Stn2_OldV_02,Stn2_OldV_03,Stn2_OldV_04,Stn2_OldV_05,Stn2_OldV_07,Stn2_OldV_08,Stn2_OldV_09,Stn2_OldV_10)
rm(Stn2_OldV_01,Stn2_OldV_02,Stn2_OldV_03,Stn2_OldV_04,Stn2_OldV_05,Stn2_OldV_07,Stn2_OldV_08,Stn2_OldV_09,Stn2_OldV_10)

Stn2_newV$DateTime <- as.POSIXct(paste(Stn2_newV$Date, Stn2_newV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
Stn2_newV$Date <- as.Date(Stn2_newV$Date, format="%m/%d/%Y" )

Stn2_newV$VailsalaType <- "new"

Stn2_newV$Name <- "Stn2_newV"

#Station 4
Stn4_OldV_01 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station4_OldV_01_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn4_OldV_02 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station4_OldV_02_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn4_OldV_03 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station4_OldV_03_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Stn4_OldV_04 <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03/Station4_OldV_04_2022-06-03.csv"), skip=6, header = TRUE, sep = ",",
                          quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Stn4_OldV <- rbind(Stn4_OldV_01,Stn4_OldV_02,Stn4_OldV_03,Stn4_OldV_04)
rm(Stn4_OldV_01,Stn4_OldV_02,Stn4_OldV_03,Stn4_OldV_04)

Stn4_OldV$DateTime <- as.POSIXct(paste(Stn4_OldV$Date, Stn4_OldV$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
Stn4_OldV$Date <- as.Date(Stn4_OldV$Date, format="%m/%d/%Y" )

Stn4_OldV$VailsalaType <- "old"

Stn4_OldV$Name <- "Stn4_OldV"

#Bind All
VaisalaData <- rbind(Box1_NewV,Box3_NewV,Box6_OldV,K600_OldV,Stn1_OldV,Stn2_newV,Stn4_OldV)
#remove dups
VaisalaData <- VaisalaData %>% distinct()
#merge with continuous data baro and temp
VaisalaData <- full_join(Temp,VaisalaData)
VaisalaData <- full_join(Baro,VaisalaData)

#adjust ppm
##Correct for temp and pressure
#assume 2 cm submersion of vaisala
VaisalaData$WaterPressure_hPa <- VaisalaData$WaterPressure_kpa * 10 + 1*2.4884
VaisalaData$AirPressure_hPa <- VaisalaData$AirPressure_kpa * 10 

VaisalaData <- VaisalaData%>%filter(DateTime > as.POSIXct("2022-06-03 21:00",tz="UTC"))

old <- VaisalaData[VaisalaData$VailsalaType == "old", ]
new <- VaisalaData[VaisalaData$VailsalaType == "new", ]


old$adjusted_ppm_Water <- 
  old$Voltage..ppm. * (1 + (1013 - old$WaterPressure_kpa) * 0.0015) *
  (1 - (25 - old$WaterTemp_c) * 0.003)
old$adjusted_ppm_Air <- 
  old$Voltage..ppm. * (1 + (1013 - old$AirPressure_hPa) * 0.0015) *
  (1 - (25 - old$AirTemp_c) * 0.003)


new$adjusted_ppm_Water <- 
  new$Voltage..ppm. * (1 + (1013 - new$WaterPressure_kpa) * 0.0015) 
new$adjusted_ppm_Air <- 
  new$Voltage..ppm. * (1 + (1013 - new$WaterPressure_kpa) * 0.0015) 


VaisalaData <- rbind(old,new)



##plot
ggplot(new, aes(DateTime, Voltage..ppm.)) +
  geom_point() +
  facet_wrap(vars(Name))

