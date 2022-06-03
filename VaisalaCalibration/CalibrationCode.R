#this code is to to combine files in the Vaisala calibration folder so that each viasala can be linerly corrected

library(here)
library(dplyr)
library(lubridate)
library(plotly)
library(tidyr)
#read in  data 

#temp and pressure
temp <- read.csv(here::here("VaisalaCalibration/Kmeasurement_Temp.csv"), skip = 1)[,2:4]
colnames(temp) <- c("DateTime","pressure_kPa","temp_c")
temp$DateTime <- as.POSIXct(temp$DateTime, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")

#vaisala new
V1_R98459 <- read.csv(here::here("VaisalaCalibration/V1_R98459.csv"), skip = 6)
V2_Q98478 <- read.csv(here::here("VaisalaCalibration/V2_Q98478.csv"), skip = 6)
V3_R35852 <- read.csv(here::here("VaisalaCalibration/V3_R35852.csv"), skip = 6)
V4_S02591 <- read.csv(here::here("VaisalaCalibration/V4_S02591.csv"), skip = 6)
V5_S02590 <- read.csv(here::here("VaisalaCalibration/V5_S02590.csv"), skip = 6)

#OldV

V6_S02587 <- read.csv(here::here("VaisalaCalibration/V6_S02587.csv"), skip = 6)
k600_R37297 <- read.csv(here::here("VaisalaCalibration/k600_R37297.csv"), skip = 6)

# Colnames
colnames(V1_R98459) <- c("Date","Time","V1_R98459")
colnames(V2_Q98478) <- c("Date","Time","V2_Q98478")
colnames(V3_R35852) <- c("Date","Time","V3_R35852")
colnames(V4_S02591) <- c("Date","Time","V4_S02591")
colnames(V5_S02590) <- c("Date","Time","V5_S02590")

colnames(V6_S02587) <- c("Date","Time","V6_S02587")
colnames(k600_R37297) <- c("Date","Time","k600_R37297")

#Date Time
V1_R98459$DateTime <- as.POSIXct(paste(V1_R98459$Date, V1_R98459$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
V2_Q98478$DateTime <- as.POSIXct(paste(V2_Q98478$Date, V2_Q98478$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
V3_R35852$DateTime <- as.POSIXct(paste(V3_R35852$Date, V3_R35852$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
V4_S02591$DateTime <- as.POSIXct(paste(V4_S02591$Date, V4_S02591$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
V5_S02590$DateTime <- as.POSIXct(paste(V5_S02590$Date, V5_S02590$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

V6_S02587$DateTime <- as.POSIXct(paste(V6_S02587$Date, V6_S02587$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
k600_R37297$DateTime <- as.POSIXct(paste(k600_R37297$Date, k600_R37297$Time), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

V1_R98459 <- V1_R98459[,3:4]
V2_Q98478 <- V2_Q98478[,3:4]
V3_R35852 <- V3_R35852[,3:4]
V4_S02591 <- V4_S02591[,3:4]
V5_S02590 <- V5_S02590[,3:4]
V6_S02587 <- V6_S02587[,3:4]
k600_R37297 <- k600_R37297[,3:4]

#round times
V1_R98459$DateTime <- lubridate::round_date(V1_R98459$DateTime, "15 seconds") 
V2_Q98478$DateTime <- lubridate::round_date(V2_Q98478$DateTime, "15 seconds") 
V3_R35852$DateTime <- lubridate::round_date(V3_R35852$DateTime, "15 seconds") 
V4_S02591$DateTime <- lubridate::round_date(V4_S02591$DateTime, "15 seconds") 
V5_S02590$DateTime <- lubridate::round_date(V5_S02590$DateTime, "15 seconds") 
V6_S02587$DateTime <- lubridate::round_date(V6_S02587$DateTime, "15 seconds") 
k600_R37297$DateTime <- lubridate::round_date(k600_R37297$DateTime, "15 seconds") 

