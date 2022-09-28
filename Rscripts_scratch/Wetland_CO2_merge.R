#MERGE WETLAND CO2 DATA
#Kriddie Whitmore
#2022-18-08
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotly)
library(here)

setwd(here::here("Wetlands/Files_Renamed/VaisalaDATA/"))
all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder

#rm old files, if they exist
rm(CO2Data)
rm(Temp_CO2Data)

for (file in all_files){
    if (!exists("CO2Data")){
      CO2Data <- read.csv(file, skip=6, header = TRUE)
      CO2Data=CO2Data[,1:3]
      if(names(CO2Data)[1] == "Date"){
        colnames(CO2Data) <- c("Date","Time","ppm")
        CO2Data$DateTime <- as.POSIXct(paste(CO2Data$Date, CO2Data$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
      } else { 
        colnames(CO2Data) <- c("Date","Time","ppm")
        CO2Data$DateTime <- as.POSIXct(paste(CO2Data$Date, CO2Data$Time), format="%d/%m/%Y %H:%M:%S", tz = "UTC")
      }
    
    }
    if (exists("CO2Data")) {
      Temp_CO2Data <- read.csv(file, skip=6, header = TRUE)  
      Temp_CO2Data=Temp_CO2Data[,1:3]
      if(colnames(Temp_CO2Data)[1]=="Date"){
        colnames(Temp_CO2Data) <- c("Date","Time","ppm")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Date, Temp_CO2Data$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
      } else {
        #        Temp_CO2Data$Fecha <- as.Date(Temp_CO2Data$Fecha, format = "%d / %m / %Y")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Fecha, Temp_CO2Data$Tiempo), format="%d/%m/%Y %H:%M:%S", tz = "UTC")
        colnames(Temp_CO2Data) <- c("Date","Time","ppm","DateTime")

      }
      CO2Data <- rbind(CO2Data, Temp_CO2Data)
      rm(Temp_CO2Data)
    }
    
  }
  
CO2Data=unique(CO2Data)
CO2Data <- CO2Data[,c(1,2,4,3)]
CO2Data <- rename(CO2Data, ppm_NOTcorrected=ppm)

ggplot(CO2Data, aes(x=DateTime, y=ppm_NOTcorrected)) +
    geom_point(size=2, shape=23)

ggplot(CO2Data%>%filter(Date=="6/29/2022"), aes(x=DateTime, y=ppm_NOTcorrected)) +
  geom_point(size=2, shape=23)

CO2Data <- CO2Data%>%filter(Date=="7/11/2022" & ppm_NOTcorrected > 25)

write.csv(CO2Data,here::here("Wetlands/Wetland_CO2.csv"))  
