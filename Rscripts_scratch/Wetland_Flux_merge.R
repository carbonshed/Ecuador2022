#MERGE WETLAND CO2 DATA
#Kriddie Whitmore
#2022-09-17
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotly)
library(here)


setwd(here::here("Wetlands/Files_Renamed/FluxDATA/"))
all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder



#rm old files, if they exist
rm(FluxData)
rm(Temp_FluxData)

for (file in all_files){
  if (!exists("FluxData")){
    FluxData <- read.csv(file, skip=0, header = TRUE)
    FluxData=FluxData[,1:8]
    
  }
  if (exists("FluxData")) {
    Temp_FluxData <- read.csv(file, skip=0, header = TRUE)  
    Temp_FluxData=Temp_FluxData[,1:8]
    }
    FluxData <- rbind(FluxData, Temp_FluxData)
    rm(Temp_FluxData)

  }

FluxData=unique(FluxData)

FluxData$Date<-as.Date(with(FluxData,paste(Year,Month,Day,sep="-")),"%y-%m-%d")
FluxData$DateTime <- as.POSIXct(paste(FluxData$Date, FluxData$Time), format="%Y-%m-%d %H:%M:%S", tz = "UTC")

FluxData <- FluxData%>%filter(DateTime > "2022-06-01 00:00:00")
#before october, EOS was in EST, not ecuador time, so subtract 1 hour
FluxData$DateTime <- FluxData$DateTime - hours(1)
#FluxData$DateTime_adjusted <- as.POSIXct("2022-08-01 00:00:01", tz = "UTC")
FluxData[FluxData$DateTime < "2022-08-01 00:00:00",]$DateTime <- FluxData[FluxData$DateTime < "2022-08-01 00:00:00",]$DateTime - hours(1)


ggplot(FluxData, aes(x=DateTime, y=Flux)) +
  geom_point(size=2, shape=23)


write.csv(FluxData,here::here("Wetlands/Wetland_Flux.csv"))  


