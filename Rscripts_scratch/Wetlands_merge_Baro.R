#MERGE WETLAND Water Level DATA
#Kriddie Whitmore
#2022-18-08
library(lubridate) #package for our date parsing
library(here)
library(dplyr)
library(ggplot2)
library(reshape2)
library(purrr)
library(sjmisc)
library(plotly)
#theme_set(theme_bw())

setwd(here::here("Wetlands/Files_Renamed/BaroDATA/"))
all_files=list.files(pattern=".csv") #pulls out the csv files from CO2 folder
#sites_rp = sub('_[^_]+$', '', all_files)
#site_names=unique(sites_rp) #creates list of site names for following loop

#site_names = site_names[c(2:6)]
#site_names = site_names[c(2,3,5,6)]

#rm old files, if they exist
rm(BaroData)
rm(Temp_BaroData)

#for (site in site_names){
  
#  list1=list.files(pattern=site) #finds all files for the site
#  sitelist_csv=grep(".csv",list1) #creates list of all files for site
#  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in all_files){
    if (!exists("BaroData")){
       
      BaroData <- read.csv(file, skip=1, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
      
      if(str_contains(BaroData[1,3],"Abs Pres, psi")){
        BaroData <- BaroData[-1,]
        colnames(BaroData)=c("row","DateTime","AirPres_kpa","AirTemp_c")
        BaroData <- BaroData[2:4]
        BaroData$AirPres_kpa <- as.numeric(as.character(BaroData$AirPres_kpa), digits=6)
        BaroData$AirTemp_c <- as.numeric(as.character(BaroData$AirTemp_c), digits=5)
        BaroData$AirPres_kpa <- BaroData$AirPres_kpa*6.89476
        BaroData$AirTemp_c <- (BaroData$AirTemp_c - 32)/1.8000
        
      } else { 
        BaroData <- BaroData[-1,]
        colnames(BaroData)=c("row","DateTime","AirPres_kpa","AirTemp_c")
        BaroData <- BaroData[2:4]
        BaroData$AirPres_kpa <- as.numeric(as.character(BaroData$AirPres_kpa), digits=6)
        BaroData$AirTemp_c <- as.numeric(as.character(BaroData$AirTemp_c), digits=5)
      }
      
      #      WLData$DateTime <- as.POSIXct(WLData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
      BaroData$DateTime <- as.POSIXct(BaroData$DateTime, tz="UTC",
                                    tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                                   "%m/%d/%Y %H:%M"))
    }
    if (exists("BaroData")){
      Temp_BaroData <- read.csv(file, skip=1, header = FALSE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")  
      #      
      
      if(str_contains(Temp_BaroData[1,3],"Abs Pres, psi")){
        Temp_BaroData <- Temp_BaroData[-1,]
        colnames(Temp_BaroData)=c("row","DateTime","AirPres_kpa","AirTemp_c")
        Temp_BaroData <- Temp_BaroData[2:4]
        Temp_BaroData$AirTemp_c <- as.numeric(as.character(Temp_BaroData$AirTemp_c), digits=6)
        Temp_BaroData$AirPres_kpa <- as.numeric(as.character(Temp_BaroData$AirPres_kpa), digits=5)
        Temp_BaroData$AirPres_kpa <- Temp_BaroData$AirPres_kpa*6.89476
        Temp_BaroData$AirTemp_c <- (Temp_BaroData$AirTemp_c - 32)/1.8000
        
        
      } else { 
        Temp_BaroData <- Temp_BaroData[-1,]
        colnames(Temp_BaroData)=c("row","DateTime","AirPres_kpa","AirTemp_c")
        Temp_BaroData <- Temp_BaroData[2:4]
        Temp_BaroData$AirPres_kpa <- as.numeric(as.character(Temp_BaroData$AirPres_kpa), digits=6)
        Temp_BaroData$AirTemp_c <- as.numeric(as.character(Temp_BaroData$AirTemp_c), digits=5)
      }
      
      #      Temp_WLData$DateTime <- as.POSIXct(Temp_BaroData$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")
      Temp_BaroData$DateTime <- as.POSIXct(Temp_BaroData$DateTime, tz="UTC",
                                         tryFormats = c("%m/%d/%y %I:%M:%S %p",
                                                        "%m/%d/%Y %H:%M"))
      
      
      BaroData <- rbind(BaroData, Temp_BaroData)
      rm(Temp_BaroData)
    }
    
  }

BaroData=unique(BaroData)

#write.csv(BaroData,here::here("Wetlands/Wetland_Baro.csv"))  
