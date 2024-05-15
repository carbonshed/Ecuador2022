#Calc k600 

library(here)
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(cowplot)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dplyr)
library(lubridate)

#step 1: Read in flux data

Flux <-  read.csv(here::here("Flux/EOS_Flux_2022-06-06.csv"), skip=0, header = TRUE, sep = ",",
                              quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Flux <- Flux[,c(1:6)]
colnames(Flux) <- c("Month","Day","Year","Time","Flux","Temp")
Flux$EOS_no <- "EOS_1"
Flux$Date <- as.Date(with(Flux, paste(Year, Month, Day,sep="-")), "%y-%m-%d")

Flux$DateTime <- as.POSIXct(paste(Flux$Date, Flux$Time), format="%Y-%m-%d %H:%M:%S")
#step 4: merge all data by DateTime columne (use full_join)
Flux$DateTime <- round_date()

setwd(here::here("Injections/2022-06-06/CO2"))

all_files=list.files(pattern=".csv")        #pulls out the csv files from WL folder in HOBOware folder

#sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to select only desired files
#sites_rp = sub('_[^_]+$', '', all_files)


#shorten site name by removing underscores
sites_rp = sapply(strsplit(all_files, "_"), function(x) x[3])
sites_rp <- "xn10m"
site_names=unique(sites_rp) #creates list of site names for following loop



#rm old files, if they exist
rm(CO2Data)
rm(Temp_CO2Data)

for (site in site_names){
  # if(str_contains("CO2"){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
    if (!exists("CO2Data")){
      CO2Data <- read.csv(file, skip=7, header = TRUE)
      CO2Data=CO2Data[,1:3]
      if(names(CO2Data)[1] == "Date"){
        colnames(CO2Data) <- c("Date","Time","ppm")
        CO2Data$DateTime <- as.POSIXct(paste(CO2Data$Date, CO2Data$Time), format="%m/%d/%y %I:%M:%S %p", tz = "UTC")
      } else { 
        colnames(CO2Data) <- c("Date","Time","ppm")
        CO2Data$DateTime <- as.POSIXct(paste(CO2Data$Date, CO2Data$Time), format="%d/%m/%y %H:%M:%S", tz = "UTC")
      }
      
      CO2Data$Station <- site
    }
    if (exists("CO2Data")) {
      Temp_CO2Data <- read.csv(file, skip=7, header = TRUE)  
      Temp_CO2Data=Temp_CO2Data[,1:3]
      if(colnames(Temp_CO2Data)[1]=="Date"){
        colnames(Temp_CO2Data) <- c("Date","Time","ppm")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Date, Temp_CO2Data$Time), format="%m/%d/%y %I:%M:%S %p", tz = "UTC")
        Temp_CO2Data$Station <- site
      } else {
        #        Temp_CO2Data$Fecha <- as.Date(Temp_CO2Data$Fecha, format = "%d / %m / %Y")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Fecha, Temp_CO2Data$Tiempo), format="%d/%m/%y %H:%M:%S", tz = "UTC")
        colnames(Temp_CO2Data) <- c("Date","Time","ppm","DateTime")
        Temp_CO2Data$Station <- site
      }
      CO2Data <- rbind(CO2Data, Temp_CO2Data)
      rm(Temp_CO2Data)
    }
    
  }
  
  #  CO2Data$DateTime <- round_date(CO2Data$DateTime, "15 mins") #Round if needed
  
  CO2Data=unique(CO2Data)
  CO2Data$Date <- NULL
  CO2Data$Time <- NULL
  CO2Data <- CO2Data[,c(3,2,1)]
  assign((paste(site,sep="_")),CO2Data) #creates object with new appended data
  rm(CO2Data) #removes CO2 data so that multiple sites aren't appended together
}



## Correct viasalas temperature and pressure with HOBO data for the time period we collected data

setwd(here::here("Injections/2022-06-06"))


# first, read in WL hobo at -10m 
waterlevel_n10 <- read.csv("Inj_TEMP_-10m_2022-06-06.csv", skip = 1)
waterlevel_n10 <- waterlevel_n10[,c(2:4)]
colnames(waterlevel_n10) <- c("DateTime", "WL_Pressure", "Temp")

# format DateTime in WL 
waterlevel_n10$DateTime <- as.POSIXct(paste(waterlevel_n10$Date, waterlevel_n10$Time), format="%m/%d/%y %I:%M:%S %p", tz = "UTC")

# correct kpa to hpa 
##1 kPa = 10 hPa
##1 kPa = 0.101972 m
waterlevel_n10$WL_Pressure <- waterlevel_n10$WL_Pressure * 10

#full_join to join CO2 data to WL by datetime
xn10m <- full_join(xn10m, waterlevel_n10, by = "DateTime" )

# clean data <- remove NA values bc vaisala reads every 1s and WL reads ever 10s
xn10m <- na.omit(xn10m) 

# create a new column for adjusted ppm
xn10m$adjusted_ppm <- 0
xn10m$adjusted_ppm <- as.numeric(xn10m$adjusted_ppm)
xn10m$ppm <- as.numeric(xn10m$ppm)


# Make a new data frame from df_corr but just with new vaisalas

# Now correct the adjusted ppm 
xn10m$adjusted_ppm <- (xn10m$ppm)* (1 + (1013 - xn10m$WL_Pressure) * 0.0015)

# Now correct for individual vaisala calibration


xn10m$adjusted_ppm <- (xn10m$adjusted_ppm * 1.00054) - 65.15813


### Calculate concentration (gCO2-C per Liter) in air using ###

#concentration of air
#from www.esrl.noaa.gov
#average of July and August
#410.86 ppm
CO2_air_ppm <- 410*10^-6
R=0.08205736608096
gCO2asC <- 12

#convert hpa to atm; 1hPa = 0.0009869233 atm
df$air_pressure_atm <- df$Total_hPa * 0.0009869233

df$VaporPressure_atm <- 10^(5.40221-(1838.675/((df$AirTemp_c + 273.15)-31.737)))

df$TotalAir_atm <- synoptic.df$air_pressure_atm - df$VaporPressure_atm
df$Total_air_MolperL <- df$TotalAir_atm/(R*(df$AirTemp_c + 273.15)) #PV=nRT; n/V=P/RT
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
df$WaterTemp_K <- df$WaterTemp_c + 273.15
T_STP_K = 298.15

#calculate henry's law constant using 
df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/df$WaterTemp_K - 1/T_STP_K))


UatmToatm <- 10^6

#calculate mass equivalence of CO2 in water
df$CO2_water_gCO2asCPerLiter <- (df$adjusted_ppm / UatmToatm) * synoptic.df$KH_mol.L.atm * gCO2asC

#calculate concentration difference between CO2 in the water and in the air
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

df$Flux_gCO2asCperM2perDay <- df$Flux_ave * SecToDay /  UmoleToMole *gCO2asC

#Fick' Law: calculate K

#**F(aq) = K[pCO2(aq) - pCO2(air)] x KH**
#  * k = F/(pCO2(aq)-pC02(air))/KH
#* F = Flux mole/m2/d
#* K = gas transfer rate m/d
#* KH = henry's constant for molar concentration of CO2 in water 

#*rearrange equation to solve for k*
#**k = F(aq) / ([pCO2(aq) - pCO2(air)] x KH)**

#convert Liters to cubic meters
#LiterToCubicMeter <- 1000
#df$deltaCO2_gCO2asCPerCubicMeter <- df$deltaCO2_gCO2asCPerLiter * LiterToCubicMeter

#calculate

df$k_m.d <- df$Flux_gCO2asCperM2perDay  / df$deltaCO2_gCO2asCperM3


#convert to k600
#k600 = kCO2 * (600/SC)^-0.5
#SC= schmidst constant, temperature dependent
#SC = 1911.1 - 118.11*T + 3.4527*T^2 - 0.04132*T^3
#T = temperature in c

#Wanninkhop 2014 update: Relationshipbetweenwindspeedandgasexchangeovertheoceanrevisited
df$Sc <- 1923.6 - 125.06*df$WaterTemp_c + 4.3773*(df$WaterTemp_c)^2 - 0.085681*(synoptic.df$WaterTemp_c)^3 + 0.00070284*(df$WaterTemp_c)^4

df$K600.effective <- df$k_m.d * (600/df$Sc)^(-0.5)


##write out

