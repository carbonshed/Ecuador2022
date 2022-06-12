##script to merge co2 data for vaisala calibration
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotly)

setwd(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/2022-06-03"))
all_files=list.files(pattern=".csv") #pulls out the csv files from WL folder in HOBOware folder
sites_rp=gsub("_.*","",all_files) #selects the correct pattern so as to seelct only desired files
#sites_rp = sub('_[^_]+$', '', all_files)
site_names=unique(sites_rp) #creates list of site names for following loop
#site_names=site_names[-2]



#rm old files, if they exist
rm(CO2Data)
rm(Temp_CO2Data)

for (site in site_names){
  # if(site == "CO2_02"){
  
  list1=list.files(pattern=site) #finds all files for the site
  sitelist_csv=grep(".csv",list1) #creates list of all files for site
  file_list=list1[sitelist_csv]
  
  #reads in files in list and appends
  for (file in file_list){
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
      
      CO2Data$Station <- site
    }
    if (exists("CO2Data")) {
      Temp_CO2Data <- read.csv(file, skip=6, header = TRUE)  
      Temp_CO2Data=Temp_CO2Data[,1:3]
      if(colnames(Temp_CO2Data)[1]=="Date"){
        colnames(Temp_CO2Data) <- c("Date","Time","ppm")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Date, Temp_CO2Data$Time), format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
        Temp_CO2Data$Station <- site
      } else {
        #        Temp_CO2Data$Fecha <- as.Date(Temp_CO2Data$Fecha, format = "%d / %m / %Y")
        Temp_CO2Data$DateTime <- as.POSIXct(paste(Temp_CO2Data$Fecha, Temp_CO2Data$Tiempo), format="%d/%m/%Y %H:%M:%S", tz = "UTC")
        colnames(Temp_CO2Data) <- c("Date","Time","ppm","DateTime")
        Temp_CO2Data$Station <- site
      }
      CO2Data <- rbind(CO2Data, Temp_CO2Data)
      rm(Temp_CO2Data)
    }
    
  }
  
  CO2Data$DateTime <- round_date(CO2Data$DateTime, "1 mins")
  
  CO2Data=unique(CO2Data)
  CO2Data$Date <- NULL
  CO2Data$Time <- NULL
  CO2Data <- CO2Data[,c(3,2,1)]
  assign((paste(site,sep="_")),CO2Data) #creates object with new appended data
  rm(CO2Data) #removes CO2 data so that multiple sites aren't appended together
}



#plot'
CO2data <- Box3

CO2data <- CO2data %>%
  group_by(DateTime) %>%
  summarise(ppm = mean(ppm))

fig <- plot_ly(data = CO2data, x = ~DateTime, y = ~ppm)

#Temperature and baro
Temp <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/Kmeasurement_Temp.csv"), skip=1, header = TRUE, sep = ",",
                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#Temp <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-09/Kmeasurement_Temp.csv"), skip=1, header = TRUE, sep = ",",
#                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Temp <- Temp[,2:4]
colnames(Temp) <- c("DateTime","WaterPressure_kpa","WaterTemp_c")
Temp$DateTime <- as.POSIXct(Temp$DateTime,  format="%m/%d/%y %I:%M:%S %p", tz = "UTC")

fig <- plot_ly(data = Temp, x = ~DateTime, y = ~WaterPressure_kpa)
fig <- plot_ly(data = Temp, x = ~DateTime, y = ~WaterTemp_c)

summary(Temp)

#
#Baro <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-09/Kmeasure_Baro.csv"), skip=1, header = TRUE, sep = ",",
#                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Baro <-  read.csv(here::here("VaisalaCalibration/VaisalaCalibration_2022-06-03/Kmeasure_Baro.csv"), skip=1, header = TRUE, sep = ",",
                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Baro <- Baro[,2:4]
colnames(Baro) <- c("DateTime","AirPressure_kpa","AirTemp_c")
Baro$DateTime <- as.POSIXct(Baro$DateTime,  format="%m/%d/%y %I:%M:%S %p", tz = "UTC")

fig <- plot_ly(data = Baro, x = ~DateTime, y = ~AirPressure_kpa)
fig <- plot_ly(data = Baro, x = ~DateTime, y = ~AirTemp_c)

summary(Baro)

#Date 2022-06-09
# Time   | Box1   | Box3   | Box6   |  K600  | Riveros4 | stn01  | stn04  | stn3   | Unnamed | stn02
# 18:00  | 8625.2 | 8147.1 | 8968.2 | 8141.8 | 8257.8   | 9370.6 | 8674   | 9092.5 | 9652.6  | NA
# 20:15  | 4333.9 | 4307.7 | NA     | 4399.4 | 4304.0   | 5348.8 | 5069.9 | 4688.8 | 5365.0  | NA   
# 21:00  | 2661.8 | 2679.1 | NA     | 2793.2 | 2763.9   | 3732.5 | 3712.1 | 2886.5 | 3399.0  | NA
# 22:10  | 1985.6 | 2135.7 | NA     | 2236.3 | 2167.0   | 2847.1 | 2850.3 | 2315.9 | 2706.8  | NA

#*21:00 not long enough to fully reach equilibrium

#      | WaterTemp | Waterpress | AirTemp | AirPress
# mean | 20.0      | 78.6       | 20.6    | 76.8

#####
#*Date 2022-06-03
# Time   | Box1   | Box3   | Box6   |  K600  | Riveros4 | station1  | station04  | stn3   | Unnamed | station02
# 22:45  | 1191.9 | 1058.3 | 1612.8 | 1208.8 | NA       | 2477      | 2745.6     |        |         | 1339.3
# 21:01  | 379.9  | 358.0  | 472.4  | 330    | NA       | 409.1     | 392.8      |        |         | 361
# 23:20  | 401.8  | NA     | 520.5  | 330    | NA       | 449.8     | 449.8      |        |         | 400.9


#      | WaterTemp | Waterpress | AirTemp | AirPress
# mean |   22      |   101.7    |  20.33   | 77.09

##Read in Calibration Data
CalibrationData <-  read.csv(here::here("VaisalaCalibration/CalibrationData.csv"), skip=0, header = TRUE, sep = ",",
                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")


#old
df_old <- CalibrationData %>% filter(VaisalaType == "old")
df_old$adjusted_ppm <- df_old$ppm * (1 + (1013 - df_old$Press*10) * 0.0015) * (1 - (25 - df_old$Temp) * 0.003)
#new
#The new viasalas have an internal temperature, so do not ned to be corrected for temp
df_new <- CalibrationData %>% filter(VaisalaType == "new")
df_new$adjusted_ppm <- (df_new$ppm)* (1 + (1013 - df_new$Press*10) * 0.0015) 

#bind
df <- rbind(df_old,df_new)

#cast
Box1 <- df %>% filter(Vaisala == "Box1")
Box1 <- Box1[,c(1:6,9)]
colnames(Box1)[which(names(Box1) == "ppm")] <- "Box1_ppm"
colnames(Box1)[which(names(Box1) == "adjusted_ppm")] <- "Box1_adjusted"

Box3 <- df %>% filter(Vaisala == "Box3")
Box3 <- Box3[,c(1:6,9)]
colnames(Box3)[which(names(Box3) == "ppm")] <- "Box3_ppm"
colnames(Box3)[which(names(Box3) == "adjusted_ppm")] <- "Box3_adjusted"

Box6 <- df %>% filter(Vaisala == "Box6")
Box6 <- Box6[,c(1:6,9)]
colnames(Box6)[which(names(Box6) == "ppm")] <- "Box6_ppm"
colnames(Box6)[which(names(Box6) == "adjusted_ppm")] <- "Box6_adjusted"

K600 <- df %>% filter(Vaisala == "K600")
K600 <- K600[,c(1:6,9)]
colnames(K600)[which(names(K600) == "ppm")] <- "K600_ppm"
colnames(K600)[which(names(K600) == "adjusted_ppm")] <- "K600_adjusted"

Riveros4 <- df %>% filter(Vaisala == "Riveros4")
Riveros4 <- Riveros4[,c(1:6,9)]
colnames(Riveros4)[which(names(Riveros4) == "ppm")] <- "Riveros4_ppm"
colnames(Riveros4)[which(names(Riveros4) == "adjusted_ppm")] <- "Riveros4_adjusted"

stn01 <- df %>% filter(Vaisala == "stn01")
stn01 <- stn01[,c(1:6,9)]
colnames(stn01)[which(names(stn01) == "ppm")] <- "stn01_ppm"
colnames(stn01)[which(names(stn01) == "adjusted_ppm")] <- "stn01_adjusted"

stn04 <- df %>% filter(Vaisala == "stn04")
stn04 <- stn04[,c(1:6,9)]
colnames(stn04)[which(names(stn04) == "ppm")] <- "stn04_ppm"
colnames(stn04)[which(names(stn04) == "adjusted_ppm")] <- "stn04_adjusted"

stn2 <- df %>% filter(Vaisala == "stn2")
stn2 <- stn2[,c(1:6,9)]
colnames(stn2)[which(names(stn2) == "ppm")] <- "stn2_ppm"
colnames(stn2)[which(names(stn2) == "adjusted_ppm")] <- "stn2_adjusted"

stn3 <- df %>% filter(Vaisala == "stn3")
stn3 <- stn3[,c(1:6,9)]
colnames(stn3)[which(names(stn3) == "ppm")] <- "stn3_ppm"
colnames(stn3)[which(names(stn3) == "adjusted_ppm")] <- "stn3_adjusted"

Unnamed <- df %>% filter(Vaisala == "Unnamed")
Unnamed <- Unnamed[,c(1:6,9)]
colnames(Unnamed)[which(names(Unnamed) == "ppm")] <- "Unnamed_ppm"
colnames(Unnamed)[which(names(Unnamed) == "adjusted_ppm")] <- "Unnamed_adjusted"

#merge
df2 <- full_join(Box1, Box3, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, Box6, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, K600, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, Riveros4, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, stn01, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, stn04, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, stn2, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, stn3, by = c("Date","Time","WaterAir","Temp","Press"))
df2 <- full_join(df2, Unnamed, by = c("Date","Time","WaterAir","Temp","Press"))

#df2$new_ave_ppm <- mean(df2$Box1_ppm,df2$Box3_ppm,df2$Box4_ppm,df2$Box6_ppm,df2$K600_ppm,df2$Riveros4_ppm,df2$stn01_ppm)

df2$new_ave_ppm <- mean(c(df2$Box1_ppm,df2$Box3_ppm,df2$stn2_ppm,df2$stn3_ppm),na.rm=TRUE)
df2$new_ave_ppm <- df2$Box1_ppm + df2$Box3_ppm  + df2$stn2_ppm + df2$stn3_ppm

df2$new_ave_ppm <- rowMeans(df2[,c("Box1_ppm","Box3_ppm","stn2_ppm","stn3_ppm")],na.rm=TRUE)
df2$new_ave_adjusted <- rowMeans(df2[,c("Box1_adjusted","Box3_adjusted","stn2_adjusted","stn3_adjusted")],na.rm=TRUE)

##plot
plot(df2$new_ave_adjusted, df2$Box1_adjusted)
model <- lm(new_ave_adjusted ~ Box1_adjusted, data = df2)
summary(model)

plot(df2$new_ave_adjusted, df2$Box3_adjusted)
model <- lm(new_ave_adjusted ~ Box3_adjusted, data = df2)
summary(model)

plot(df2$new_ave_adjusted, df2$stn3_adjusted)
model <- lm(new_ave_adjusted ~ stn3_adjusted, data = df2)
summary(model)

#
plot(df2$new_ave_adjusted, df2$Box6_adjusted)
model <- lm(new_ave_adjusted ~ Box6_adjusted, data = df2)
summary(model)

plot(df2$new_ave_adjusted, df2$K600_adjusted)
model <- lm(new_ave_adjusted ~ K600_adjusted, data = df2)
summary(model)

plot(df2$new_ave_adjusted, df2$Riveros4_adjusted)
model <- lm(new_ave_adjusted ~ Riveros4_adjusted, data = df2)
summary(model)

plot(df2$new_ave_adjusted, df2$stn01_adjusted)
model <- lm(new_ave_adjusted ~ stn01_adjusted, data = df2)
summary(model)

plot(df2$new_ave_adjusted, df2$stn04_adjusted)
model <- lm(new_ave_adjusted ~ stn01_adjusted, data = df2)
summary(model)

plot(df2$new_ave_adjusted, df2$Unnamed_adjusted)
model <- lm(new_ave_adjusted ~ stn01_adjusted, data = df2)
summary(model)
