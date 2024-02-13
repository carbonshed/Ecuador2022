#Cross Correlation Analysis
#K. Whitmore
#Feb 13, 2024

#About: this script it to look at the realitonship between surface area time series and precipitation


#library 
library(here)
library(dplyr)
library(tidyr)

#read in

df_wetland <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
df_wetland$DateTime <- as.POSIXct(df_wetland$DateTime, format="%Y-%m-%d %H:%M:%S")