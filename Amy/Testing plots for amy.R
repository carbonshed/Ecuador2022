---
  title: "Amys CO2 plots pt. 2"
author: "Amy"
date: "2022-11-09"
output: html_document
---



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotly)
library(here)
```
library(here) 

## Description Water temperature vs water level 
#Step one: read in data for wetland


```{r read in data 1}

wetland.data  <- read.csv(here::here("wetlands_df.csv"), skip=0, header = TRUE)
wetland.data  <- wetland[,2:5]
colnames(wetland) <- c("Date","Time","DateTime","ppm_NOTcorrected")

#CO2Data$DateTime <- as.POSIXct(CO2Data$DateTime, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
#CO2Data$Date <- as.Date(CO2Data$DateTime)
#CO2Data$Time <- format(as.POSIXct(CO2Data$DateTime), format = "%H:%M:%S", tz = "UTC")

```


#PEEEP THIS IS JUST FOR REFRENCE IT IS IRRELEVANT
#wetland.data <- read.csv(file="wetlands_df.csv")         
#view(wetland.data)

#colnames(wetland.data) <- c("Wetland", "Location","Date","Watertemp_c","Waterlevel","ppm_NOTcorrected","Flux_mean","Flux_stdev","Time_Baro","AirPress_kpa", "AirTemp_C", "AirPress_hpa", "CO2_ppm","AirPress_atm","VaporPressure_atm","TotalAir_atm", "Total_air_MolperL","CO2_air_MolesPerLiter","CO2_air_gCO2asCPerLiter","Watertemp_K","KH_mol.L.atm", "CO2_water_gCO2asCPerLiter","deltaCO2_gCO2asCperM3", "Flux_gCO2asCperM2perDay",	"k_m.d", "Sc", "K600")
#THIS IS THE END OF THE PEEP. 

## Quickly plot to check Data


#```{r Check Data, echo=FALSE}
#ggplot(CO2Data, aes(x=DateTime, y=ppm_NOTcorrected)) +
#  geom_point(size=2, shape=23)

```