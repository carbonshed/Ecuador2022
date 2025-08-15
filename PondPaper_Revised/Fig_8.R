# Figure 8
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)


#read in CH4 data
CH4_summary <- read.csv(here::here("PondPaper_Revised/Pond_pCH4_data_summary.csv"))
CH4_summary$Date <- as.Date(CH4_summary$Date,format="%Y-%m-%d")

#read in CO2 data
CO2_summary <- read.csv(here::here("PondPaper_Revised/Pond_pCO2_data_summary.csv"))%>%dplyr::select(!Watertemp_c)
CO2_summary$Date <- as.Date(CO2_summary$Date,format="%Y-%m-%d")

predictor_variables <- read.csv(here::here("PondPaper_Revised/predictor_variables_df.csv"))
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")

df_merge <- full_join(CO2_summary,CH4_summary,by=c("Site","Date"))
df_merge <- left_join(df_merge,predictor_variables,by=c("Site","Date"))


df_merge$Area_ha <-  df_merge$surface_area_m2 / 10000
df_merge$Reference <- "This Study"
df_merge$Location <- "La Virgen"
df_merge$latitude <- -0.324311
df_merge$longitude <- -78.1995
df_merge$Study.Years <- 2022
df_merge <- df_merge%>%dplyr::select(Reference,Site,Location,latitude,longitude,Study.Years,CH4_umol.L,CO2_umol.L,Area_ha,Watertemp_c)%>%rename(Site_Name=Site)

#read in Holgerston & Raymond data set
df_Holg <- read.csv(here::here("PondPaper_Revised/Holgerson_and_Raymond2016_df.csv"))%>%dplyr::select(!X)

df_Holg <- rbind(df_Holg,df_merge)
df_Holg$CH4_umol.L <- as.numeric(df_Holg$CH4_umol.L)
df_Holg$CO2_umol.L <- as.numeric(df_Holg$CO2_umol.L)
df_Holg$Watertemp_c <- as.numeric(df_Holg$Watertemp_c)
df_Holg <- df_Holg%>%drop_na(CO2_umol.L)%>%drop_na(CH4_umol.L)


df_Holg$Site <- df_Holg$Site_Name
df_Holg$Site <- factor(df_Holg$Site, levels = c("S1", "S2", "S3","S4","S5","S6","S7","S8","Wetland","S10","S11","S12"))

#Plot
p <- ggplot() +
  geom_point(data=df_Holg#%>%filter(Area_ha < .5)
             ,aes(x=CO2_umol.L,y=CH4_umol.L),color="grey",size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),aes(x=CO2_umol.L,y=CH4_umol.L,fill=Site),shape=21,size=3) +
  xlab(expression(paste("CO"[2] ," (",mu,"mol ", L^-1,")"))) +
  ylab(expression(paste("CH"[4] ," (",mu,"mol ", L^-1,")"))) +
  scale_y_log10() + scale_x_log10() + scale_fill_discrete(name = "Site") +
  theme_bw(base_size = 14) 

