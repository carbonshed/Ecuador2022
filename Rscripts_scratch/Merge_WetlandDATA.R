#Wetland dataframe
#add environmental factors
#add depth/vol ratio and surface area
#also add information that doesnt vary by time (water samples, watershed size)

library(dplyr)
library(tidyverse)
library(here)
library(lubridate)
library(zoo)

#read in
df <- read.csv(here::here("Wetlands/Wetland_df_2023-10-26.csv"))
df$DateTime <- as.POSIXct(df$DateTime,format="%m/%d/%y %H:%M",tz="UTC")
df$DateTime <- round_date(df$DateTime,unit = "15 minutes")
df$Date <- as.Date(df$Date,format = "%m/%d/%y")

df$X <- NULL

#calc diff in temp water/air
df$airwaterTemp_diff <-  df$AirTemp_c - df$Watertemp_c

############
#water samples
###############

DOC_df <- read.csv(here::here("WaterSamples/DOC_KW_08-25-2022.csv"))
DOC_df <- DOC_df[,c("DOC..mg.L.","TDN..mg.L.","Date","site")]
DOC_df <- DOC_df %>% 
  rename(
    Wetland = site,
    DOC_mg.L = DOC..mg.L.,
    TDN_mg.L = TDN..mg.L.
  )
DOC_df$Date <- as.Date.character(DOC_df$Date,format="%m/%d/%y")

DOC_df <- DOC_df%>%
  filter(Wetland!="Colm")%>%
  filter(Wetland!="colm")%>%
  filter(Wetland!="gavi")%>%
  filter(Wetland!="gavi-trib")%>%
  filter(Wetland!="gavi trib")%>%
  filter(Wetland!="Gavi trib")


#merge data
#df <- full_join(CO2_df,CH4_df, by=c("Wetland","Date"))
df <- left_join(df,DOC_df [,c("Wetland","DOC_mg.L","TDN_mg.L")], by=c("Wetland"))

#add watershed area
df$Wetland <-gsub("_","",df$Wetland) 
ws_area <- read.csv(here::here("Wetlands/Watershed_size.csv"))
df <- left_join(df,ws_area, by=c("Wetland"))

#####################
#add surface areas and depth to vol ratio
WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
WL_df <- WL_df[c("DateTime","Station","WLTemp_c","depth_ave_m","surface_area_m2","Volumn_m3","SA_to_Vol_ratio")]
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

WL_df$Wetland <- gsub(".*_","",WL_df$Station)
WL_df$Station <- NULL

#summarize
WL_summary_yearly <- WL_df %>%
  filter(DateTime > as.POSIXct("2022-06-28 00:00:00")&
           DateTime < as.POSIXct("2023-07-01 00:00:00"))%>%
  group_by(Wetland)%>%
  summarize(
    waterTemp_c_yearly = mean(WLTemp_c, na.rm = TRUE),
    depth_m_yearly = mean(depth_ave_m, na.rm = TRUE),
    depth_m_yearly = mean(depth_ave_m, na.rm = TRUE),
    surface_area_yearly = mean(surface_area_m2,na.rm = TRUE), 
    Volumn_m3_yearly = mean(Volumn_m3, na.rm = TRUE),
    SA_to_Vol_ratio_yearly = mean(SA_to_Vol_ratio,na.rm = TRUE)
  )

WL_summary_summer <- WL_df%>%
  filter(DateTime > as.POSIXct("2022-06-28 00:00:00") &
           DateTime < as.POSIXct("2022-07-27 00:00:00"))%>%
  group_by(Wetland)%>%
  summarize(
    waterTemp_c_summer = mean(WLTemp_c, na.rm = TRUE),
    depth_m_summer = mean(depth_ave_m, na.rm = TRUE),
    surface_area_summer = mean(surface_area_m2,na.rm = TRUE), 
    Volumn_m3_summer = mean(Volumn_m3, na.rm = TRUE),
    SA_to_Vol_ratio_summer = mean(SA_to_Vol_ratio,na.rm = TRUE)
  )

WL_summary_fall <- WL_df%>%
  filter(DateTime > as.POSIXct("2022-10-01 00:00:00") &
           DateTime < as.POSIXct("2022-10-15 00:00:00"))%>%
  group_by(Wetland)%>%
  summarize(
    waterTemp_c_fall = mean(WLTemp_c, na.rm = TRUE),
    depth_m_fall = mean(depth_ave_m, na.rm = TRUE),
    surface_area_fall = mean(surface_area_m2,na.rm = TRUE), 
    Volumn_m3_fall = mean(Volumn_m3, na.rm = TRUE),
    SA_to_Vol_ratio_fall = mean(SA_to_Vol_ratio,na.rm = TRUE)
  )


##merge with data frame

WL_df <- WL_df[c("DateTime","Wetland","depth_ave_m","surface_area_m2","Volumn_m3","SA_to_Vol_ratio")]

df_1 <- left_join(df,WL_df, by=c("Wetland","DateTime"))
df_1$DateTime_used <- df_1$DateTime

df_1_select <-df_1[is.na(df_1$surface_area_m2),]%>%select(-depth_ave_m,-surface_area_m2,-Volumn_m3,-SA_to_Vol_ratio)
df_1 <-df_1%>%drop_na(surface_area_m2)

df_1_select$DateTime <- df_1_select$DateTime + 60*15
df_1_select$DateTime_used <- df_1_select$DateTime
df_1_select <- left_join(df_1_select,WL_df, by=c("Wetland","DateTime"))

df_1_select_2 <-df_1_select[is.na(df_1_select$surface_area_m2),]%>%select(-depth_ave_m,-surface_area_m2,-Volumn_m3,-SA_to_Vol_ratio)
df_1_select <-df_1_select%>%drop_na(surface_area_m2)

df_1_select_2$DateTime_used <- df_1_select_2$DateTime
df_1_select_2$DateTime <- df_1_select_2$DateTime + 60*15
df_1_select_2 <- left_join(df_1_select_2,WL_df, by=c("Wetland","DateTime"))

df_1_select_3 <-df_1_select_2[is.na(df_1_select_2$surface_area_m2),]%>%select(-depth_ave_m,-surface_area_m2,-Volumn_m3,-SA_to_Vol_ratio)
df_1_select_2 <-df_1_select_2%>%drop_na(surface_area_m2)

df_1_select_3$DateTime_used <- df_1_select_3$DateTime
df_1_select_3$DateTime <- df_1_select_3$DateTime - 60*45
df_1_select_3 <- left_join(df_1_select_3,WL_df, by=c("Wetland","DateTime"))

df_1_select_4 <-df_1_select_3[is.na(df_1_select_3$surface_area_m2),]%>%select(-depth_ave_m,-surface_area_m2,-Volumn_m3,-SA_to_Vol_ratio)
df_1_select_3 <-df_1_select_3%>%drop_na(surface_area_m2)

df_1_select_4$DateTime_used <- df_1_select_4$DateTime
df_1_select_4$DateTime <- df_1_select_4$DateTime - 60*15
df_1_select_4 <- left_join(df_1_select_4,WL_df, by=c("Wetland","DateTime"))

df_1_select_5 <-df_1_select_4[is.na(df_1_select_4$surface_area_m2),]%>%select(-depth_ave_m,-surface_area_m2,-Volumn_m3,-SA_to_Vol_ratio)
df_1_select_4 <-df_1_select_4%>%drop_na(surface_area_m2)

df_1_select_5$DateTime_used <- df_1_select_5$DateTime
df_1_select_5$DateTime <- df_1_select_5$DateTime + 60*15*7
df_1_select_5 <- left_join(df_1_select_5,WL_df, by=c("Wetland","DateTime"))

df_1_select_6 <-df_1_select_5[is.na(df_1_select_5$surface_area_m2),]%>%select(-depth_ave_m,-surface_area_m2,-Volumn_m3,-SA_to_Vol_ratio)
df_1_select_5 <-df_1_select_5%>%drop_na(surface_area_m2)

df_1_select_6$DateTime_used <- df_1_select_6$DateTime
df_1_select_6$DateTime <- df_1_select_6$DateTime + 60*15*7
df_1_select_6 <- left_join(df_1_select_6,WL_df, by=c("Wetland","DateTime"))

df_2 <- rbind(df_1,df_1_select,df_1_select_2,df_1_select_3,df_1_select_4,df_1_select_5,df_1_select_6)
df_2$DateTime <- df_2$DateTime_used
df_2$DateTime_used <- NULL

rm(df_1,df_1_select,df_1_select_2,df_1_select_3,df_1_select_4,df_1_select_5,df_1_select_6)

df_2 <- left_join(df_2,WL_summary_summer, by=c("Wetland"))
df_2 <- left_join(df_2,WL_summary_fall, by=c("Wetland"))
df_2 <- left_join(df_2,WL_summary_yearly, by=c("Wetland"))

rm(WL_summary_summer,WL_summary_yearly)


#########################
### add envirofactors ####
########################

df_2$DateTime <- as.POSIXct(paste(df_2$Date,df_2$Time_Baro), format="%Y-%m-%d %H:%M",tz="UTC")
df_2$DateTime <- round_date(df_2$DateTime,unit = "15 minutes")

##read in enviro data from la virgen weather station
viento_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Dirección_del_viento_Dato_validado.csv"))
colnames(viento_df) <- c("DateTime","windspeed_m_s","windspeedMAX_m_s","windspeedMIN_m_s","winddirecion","Cadinal_point") 
viento_df$DateTime <- as.POSIXct(viento_df$DateTime,format="%m/%d/%y %H:%M",tz="UTC")
viento_df$DateTime <- round_date(viento_df$DateTime,unit = "15 minutes")
viento_df <- na.omit(viento_df)
viento_df <- viento_df%>%group_by(DateTime)%>% 
  summarize(
    windspeed_m_s = mean(windspeed_m_s, na.rm = TRUE),
    windspeedMAX_m_s = max(windspeedMAX_m_s,na.rm = TRUE), 
    windspeedMIN_m_s = min(windspeedMIN_m_s, na.rm = TRUE),
    winddirecion = mean(winddirecion, na.rm = TRUE)
  )
  

Solar_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Radiacion_Solar_Dato_validado.csv"))
colnames(Solar_df) <- c("DateTime","solarrad_W_m2","Solarrad_max","solarrad_min")
Solar_df$DateTime <- as.POSIXct(Solar_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
Solar_df$DateTime <- round_date(Solar_df$DateTime,unit = "15 minutes")
Solar_df <- na.omit(Solar_df)
Solar_df <- Solar_df%>%group_by(DateTime)%>% 
  summarize(
    solarrad_W_m2 = mean(solarrad_W_m2, na.rm = TRUE),
    Solarrad_max = max(Solarrad_max,na.rm = TRUE), 
    solarrad_min = min(solarrad_min, na.rm = TRUE)
  )

humidad_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Humedad.csv"))
colnames(humidad_df) <- c("DateTime","humidity_per","humidity_max","humidity_min")
humidad_df$DateTime <- as.POSIXct(humidad_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
humidad_df$DateTime <- round_date(humidad_df$DateTime,unit = "15 minutes")
humidad_df <- na.omit(humidad_df)
humidad_df <- humidad_df%>%group_by(DateTime)%>% 
  summarize(
    humidity_per = mean(humidity_per, na.rm = TRUE),
    humidity_max = max(humidity_max,na.rm = TRUE), 
    humidity_min = min(humidity_min, na.rm = TRUE)
  )

precip_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Precipitacion_Dato_validado.csv"))
colnames(precip_df) <- c("DateTime","precipt_mm")
precip_df$DateTime <- as.POSIXct(precip_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
precip_df$Date <- as.Date(precip_df$DateTime,format="%Y-%m-%d")
precip_summary <- precip_df%>%group_by(Date)%>%summarise(PrecipAccuDay_mm = sum(precipt_mm))
#now add previous amount of precipitation from day previous
precip_summary$Date_used <- precip_summary$Date
precip_summary$Date <- precip_summary$Date - 1
precip_summar_previous <- precip_summary[,c("Date","PrecipAccuDay_mm")]
colnames(precip_summar_previous) <- c("Date","PrecipAccuPreviousDay_mm")
precip_summary <- precip_summary[,c("Date_used","PrecipAccuDay_mm")]
colnames(precip_summary) <- c("Date","PrecipAccuDay_mm")

precip_df_2 <- full_join(precip_summary,precip_summar_previous, by="Date")

#I would like to do a 7 day average
precip_weekAve <- transform(precip_df_2, avg7 = rollmeanr(PrecipAccuDay_mm, 7, fill = NA,na.rm=TRUE))
colnames(precip_weekAve) <- c("Date","PrecipAccuDay_mm","PrecipAccu_mm_PreviousDay","Precip_mm_ave7")


enviro_df <- full_join(viento_df,Solar_df,by="DateTime")
enviro_df <- full_join(enviro_df,humidad_df,by="DateTime")
#enviro_df <- full_join(enviro_df,precip_df,by="DateTime")

#join enviro data
df_3 <- left_join(df_2,enviro_df,by="DateTime")
df_3 <- left_join(df_3,precip_weekAve,by="Date")

#write
write.csv(df_3, here::here("Wetlands/Wetland_df_MERGE_2023-10-28.csv"))
 
