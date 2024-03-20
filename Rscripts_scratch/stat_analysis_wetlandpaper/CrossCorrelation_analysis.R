#Cross Correlation Analysis
#K. Whitmore
#Feb 13, 2024

#About: this script it to look at the realitonship between surface area time series and precipitation


#library 
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(zoo)
library(ggplot2)
library(lme4)
library(lmerTest)

#read in

df_wetland <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
df_wetland$DateTime <- as.POSIXct(df_wetland$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")


df_precip <- read.csv(here::here("WeatherStation_LaVirgen/M5025_Precipitacion_Dato_validado.csv"))
df_humedad <- read.csv(here::here("WeatherStation_LaVirgen/M5025_Humedad.csv"))
df_airetemp <-read.csv(here::here("WeatherStation_LaVirgen/M5025_Temp.csv")) 

colnames(df_precip) <- c("DateTime","Precip_cm")
df_precip$DateTime <- as.POSIXct(df_precip$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")

colnames(df_humedad) <- c("DateTime","humidity_perc","humidity_max","humidity_min")
df_humedad$DateTime <- as.POSIXct(df_humedad$DateTime, format="%Y-%m-%d %H:%M:%S",tz="UTC")
df_humedad$humidity_max <- NULL
df_humedad$humidity_min <- NULL

df_airetemp$DateTime <- as.POSIXct(df_airetemp$DateTime, format="%m/%d/%y %H:%M",tz="UTC")
df_airetemp$AirTemp_max <- NULL
df_airetemp$AirTemp_min <- NULL

df_vpd <- full_join(df_airetemp,df_humedad,by="DateTime")
df_vpd$es <- .611*exp(17.502*df_vpd$AirTemp_c/(240.97+df_vpd$AirTemp_c))
df_vpd$vpd <- df_vpd$es*(1-df_vpd$humidity_perc)

#summarize vpd
df_vpd$Date <- as.Date(df_vpd$DateTime)
vpd_summary <- df_vpd%>%group_by(Date)%>%
  summarise(vpd_ave= mean(vpd, na.rm = TRUE))

#summarize by day precip
df_precip$Date <- as.Date(df_precip$DateTime)

Precip_summary <- df_precip%>%group_by(Date)%>%
  summarise(precip_accumulation= sum(Precip_cm
                                     ))
precip_summary2 <- transform(Precip_summary, precip_avg3 = rollmeanr(precip_accumulation, 3, fill = NA,na.rm=TRUE),
                         precip_avg7 = rollmeanr(precip_accumulation, 7, fill = NA,na.rm=TRUE),
                         precip_avg14 = rollmeanr(precip_accumulation, 14, fill = NA,na.rm=TRUE),
                         precip_avg30 = rollmeanr(precip_accumulation, 30, fill = NA,na.rm=TRUE),
                         precip_avg60 = rollmeanr(precip_accumulation, 60, fill = NA,na.rm=TRUE),
                         precip_avg182 = rollmeanr(precip_accumulation, 182, fill = NA,na.rm=TRUE),
                         precip_avg365 = rollmeanr(precip_accumulation, 365, fill = NA,na.rm=TRUE))
precip_summary2$precip_sum3and7 <- precip_summary2$precip_avg3 + precip_summary2$precip_avg7
precip_summary2$precip_sum7and14 <- precip_summary2$precip_avg3 + precip_summary2$precip_avg7
precip_summary2$precip_sum14and30 <- precip_summary2$precip_avg3 + precip_summary2$precip_avg7
precip_summary2$precip_sum3and7and14 <- precip_summary2$precip_avg3 + precip_summary2$precip_avg7 + precip_summary2$precip_avg14
precip_summary2$precip_sum3and7and14and30 <- precip_summary2$precip_avg3 + precip_summary2$precip_avg7 + precip_summary2$precip_avg14 + precip_summary2$precip_avg30
precip_summary2$precip_sum7and14and30 <- precip_summary2$precip_avg7 + precip_summary2$precip_avg14 + precip_summary2$precip_avg30
precip_summary2$precip_sum3and14 <- precip_summary2$precip_avg3 + precip_summary2$precip_avg14 
precip_summary2$precip_sum3and30 <- precip_summary2$precip_avg3 + precip_summary2$precip_avg30 

precip_summary2 <- precip_summary2%>%filter(Date > as.Date("2021-01-01"))

df_wetland$Date <- as.Date(df_wetland$DateTime)
WL_summary <- df_wetland%>%group_by(Station,Date)%>%
  summarise(depth_mean = mean(depth_ave_m,na.rm = TRUE),
            WaterLevel_mean = mean(WaterLevel_m,na.rm = TRUE),
            SA_mean = mean(surface_area_m2,na.rm = TRUE),
            WaterTemp_mean = mean(WLTemp_c,na.rm = TRUE),
            Vol_mean = mean(Volumn_m3,na.rm = TRUE)
            )

#WL_summary_test <- left_join(precip_summary2,WL_summary,by="Date")

WL_01_test <- WL_summary%>%filter(Station=="WL_Wetland01")
WL_01_test$WL_diff <- as.numeric(NA)
WL_01_test$vol_diff <- as.numeric(NA)
for(i in 2:length(WL_01_test$WL_diff)){
  WL_01_test$WL_diff[1] <- NA 
  WL_01_test$WL_diff[i] <- WL_01_test$depth_mean[i] - WL_01_test$depth_mean[i-1]
  WL_01_test$vol_diff[1] <- NA 
  WL_01_test$vol_diff[i] <- WL_01_test$Vol_mean[i] - WL_01_test$Vol_mean[i-1]
  }
WL_01_test <- left_join(precip_summary2,WL_01_test,by="Date")
WL_01_test <- left_join(WL_01_test,vpd_summary,by="Date")

WL_02_test<- WL_summary%>%filter(Station=="WL_Wetland02")
WL_02_test$WL_diff <- as.numeric(NA)
WL_02_test$vol_diff <- as.numeric(NA)
for(i in 2:length(WL_02_test$WL_diff)){
  WL_02_test$WL_diff[1] <- NA 
  WL_02_test$WL_diff[i] <- WL_02_test$depth_mean[i] - WL_02_test$depth_mean[i-1]
  WL_02_test$vol_diff[1] <- NA 
  WL_02_test$vol_diff[i] <- WL_02_test$Vol_mean[i] - WL_02_test$Vol_mean[i-1]
}
WL_02_test <- left_join(precip_summary2,WL_02_test,by="Date")
WL_02_test <- left_join(WL_02_test,vpd_summary,by="Date")


WL_03_test<- WL_summary%>%filter(Station=="WL_Wetland03")
WL_03_test$WL_diff <- as.numeric(NA)
WL_03_test$vol_diff <- as.numeric(NA)
for(i in 2:length(WL_03_test$WL_diff)){
  WL_03_test$WL_diff[1] <- NA 
  WL_03_test$WL_diff[i] <- WL_03_test$depth_mean[i] - WL_03_test$depth_mean[i-1]
}
WL_03_test <- left_join(precip_summary2,WL_03_test,by="Date")
WL_03_test <- left_join(WL_03_test,vpd_summary,by="Date")


WL_04_test<- WL_summary%>%filter(Station=="WL_Wetland04")
WL_04_test$WL_diff <- as.numeric(NA)
WL_04_test$vol_diff <- as.numeric(NA)
for(i in 2:length(WL_04_test$WL_diff)){
  WL_04_test$WL_diff[1] <- NA 
  WL_04_test$WL_diff[i] <- WL_04_test$depth_mean[i] - WL_04_test$depth_mean[i-1]
  WL_04_test$vol_diff[1] <- NA 
  WL_04_test$vol_diff[i] <- WL_04_test$Vol_mean[i] - WL_04_test$Vol_mean[i-1]
}
WL_04_test <- left_join(precip_summary2,WL_04_test,by="Date")
WL_04_test <- left_join(WL_04_test,vpd_summary,by="Date")

WL_05_test<- WL_summary%>%filter(Station=="WL_Wetland05")
WL_05_test$WL_diff <- as.numeric(NA)
WL_05_test$vol_diff <- as.numeric(NA)
for(i in 2:length(WL_05_test$WL_diff)){
  WL_05_test$WL_diff[1] <- NA 
  WL_05_test$WL_diff[i] <- WL_05_test$depth_mean[i] - WL_05_test$depth_mean[i-1]
}
WL_05_test <- left_join(precip_summary2,WL_05_test,by="Date")
WL_05_test <- left_join(WL_05_test,vpd_summary,by="Date")

WL_06_test<- WL_summary%>%filter(Station=="WL_Wetland06")
WL_06_test$WL_diff <- as.numeric(NA)
WL_06_test$vol_diff <- as.numeric(NA)
for(i in 2:length(WL_06_test$WL_diff)){
  WL_06_test$WL_diff[1] <- NA 
  WL_06_test$WL_diff[i] <- WL_06_test$depth_mean[i] - WL_06_test$depth_mean[i-1]
}
WL_06_test <- left_join(precip_summary2,WL_06_test,by="Date")
WL_06_test <- left_join(WL_06_test,vpd_summary,by="Date")

WL_07_test<- WL_summary%>%filter(Station=="WL_Wetland07")
WL_07_test$WL_diff <- as.numeric(NA)
WL_07_test$WL=vol_diff <- as.numeric(NA)
for(i in 2:length(WL_07_test$WL_diff)){
  WL_07_test$WL_diff[1] <- NA 
  WL_07_test$WL_diff[i] <- WL_07_test$depth_mean[i] - WL_07_test$depth_mean[i-1]
}
WL_07_test <- left_join(precip_summary2,WL_07_test,by="Date")
WL_07_test <- left_join(WL_07_test,vpd_summary,by="Date")

WL_08_test <- WL_summary%>%filter(Station=="WL_Wetland08")
WL_08_test$WL_diff <- as.numeric(NA)
WL_08_test$WL=vol_diff <- as.numeric(NA)
for(i in 2:length(WL_08_test$WL_diff)){
  WL_08_test$WL_diff[1] <- NA 
  WL_08_test$WL_diff[i] <- WL_08_test$depth_mean[i] - WL_08_test$depth_mean[i-1]
}
WL_08_test <- left_join(precip_summary2,WL_08_test,by="Date")
WL_08_test <- left_join(WL_08_test,vpd_summary,by="Date")

WL_09_test <- WL_summary%>%filter(Station=="WL_Wetland09")
WL_09_test$WL_diff <- as.numeric(NA)
WL_10_test$WL=vol_diff <- as.numeric(NA)
for(i in 2:length(WL_09_test$WL_diff)){
  WL_09_test$WL_diff[1] <- NA 
  WL_09_test$WL_diff[i] <- WL_09_test$depth_mean[i] - WL_09_test$depth_mean[i-1]
}
WL_09_test <- left_join(precip_summary2,WL_09_test,by="Date")
WL_09_test <- left_join(WL_09_test,vpd_summary,by="Date")

WL_10_test <- WL_summary%>%filter(Station=="WL_Wetland10")
WL_10_test$WL_diff <- as.numeric(NA)
WL_10_test$WL=vol_diff <- as.numeric(NA)
for(i in 2:length(WL_10_test$WL_diff)){
  WL_10_test$WL_diff[1] <- NA 
  WL_10_test$WL_diff[i] <- WL_10_test$depth_mean[i] - WL_10_test$depth_mean[i-1]
}
WL_10_test <- left_join(precip_summary2,WL_10_test,by="Date")
WL_10_test <- left_join(WL_10_test,vpd_summary,by="Date")

WL_11_test <- WL_summary%>%filter(Station=="WL_Wetland11")
WL_11_test$WL_diff <- as.numeric(NA)
WL_11_test$WL=vol_diff <- as.numeric(NA)
for(i in 2:length(WL_11_test$WL_diff)){
  WL_11_test$WL_diff[1] <- NA 
  WL_11_test$WL_diff[i] <- WL_11_test$depth_mean[i] - WL_11_test$depth_mean[i-1]
}
WL_11_test <- left_join(precip_summary2,WL_11_test,by="Date")
WL_11_test <- left_join(WL_11_test,vpd_summary,by="Date")

WL_12_test <- WL_summary%>%filter(Station=="WL_Wetland12")
WL_12_test$WL_diff <- as.numeric(NA)
WL_12_test$WL=vol_diff <- as.numeric(NA)
for(i in 2:length(WL_12_test$WL_diff)){
  WL_12_test$WL_diff[1] <- NA 
  WL_12_test$WL_diff[i] <- WL_12_test$WaterLevel_mean[i] - WL_12_test$WaterLevel_mean[i-1]
}
WL_12_test <- left_join(precip_summary2,WL_12_test,by="Date")
WL_12_test <- left_join(WL_12_test,vpd_summary,by="Date")


#run the thing. this is too easy?
ccf(WL_01_test$precip_accumulation, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_avg3, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_avg7, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_avg14, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_avg30, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_avg60, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_avg182, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_avg365, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_01_test$precip_sum3and7, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_sum3and7and14, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_sum3and14, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_sum7and14, WL_01_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_accumulation, WL_01_test$WL_diff,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$vpd_ave, WL_01_test$WL_diff,5,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$WaterTemp_mean, WL_01_test$WL_diff,50,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_accumulation, WL_01_test$WL_diff,20,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$vpd_ave, WL_01_test$WL_diff,5,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_01_test$precip_accumulation, WL_01_test$vol_diff,20,na.action =	na.pass,ylab = "cross-correlation") 



ccf(WL_03_test$precip_accumulation, WL_03_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_03_test$precip_avg3, WL_03_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_03_test$precip_avg7, WL_03_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_03_test$precip_avg14, WL_03_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation")  
ccf(WL_03_test$precip_avg30, WL_03_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation")  
ccf(WL_03_test$precip_avg60, WL_03_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation")  
ccf(WL_03_test$precip_avg120, WL_03_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation")  

ccf(WL_03_test$precip_accumulation, WL_03_test$WL_diff,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_03_test$vpd_ave, WL_03_test$WL_diff,5,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_04_test$precip_accumulation, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$precip_avg3, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$precip_avg7, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$precip_avg14, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$precip_avg30, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$precip_avg60, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$precip_avg120, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 

ccf(WL_04_test$precip_sum3and30, WL_04_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$precip_accumulation, WL_04_test$WL_diff,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_04_test$vpd_ave, WL_04_test$WL_diff,5,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_05_test$precip_accumulation, WL_05_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_05_test$precip_avg3, WL_05_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_05_test$precip_avg7, WL_05_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_05_test$precip_avg14, WL_05_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_05_test$precip_avg30, WL_05_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_05_test$precip_accumulation, WL_05_test$WL_diff,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_05_test$vpd_ave, WL_05_test$WL_diff,5,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_06_test$precip_accumulation, WL_06_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_06_test$precip_avg3, WL_06_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_06_test$precip_avg7, WL_06_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_06_test$precip_avg14, WL_06_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_06_test$precip_avg30, WL_06_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_06_test$precip_accumulation, WL_06_test$WL_diff,10,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_06_test$vpd_ave, WL_06_test$WL_diff,10,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_07_test$precip_accumulation, WL_07_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$precip_avg3, WL_07_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$precip_avg7, WL_07_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$precip_avg14, WL_07_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$precip_avg30, WL_07_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$precip_avg30, WL_07_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$precip_sum3and30, WL_07_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$precip_accumulation, WL_07_test$WL_diff,10,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_07_test$vpd_ave, WL_07_test$WL_diff,10,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_08_test$precip_accumulation, WL_08_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_08_test$precip_avg3, WL_08_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_08_test$precip_avg7, WL_08_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_08_test$precip_avg14, WL_08_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_08_test$precip_avg30, WL_08_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_08_test$precip_accumulation, WL_08_test$WL_diff,10,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_09_test$precip_accumulation, WL_09_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_09_test$precip_accumulation, WL_09_test$WL_diff,10,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_09_test$vpd_ave, WL_09_test$WL_diff,10,na.action =	na.pass,ylab = "cross-correlation") 


ccf(WL_10_test$precip_accumulation, WL_10_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_11_test$precip_accumulation, WL_11_test$depth_mean,800,na.action =	na.pass,ylab = "cross-correlation") 
ccf(WL_12_test$precip_accumulation, WL_12_test$WaterLevel_mean,800,na.action =	na.pass,ylab = "cross-correlation") 

model <- lm(WL_diff~precip_accumulation, data=WL_03_test)
summary(model)
model1 <- lm(WL_diff~precip_accumulation+vpd_ave, data=WL_05_test)
summary(model1)

plot(WL_09_test$precip_accumulation,WL_09_test$WL_diff) + abline(model)
plot(WL_03_test$vpd_ave,WL_03_test$WL_diff)# + abline(model)


plot(WL_06_test$precip_accumulation,WL_06_test$WL_diff)
plot(WL_07_test$precip_accumulation,WL_07_test$WL_diff)
plot(WL_08_test$precip_accumulation,WL_08_test$WL_diff)
plot(WL_09_test$precip_accumulation,WL_09_test$WL_diff)
plot(WL_10_test$precip_accumulation,WL_10_test$WL_diff)


ccfvalues = ccf(WL_03_test$precip_accumulation,WL_03_test$depth_mean,400)
ccfvalues

Full_df <- rbind(WL_01_test,WL_02_test,WL_03_test,WL_04_test,WL_05_test,
                 WL_06_test,WL_07_test,WL_08_test,WL_09_test,WL_10_test,
                 WL_11_test,WL_12_test)
#model
#water temperature yeah man.
m1 <- lmer(WL_diff ~ scale(precip_accumulation) + scale(vpd_ave) + scale(WaterTemp_mean) + (1|Station), data=Full_df)
summary(m1)
m2 <- lm(Vol_diff ~ scale(precip_accumulation) + scale(vpd_ave) + scale(WaterTemp_mean) + (1|Station), data=Full_df)
summary(m2)

m1 <- lm(WL_diff ~ scale(precip_accumulation), data=WL_04_test)
summary(m1)
m2 <- lm(vol_diff ~ scale(precip_accumulation), data=WL_04_test)
summary(m2)
m3 <- lm(vol_diff ~ scale(precip_accumulation) + scale(vpd_ave) + scale(WaterTemp_mean), data=WL_04_test)
summary(m3)

ggplot(Full_df%>%filter(Station!="WL_Wetland05"),aes(x=precip_accumulation + vpd_ave + WaterTemp_mean, y=WL_diff, color=Station)) +
  geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(Full_df%>%filter(Station!="WL_Wetland05"),aes(x=log(precip_accumulation), y=WL_diff, color=Station)) +
  geom_point() + geom_smooth(method='lm', formula= y~x)

ggplot(WL_03_test%>%filter(Station!="WL_Wetland05"),aes(x=precip_accumulation + vpd_ave + WaterTemp_mean, y=WL_diff, color=Station)) +
  geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(WL_03_test%>%filter(Station!="WL_Wetland05"),aes(x=precip_accumulation + vpd_ave + WaterTemp_mean, y=WL_diff, color=Station)) +
  geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(WL_06_test%>%filter(Station!="WL_Wetland05"),aes(x=log(precip_accumulation)+log(vpd_ave), y=WL_diff, color=Station)) +
  geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(WL_06_test%>%filter(Station!="WL_Wetland05"),aes(x=log(precip_accumulation)+log(vpd_ave), y=WL_diff, color=Station)) +
  geom_point() + geom_smooth(method='lm', formula= y~x)

ggplot(WL_07_test%>%filter(Date>as.Date("2022-06-01")),aes(x=Date,y=WL_diff,color=WL_diff)) + geom_point()
ggplot(df_wetland %>%filter(Station=="WL_Wetland07"),aes(x=DateTime,y=depth_ave_m)) + geom_point()

#seperate by season???

#max = -53

