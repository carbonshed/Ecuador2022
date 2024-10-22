#this is an example of analysis I have been running
library(here)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(JWileymisc)

df <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))
df$New.Name <- df$Site
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")


GHGsummary_df<-df %>%group_by(New.Name,Site,Elevation_m,percent_DTW,WS_area_minus_pond,waterTemp_c_yearly)%>%
  summarise(mean_ch4 = mean(pCH4_ppm),
            mean_co2 = mean(pCO2_ppm),
            mean_watertemp = mean(Watertemp_c),
            mean_airtemp = mean(AirTemp_c),
            mean_SA = mean(surface_area_m2),
            mean_vol = mean(Volumn_m3),
            mean_vol_to_SA = mean(SA_to_Vol_ratio),
            mean_depth = mean(depth_ave_m))


#examples of models that confuse me. 

#In the mixed model, elevation is a significant predictor
#in the multilinear, water temperature is significant

#why?


m3 <- lmer(log(pCH4_ppm) ~ 
             #   scale(AirTemp_c) +
             #   scale(log(surface_area_m2)) +
             #   scale(log(Volumn_m3)) +
             #   scale(SA_to_Vol_ratio) +
             #   scale(depth_ave_m) +
             #  scale(percent_DTW) +
             #   scale(WS_area_minus_pond)+
             scale(Watertemp_c) +
             scale(Elevation_m)+
             (1 |Site), data =df)

summary(m3)
modelPerformance(m3)


m4 <- lm(log(mean_ch4) ~ 
           #scale(mean_airtemp) +
           #scale(log(mean_SA)) +
           #scale(log(mean_vol)) +
           #scale(mean_vol_to_SA) + 
           #scale(mean_depth) +
           #scale(percent_DTW)+
           #scale(WS_area_minus_pond)+
           scale(Elevation_m) +
           scale(mean_watertemp),
         data =GHGsummary_df)

summary(m4)

######################################################
