
library(here)
library(dplyr)
library(ggplot2)
library(lubridate)

library(lme4)
library(lmerTest)
library(jtools)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)
library("Hmisc")
library(car)
library(GGally)
library(MCMCglmm)
library(effects)

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2024-04-14.csv"))

#without wetland 12
#(Intercept), scale(waterTemp_c_yearly), scale(log(surface_area_m2)), scale(log(SA_to_Vol_ratio)), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(Elevation_m)

M_CH4_BIC <- lmer(log(CH4_umol.L) ~ 
                scale(Elevation_m)+ 
                scale(waterTemp_c_yearly) +
                scale(log(SA_to_Vol_ratio)) + 
                scale(log(surface_area_m2)) +
                scale(log(WS_area_minus_pond)) +
                scale(percent_DTW) +
                (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_CH4_BIC)
modelPerformance(M_CH4_BIC)

lm_CH4 <- lm(log(CH4_umol.L) ~ 
                scale(Elevation_m)+ 
                scale(waterTemp_c_yearly) +
                scale(log(SA_to_Vol_ratio)) + 
#                scale(log(surface_area_m2)) +
                scale(log(WS_area_minus_pond)) +
                scale(percent_DTW),
             data =df%>%filter(Site!="Wetland_12"))
summary(lm_CH4)

ggplot(df,aes(x=Elevation_m,y=log(CH4_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=waterTemp_c_yearly,y=log(CH4_umol.L),color=Site),size=3) + geom_point(size=3)
ggplot(df,aes(x=log(SA_to_Vol_ratio),y=log(CH4_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=log(surface_area_m2),y=log(CH4_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=log(WS_area_minus_pond),y=log(CH4_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=percent_DTW,y=log(CH4_umol.L),color=Site),size=3) + geom_point(size=3)

#######
##CO2##
######

#without wetland 12


#(Intercept), scale(Watertemp_c), scale(waterTemp_c_yearly), scale(Elevation_m), scale(log(surface_area_m2)), scale(log(WS_area_minus_pond)), scale(percent_DTW)

M_CO2_BIC <- lmer(log(CO2_umol.L) ~ 
                  scale(Watertemp_c) + 
                  scale(waterTemp_c_yearly) +
                  scale(Elevation_m) +
                  scale(log(surface_area_m2)) +
                  scale(log(WS_area_minus_pond)) +
                  scale(percent_DTW) +
                  (1 |Site), data =df_1,REML = FALSE)
summary(M_BIC_2)
modelPerformance(M_BIC_2)


lm_CO2 <- lm(log(CO2_umol.L) ~ 
             scale(Watertemp_c) + 
             scale(waterTemp_c_yearly) +
             scale(Elevation_m) +
             scale(log(surface_area_m2)) +
             scale(log(WS_area_minus_pond)) +
             scale(percent_DTW),
           data =df_1)
summary(lm_CO2)

ggplot(df,aes(x=BaroTemp_c_yearly,y=log(CO2_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=precip_mm_ave2,y=log(CO2_umol.L),color=Site)) + geom_point(size=3)

ggplot(df,aes(x=log(surface_area_m2),y=log(CO2_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=log(SA_to_Vol_ratio),y=log(CO2_umol.L),color=Site)) + geom_point(size=3)



ggplot(df,aes(x=log(DOC_mg.L),y=log(CO2_umol.L),color=Site)) + geom_point(size=3)


###########
###ratio###
###########


M_AIC <- lmer(log(ratio) ~ 
                scale(BaroTemp_c_yearly)+ 
                #  scale(waterTemp_c_yearly) +
                scale(waterTemp_c_day) + 
                scale(SA_to_Vol_ratio) + 
                # scale(surface_area_m2) +
                #  scale(Watershed_m2) +
                scale(precip_mm_ave2) + 
                #  scale(solarrad_Wm2_daymean) +
                (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_AIC)
modelPerformance(M_AIC)

#without wetland 12
# scale(BaroTemp_c_yearly), scale(waterTemp_c_day), scale(SA_to_Vol_ratio)

M_BIC <- lmer(log(ratio) ~ 
                scale(BaroTemp_c_yearly)+ 
                #  scale(waterTemp_c_yearly) +
                #   scale(Water_minus_air_Temp) +
                scale(waterTemp_c_day) + 
                scale(SA_to_Vol_ratio) + 
                #  scale(surface_area_m2) +
                #  scale(Watershed_m2) +
                #   scale(precip_mm_ave2) + 
                #  scale(solarrad_Wm2_daymean) +
                (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_BIC)
modelPerformance(M_BIC)

ggplot(df,aes(x=log(BaroTemp_c_yearly),y=log(ratio),color=Site)) + geom_point()
ggplot(df,aes(x=log(waterTemp_c_day),y=log(ratio),color=Site)) + geom_point()
ggplot(df,aes(x=log(SA_to_Vol_ratio),y=log(ratio),color=Site)) + geom_point()
ggplot(df,aes(x=log(precip_mm_ave2),y=log(ratio),color=Site)) + geom_point()

