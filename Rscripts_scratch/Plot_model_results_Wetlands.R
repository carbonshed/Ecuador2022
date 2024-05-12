
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
#surface are and elevation coorelate. so what are results with
#1. surface NOT elevation
#AIC: (Intercept), scale(Watertemp_c), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(percent_DTW), scale(log(surface_area_m2))
#BIC: (Intercept), scale(log(SA_to_Vol_ratio)), scale(log(surface_area_m2))

#2. elevation NOT surface area
#AIC: (Intercept), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(precip_mm_ave2), scale(Elevation_m)
#BIC: (Intercept), scale(log(SA_to_Vol_ratio)), scale(Elevation_m)

#3. surface are AND Elevation
#AIC: (Intercept), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(precip_mm_ave2), scale(Elevation_m), scale(log(surface_area_m2))
#BIC: (Intercept), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(Elevation_m), scale(log(surface_area_m2))

###################
#AIC: (Intercept), scale(Watertemp_c), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(percent_DTW), scale(log(surface_area_m2))
M_CH4_AIC_1 <- lmer(log(CH4_umol.L) ~ 
                      scale(Watertemp_c) +
                      scale(waterTemp_c_yearly) +
                      scale(log(SA_to_Vol_ratio)) + 
                      scale(percent_DTW) +
                      scale(log(surface_area_m2)) +
                      (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_CH4_AIC_1)
modelPerformance(M_CH4_AIC_1)

#AIC: (Intercept), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(precip_mm_ave2), scale(Elevation_m)
M_CH4_AIC_2 <- lmer(log(CH4_umol.L) ~ 
                      scale(waterTemp_c_yearly) +
                      scale(log(SA_to_Vol_ratio)) + 
                      scale(log(WS_area_minus_pond)) +
                      scale(percent_DTW) +
                      scale(precip_mm_ave2) +
                      scale(log(Elevation_m)) +
                      (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_CH4_AIC_2)
modelPerformance(M_CH4_AIC_2)

#AIC: (Intercept), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(precip_mm_ave2), scale(Elevation_m), scale(log(surface_area_m2))
M_CH4_AIC_3 <- lmer(log(CH4_umol.L) ~ 
                      scale(waterTemp_c_yearly) +
                      scale(log(SA_to_Vol_ratio)) + 
                      scale(log(WS_area_minus_pond)) +
                      scale(percent_DTW) +
                      scale(precip_mm_ave2) +
                    scale(log(Elevation_m)) +
                      scale(log(surface_area_m2)) +
                      (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_CH4_AIC_3)
modelPerformance(M_CH4_AIC_3)

#BIC: (Intercept), scale(log(SA_to_Vol_ratio)), scale(log(surface_area_m2))
M_CH4_BIC_1 <- lmer(log(CH4_umol.L) ~ 
                scale(log(SA_to_Vol_ratio)) + 
                scale(log(surface_area_m2)) +
                (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_CH4_BIC_1)
modelPerformance(M_CH4_BIC_1)

#BIC: (Intercept), scale(log(SA_to_Vol_ratio)), scale(Elevation_m)
M_CH4_BIC_2 <- lmer(log(CH4_umol.L) ~ 
                      scale(log(SA_to_Vol_ratio)) + 
                      scale(Elevation_m)+ 
                      (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_CH4_BIC_2)
modelPerformance(M_CH4_BIC_2)

#BIC: (Intercept), scale(waterTemp_c_yearly), scale(log(SA_to_Vol_ratio)), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(Elevation_m), scale(log(surface_area_m2))
M_CH4_BIC_3 <- lmer(log(CH4_umol.L) ~ 
                      scale(waterTemp_c_yearly) +
                      scale(log(SA_to_Vol_ratio)) +
                      scale(log(WS_area_minus_pond)) +
                      scale(percent_DTW) +
                      scale(log(surface_area_m2)) +
                      scale(Elevation_m)+ 
                      (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_CH4_BIC_3)
modelPerformance(M_CH4_BIC_3)


lm_CH4 <- lm(log(CH4_umol.L) ~ 
               scale(waterTemp_c_yearly) +
               scale(log(SA_to_Vol_ratio)) +
               scale(log(WS_area_minus_pond)) +
               scale(percent_DTW) +
               scale(log(surface_area_m2)) +
               scale(Elevation_m),
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


#surface are and elevation coorelate. so what are results with
#1. surface NOT elevation
      #AIC: (Intercept), scale(Watertemp_c), scale(log(surface_area_m2))
      #BIC: (Intercept), scale(log(surface_area_m2))

#2. elevation NOT surface area
      #AIC: (Intercept), scale(Watertemp_c), scale(waterTemp_c_yearly), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(Elevation_m)
      #BIC: same

#3. surface are AND Elevation
      #AIC (Intercept), scale(Watertemp_c), scale(waterTemp_c_yearly), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(Elevation_m), scale(log(surface_area_m2))
       #BIC: same

#AIC: (Intercept), scale(Watertemp_c), scale(log(surface_area_m2))
M_CO2_AIC_1 <- lmer(log(CO2_umol.L) ~ 
                      scale(Watertemp_c) + 
                      scale(log(surface_area_m2)) +
                    (1 |Site), data =df_1,REML = FALSE)
summary(M_CO2_AIC_1)
modelPerformance(M_CO2_AIC_1)

#AIC: (Intercept), scale(Watertemp_c), scale(waterTemp_c_yearly), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(Elevation_m)
M_CO2_AIC_2 <- lmer(log(CO2_umol.L) ~ 
                      scale(Watertemp_c) + 
                      scale(waterTemp_c_yearly) +
                      scale(log(WS_area_minus_pond)) +
                      scale(percent_DTW) +
                      scale(Elevation_m) +
                      (1 |Site), data =df_1,REML = FALSE)
summary(M_CO2_AIC_2)
modelPerformance(M_CO2_AIC_2)

#AIC (Intercept), scale(Watertemp_c), scale(waterTemp_c_yearly), scale(log(WS_area_minus_pond)), scale(percent_DTW), scale(Elevation_m), scale(log(surface_area_m2))
M_CO2_AIC_3 <- lmer(log(CO2_umol.L) ~ 
                      scale(Watertemp_c) + 
                      scale(waterTemp_c_yearly) +
                      scale(log(WS_area_minus_pond)) +
                      scale(percent_DTW) +
                      scale(Elevation_m) +
                      scale(log(surface_area_m2)) +
                      (1 |Site), data =df_1,REML = FALSE)
summary(M_CO2_AIC_3)
modelPerformance(M_CO2_AIC_3)

#BIC: (Intercept), scale(log(surface_area_m2))
M_CO2_BIC_1 <- lmer(log(CO2_umol.L) ~ 
                  scale(log(surface_area_m2)) +
                  (1 |Site), data =df_1,REML = FALSE)
summary(M_CO2_BIC_1)
modelPerformance(M_CO2_BIC_1)

#BIC with elevation is same as AIC


##### PUT IT IN THE EQUATION

#go to code - "RScripts_scratch/Model_WetlandData/ExtrapolateYearlyFlux_Wetlands"


#question - what if we minimized elevation differences - would temperature pop out?
m1 <- lmer(log(CH4_umol.L) ~ 
           #  scale(Watertemp_c) + 
           #  scale(waterTemp_c_yearly) +
          #   scale(log(WS_area_minus_pond)) +
          #   scale(percent_DTW) +
          #   scale(Elevation_m) +
           #  scale(log(surface_area_m2)) +
       (1 |Site), data =df_1%>%filter(Site=="Wetland04"|Site=="Wetland05"|Site=="Wetland06"|Site=="Wetland07"|Site=="Wetland11"),REML = FALSE)
summary(m1)
modelPerformance(m1)

m1 <- lm(log(CH4_umol.L) ~ 
            #   scale(Watertemp_c) + 
            #    scale(waterTemp_c_yearly) +
            #    scale(log(WS_area_minus_pond)) +
             scale(percent_DTW) #+
            #    scale(Elevation_m) #+
             #scale(log(surface_area_m2)) 
             , data =df_1%>%filter(Site=="Wetland04"|Site=="Wetland05"|Site=="Wetland06"|Site=="Wetland07"|Site=="Wetland11"))
summary(m1)
modelPerformance(m1)

m1 <- lmer(log(CO2_umol.L) ~ 
             #  scale(Watertemp_c) + 
             #   scale(waterTemp_c_yearly) +
             #   scale(log(WS_area_minus_pond)) +
             scale(percent_DTW) +
             #   scale(Elevation_m) +
             #scale(log(surface_area_m2)) +
             (1 |Site), data =df_1%>%filter(Site=="Wetland04"|Site=="Wetland05"|Site=="Wetland06"|Site=="Wetland07"|Site=="Wetland11"),REML = FALSE)
summary(m1)
modelPerformance(m1)

m1 <- lm(log(CO2_umol.L) ~ 
           #   scale(Watertemp_c) + 
           #    scale(waterTemp_c_yearly) +
           #    scale(log(WS_area_minus_pond)) +
           scale(percent_DTW) #+
         #    scale(Elevation_m) #+
         #scale(log(surface_area_m2)) 
         , data =df%>%filter(Site=="Wetland04"|Site=="Wetland05"|Site=="Wetland06"|Site=="Wetland07"|Site=="Wetland11"))
summary(m1)
modelPerformance(m1)

