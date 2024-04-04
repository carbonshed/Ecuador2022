
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

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2024-03-27.csv"))

#scale(BaroTemp_c_day), scale(waterTemp_c_yearly), scale(log(surface_area_m2)), scale(log(SA_to_Vol_ratio))
M_AIC <- lmer(log(CH4_umol.L) ~ 
                scale(BaroTemp_c_day) +
#                scale(BaroTemp_c_yearly)+ 
                  scale(waterTemp_c_yearly) +
              #  scale(waterTemp_c_day) + 
                scale(log(SA_to_Vol_ratio)) + 
                scale(log(surface_area_m2)) +
                #  scale(Watershed_m2) +
              #  scale(precip_mm_ave2) + 
                #  scale(solarrad_Wm2_daymean) +
                (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_AIC)
modelPerformance(M_AIC)

#without wetland 12
#(Intercept), scale(BaroTemp_c_day), scale(log(surface_area_m2)), scale(log(SA_to_Vol_ratio))

M_BIC <- lmer(log(CH4_umol.L) ~ 
                # scale(BaroTemp_c_yearly)
                 scale(BaroTemp_c_day)+ 
              #    scale(waterTemp_c_yearly) +
                #   scale(Water_minus_air_Temp) +
                # scale(waterTemp_c_day) + 
                scale(log(SA_to_Vol_ratio)) + 
                  scale(log(surface_area_m2)) +
                #  scale(Watershed_m2) +
                #   scale(precip_mm_ave2) + 
                #  scale(solarrad_Wm2_daymean) +
                (1 |Site), data =df %>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_BIC)
modelPerformance(M_BIC)

df$CH4_sat
ggplot(df,aes(x=BaroTemp_c_yearly,y=log(CH4_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=BaroTemp_c_day,y=log(CH4_umol.L),color=Site),size=3) + geom_point(size=3)

ggplot(df,aes(x=waterTemp_c_yearly,y=log(CH4_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=log(SA_to_Vol_ratio),y=log(CH4_umol.L),color=Site)) + geom_point(size=3)
ggplot(df,aes(x=log(surface_area_m2),y=log(CH4_umol.L),color=Site)) + geom_point(size=3)

#######
##CO2##
######

#without wetland 12


#AIC: (Intercept), scale(BaroTemp_c_day), scale(log(surface_area_m2)), scale(precip_mm_ave2), scale(log(SA_to_Vol_ratio))

M_AIC_2 <- lmer(log(CO2_umol.L) ~ 
                  scale(BaroTemp_c_day)+ 
                  scale(log(surface_area_m2)) +
                  scale(precip_mm_ave2) +
                  scale(log(SA_to_Vol_ratio)) +
                  (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_AIC_2)
modelPerformance(M_AIC_2)

#BIC: (Intercept), scale(BaroTemp_c_day), scale(precip_mm_ave2)
M_BIC_2 <- lmer(log(CO2_umol.L) ~ 
                  scale(BaroTemp_c_day)+ 
                  scale(precip_mm_ave2) +
                  (1 |Site), data =df%>%filter(Site!="Wetland_12"),REML = FALSE)
summary(M_BIC_2)
modelPerformance(M_BIC_2)

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

