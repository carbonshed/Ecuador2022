#saturation

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
library(MuMIn)

####

#adjust henry to temp: KH = KH(STP) x exp(D(1/T-1/T(STP)))
#use constants in  	Burkholder et al. (2019) and convert to desired units
#  k°H (mol/(kg*bar) = mol/l/atm
#d(ln(kH))/d(1/T) (K)
kH_STP_mol.l.atm = .035*1/0.986923
D_K = 2400 
T_STP_K = 298.15

df_test <- data.frame(pco2=c(500,500,500,500),pressure_atm=c(1,2,1,2),Watertemp_c=c(6,6,12,12))
df_test$pCO2_air_ppm <- 418.53 # 2022 average manoa
#########

df_test$pCO2_air_atm <- df_test$pCO2_air_ppm/10^6*df_test$pressure_atm

df_test$pCO2_w_atm <- df_test$pco2 / 10^6 

#henry's constant adjust for temp
df_test$KH_mol.l.atm <- kH_STP_mol.l.atm * exp(D_K*(1/(df_test$Watertemp_c+273.15) - 1/T_STP_K))
df_test$KH_mol.m3.atm <- df_test$KH_mol.l.atm * 1000

df_test$CO2_sat_mol.L <- df_test$KH_mol.l.atm*df_test$pCO2_air_atm

df_test$CO2_mol.L <- df_test$KH_mol.l.atm*df_test$pCO2_w_atm
df_test$CO2_umol.L <- df_test$CO2_mol.L*10^6

df_test$CO2_sat_precent <- df_test$CO2_mol.L/df_test$CO2_sat_mol.L*100




######
df <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))

#perecent diff
(max(df$Watertemp_c)-min(df$Watertemp_c))/(max(df$Watertemp_c)+min(df$Watertemp_c))/2 * 100

(max(df$AirPress_kpa)-min(df$AirPress_kpa))/(max(df$AirPress_kpa)+min(df$AirPress_kpa))/2 * 100


ggplot(df ,aes(x=Watertemp_c,y=CO2_sat_mol.L)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=AirPress_kpa,y=CO2_sat_mol.L)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=Elevation_m,y=CO2_sat_mol.L)) + geom_point() + scale_y_continuous(transform = "log")

ggplot(df ,aes(x=Watertemp_c,y=CH4_sat_umol.L)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=AirPress_kpa,y=CH4_sat_umol.L)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=Elevation_m,y=CH4_sat_umol.L)) + geom_point() + scale_y_continuous(transform = "log")

ggplot(df ,aes(x=Watertemp_c,y=CO2_sat_precent)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=AirPress_kpa,y=CO2_sat_precent)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=Elevation_m,y=CO2_sat_precent)) + geom_point() + scale_y_continuous(transform = "log")

ggplot(df ,aes(x=Watertemp_c,y=CH4_pSat)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=AirPress_kpa,y=CH4_pSat)) + geom_point() + scale_y_continuous(transform = "log")


p1 <- ggplot(df ,aes(x=Elevation_m,y=CH4_pSat)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("% saturation CH4") +
  theme_bw(base_size = 18)
p2 <-ggplot(df ,aes(x=Elevation_m,y=CO2_sat_precent)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("% saturation CO2") +
  theme_bw(base_size = 18)

p3 <- ggplot(df ,aes(x=Elevation_m,y=pCH4_ppm)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("pCH4") +
  theme_bw(base_size = 18)
p4 <-ggplot(df ,aes(x=Elevation_m,y=pCO2_ppm)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("pCO2") +
  theme_bw(base_size = 18)

full_plot <-plot_grid(p1,p3,p2,p4,ncol=2, nrow=2)


p1 <- ggplot(df ,aes(x=AirPress_kpa,y=CH4_pSat)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("% saturation CH4") +
  theme_bw(base_size = 18)
p2 <-ggplot(df ,aes(x=AirPress_kpa,y=CO2_sat_precent)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("% saturation CO2") +
  theme_bw(base_size = 18)
p3 <- ggplot(df ,aes(x=AirPress_kpa,y=pCH4_ppm)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("pCH4") +
  theme_bw(base_size = 18)
p4 <-ggplot(df ,aes(x=AirPress_kpa,y=pCO2_ppm)) + geom_point() + scale_y_continuous(transform = "log") +
  ylab("pCO2") +
  theme_bw(base_size = 18)

full_plot <-plot_grid(p1,p3,p2,p4,ncol=2, nrow=2)

ggplot(df ,aes(x=Watertemp_c,y=AirPress_kpa)) + geom_point() 

ggplot(df ,aes(x=Elevation_m,y=AirPress_kpa)) + geom_point() 
ggplot(df ,aes(x=Elevation_m,y=KH_mol.m3.atm)) + geom_point() 
ggplot(df ,aes(x=Elevation_m,y=AirTemp_c)) + geom_point() + scale_y_continuous(transform = "log")
ggplot(df ,aes(x=Elevation_m,y=Watertemp_c)) + geom_point() + scale_y_continuous(transform = "log")


M1 <- lmer(log(pCH4_ppm) ~ 
              scale(AirPress_kpa) +
             (1 |Site), data =df)

M1 <- lmer(log(pCH4_ppm) ~ 
             scale(Elevation_m) +
             (1 |Site), data =df)

M1 <- lmer(log(CH4_pSat) ~ 
             scale(AirPress_kpa) +
             (1 |Site), data =df)

M1 <- lmer(log(CH4_pSat) ~ 
             scale(Elevation_m) +
             (1 |Site), data =df)

summary(M1)
modelPerformance(M1)


M2 <- lmer(log(pCO2_ppm) ~ 
             scale(AirPress_kpa) +
             (1 |Site), data =df)

M2 <- lmer(log(pCO2_ppm) ~ 
             scale(Elevation_m) +
             (1 |Site), data =df)

M2 <- lmer(log(CO2_sat_precent) ~ 
             scale(AirPress_kpa) +
             (1 |Site), data =df)

M2 <- lmer(log(CO2_sat_precent) ~ 
             scale(Elevation_m) +
             (1 |Site), data =df)

summary(M2)
modelPerformance(M2)


M1 <- lmer(log(pCH4_ppm) ~ 
             scale(Watertemp_c) + scale(AirPress_kpa) +
             (1 |Site), data =df)

summary(M1)
modelPerformance(M1)

ggplot(df,aes(x=Elevation_m,y=CH4_sat)) + geom_point() + scale_y_continuous(transform = "log")

