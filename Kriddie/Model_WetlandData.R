# mixed model for wetland data 

#excellant tutorial, includes dragons
# https://ourcodingclub.github.io/tutorials/mixed-models/

###define function####

#use this to standardize regression coefficients in mixed effects model post-hoc

    # https://rdrr.io/github/gmonette/spida15/man/vif.lme.html
 
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

###to test for variance inflation

#  https://rdrr.io/github/gmonette/spida15/man/vif.lme.html

#########


library(here)
library(dplyr)
library(lme4)
library(ggplot2)
library(jtools)
library(lmerTest)
library(lubridate)

#notes from stats consultant:

#need a thery befor applying transformations
#test for homodesticity
    #https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/
#normality of residuals: use shapiro wilk test
  #https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html

#builing a model
  #put everything in one model 
  #step-wise is old-fashioned but people still do it
  #first think theoretically - and report all the effects, whether or not they are significant
    # don't add more than 20 predictors

#model interpretation!
  #Estimate is my co-efficient, if it is positive or negative is direction of relationship
    #In raw metric is can be easy to interpret – ie for each degree temperature  x ppm increase
    #	If you scale both, then beta cooefficient is comparable to one another
        #	(rules of thumb,, something below point .3 is small, 
   
#####################
#### study design####
#####################
# 12 wetlands, sampled 3 times within a 2 month period
  # wetlands were sampled on different days throughout the period - not exactly evenly spread throughout the period
  # sampled and CO2 concentration, CO2 degassing, gas transfer velocity (k)
  # other factors: weather data - precipitation, winddirection, wind velocity, water temp, air temp

##Mixed model with repeated measures
  #CO2 degassing is the response variable
  #Other variables are predictors (fixed effects)
  #Wetland site is random effect

####predictions for co2
  #precipitation
  #landscape position (watershed size) - GIS
  #Volumn:perimeter ratio
#### prediction for flux
  #CO2
  #k (see below)
#### prediction of k
  #temperature of air, water
  #wind speed and direction (depending on position on landscape, aspect) - GIS
  #wetland size - GIS

##use log variables when you expect diminishing returns

 #transform regressors for normality and scale
  #adjust for scale can help with interpretation in some ways, but not necessarily. It depends on how you want to interpret the data
  # scaling won't change your results, but it may help you to interpret the data. If you have 2 predictor you can compare the 2 when you scale

#the assumption isn't on normality - the assumption is on the residculas
  #assumption is the *error* is  normaly distributed

#develop theory first, then make the model

#what other assumptions should I test for? Other transformations?

    #post-hoc tests: 
          #check for variance inflation vif()
              #among predictors, if there are high correlations between predictors, this can skew the effects
        #homoskedasticisty: check that error variance is normal and constant across the predictor variable
              #extract residuals - see that they are random

#should I be testing for cross effects, or does that not apply in this case?
#how do I interpret the model summary (both parts)?

###first we will prepare the data -- test for homodaskcity of residuals and also random
  #transform data if necessary

#well maybe we should build the model first and then run the pos-hoc tests. I'm still a little confused.

##Log these
    #CO2_umo.L
    #CH4_umol.L
    #Flux

#do not log these (not skewed, fails shapiro-wilka)
    #k_m.d
    #k600
    #Watertemp_c
    #AirTemp_c

#not sure about these
    #AirPress_kpa
    #Water_minus_air_Temp
    #DOC_mg.L
    #TDN_mg.L
    #Watershed_m2
    #Elevation_m
    #depth_ave_m
    #surface_area_m2
    #Volumn_m3

#read in data
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-04.csv"))
df$X <- NULL

df$Date <- as.Date(df$Date)
df$Time <- as.POSIXct(df$Time_Baro, format = "%H:%M", tz = "UTC")


#selecting a model:
  #Focus on your question, don’t just plug in and drop variables from a model haphazardly until you make something “significant”.
  #Always choose variables based on biology/ecology: I might use model selection to check a couple of non-focal parameters, but I keep the “core” of the model untouched in most cases. 
  #Define your goals and questions and focus on that. 
  #Also, don’t just put all possible variables in (i.e. don’t overfit). 
  #Remember that as a rule of thumb, you need 10 times more data than parameters you are trying to estimate.

#Predictors of CO2
  #water temp (ave day, ave year)
  #AirTemp
  #AirPressure
  #DOC/TDN
  #Watershed (total & watershed-sa) -- don't use this 
  #Elevation
  #Surface area (instant, year ave)
  #Volumne
  #SA:Volumne
  #Solar radiation
  #precipitation
  

ggplot(df , aes(x=AirTemp_c, y=AirPress_kpa)) + 
  geom_point(aes(color=Wetland))





M1_test <- lmer(log(Flux_umol_m2_s) ~ log(CO2_umol.L) + 
                (1 |Wetland), data =df )
M1_test <- lm(log(Flux_umol_m2_s) ~ log(CO2_umol.L) , data =df )
summary(M1_test)
stdCoef.merMod(M1_test)

lmMod <- lm(Flux_umol_m2_s ~ CO2_umol.L, data=df) # initial model
lmMod <- lm(log(Flux_umol_m2_s) ~ log(CO2_umol.L), data=df) # initial model

par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lmMod)

#flux
df$waterTemp_c_day
M1_test <- lmer(log(Flux_umol_m2_s) ~ BaroTemp_c_yearly+ waterTemp_c_day +# AirTemp_c +
                  Water_minus_air_Temp +
                (1 |Wetland), data =df )
summary(M1_test)
M1_test <- lmer(log(CO2_umol.L) ~ BaroTemp_c_yearly + AirTemp_c +
                  (1 |Wetland), data =df )
summary(M1_test)
M2 <- lmer(k_m.d ~ BaroTemp_c_yearly + AirTemp_c +
                  (1 |Wetland), data =df )
summary(M2)

df$Water_minus_air_Temp

##drivers of CO2
  #CO2 increases with Air temperature (increased respiration)
  #CO2 increases with Water temperature (increased respiration)

  #precipitation effects CO2 concentration because it dilutes the water
  #CO2 increases with depth vol ratio (more contact with sediments)
  #CO2 decreases with surface area
  #CO2 increases with watershed size 

#Barometric data run each wetland individually to see if that is also significant
M1_test <- lmer(log(CO2_umol.L) ~ scale(Watershed_m2)+
         #         scale(Elevation_m) +
         #        scale(BaroTemp_c_yearly)+ 
                 scale(waterTemp_c_day) + 
        #        scale(AirTemp_c) +
        #         scale(Water_minus_air_Temp) +
                  (1 |Wetland), data =df )
summary(M1_test)


#####lets try modeling k

#gas transfer. K600 k_m.d
M1_test <- lmer(K600 ~ scale(BaroTemp_c_yearly)+ 
                  scale(waterTemp_c_day) + 
                  scale(AirTemp_c) +
                  scale(Water_minus_air_Temp) +
                  (1 |Wetland), data =df )
summary(M1_test)


###
#DOC_mg.L
M1_test <- lm(DOC_mg.L ~ BaroTemp_c_yearly + WS_size_minusSA_yearly, data =df )
summary(M1_test)

#TDN_mg.L
M1_test <- lm(TDN_mg.L ~ BaroTemp_c_yearly + WS_size_minusSA_yearly, data =df )
summary(M1_test)
