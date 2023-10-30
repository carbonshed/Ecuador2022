# mixed model for wetland data 

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

#read in data
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-10-28.csv"))
df$X <- NULL

df$Date <- as.Date(df$Date)
df$Time <- as.POSIXct(df$Time_Baro, format = "%H:%M", tz = "UTC")


#transform regressors for normality and scale
  #adjust for scale can help with interpretation in some ways, but not necessarily. It depends on how you want to interpret the data

#the assumption isn't on normality - the assumption is on the residculas
  #assumption is the *error* is  normaly distributed

# Should we scale everything? I need to talk to stats people about this. Confused.


#example:
#df$CO2_ppm_scale <- scale(log(df$CO2_ppm),center=TRUE,scale=TRUE)

#develop theory first, then make the model

#what other assumptions should I test for? Other transformations?

    #post-hoc tests: 
          #check for variance inflation vif()
              #among predictors, if there are high corrilations between predictors, this can skew the effects
        #homoskedasticisty: check that error variance is normal and constant across the preditor variable
              #extract residuals - see that they are random

#in these tests- should I also transform the response variable?
#should I be testing for cross effects, or does that not apply in this case?
#how do I interpret the model summary (both parts)?

#drivers of flux - I predict will be CO2 and temperature (anything else?)
  #water, air, and water-air temp differance is not significant. But of the three air/temp diff is the most signifcant, water temp is very not significant
M1_1 <- lmer(Flux_umol_m2_s ~ scale(CO2_ppm) + (1 |Wetland),data=df%>%filter(Date<"2022-09-01") )
summary(M1_1)
M1_2 <- lmer(Flux_umol_m2_s ~ scale(CO2_ppm) + scale(airwaterTemp_diff) + (1 |Wetland),data=df%>%filter(Date<"2022-09-01") )
summary(M1_2)


#drivers of k
M1_3 <- lmer(K600 ~ scale(AirTemp_c-Watertemp_c) +  (1 |Wetland),data=df%>%filter(Date<"2022-09-01") )
summary(M1_3)
M1_4 <- lmer(K600 ~ Watertemp_c +  (1 |Wetland),data=df%>%filter(Date<"2022-09-01" ))
summary(M1_4)
M1_4 <- lmer(log(K600) ~ log(AirTemp_c) +  (1 |Wetland),data=df%>%filter(Date<"2022-09-01" ))
summary(M1_4)
M1_4 <- lmer(log(K600) ~ log(AirPress_kpa) +  (1 |Wetland),data=df%>%filter(Date<"2022-09-01" ))
summary(M1_4)

M1_5 <- lmer(log(K600) ~ log(surface_area_m2) +  (1 |Wetland),data=df%>%filter(Date<"2022-09-01" ))
summary(M1_5)
stdCoef.merMod(M1_5)
M1_6 <- lmer(k_m.d ~ windspeed_m_s +  (1 |Wetland),data=df%>%filter(Date<"2022-09-01" ) )
summary(M1_6)
M1_7 <- lmer(k_m.d ~ PrecipAccuDay_mm +  (1 |Wetland),data=df%>%filter(Date<"2022-09-01" ) )
summary(M1_7)


M1_8 <- lmer(k_m.d ~ scale(airwaterTemp_diff) + scale(PrecipAccuDay_mm) + scale(windspeed_m_s)+scale(surface_area_m2)+  (1 |Wetland),data=df )
summary(M1_8)
stdCoef.merMod(M1_8)
M1_10 <- lmer(k_m.d ~ scale(airwaterTemp_diff) + scale(AirTemp_c) + scale(Watertemp_c) + (1 |Wetland),data=df )
summary(M1_10)
M1_9 <- lmer(K600 ~ scale(airwaterTemp_diff) + scale(surface_area_m2) +(1 |Wetland),data=df )
summary(M1_9)


####Controls on CO2
#predictions: 
#CO2 increases with temperature 
#precipitation effects CO2 concentration because it dilutes the water (check previous day and week ave)
#CO2 increases with depth vol ratio (check that and depth and surface area)

ggplot(df,aes(x=PrecipAccuDay_mm,y=log(CO2_ppm), color = Wetland)) + geom_point(size=3) #+
df$Precip_mm_ave7
M1_11 <- lmer(log(CO2_ppm) ~ log(Precip_mm_ave7) + (1 |Wetland), data =df )
summary(M1_11)
M1_12 <- lmer(log(CO2_ppm) ~ log(waterTemp_c_yearly) + (1 |Wetland), data =df)
M1_12 <- lmer(log(CO2_ppm) ~ log(waterTemp_c_summer) + (1 |Wetland), data =df%>%filter(Date<"2022-09-01"))
M1_12 <- lm(log(CO2_ppm) ~ log(waterTemp_c_fall), data =df%>%filter(Date>"2022-09-01"))
summary(M1_12)

M1_13 <- lmer(log(CO2_ppm) ~ log(surface_area_yearly)+ (1 |Wetland), data =df )
M1_13 <- lmer(log(CO2_ppm) ~log(surface_area_summer)+ (1 |Wetland), data =df%>%filter(Date<"2022-09-01") )
M1_13 <- lm(log(CO2_ppm) ~ log(surface_area_fall), data =df%>%filter(Date>"2022-09-01"))
summary(M1_13)

M1_13.1 <- lmer(log(CO2_ppm) ~ log(SA_to_Vol_ratio)+ (1 |Wetland), data =df )
M1_13.1 <- lmer(log(CO2_ppm) ~ log(SA_to_Vol_ratio_yearly)+ (1 |Wetland), data =df )
M1_13.1 <- lmer(log(CO2_ppm) ~log(SA_to_Vol_ratio_summer)+ (1 |Wetland), data =df%>%filter(Date<"2022-09-01") )
M1_13.1 <- lm(log(CO2_ppm) ~ log(SA_to_Vol_ratio_fall), data =df%>%filter(Date>"2022-09-01"))
summary(M1_13.1)

M1_13.2 <- lmer(log(CO2_ppm) ~ log(Volumn_m3)+ (1 |Wetland), data =df )
M1_13.2 <- lmer(log(CO2_ppm) ~ log(Volumn_m3_yearly)+ (1 |Wetland), data =df )
M1_13.2 <- lmer(log(CO2_ppm) ~log(Volumn_m3_summer)+ (1 |Wetland), data =df%>%filter(Date<"2022-09-01") )
M1_13.2 <- lm(log(CO2_ppm) ~ log(Volumn_m3_fall), data =df%>%filter(Date>"2022-09-01"))
summary(M1_13.2)



M1_14 <- lmer(log(CO2_ppm) ~ log(waterTemp_c_summer)+ (1 |Wetland), data =df )
summary(M1_14)

M1_15 <- lmer(log(Flux_umol_m2_s) ~ log(AirPress_kpa) + (1 |Wetland),data=df#%>%filter(Date<"2022-09-01") 
              )
summary(M1_15)

M1_15 <- lmer(log(CO2_ppm) ~ log(waterTemp_c_summer) +log(AirPress_kpa)+ (1 |Wetland),data=df%>%filter(Date<"2022-09-01") )
summary(M1_15)


##same but for CH4
ggplot(df,aes(x=PrecipAccuDay_mm,y=log(CH4_umol.L), color = Wetland)) + geom_point(size=3) #+
df$Precip_mm_ave7
M1_11 <- lmer(CH4_umol.L ~ PrecipAccuDay_mm + (1 |Wetland), data =df )
summary(M1_11)

M1_12 <- lmer(CH4_umol.L ~ waterTemp_c_yearly + (1 |Wetland), data =df)
M1_12 <- lmer(CH4_umol.L ~ waterTemp_c_summer + (1 |Wetland), data =df%>%filter(Date<"2022-09-01"))
summary(M1_12)

M1_13 <- lmer(CH4_umol.L ~ SA_to_Vol_ratio_summer+ (1 |Wetland), data =df )
M1_13 <- lmer(CH4_umol.L ~ scale(SA_to_Vol_ratio_yearly)+ (1 |Wetland), data =df%>%filter(Date<"2022-09-01") )
summary(M1_13)

M1_14 <- lmer(log(CH4_umol.L) ~ log(surface_area_m2)+ (1 |Wetland), data =df )
summary(M1_14)

#relationship between water and air temp -- averages perhapes
M1_15 <- lm(AirTemp_c ~ Watertemp_c, data =df )
summary(M1_15)

# interpret model
    #for random effects - if you have more than one random variable, you could for example say how much each grouping exmplain variance


#yes!

M2B <- lmer(log(Flux_umol_m2_s + 1) ~ CO2_ppm_scale + AirTemp_scale + WindVelocity_scale + (1|Wetland),data=df)
summary(M2B)

#first you have to scale response variable (use function at top)
stdCoef.merMod(M2B)

#To approximate amount that the variable explains in the outcome
  #square the stdcoef -- persons r -> r^2

##more complex model
  #when you have standardized coefficient
  #eye-ball it
  #take square of predictor --> get to approximate value of how much that predictor explains variance in outcome variable


###Visualize
## good way to visualize mixed methods model?
  #not really: maybe 2+ plots, one for each predictor; 3-d plots

#####look at variance in k across sites
boxplot(log(CO2_ppm)  ~ Wetland, data = df)  # certainly looks like something is going on here
wetland.lm <- lm(log(CO2_ppm) ~ Wetland, data = df)
wetland.av <- aov(wetland.lm)
summary(wetland.av)

tukey.test <- TukeyHSD(wetland.av)
tukey.test


### Assumptions?

## Plot the residuals - the red line should be close to being flat, like the dashed grey line
plot(basic.lm, which = 1)  
## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line
plot(basic.lm, which = 2)  
## Have a look at the data to see if above is true
boxplot(Watertemp_c  ~ Wetland, data = df)  # certainly looks like something is going on here

##----- Modify the model -----###

## We want to use all the data, but account for the data coming from different mountain ranges

## let's add mountain range as a fixed effect to our basic.lm

wetland.lm <- lm(log(Flux_umol_m2_s) ~ log(CO2_ppm) + Wetland, data = df)
summary(wetland.lm)

mixed.lmer <- lmer(log(Flux_umol_m2_s) ~ log(CO2_ppm) + (1|Wetland), data = df)
summary(mixed.lmer)

#calculate % of variance from wetland
0.4345/(0.4345+0)

#test assumptions
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

#Once we account for the mountain ranges, it’s obvious that dragon body length doesn’t actually explain the differences in the test scores. How is it obvious? I hear you say?
#Take a look at the summary output: notice how the model estimate is smaller than its associated error? That means that the effect, or slope, cannot be distinguised from zero.

mixed.lmer <- lm(k_m.d ~ winddirecion + (1|Wetland) , data = df )
summary(mixed.lmer)

### is k600 differnt by wetland?

res.aov2 <- aov(K600 ~ Wetland, data = df)
summary(res.aov2)
mean(df$K600)
