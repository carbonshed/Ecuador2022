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
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-08-01.csv"))
df$X <- NULL

df$Date <- as.Date(df$Date)
df$Time <- as.POSIXct(df$Time_Baro, format = "%H:%M", tz = "UTC")


#transform regressors for normality and scale
  #adjust for scale can help with interpretation in some ways, but not necessarily. It depends on how you want to interpret the data

#the assumption isn't on normality - the assumption is on the residculas
  #assumption is the *error* is  normaly distributed

hist(df$CO2_ppm)
hist(log(df$CO2_ppm))

df$CO2_ppm_scale <- scale(log(df$CO2_ppm),center=TRUE,scale=TRUE)

df$precpt_scale <- scale(log(df$PrecipAccuDay_mm+1),center=TRUE,scale=TRUE)

hist(df$PrecipAccuDay_mm)
hist(log(df$PrecipAccuDay_mm))
hist(df$winddirecion)

plot(log(df$PrecipAccuDay_mm),df$Flux_umol_m2_s)

df$winddirr_scale <- scale(df$winddirecion,center=TRUE,scale=TRUE)
df$WindVelocity_scale <- scale(df$winddirecion,center=TRUE,scale=TRUE)
#df$airwaterTemp_diff_scale <- scale(df$airwaterTemp_diff,center=TRUE,scale=TRUE)
df$AirTemp_scale <- scale(df$AirTemp_c,center=TRUE,scale=TRUE)
df$Watertemp_scale <- scale(df$Watertemp_c,center=TRUE,scale=TRUE)

hist(df$CO2_ppm_scale)
hist(df$AirTemp_scale)
hist(df$airwaterTemp_diff_scale)
hist(df$WindVelocity_scale)
hist(df$winddirr_scale)
hist(df$precpt_scale)

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

#predictions: precipitation effects CO2 concentration
ggplot(df,aes(x=PrecipAccuDay_mm,y=log(CO2_ppm), color = Wetland)) + geom_point(size=3) #+

  
M1A <- lm(CO2_ppm ~ PrecipAccuDay_mm, data =df)
summary(M1A)
M1B <- lmer(CO2_ppm ~ precpt_scale + (1 |Wetland),data=df)
summary(M1B)
summ(M1B)
ranova(M1B)

#CO2 concentrations effect flux

ggplot(df,aes(x=CO2_ppm_scale,y=Flux_umol_m2_s, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))
ggplot(df,aes(x=CO2_ppm_scale,y=log(Flux_umol_m2_s), color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))

#M2A <- lmer(Flux_umol_m2_s ~ CO2_ppm_scale + (1|Wetland),data=df)
M2B <- lmer(log(Flux_umol_m2_s + 1) ~ CO2_ppm_scale + (1|Wetland),data=df)

M2B <- lmer(log(Flux_umol_m2_s + 1) ~ log(CO2_ppm) + (1|Wetland),data=df)
summary(M2B)
  #WHY THIS ERROR?? Am I supposed to get rid of of the random effect here?
        #because loging negatives
#summary(M2A)
summary(M2B)
stdCoef.merMod(M2B)

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


##wind direction and velocity effect k
# how to interpret these multiple regressors?
#how do I decide how complicated my model should be?
#does it matter what order these re in?

#bar plots
ggplot(data=df, aes(x=Wetland, y=k_m.d, fill=Date)) +
  geom_bar(stat="identity", position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=df, aes(x=Date, y=k_m.d, fill=Wetland)) +
  geom_bar(stat="identity", position=position_dodge())+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#scatter plot

ggplot(df,aes(x=AirTemp_c,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12)) #+ geom_smooth(method="lm)")
ggplot(df,aes(x=airwaterTemp_diff,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))
ggplot(df,aes(x=Watertemp_c,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))

ggplot(df,
       aes(x=winddirecion,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))

ggplot(df%>%filter(Wetland!="wetland_1"|Wetland!="wetland_2"|Wetland!="wetland_3"),
       aes(x=winddirecion,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))

ggplot(df%>%filter(Wetland!="wetland_1"|Wetland!="wetland_2"|Wetland!="wetland_3"),
       aes(x=winddirecion,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))

ggplot(df%>%filter(Wetland=="wetland_1"|Wetland=="wetland_2"|Wetland=="wetland_3"),
       aes(x=windspeed_m_s,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))

ggplot(df%>%filter(Wetland=="wetland_11"),
       aes(x=winddirecion,y=k_m.d, color = Wetland)) + geom_point(size=3) +
  scale_colour_manual(values = rainbow(12))+
  geom_smooth(method = "lm")

#wind speed
#positive: 01, 04, 05, 08, 10, 11, 12
#negative: 02, 03, 06, 07, 09

M3A <- lmer(k_m.d ~  winddirecion + (1|Wetland),data=df%>%
              filter(Wetland=="wetland_10"|Wetland=="wetland_9"|Wetland=="wetland_08"))

M3A <- lmer(k_m.d ~  windspeed_m_s + (1|Wetland),data=df%>%
              filter(Wetland=="wetland_1"|Wetland=="wetland_4"|Wetland=="wetland_5"|
                       Wetland=="wetland_8"|Wetland=="wetland_10"|Wetland=="wetland_11"|
                       Wetland=="wetland_12"))

M3A <- lmer(k_m.d ~ AirTemp_scale  +(1|Wetland), data=df)
M3A <- lmer(k_m.d ~ windspeed_m_s  + (1|Wetland), data=df)
L3 <- lm(k_m.d ~ winddirr_scale,data=df)
M3A <- lmer(k_m.d ~ airwaterTemp_diff_scale  +(1|Wetland),data=df)
M3A <- lmer(k_m.d ~ Watertemp_c  +(1|Wetland),data=df)
M3A <- lmer(k_m.d ~ Watertemp_c + AirTemp_scale +(1|Wetland),data=df)

summary(L3)
summary(M3A)
stdCoef.merMod(M3A)

#air-wind effects k

M4 <- lmer(k_m.d ~ airwaterTemp_diff_scale + (1|Wetland),data=df)
M4 <- lmer(K600 ~ airwaterTemp_diff_scale + (1|Wetland),data=df)
M4 <- lmer(k_m.d ~ AirTemp_scale + (1|Wetland),data=df)
M4 <- lmer(K600 ~ AirTemp_scale + (1|Wetland),data=df)
M4 <- lmer(k_m.d ~ Watertemp_scale + (1|Wetland),data=df)
M4 <- lmer(K600 ~ Watertemp_scale + (1|Wetland),data=df)
M4 <- lmer(k_m.d ~ Watertemp_scale + AirTemp_scale  + airwaterTemp_diff_scale +(1|Wetland),data=df)
M4 <- lmer(k_m.d ~ Watertemp_scale + AirTemp_scale  + airwaterTemp_diff_scale +(1|Wetland),data=df)

 
summary(M4)
stdCoef.merMod(M4)

### 

M5 <- lmer(Flux_umol_m2_s ~  CO2_ppm_scale + airwaterTemp_diff_scale + (1|Wetland),data=df)

summary(M5)
stdCoef.merMod(M5) 

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

###

basic.lm <- lm(log(Flux_umol_m2_s) ~ log(CO2_ppm), data = df)
basic.lm <- lm(k_m.d ~ winddirecion, data = df)
basic.lm <- lm(Flux_umol_m2_s ~ airwaterTemp_diff, data = df )

summary(basic.lm)

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

