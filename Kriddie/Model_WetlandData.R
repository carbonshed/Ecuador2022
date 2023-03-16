# mixed model for wetland data 

library(here)
library(dplyr)
library(lme4)
library(ggplot2)
library(jtools)
library(lmerTest)

#####################
#### study design####
#####################
# 12 wetlands, sampled 3 times within a 2 month and a half period
  # wetlands were sampled on different days throughout the period - not exactly evenly spread throughout the period
  # sampled and CO2 concentration, CO2 degassing, gas transfer velocity (k)
  # other factors: weather data - precipitation, winddirection, wind velocity, water temp, air temp

##Mixed model with repeated measures
  #CO2 degassing is the response variable
  #Other variables are predictors (fixed effects)
  #Wetland site is random variable

#ead in data
df <- read.csv(here::here("Wetlands/Wetland_df_2023-03-15.csv"))
df$X <- NULL


#transform regressors for normality and scale
hist(df$CO2_ppm)
hist(log(df$CO2_ppm))

df$CO2_ppm_scale <- scale(log(df$CO2_ppm),center=TRUE,scale=TRUE)
df$precpt_scale <- scale(log(df$PrecipAccuDay_mm+1),center=TRUE,scale=TRUE)
df$winddirr_scale <- scale(df$winddirecion,center=TRUE,scale=TRUE)
df$WindVelocity_scale <- scale(df$Wind_velocity_day,center=TRUE,scale=TRUE)
df$airwaterTemp_diff_scale <- scale(df$airwaterTemp_diff,center=TRUE,scale=TRUE)
df$AirTemp_scale <- scale(df$AirTemp_c,center=TRUE,scale=TRUE)
df$Watertemp_scale <- scale(df$Watertemp_c,center=TRUE,scale=TRUE)

hist(df$CO2_ppm_scale)
hist(df$AirTemp_scale)
hist(df$airwaterTemp_diff_scale)
hist(df$WindVelocity_scale)
hist(df$winddirr_scale)
hist(df$precpt_scale)

#what other assumptions should I test for? Other transformations?

#in these tests- should I also transform the response variable?
#should I be testing for cross effects, or does that not apply in this case?
#how do I interpret the model summary (both parts)?

#predictions: precipitation effects CO2 concentration
ggplot(df,aes(x=precpt_scale,y=CO2_ppm, color = Wetland)) + geom_point() #+

  
M1A <- lm(CO2_ppm ~ PrecipAccuDay_mm, data =df)
summary(M1A)
M1B <- lmer(CO2_ppm ~ precpt_scale + (1 |Wetland),data=df)
summary(M1B)
summ(M1B)
ranova(M1B)

#CO2 concentrations effect flux

ggplot(df,aes(x=CO2_ppm_scale,y=Flux_umol_m2_s, color = Wetland)) + geom_point() +
  scale_colour_manual(values = rainbow(12))
ggplot(df,aes(x=CO2_ppm_scale,y=log(Flux_umol_m2_s), color = Wetland)) + geom_point() +
  scale_colour_manual(values = rainbow(12))

M2A <- lmer(Flux_umol_m2_s ~ CO2_ppm_scale + (1|Wetland),data=df)
M2B <- lmer(log(Flux_umol_m2_s) ~ CO2_ppm_scale + (1|Wetland),data=df)
  #WHY THIS ERROR?? Am I supposed to get rid of of the random effect here?
summary(M2A)
summary(M2B)

#yes!

##wind direction and velocity effect k
# how to interpret these multiple regressors?
#how do I decide how complicated my model should be?
#does it matter what order these re in?

M3A <- lmer(k_m.d ~ winddirr_scale + WindVelocity_scale + Watertemp_scale + AirTemp_scale  + (1|Wetland),data=df)
M3A <- lmer(k_m.d ~ winddirr_scale  +(1|Wetland),data=df)

summary(M3A)


#air-wind effects flux/k not sure wich

df$AirTemp_scale
M4 <- lmer(k_m.d ~ airwaterTemp_diff_scale + (1|Wetland),data=df)
M4 <- lmer(k_m.d ~ AirTemp_scale + (1|Wetland),data=df)

summary(M4)

#put them all together
M5 <- lmer(log(Flux_umol_m2_s) ~ CO2_ppm_scale + winddirr_scale + airwaterTemp_diff_scale + (1|Wetland),data=df)

summary(M5)

###Visualize
## good way to visualize mixed methods model?


######STOP HERE FOR STATS SESSSION#######





#df <- read.csv(here::here("Kriddie/wetlands_df.csv"))
df <- read.csv(here::here("Wetlands/Wetland_df_2023-03-15.csv"))

student_data <-
  student_data %>%
  mutate(mathkind_scaled = scale(mathkind))



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

