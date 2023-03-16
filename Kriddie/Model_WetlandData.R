# mixed model for wetland data 

library(here)
library(dplyr)
library(lme4)
library(ggplot2)
library(jtools)
library(lmerTest)

#repeated measures



df <- read.csv(here::here("Wetlands/Wetland_df_2023-03-15.csv"))
df$X <- NULL

df$CO2_ppm_scale <- scale(log(df$CO2_ppm),center=TRUE,scale=TRUE)
df$precpt_scale <- scale(log(df$PrecipAccuDay_mm+1),center=TRUE,scale=TRUE)
df$winddirr_scale <- scale(df$winddirecion,center=TRUE,scale=TRUE)
df$WindVelocity_scale <- scale(df$Wind_velocity_day,center=TRUE,scale=TRUE)
df$airwaterTemp_diff_scale <- scale(df$airwaterTemp_diff,center=TRUE,scale=TRUE)
df$AirTemp_scale <- scale(df$AirTemp_c,center=TRUE,scale=TRUE)
df$Watertemp_scale <- scale(df$Watertemp_c,center=TRUE,scale=TRUE)


hist(df$AirTemp_scale)
hist(df$airwaterTemp_diff_scale)
hist(df$WindVelocity_scale)
hist(df$winddirr_scale)
hist(df$precpt_scale)
hist(df$CO2_ppm_scale)



#predictor vari


#predictions: precipitation effects CO2 concentration
ggplot(df,aes(x=precpt_scale,y=CO2_ppm_scale, color = Wetland)) + geom_point() #+

  
M1A <- lm(CO2_ppm ~ PrecipAccuDay_mm, data =df)
summary(M1A)
M1B <- lmer(CO2_ppm ~ precpt_scale + (1 |Wetland),data=df)
summary(M1B)
summ(M1B)
ranova(M1B)

# I guess not : (

#CO2 concentrations effect flux

M2 <- lmer(Flux_umol_m2_s ~ CO2_ppm_scale + (1|Wetland),data=df)
summary(M2)
summ(M2)
ranova(M2)

#yes!


##wind direction and velocity effect k

M3A <- lmer(K600 ~ winddirr_scale + WindVelocity_scale +(1|Wetland),data=df)
M3A <- lmer(k_m.d ~ winddirr_scale  +(1|Wetland),data=df)

summary(M3A)
summ(M3A)
ranova(M3A)

#air-wind effects flux/k not sure wich

df$AirTemp_scale
M4 <- lmer(k_m.d ~ airwaterTemp_diff_scale + (1|Wetland),data=df)
M4 <- lmer(k_m.d ~ AirTemp_scale + (1|Wetland),data=df)

summary(M4)
summ(M4)
ranova(M4)

#put them all together
M5 <- lmer(Flux_umol_m2_s ~ CO2_ppm_scale + winddirr_scale + airwaterTemp_diff_scale + (1|Wetland),data=df)
summary(M5)
summ(M5)
ranova(M5)

###Visulaze




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

