# mixed model for wetland data 
#just working on CO2 for this script because it's getting confusing

#excellant tutorial, includes dragons
# https://ourcodingclub.github.io/tutorials/mixed-models/

##mulitvariate model building
# https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html

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
library(ggplot2)
library(lubridate)

library(lme4)
library(lmerTest)
library(jtools)
library(extraoperators)
library(JWileymisc)
library(multilevelTools)


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
   
##Mixed model with repeated measures
  #CO2 degassing is the response variable
  #Other variables are predictors (fixed effects)
  #Wetland site is random effect

##What's up with + (1|Wetland) ?:
  #with this notation we assume an intercept that’s different for each subject” … and “1” stands for the intercept here. 
  #You can think of this formula as telling your model that it should expect that there’s going to be multiple responses per subject, 
  #and these responses will depend on each subject’s baseline level. 
  #This effectively resolves the non-independence that stems from having multiple responses by the same subject.

##use log variables when you expect diminishing returns

 #transform regressors for normality and scale
  #adjust for scale can help with interpretation in some ways, but not necessarily. It depends on how you want to interpret the data
  # scaling won't change your results, but it may help you to interpret the data. If you have 2 predictor you can compare the 2 when you scale

#the assumption isn't on normality - the assumption is on the residculas
  #assumption is the *error* is  normaly distributed

    #post-hoc tests: 
          #check for variance inflation vif()
              #among predictors, if there are high correlations between predictors, this can skew the effects
        #homoskedasticisty: check that error variance is normal and constant across the predictor variable
              #extract residuals - see that they are random

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

####predictions for co2
#precipitation
#landscape position (watershed size) - GIS
#Volumn:perimeter ratio
#Predictors of CO2
  #water temp (ave day, ave year)
  #AirTemp (ave day, ave year)
  #AirPressure and Elevation (why?)
  #Watershed (total & watershed-sa) -- don't use this 
  #Surface area (instant, year ave)
  #SA:Volumne
  #Solar radiation
  #precipitation
  #DOC/TDN
  
##drivers of gas transfer velocity
  #K increases with Air temperature (lower solubility) - (point, ave day, ave year)
  #K increases with Water temperature (lower solubility) - (ave day, ave year)
  #K is influenced by air-water diff (evaporative cooling or something)
  #K increases with surface area size (more fetch)
  #K increased with wind (more turbulance)
  #K increases with precipitation (more turbulance)
  #Solar radiation?

#Big model
#gas transfer. K600 k_m.d
M1 <- lmer(k_m.d ~ scale(BaroTemp_c_yearly)+ 
                  scale(waterTemp_c_day) + 
                  scale(AirTemp_c) +
                  scale(Water_minus_air_Temp) +
                   scale(windspeed_m_s) +
                   scale(surface_area_m2) +
                   scale(PrecipAccuDay_mm) + 
                  (1 |Wetland), data =df )
summary(M1)
qqnorm(resid(M1))
qqline(resid(M1)) 

md <- modelDiagnostics(M1, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)



#paired down 1
M2 <- lmer(k_m.d ~ #scale(BaroTemp_c_yearly)+ 
             scale(waterTemp_c_day) + 
             scale(AirTemp_c) +
             scale(Water_minus_air_Temp) +
             # scale(windspeed_m_s) +
             # scale(surface_area_m2) +
             # scale(PrecipAccuDay_mm) + 
             (1 |Wetland), data =df )

summary(M2)
md <- modelDiagnostics(M2, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)
modelPerformance(M2)

#k600
M_K600_1 <- lmer(K600 ~ scale(BaroTemp_c_yearly)+ 
             scale(waterTemp_c_day) + 
             scale(AirTemp_c) +
             scale(Water_minus_air_Temp) +
              scale(windspeed_m_s) +
              scale(surface_area_m2) +
              scale(PrecipAccuDay_mm) + 
              scale(solarrad_Wm2_daymean) +
             (1 |Wetland), data =df )

summary(M_K600_1)
md <- modelDiagnostics(M_K600_1, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)
modelPerformance(M_K600_1)

##paired down
M_K600_2 <- lmer(K600 ~ #scale(BaroTemp_c_yearly)+ 
                   scale(waterTemp_c_day) + 
                   scale(AirTemp_c) +
                   scale(Water_minus_air_Temp) +
               #    scale(windspeed_m_s) +
               #    scale(surface_area_m2) +
               #    scale(PrecipAccuDay_mm) + 
              #   scale(solarrad_Wm2_daymean) +
                   (1 |Wetland), data =df )

summary(M_K600_2)
md <- modelDiagnostics(M_K600_2, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)
modelPerformance(M_K600_2)



#Model Diagnostices for linear mixed effects / multilevel models
#the residuals should follow a normal distribution, 
#the random effects should follow a multivariate normal distribution, 
#and the residual variance should be homogenous (the same) as a single residual variance is estimated and used for the whole model. 
#The modelDiagnostics() function in multilevelTools helps to evaluate these assumptions graphically.

#The first plot shows (top left) the distribution of residuals.
#The second plot (top right) shows the fitted (predicted) values for each observation against the residuals. 
  #The colour of the blocks indicates how many observations fall at a particular point. 
  #The solid blue line is a loess smooth line. Hopefully this line is about flat and stays consistently at residuals of 0 regardless of the predicted value, indicating no systematic bias. 
  #Finally the dashed blue lines indicate the 10th and 90th percentile (estimated from a quantile regression model) of the residuals across predicted values. 
  #If the residual variance is homogenous across the spread of predicted values, we would expect these dashed lines to be flat and parallel to each other.
#The last three plots show the univariate distribution of the intercept and stress slope by UserID, 
  #the random intercept and slope and a test of whether the random intercept and slope are multivariate normal The multivariate normality test, based on the mahalonbis distances, suggests that there are a few, relatively extreme people. 
  #We might consider dropping these individuals from the analysis to examine whether results are sensitive to these extreme cases.
