# mixed model for wetland data 
#just working on CO2 for this script because it's getting confusing

#excellant tutorial, includes dragons
# https://ourcodingclub.github.io/tutorials/mixed-models/

##mulitvariate model building
# https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html

##article:https://peerj.com/articles/4794/
# https://peerj.com/articles/4794/

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
library("Hmisc")


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

#selecting a model:
  #Focus on your question, don’t just plug in and drop variables from a model haphazardly until you make something “significant”.
  #Always choose variables based on biology/ecology: I might use model selection to check a couple of non-focal parameters, but I keep the “core” of the model untouched in most cases. 
  #Define your goals and questions and focus on that. 
  #Also, don’t just put all possible variables in (i.e. don’t overfit). 
  #Remember that as a rule of thumb, you need 10 times more data than parameters you are trying to estimate.


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
#df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-04.csv"))[c(17,2,6,14:16,18:45,52:59)]
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-10.csv"))[c(17,2,6,14:16,18:45,52:59)]
df$num <- 1:44
################################
###Predicted drivers of CO2###
################################
#CO2 increases with Air temperature (increased respiration) - (ave day, ave year)
      #AirTemp_c
      #BaroTemp_c_yearly
#CO2 increases with Water temperature (increased respiration) - (ave day, ave year)
      #WaterTemp_c
      #WaterTemp_c_day
      #waterTemp_c_yearly
#CO2 increases with depth vol ratio (more contact with sediments) 
      #SA_to_Vol_ratio_day --- not all wetlands
#CO2 decreases with surface area
      #Surface_area_m2
#CO2 increases with watershed size 
    #WS_size_minusSA --- not all wetlands
#precipitation effects CO2 concentration because it dilutes the water (ave of sum day and previous day)
      #Precio_mm_ave2
#Solar radiation because stimulates gpp (ave day)
      #Solarrad_Wm2_daymean


########################
##CHecking Assumptions
########################
#Step 1: Are there outliers in Y and X?

#boxplots galore
boxplot(log(df$CO2_umol.L)) ##transformed
hist(log(df$CO2_umol.L))
ggplot(df, aes(log(CO2_umol.L), num)) + geom_point()

boxplot(df$AirTemp_c)  # one?? - checked, it is as legit as the data collected can be
hist(df$AirTemp_c)
ggplot(df, aes(AirTemp_c, num)) + geom_point()

boxplot(df$BaroTemp_c_yearly) 
hist(df$BaroTemp_c_yearly)

boxplot(df$Watertemp_c)
hist(df$Watertemp_c)
ggplot(df, aes(Watertemp_c, num)) + geom_point()

boxplot(df$waterTemp_c_day) #one? - checked, it is legit
hist(df$waterTemp_c_day)
ggplot(df, aes(waterTemp_c_day, num)) + geom_point()

boxplot(df$waterTemp_c_yearly) #one?,- wetland 5 yes, legit
hist(df$waterTemp_c_yearly)
ggplot(df, aes(waterTemp_c_yearly, num)) + geom_point() #also this looks good

boxplot(df$SA_to_Vol_ratio) #transform? I don't know! The extreme outliers are legit
#boxplot(log(df$SA_to_Vol_ratio)) #lots?
hist(df$SA_to_Vol_ratio)
ggplot(df, aes(SA_to_Vol_ratio, num)) + geom_point()

boxplot(log(df$surface_area_m2)) #seems like log transform is the way to go here
hist(log(df$surface_area_m2))
ggplot(df, aes(log(surface_area_m2), num)) + geom_point()

boxplot(df$WS_size_minusSA) # this is wetland 12, wich, yes is very large watershed
hist(df$WS_size_minusSA)
ggplot(df, aes(WS_size_minusSA, num)) + geom_point()

boxplot(df$precip_mm_ave2) 
hist(df$precip_mm_ave2)
ggplot(df, aes(precip_mm_ave2, num)) + geom_point()

boxplot(df$solarrad_Wm2_daymean) 
hist(df$solarrad_Wm2_daymean)
ggplot(df, aes(solarrad_Wm2_daymean, num)) + geom_point()

###findings:


#correlation matrix

df_2 <- df[,c("AirTemp_c","BaroTemp_c_yearly","waterTemp_c_yearly"
              ,"waterTemp_c_day","Water_minus_air_Temp","SA_to_Vol_ratio",
              "surface_area_m2","Watershed_m2","precip_mm_ave2","Solar_Wm2_ave3"
              )
           ]
cor(df_2, use = "complete.obs")
res2 <- rcorr(as.matrix(df_2))
res2
correlation_pvalue <- res2$P
correlation_coefficiant <- res2$r


#dealing with colliniarity in fixed effects: (cutoff is > .7 (Dormann et al., 2013))

#AirTemp_c and Water_minus_air_Temp: -0.97042739
#Remove airtemp

df_3 <- df[,c("BaroTemp_c_yearly","waterTemp_c_yearly"
              ,"waterTemp_c_day","Water_minus_air_Temp","SA_to_Vol_ratio",
              "surface_area_m2","Watershed_m2","precip_mm_ave2","Solar_Wm2_ave3")]
cor(df_3, use = "complete.obs")
res2 <- rcorr(as.matrix(df_2))
res2
correlation_pvalue <- res2$P
correlation_coefficiant <- res2$r


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
  


#Global model
  #had to remove SA to vol ratio and watershed size because don't have it for all variables
M1 <- lmer(log(CO2_umol.L) ~ #scale(AirTemp_c) +
                  scale(BaroTemp_c_yearly)+ 
                  scale(waterTemp_c_yearly) +
                  scale(Water_minus_air_Temp) +
                  scale(waterTemp_c_day) + 
                #  scale(SA_to_Vol_ratio) + 
                  scale(surface_area_m2) +
                #  scale(Watershed_m2) +
                  scale(precip_mm_ave2) + 
                  scale(Solar_Wm2_ave3) +
                  (1 |Wetland), data =df)
summary(M1)


md <- modelDiagnostics(M1, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)

#paired down 1
M2 <- lmer(log(CO2_umol.L) ~ #scale(AirPress_kpa) +
            
           #  scale(Watertemp_c) +
             scale(BaroTemp_c_yearly)+ 
           #  scale(waterTemp_c_yearly) +
            #    scale(Water_minus_air_Temp) +
            #    scale(waterTemp_c_day) + 
              # scale(SA_to_Vol_ratio) + 
             #   scale(surface_area_m2) +
              #  scale(Watershed_m2) +
             #  scale(precip_mm_ave2) + 
            #   scale(Solar_Wm2_ave3) +
             (1 |Wetland), data =df )
summary(M2)
md <- modelDiagnostics(M2, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)
modelPerformance(M2)

#paired down 2
M3 <- lmer(log(CO2_umol.L) ~ #scale(AirPress_kpa) +
             #scale(AirTemp_c) +
             # scale(Watertemp_c) +
             #scale(BaroTemp_c_yearly)+ 
             #scale(waterTemp_c_yearly) +
             #   scale(Water_minus_air_Temp) +
             #   scale(waterTemp_c_day) + 
             #  scale(SA_to_Vol_ratio) + 
             #  scale(surface_area_m2) +
                scale(Watershed_m2) +
             #   scale(precip_mm_ave2) + 
             #   scale(solarrad_Wm2_daymean) +
             #  scale(Solar_Wm2_ave3) +
             (1 |Wetland), data =df )
summary(M3)
md <- modelDiagnostics(M2, ev.perc = .001)
plot(md, ask = FALSE, ncol = 2, nrow = 3)
modelPerformance(M2)


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

md <- modelDiagnostics(M1, ev.perc = .001)

plot(md, ask = FALSE, ncol = 2, nrow = 3)


modelPerformance(M2)
summary(M2)
mt2 <- modelTest(M2)
names(mt2)

modelPerformance(M1)
summary(M1)

#plot
ggplot(df , aes(x=AirTemp_c, y=AirPress_kpa)) + 
  geom_point(aes(color=Wetland))



###
#DOC_mg.L
M_DOC_1 <- lm(DOC_mg.L ~ scale(BaroTemp_c_yearly) + 
                scale(WS_size_minusSA_yearly) +
                scale(waterTemp_c_yearly) +
                scale(SA_to_Vol_ratio) + 
                scale(surface_area_m2) +
                scale(Watershed_m2) 
              , data =df )
summary(M_DOC_1)

#TDN_mg.L
M_TDN_1 <- lm(TDN_mg.L ~ scale(BaroTemp_c_yearly) + 
     scale(WS_size_minusSA_yearly) +
     scale(waterTemp_c_yearly) +
     scale(SA_to_Vol_ratio) + 
     scale(surface_area_m2) +
     scale(Watershed_m2) 
   , data =df )
summary(M_TDN_1)
