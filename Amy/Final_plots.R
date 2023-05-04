#r load packages
remove.packages("tidyverse")
install.packages("tidyverse")
library(tidyverse) # includes package ggplot that we will use to visualize data
library(lubridate) # helps with formating date and time
library(dplyr) # lots of useful functions in this package
library(here) # use this when reading in data, here sets our working directory to the folder containing our R project. This makes code sharing easier.

library(ggplot2)

wetland.data <- read.csv(file="wetlands_df.csv")         
colnames(wetland.data) <- c("Wetland", "Location","Date","Watertemp_c","Waterlevel","ppm_NOTcorrected","Flux_mean","Flux_stdev","Time_Baro","AirPress_kpa", "AirTemp_C", "AirPress_hpa", "CO2_ppm","AirPress_atm","VaporPressure_atm","TotalAir_atm", "Total_air_MolperL","CO2_air_MolesPerLiter","CO2_air_gCO2asCPerLiter","Watertemp_K","KH_mol.L.atm", "CO2_water_gCO2asCPerLiter","deltaCO2_gCO2asCperM3", "Flux_gCO2asCperM2perDay",	"k_m.d", "Sc", "K600", "Average_wind", "Solar_radiation", "Wetland_wind", "wetland_solar", "wetland_watertemp", "Wetlandairtemp", "wetland_flux", "Average_area", "average_precip", "average_Co2","AverageWL", "Wetlandpt2", "average_k")

view(wetland.data)

wetland.data$wetland_factor <- as.factor(wetland.data$Wetland)
wetland.data$wetland_factor <- as.factor(wetland.data$Wetlandpt2)

wetland.data$average_Co2 <- as.numeric(wetland.data$average_Co2)

wetland.data$Average_wind <- as.numeric(wetland.data$Average_wind)
wetland.data$Date <- as.Date(wetland.data$Date,"%m/%d/%Y")

wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)
wetland.data$Wetland_wind <- as.numeric(wetland.data$Wetland_wind)
wetland.data$wetland_watertemp <- as.numeric(wetland.data$wetland_watertemp)


wetland.data$wetland_flux <- as.numeric(wetland.data$wetland_flux)

wetland.data$AverageWL <- as.numeric(wetland.data$AverageWL)
wetland.data$AverageWL <- as.numeric(wetland.data$AverageWL)
wetland.data$average_k <- as.numeric(wetland.data$average_k)

view(wetland.data)




#NOTE these are the start of the average of everything for the wetlands *IMPORTANT




#Flux vs windspeed
ggplot(data = wetland.data) +
  geom_point(aes(x = Wetland_wind, y = wetland_flux, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") 

ggplot(data = wetland.data) +
  geom_point(aes(x = Wetland_wind, y = wetland_flux, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10()

ggplot(data = wetland.data) +
  geom_point(aes(x = Wetland_wind, y = wetland_flux, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10() + scale_y_log10()

ggplot(data = wetland.data) +
  geom_point(aes(x = Wetland_wind, y = wetland_flux, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_y_log10()



ggplot(data = wetland.data) +
  geom_point(aes(x = Wetland_wind, y = wetland_flux, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10() + scale_y_log10()

ggplot(data = wetland.data) +
  geom_point(aes(x = Wetland_wind, y = wetland_flux, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10()



ggplot(data = wetland.data) +
  geom_point(aes(x = Wetland_wind, y = Flux_mean, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10() + scale_y_log10(.01,.09) 


#UNDER THIS ISNT RIGHT
ggplot(data = wetland.data, aes(x = Wetland_wind, y = wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') +
  theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = " Wind speed (m/s)") +  scale_x_log10()

lm(formula = wetland_flux ~ Wetland_wind, data = wetland.data)


ggplot(data = wetland.data, aes(x = log(Wetland_wind), y = log(wetland_flux))) + geom_point() + labs(y = "Flux (μmol/m^2/s)", x = "Wind speed (m/s)")
       
       
ggplot(data = wetland.data, aes(x = log(Wetland_wind), y = log(wetland_flux))) +
  geom_point(col = 'red', size = 3) +
  geom_smooth(method = lm, se = FALSE, col = 'black', linetype = 'dashed') +
  theme_bw() +
  labs(y = "Flux (μmol/m^2/s)", x = "Wind speed (m/s)") +
  scale_x_log10()

#CORRECT
#fit the model
model <- lm(log(wetland_flux) ~ Wetland_wind, data = wetland.data)
summary(model)
#ln(wetland flux) = -6.682 + 4.665(wetland wind)
     
#OR wetland flux = −18.16355917 + 12.68078472^(wetland wind)
#non excel formula 



exponential.model <- lm(log(wetland.data$wetland_flux)~ wetland.data$Wetland_wind)  # Create the model

# Print the Summary
summary(exponential.model)

values <- seq(1, 5, .1)
Counts.exponential2 <- exp(predict(exponential.model,list(x=values)))

#plot(x, y,pch=16)
#lines(values, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")

x_exp <- seq(from = 1, to = 5, by = .1)
#log(y) = -3.01305 +  0.25915 * x

y_exp = exp(exponential.model$coefficients[1] +  exponential.model$coefficients[2] * x_exp) 

install.packages("plot_ly")
library(plotly)
plot_ly(x = wetland.data$Wetland_wind, y= wetland.data$wetland_flux)%>%
  add_markers(size=4)%>%
  #  add_lines(x = values, y=Counts.exponential2)%>%
  add_lines(x = x_exp, y=y_exp)%>%
  layout(
    xaxis = list(title = "Wind"),
    yaxis = list(title = "Flux")
  )


###NEW try
exponential.model <- lm(log(wetland_flux)~ Wetland_wind, data = wetland.data)
summary(exponential.model)


Wind_speedvalues <- seq (0, 4.25, 0.5)
wetland_flux.exponential2 <- exp(predict(exponential.model,list(Wetland_wind=Wind_speedvalues)))

plot(wetland.data$Wetland_wind, wetland.data$wetland_flux,pch=16, data = wetland.data)

lines(wetland.data$wetland_flux, wetland_wind.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")



#WIND FORMULA
#log base 10
#y = 0.0005*(e^(1.5754*x)
#R² = 0.6851





#LOOK HERE #Foreal look here
ggplot(data = wetland.data, aes(x = Wetland_wind, y = wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Wind speed (m/s)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ Wetland_wind, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-1.0599+(0.3856 *average wind)


fluxandwind <- read.csv("wetlands_df.csv") #Upload the data
 lmwetland_flux = lm(wetland_flux ~ Wetland_wind, data = wetland.data) #Create the linear regression
summary(lmwetland_flux) #Review the results

#Adjusted R-squared:  0.4883 &   p-value: 0.00688



plot(wetland_flux~Wetland_wind, data= wetland.data) 
log.model=lm(wetland_flux~log(Wetland_wind), data= wetland.data)
summary(log.model)
wetland.data %>%
  ggplot(aes(x = Wetland_wind, y = wetland_flux))+geom_point()+stat_smooth(method="lm", formula=y~log(x), se=FALSE) + xlab("Wind speed (m/s)") + ylab("Flux (μmol/m^2/s)") 


#ABOVE THIS IS THE CURVED LINE

hist(log(wetland.data$wetland_flux))
hist(log(wetland.data$Flux_mean))

install.packages("lme4")
library(lme4)
mixed.lmer <- lmer(log(Flux_mean) ~ wetland_watertemp*Wetland_wind + (1|wetland_factor), data = wetland.data)



ggplot(aes(x = Wetland_wind, y = wetland_flux))+geom_point()+stat_smooth(method="lm", formula=y~log(x), se=FALSE) + xlab("Wind speed (m/s)") + ylab("Flux (μmol/m^2/s)") 



  
#flux watertemp
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)")

ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_x_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_y_log10() + scale_x_log10()






#log base 10
#y = 5E-05*e^(1.123x)
#R² = 0.382

model <- lm(log(wetland_flux) ~ (wetland_watertemp), data = wetland.data)
summary(model)
hist(wetland.data$wetland_watertemp)

model <- lm((wetland_flux) ~ (wetland_watertemp), data = wetland.data)
summary(model)

ggplot(data = wetland.data, aes(x = wetland_watertemp, y = wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ wetland_watertemp, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-1.46+(0.2479 *average water temperature)

fluxandwind <- read.csv("wetlands_df.csv") #Upload the data
lmwetland_flux = lm(wetland_flux ~ wetland_watertemp, data = wetland.data) #Create the linear regression
summary(lmwetland_flux) #Review the results
#Adjusted R-squared:  0.1667
#p-value: 0.1039


plot((wetland.data$wetland_watertemp), log(wetland.data$wetland_flux)) 


#PEEP this 
plot(wetland_flux~wetland_watertemp, data= wetland.data) 
log.model=lm(wetland_flux~log(wetland_watertemp), data= wetland.data)
summary(log.model)
wetland.data %>%
ggplot(aes(x = wetland_watertemp, y = wetland_flux))+geom_point()+stat_smooth(method="lm", se=FALSE)


formula=y~log(x)#this only for the lod 
hist(wetland.data$wetland_watertemp)
hist(wetland.data$wetland_watertemp)

#histogram here
#LOOK HERE ABOVE


ggplot(aes(x = wetland_watertemp, y = wetland_flux))+ labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") +geom_point()+stat_smooth(method="lm", formula=y~log(x), se=FALSE)
ggplot(aes(x = wetland_watertemp, y = wetland_flux))+ labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + geom_point() + stat_smooth(method="lm", formula=y~log(x), se=FALSE)


#ggplot(aes(x = wetland_watertemp, y = wetland_flux))+geom_point(size = 3.5) + stat_smooth(method="lm", formula=y~log(x), se=FALSE)


#ggplot(aes(x = wetland_watertemp, y = wetland_flux)+ labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + geom_point(size = 3.5)+stat_smooth(method="lm", formula=y~log(x), se=FALSE)







ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_x_log10() 

model <- lm((wetland_flux) ~ log(wetland_watertemp), data = wetland.data)
summary(model)
x=seq(from=0,to=8,length.out=1000)
y=predict(model,newdata=list(x=seq(from=0,to=1.2,length.out=1000)), interval="confidence")

ggplot(data = wetland.data) + geom_point(aes(x = wetland.data$wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_x_log10() + matlines(wetland_watertemp, wetland_flux, lwd=2)




#flux co2
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)")
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") +  scale_x_log10()


ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()


ggplot(data = subset(wetland.data, Wetlandpt2 != 13), aes(x = average_Co2, y = wetland_flux)) +
  geom_point(aes(col = factor(Wetlandpt2)), size = 3) +
  geom_smooth(method = lm, se = FALSE, col = 'black', linetype = 'dashed') +
  theme_bw() +
  labs(y = "CO2_Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") +
  scale_y_log10() +
  scale_x_log10()

lm(formula = wetland_flux ~ average_Co2, data = wetland.data)


ggplot(data = subset(wetland.data, Wetlandpt2 != 13), aes(x = average_Co2, y = wetland_flux)) +
  geom_point(aes(col = factor(Wetlandpt2)), size = 3) +
  geom_smooth(method = lm, se = FALSE, col = 'black', linetype = 'dashed') +
  theme_bw() +
  labs(y = expression(paste("CO"[2], " Flux (μmol/m"^2*"/s)")), x = expression(paste("Average CO"[2], " (ppm)"))) +
  scale_y_log10() +
  scale_x_log10()





#LOOK HERE 
ggplot(data = wetland.data, aes(x = average_Co2, y = wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ average_Co2, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-0.0383787+(.0001172*average CO2)

fluxandwind <- read.csv("wetlands_df.csv") #Upload the data
lmwetland_flux = lm(wetland_flux ~ average_Co2, data = wetland.data) #Create the linear regression
summary(lmwetland_flux) #Review the results
#Adjusted R-squared:  0.9659 
#p-value: 7.157e-09























#ANOVA between airtemp and windspeed 
two.way <- aov(wetland_flux ~ Wetland_wind + Wetlandairtemp, data = wetland.data)
summary(two.way)

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way







#ANOVA
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
summary(wetland.data)

#Average_wind", "Solar_radiation", "Wetland_wind", "wetland_solar", "wetland_watertemp", "Wetlandairtemp", "wetland_flux", "Average_area", "average_precip", "average_Co2", "Wetlandpt2")

one.way <- lm(log(wetland_flux) ~ wetland_watertemp, data = wetland.data) 
summary(one.way)

one.way <- lm(log(wetland_flux) ~ Wetland_wind, data = wetland.data) 
summary(one.way)


one.way <- aov(wetland_flux ~ Wetland_wind, data = wetland.data)
summary(one.way)

two.way <- aov(wetland_flux ~ wetland_watertemp + Wetland_wind, data = wetland.data)

two.way <- lm(log(wetland_flux) ~ wetland_watertemp + Wetland_wind, data = wetland.data)



two.way <- lm(log(wetland_flux) ~ wetland_watertemp + Wetland_wind + CO2_ppm, data = wetland.data)
two.way <- lm(log(wetland_flux) ~ wetland_watertemp + Wetland_wind + (log(CO2_ppm)), data = wetland.data)

summary(two.way)
#look here 

interaction <- aov(wetland_flux ~ wetland_watertemp*Wetland_wind, data = wetland.data)

summary(interaction)



library(AICcmodavg)

model.set <- list(one.way, two.way, interaction)
model.names <- c("one.way", "two.way", "interaction")

aictab(model.set, modnames = model.names)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))



df$wetland_watertemp <- factor(df$wetland_watertemp)
df$Wetland_wind <- factor(df$Wetland_wind)

wetland_watertemp <- function(x)
  
  tukey.two.way<-TukeyHSD(two.way)
tukey.two.way


tukey.plot.aov<-aov(wetland_flux ~ wetland_watertemp:Wetland_wind, data=wetland.data)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)






#ANOVA part 2 
wetland.data <- read.csv(file="wetlands_df.csv")         
str(wetland.data)
summary(wetland.data)

my_model <- aov(wetland_flux ~ wetland_watertemp + Wetland_wind, data=wetland.data)
summary(my_model)
TukeyHSD(my_model)















#FINAL
#LOOK HERE 

#Wind
ggplot(data = wetland.data, aes(x = Wetland_wind, y = wetland_flux)) + geom_point(col= 'darkgrey', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Wind speed (m/s)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ Wetland_wind, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-1.0599+(0.3856 *average wind)



hist(log(wetland.data$wetland_flux))
hist(log(wetland.data$Flux_mean))



#watertemp
ggplot(data = wetland.data, aes(x = wetland_watertemp, y = wetland_flux)) + geom_point(col= 'blue', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature (C)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ wetland_watertemp, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-1.46+(0.2479*average water temp)



hist(log(wetland.data$Watertemp_c))
hist(log(wetland.data$wetland_watertemp))




ggplot(data = wetland.data, aes(x = Wetlandairtemp, y = wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature(C)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ Wetlandairtemp, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-1.46+(0.2479 *average water temperature)
hist(log(wetland.data$Wetlandairtemp))
hist(log(wetland.data$AirTemp_C))




ggplot(data = wetland.data, aes(x = average_Co2, y = wetland_flux)) + geom_point(col= 'purple', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ average_Co2, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-0.0383787+(.0001172*average CO2)



hist(log(wetland.data$wetland_flux))
hist(log(wetland.data$Flux_mean))


ggplot(data = wetland.data, aes(x = wet, y = wetland_flux)) + geom_point(col= 'purple', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ average_Co2, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-0.0383787+(.0001172*average CO2)


ggplot(data = wetland.data, aes(x = AverageWL, y = wetland_flux)) + geom_point(col= 'purple', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "water level", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()

lm(formula = AverageWL ~ average_Co2, data = wetland.data)

hist(log(wetland.data$Waterlevel))
hist(log(wetland.data$AverageWL))






plot(wetland_flux~wetland_watertemp, data= wetland.data) 
model=lm(wetland_flux~(wetland_watertemp), data= wetland.data)
summary(model)
wetland.data %>%
  ggplot(aes(x = wetland_watertemp, y = wetland_flux))+geom_point()+stat_smooth(method="lm", se=FALSE)


install.packages("lmerTest")
library(lmerTest)
library(lme4)

M2B <- lmer(log(Flux_mean + 1) ~ log(CO2_ppm) + (1|Wetland),data=wetland.data)
summary(M2B)

M2B <- lmer(log(Flux_mean + 1) ~ log(Watertemp_c) + (1|Wetland),data=wetland.data)
summary(M2B)

M4 <- lmer(k_m.d ~ AirTemp_C + (1|Wetland),data=wetland.data)
summary(M4)

M3 <- lmer(k_m.d ~ Average_wind + (1|Wetland),data=wetland.data)
summary(M3)



#how and flux and what predicts gas transfer velocity.
#and mixed moddel 



#FLUX and CO2ppm
M2B <- lmer(log(Flux_mean + 1) ~ log(CO2_ppm) + (1|Wetland),data=wetland.data)
summary(M2B)


new_data <- data.frame(CO2_ppm = wetland.data$CO2_ppm, Wetland = wetland.data$Wetland)
pred <- predict(M2B, newdata = new_data)


library(ggplot2)

ggplot(wetland.data, aes(x = log(CO2_ppm), y = log(Flux_mean + 1))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ exp(x), se = FALSE, color = "red")

#log(Flux_mean + 1) = -0.0171 * log(CO2_ppm) + 5.9286




M2B <- lmer(log(Flux_mean + 1) ~ log(Watertemp_c) + (1|Wetland),data=wetland.data)
summary(M2B)

ggplot(wetland.data, aes(x = log(Watertemp_c), y = log(Flux_mean + 1))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ exp(x), se = FALSE, color = "red")




M4 <- lmer(k_m.d ~ AirTemp_C + (1|Wetland),data=wetland.data)
summary(M4)

ggplot(wetland.data, aes(x = AirTemp_C, y = k_m.d)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Air Temperature(C)", y = "k_m.d")













library(lmerTest)
library(lme4)




ggplot(wetland.data, aes(x = AirTemp_C, y = k_m.d)) +
  geom_point(size=2.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Air Temperature (C)", y = "Gas transfer velocity (m/day)")+
  theme_bw()




M3 <- lmer(k_m.d ~ Average_wind + (1|Wetland),data=wetland.data)
summary(M3)

ggplot(wetland.data, aes(x = Average_wind, y = k_m.d)) +
  geom_point(size = 2.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Average Wind Speed (m/s)", y = "Gas transfer velocity (m/day)") +
  theme_bw()





ggplot(data = subset(wetland.data, Wetlandpt2 != 13), aes(x = average_Co2, y = wetland_flux)) +
  geom_point(aes(col = factor(Wetlandpt2)), size = 3) +
  geom_smooth(method = lm, se = FALSE, col = 'black', linetype = 'dashed') +
  theme_bw() +
  labs(y = "CO2_Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") +
  scale_y_log10() +
  scale_x_log10()
