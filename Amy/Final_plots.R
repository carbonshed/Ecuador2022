```{r load packages}
remove.packages("tidyverse")
install.packages("tidyverse")
library(tidyverse) # includes package ggplot that we will use to visualize data
library(lubridate) # helps with formating date and time
library(dplyr) # lots of useful functions in this package
library(here) # use this when reading in data, here sets our working directory to the folder containing our R project. This makes code sharing easier.
library(ggplot2)
```
wetland.data <- read.csv(file="wetlands_df.csv")         
colnames(wetland.data) <- c("Wetland", "Location","Date","Watertemp_c","Waterlevel","ppm_NOTcorrected","Flux_mean","Flux_stdev","Time_Baro","AirPress_kpa", "AirTemp_C", "AirPress_hpa", "CO2_ppm","AirPress_atm","VaporPressure_atm","TotalAir_atm", "Total_air_MolperL","CO2_air_MolesPerLiter","CO2_air_gCO2asCPerLiter","Watertemp_K","KH_mol.L.atm", "CO2_water_gCO2asCPerLiter","deltaCO2_gCO2asCperM3", "Flux_gCO2asCperM2perDay",	"k_m.d", "Sc", "K600", "Average_wind", "Solar_radiation", "Wetland_wind", "wetland_solar", "wetland_watertemp", "Wetlandairtemp", "wetland_flux", "Average_area", "average_precip", "average_Co2", "Wetlandpt2")


wetland.data$wetland_factor <- as.factor(wetland.data$Wetland)
wetland.data$wetland_factor <- as.factor(wetland.data$Wetlandpt2)

wetland.data$average_Co2 <- as.numeric(wetland.data$average_Co2)

wetland.data$Average_wind <- as.numeric(wetland.data$Average_wind)
wetland.data$Date <- as.Date(wetland.data$Date,"%m/%d/%Y")

wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)
wetland.data$Wetland_wind <- as.numeric(wetland.data$Wetland_wind)
wetland.data$wetland_watertemp <- as.numeric(wetland.data$wetland_watertemp)
wetland.data$wetland_flux <- as.numeric(wetland.data$wetland_flux)


view(wetland.data)




#NOTE these are the start of the average of everything for the wetlands *IMPORTANT


#confiedence bands ignored


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
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10()



#UNDER THIS ISNT RIGHT
ggplot(data = wetland.data, aes(x = Wetland_wind, y = wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') +
  theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = " Wind speed (m/s)") +  scale_x_log10()

lm(formula = wetland_flux ~ Wetland_wind, data = wetland.data)


ggplot(log(wetland.data$Wetland_wind), log(wetland.data$wetland_flux), labs(y= "Flux (μmol/m^2/s)", x = " Wind speed (m/s)")

       
ggplot(data = wetland.data, aes(x = log(Wetland_wind), y = log(wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = " Wind speed (m/s)") +  scale_x_log10()
       
#CORRECT
#fit the model
model <- lm(log(wetland_flux) ~ Wetland_wind, data = wetland.data)
summary(model)
#ln(wetland flux) = -6.682 + 4.665(wetland wind)
     
#OR wetland flux = −18.16355917 + 12.68078472^(wetland wind)


###NEW try
exponential.model <- lm(log(wetland_flux)~ Wetland_wind, data = wetland.data)
summary(exponential.model)


Wind_speedvalues <- seq (0, 4.25, 0.5)
wetland_flux.exponential2 <- exp(predict(exponential.model,list(Wetland_wind=Wind_speedvalues)))

plot(Wetland_wind, wetland_flux,pch=16, data = wetland.data)

lines(wetland_flux, wetland_wind.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")



#WIND FORMULA
#log base 10
#y = 0.0005*(e^(1.5754*x)
#R² = 0.6851







  
#flux watertemp
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)")

ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_x_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_y_log10() + scale_x_log10()



#log base 10
#y = 5E-05*e^(1.123x)
#R² = 0.382







#flux co2
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)")
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") +  scale_x_log10()
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()


ggplot(data = wetland.data, aes(x = average_Co2, y = wetland_flux) + geom_point((col= factor(Wetlandpt2)), size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()

              



ggplot(data = wetland.data, aes(x = average_Co2, y = wetland_flux)) + geom_point(col= 'red', size = 3) + geom_smooth(method=lm, se=FALSE, col = 'black', linetype = 'dashed') + theme_bw() + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()

lm(formula = wetland_flux ~ average_Co2, data = wetland.data)

#FORMULA for line of best fit 
#Wetland flux=-0.0383787+(.0001172*average CO2)

























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





