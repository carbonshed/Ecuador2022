```{r load packages}
library(tidyverse) # includes package ggplot that we will use to visualize data
library(lubridate) # helps with formating date and time
library(dplyr) # lots of useful functions in this package
library(here) # use this when reading in data, here sets our working directory to the folder containing our R project. This makes code sharing easier.
library(ggplot2)
```
wetland.data <- read.csv(file="wetlands_df.csv")         
view(wetland.data)

colnames(wetland.data) <- c("Wetland", "Location","Date","Watertemp_c","Waterlevel","ppm_NOTcorrected","Flux_mean","Flux_stdev","Time_Baro","AirPress_kpa", "AirTemp_C", "AirPress_hpa", "CO2_ppm","AirPress_atm","VaporPressure_atm","TotalAir_atm", "Total_air_MolperL","CO2_air_MolesPerLiter","CO2_air_gCO2asCPerLiter","Watertemp_K","KH_mol.L.atm", "CO2_water_gCO2asCPerLiter","deltaCO2_gCO2asCperM3", "Flux_gCO2asCperM2perDay",	"k_m.d", "Sc", "K600", "Average_wind", "Solar_radiation", "Wetland_wind", "wetland_solar", "wetland_watertemp", "Wetlandairtemp", "wetland_flux", "Average_area", "average_precip", "average_Co2", "Wetlandpt2")


wetland.data$wetland_factor <- as.factor(wetland.data$Wetland)
wetland.data$wetland_factor <- as.factor(wetland.data$Wetlandpt2)

wetland.data$Location <- as.character(wetland.data$Location)
wetland.data$average_precip <- as.numeric(wetland.data$average_precip)
wetland.data$average_Co2 <- as.numeric(wetland.data$average_Co2)

wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)

wetland.data$Average_wind <- as.numeric(wetland.data$Average_wind)
wetland.data$Solar_radiation <- as.numeric(wetland.data$Solar_radiation)
wetland.data$Date <- as.Date(wetland.data$Date,"%m/%d/%Y")

wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)
wetland.data$Solar_radiation <- as.numeric(wetland.data$Solar_radiation)
wetland.data$Wetland_wind <- as.numeric(wetland.data$Wetland_wind)
wetland.data$wetland_solar <- as.numeric(wetland.data$wetland_solar)
wetland.data$wetland_watertemp <- as.numeric(wetland.data$wetland_watertemp)
wetland.data$Wetlandairtemp <- as.numeric(wetland.data$Wetlandairtemp)
wetland.data$wetland_flux <- as.numeric(wetland.data$wetland_flux)
wetland.data$Average_area <- as.numeric(wetland.data$Average_area)


view(wetland.data)

#wetland.data$Time_Baro <- as.numeric(wetland.data$Time_Baro)
#wetland.data$Time_Baro <- as.difftime(wetland.data$Time_Baro,"%M:%S")
#For the hashtags above I was struggling with the time but then i realized I didnt even need this for the graphs :)



#below is the start of the actual diagrams :)

#below is the water temp vs Water level for each wetland 
wetland.data <- read.csv(file="wetlands_df.csv")         

wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)

ggplot(data=wetland.data, aes(x=Watertemp_c, y=Waterlevel, col=Wetland)) + scale_color_gradientn(colours=rainbow(100)) + geom_point(size=3.5)

ggplot(data = wetland.data) + geom_point(aes(x = Watertemp_c, y = Waterlevel, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(x= "Water Temperature (C)", y = "Water Level (m)")



#below is the wetland date and waterlevel

ggplot(data = wetland.data, aes(x = Date, y = Waterlevel, group = Wetland)) + geom_line(aes(color=Wetland)) + scale_colour_gradientn (colours=rainbow(100)) + geom_point(size=1.5) + theme (axis.text.x=element_text(angle=60, hjust=1))

ggplot(data = wetland.data) + geom_line(aes(x = Date, y = Waterlevel, group = Wetland, color = factor(Wetland)), size = .95 ) + geom_point(aes(x = Date, y = Waterlevel, group = Wetland)) + labs(x= "Date", y = "Water Level (m)") + theme (axis.text.x=element_text(angle=60, hjust=1))



                                                                                        

  





#This shows the date and the water level for each wetland 
ggplot(data = wetland.data, aes(x = Date, y = Waterlevel, group = Wetland)) + geom_line(aes(color=Wetland))+geom_point()+ theme(axis.text.x=element_text(angle=60, hjust=1))




#This shows the date and the water temp for each wetland 
ggplot(data = wetland.data, aes(x = Date, y = Watertemp_c, group = Wetland)) + geom_line(aes(color=Wetland))+geom_point()+ theme(axis.text.x=element_text(angle=60, hjust=1))


ggplot(data = wetland.data, aes(x = Date, y = Watertemp_c, group = Wetland)) + geom_line(aes(color=Wetland))+ scale_colour_gradientn(colours=rainbow(100)) + geom_point(size=1.5)+ theme(axis.text.x=element_text(angle=60, hjust=1))


#ggplot(data = wetland.data) +  geom_point(aes(x=Date, y=Watertemp_c, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))+ theme(axis.text.x=element_text(angle=60, hjust=1))



#This shows the date and the CO2 for each wetland 
ggplot(data = wetland.data, aes(x = Date, y = CO2_ppm, group = Wetland)) + geom_line(aes(color=Wetland))+ scale_colour_gradientn(colours=rainbow(100)) + geom_point(size=1.5)+ theme(axis.text.x=element_text(angle=60, hjust=1))

#ggplot(data = wetland.data) +  geom_point(aes(x=Date, y=CO2_ppm, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))


#This shows CO2  & water level for each wetland 
ggplot(data = wetland.data, aes(x = Waterlevel, y = CO2_ppm, color = Wetland)) + geom_line(aes(color=Wetland))+ scale_colour_gradientn(colours=rainbow(100)) + geom_point(size=1.5)+ theme(axis.text.x=element_text(angle=60, hjust=1))
#ggplot(data = wetland.data) +  geom_point(aes(x=Waterlevel, y=CO2_ppm, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))






#This shows CO2  & water temp for each wetland 
ggplot(data = wetland.data, aes(x = Watertemp_c, y = CO2_ppm, group = Wetland)) + geom_line(aes(color=Wetland))+geom_point()
ggplot(data = wetland.data) +  geom_point(aes(x=Watertemp_c, y=CO2_ppm, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))


####ggplot(data = wetland.data, aes(x = Date, y = Watertemp_c, group = Wetland)) + geom_line(aes(color=Wetland))+ scale_colour_gradientn(colours=rainbow(100)) + geom_point(size=1.5)+ theme(axis.text.x=element_text(angle=60, hjust=1))




#side by side c02 vs water temp and water level 


ggplot(data = wetland.data) + geom_line(aes(x = Waterlevel, y = CO2_ppm, group = Wetland, color = factor(Wetland)), size = .95 ) + geom_point(aes(x = Waterlevel, y = CO2_ppm, group = Wetland)) + labs(x= "Waterlevel (m)", y = "CO2(ppm)") + theme (axis.text.x=element_text(angle=60, hjust=1))



#airtemp vs waterlevel
ggplot(data = wetland.data) + geom_line(aes(x = Waterlevel, y = AirTemp_C, group = Wetland, color = factor(Wetland)), size = .95 ) + geom_point(aes(x = Waterlevel, y = CO2_ppm, group = Wetland)) + labs(x= "Waterlevel (m)", y = "CO2(ppm)") + theme (axis.text.x=element_text(angle=60, hjust=1))

ggplot(data = wetland.data) + geom_point(aes(x = AirTemp_C, y = Waterlevel, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(x= "Air Temperature (C)", y = "Water Level (m)")


ggplot(data = wetland.data) + geom_point(aes(x = Waterlevel, y = AirTemp_C, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Air Temperature (C)", x = "Water Level (m)")


#CO2 vs flux


ggplot(data = wetland.data) + geom_point(aes(x = CO2_ppm, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "CO2 (ppm)")

ggplot(data = wetland.data) + geom_point(aes(x = CO2_ppm, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + ylim(0,.1) + xlim(250,1250) + labs(y= "Flux (μmol/m^2/s)", x = "CO2 (ppm)")

ggplot(data = wetland.data) + geom_line(aes(x = CO2_ppm, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + ylim(0,.1) + xlim(250,1250)+ labs(y= "Flux (μmol/m^2/s)", x = "CO2 (ppm)")


#Flux vs Waterlevel
ggplot(data = wetland.data) + geom_point(aes(x = Waterlevel, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Waterlevel (m)")

#flux watertemp
ggplot(data = wetland.data) + geom_point(aes(x = Watertemp_c, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)")

ggplot(data = wetland.data) + geom_point(aes(x = Watertemp_c, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_x_log10() 

ggplot(data = wetland.data, aes(x = Watertemp_c, y = Flux_mean, colour = wetland_factor)) + geom_point() + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + facet_grid(~wetland_factor) + facet_wrap(~wetland_factor, nrow = 4, scales = "free_y")


#flux Airtemp
ggplot(data = wetland.data) + geom_point(aes(x = AirTemp_C, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)")
ggplot(data = wetland.data) + geom_point(aes(x = AirTemp_C, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)") + scale_x_log10() + scale_y_log10()
ggplot(data = wetland.data) + geom_point(aes(x = AirTemp_C, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)") + scale_y_log10()
ggplot(data = wetland.data) + geom_point(aes(x = AirTemp_C, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 2.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)") + scale_y_log10() + facet_grid(~wetland_factor) + facet_wrap(~wetland_factor, nrow = 4, scales = "free_y")


#Peep Flux cs CO2
ggplot(data = wetland.data) +
  geom_point(aes(x = CO2_ppm, y = Flux_mean, group = Wetland, 
                 color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "CO2(ppm)") + scale_y_log10() + scale_x_log10()

#wind speed vs Flux 
ggplot(data = wetland.data) +
  geom_point(aes(x = Average_wind, y = Flux_mean, group = Wetland, 
                 color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") 


ggplot(data = wetland.data) + geom_point(aes(x = Average_wind, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10() 

ggplot(data = wetland.data) + geom_point(aes(x = Average_wind, y = Flux_mean, group = Wetland, color = factor(Wetland)), size = 2.5 ) + labs(y= "Flux (μmol/s)", x = "Wind speed (m/s)") + scale_x_log10() + facet_grid(~wetland_factor) + facet_wrap(~wetland_factor, nrow = 4, scales = "free_y")


#Radiation vs flux
ggplot(data = wetland.data) +
  geom_point(aes(x = Solar_radiation, y = Flux_mean, group = Wetland, 
                 color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") 

ggplot(data = wetland.data) +
  geom_point(aes(x = Solar_radiation, y = Flux_mean, group = Wetland, 
                 color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") +  scale_x_log10() + scale_y_log10()

ggplot(data = wetland.data) +
  geom_point(aes(x = Solar_radiation, y = Flux_mean, group = Wetland, 
                 color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + scale_x_log10()



ggplot(data = wetland.data) +
  geom_point(aes(x = Solar_radiation, y = Flux_mean, group = Wetland, 
                 color = factor(Wetland)), size = 2.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + facet_grid(~wetland_factor) + facet_wrap(~wetland_factor, nrow = 4, scales = "free")






#NOTE these are the start of the average of everything for the wetlands *IMPORTANT
#Radiation vs flux
ggplot(data = wetland.data) +
  geom_point(aes(x = wetland_solar, y = wetland_flux, group = Wetlandpt2, 
                 color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") 


ggplot(data = wetland.data) + geom_point(aes(x = wetland_solar, y = wetland_flux, group = Wetland, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + scale_x_log10() + scale_y_log10()


ggplot(data = wetland.data) + geom_point(aes(x = wetland_solar, y = wetland_flux, group = Wetland, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + scale_x_log10() 

ggplot(data = wetland.data) + geom_point(aes(x = wetland_solar, y = wetland_flux, group = Wetland, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + scale_y_log10() 



ggplot(data = wetland.data) + geom_point(aes(x = wetland_solar, y = wetland_flux, group = Wetland, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)")  
solar_fit = lm(log(wetland.data$wetland_flux) ~ wetland.data$wetland_solar)
summary(solar_fit)
factor = solar_fit$coefficients["wetland.data$wetland_solar"]
factor

coeff = exp(solar_fit$coefficients["(Intercept)"])
coeff

factor = round(factor,4)
coeff = round(coeff,3)
fit_eq=paste("y=",coeff,"exp(",factor,"x)")
text(20,10,fit_eq)



ggplot(data = wetland.data) + geom_point(aes(x = wetland_solar, y = wetland_flux, group = Wetland, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + geom_smooth(formula = log(y) ~ x) 



ggplot(data = wetland.data, aes(x = wetland_solar, y = wetland_flux, group = Wetland, color = factor(Wetlandpt2))) + geom_point(size = 3.5) + labs(y = "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + geom_smooth(formula = y ~ x, method = "ex", se = F, color = "blue") 




#new try 
model <- lm(log(wetland.data$wetland_flux)~wetland.data$wetland_solar)
summary(model)

lm(formula = log(wetland.data$wetland_flux) ~ wetland.data$wetland_solar)
#(ln)y= -1.9407262-0.0011576(x)

#wrong
ggplot(data = wetland.data, aes(x = wetland_solar, y = wetland_flux)) + geom_point() + geom_smooth(se=FALSE)
ggplot(data= wetland.data, aes(x = wetland_solar, y = wetland_flux)) + geom_point() + geom_smooth()




ggplot(data = wetland.data) + geom_point(aes(x = wetland_solar, y = wetland_flux, group = Wetland, color = factor(Wetlandpt2)), size = 3.5 ) + geom_smooth(method=lm) + labs(y= "Flux (μmol/s)", x = "Solar Radiation (W/m^2)") + geom_smooth(formula = log(y) ~ x) 

ggplot(data= wetland.data, aes(x=wetland_solar, y=wetland_flux)) + geom_point() + geom_smooth(method=lm, level=0.90)


#confiedence bands


model <- lm(wetland.data$wetland_flux ~ wetland.data$wetland_solar)
anova(model)
mse <- 1.3467
mean(wetland.data$wetland_solar); range(wetland.data$wetland_solar)




install.packages("mvtnorm")
library(mvtnorm)
library(lattice)

X <- matrix(rnorm(24), ncol = 2)
mu <- colMeans(wetland.data$wetland_solar)
Sigma <- cov(wetland.data$wetland_solar)





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


#flux watertemp
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)")

ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_x_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = wetland_watertemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Water Temperature(C)") + scale_y_log10() + scale_x_log10()



#flux Airtemp
ggplot(data = wetland.data) + geom_point(aes(x = Wetlandairtemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)")
ggplot(data = wetland.data) + geom_point(aes(x = Wetlandairtemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = Wetlandairtemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)") +  scale_x_log10()
ggplot(data = wetland.data) + geom_point(aes(x = Wetlandairtemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)") + scale_y_log10() + scale_x_log10()



#flux precipitation
ggplot(data = wetland.data) + geom_point(aes(x = average_precip, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average precipititation (mm)")
ggplot(data = wetland.data) + geom_point(aes(x = average_precip, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average precipititation (mm)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = average_precip, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average precipititation (mm)") +  scale_x_log10()
ggplot(data = wetland.data) + geom_point(aes(x = average_precip, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average precipititation (mm)") + scale_y_log10() + scale_x_log10()


#flux co2
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)")
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() 
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") +  scale_x_log10()
ggplot(data = wetland.data) + geom_point(aes(x = average_Co2, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Average CO2 (ppm)") + scale_y_log10() + scale_x_log10()












##LEFT OFF HERE

ggplot(data = wetland.data) + geom_point(aes(x = Wetlandairtemp, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Air Temperature (C)") + abline(lm(log(wetland_flux) ~ Wetlandairtemp), col = "red")

plot(wetland.data$Wetlandairtemp, wetland.data$wetland_flux, main = "Exponential scatter plot", xlab = "Air Temperature (C)", ylab = "Flux (μmol/m^2/s)")


model <- lm(log(wetland.data$wetland_flux) ~ wetland.data$Wetlandairtemp)

abline(model, col = "red")

#this is not right
wet_fit = lm(log(wetland.data$wetland_flux) ~ wetland.data$Wetlandairtemp)
summary(wet_fit)

factor = wet_fit$coefficients["wetland.data$Wetlandairtemp"]
factor
coeff = exp(wet_fit$coefficients["(Intercept)"])
coeff
factor = round(factor,4)
coeff = round(coeff,3)

fit_eq = (y= wetland_flux coeff*"exp(",facto*"x)")
text(20,10,fit_eq)

#disploay the curve
curve(coeff*exp(factor*x), 0, 54, add=TRUE, col="blue")



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





#wetland area vs flux
ggplot(data = wetland.data) + geom_point(aes(x = Average_area, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Wetland area average area (m^2)") + scale_x_log10()
ggplot(data = wetland.data) + geom_point(aes(x = Average_area, y = wetland_flux, group = Wetlandpt2, color = factor(Wetlandpt2)), size = 3.5 ) + labs(y= "Flux (μmol/m^2/s)", x = "Wetland area average area (m^2)") + scale_x_log10() + scale_y_log10()
