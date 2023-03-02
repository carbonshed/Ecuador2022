```{r load packages}
library(tidyverse) # includes package ggplot that we will use to visualize data
library(lubridate) # helps with formating date and time
library(dplyr) # lots of useful functions in this package
library(here) # use this when reading in data, here sets our working directory to the folder containing our R project. This makes code sharing easier.
library(ggplot2)
```


wetland.data <- read.csv(file="Flux_and_area2.csv")         
view(wetland.data)

colnames(wetland.data) <- c("Wetland", "Location","Date","Watertemp_c","Waterlevel","ppm_NOTcorrected","Flux_mean","Flux_stdev", "X", "Area", "Flux_umolps", "New equation", "area2", "Flux_conc", "CO2ppm", "Precipitation")



wetland.data$wetland_factor <- as.factor(wetland.data$Wetland)

wetland.data$Location <- as.character(wetland.data$Location)

wetland.data$Flux_mean <- as.numeric(wetland.data$Flux_mean)
wetland.data$CO2ppm <- as.numeric(wetland.data$Flux_mean)
wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)

wetland.data$Area <- as.numeric(wetland.data$Area)
wetland.data$Flux <- as.numeric(wetland.data$Flux)

wetland.data$Date <- as.Date(wetland.data$Date,"%m/%d/%Y")

view(wetland.data)



#below is the start of the actual diagrams :)



#CO2 vs Flux
ggplot(data = wetland.data) + geom_point(aes(x = CO2ppm, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "CO2(ppm)") + scale_x_log10()

ggplot(data = wetland.data) + geom_point(aes(x = CO2ppm, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "CO2(ppm)")

#area vs CO2
ggplot(data = wetland.data) + geom_point(aes(x = area2, y = CO2ppm, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "CO2 (ppm)", x = "Area(m^2)")+ theme(axis.text.x=element_text(angle=60, hjust=1))
ggplot(data = wetland.data) + geom_point(aes(x = area2, y = CO2ppm, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "CO2 (ppm)", x = "Area(m^2)")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + xlim(0,820) + ylim(0,.17)


#area vs flux

ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Area (m^2)")+ theme(axis.text.x=element_text(angle=60, hjust=1))

ggplot(data = wetland.data) + geom_line(aes(x = area2, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Area (m^2)")+ theme(axis.text.x=element_text(angle=60, hjust=1))
ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Area (m^2)")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + scale_x_log10() + scale_y_log10()
ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Area (m^2)")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + scale_x_log10()
ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Area (m^2)")+ theme(axis.text.x=element_text(angle=60, hjust=1)) + scale_y_log10()






ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + ylim(0,80) + xlim(0,1000) + labs(y= "Flux (μmol/s)", x = "Area(m^2)")

ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + ylim(.28,2) + xlim(30.55,30.84) + labs(y= "Flux (μmol/s)", x = "Area(m^2)")


#Water level vs flux  PEEP how its the same as the other one

ggplot(data = wetland.data) + geom_point(aes(x = Waterlevel, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Waterlevel")


#wetland area vs waterlevel

ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Waterlevel, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Water level (m)", x = "Area(m^2)")
ggplot(data = wetland.data) + geom_point(aes(y = area2, x = Waterlevel, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(x = "Water level (m)", y = "Area(m^2)")
ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Waterlevel, group = Wetland, color = factor(Wetland)), size = 3.5 ) + xlim(0,1000) + labs(y= "Water level (m)", x = "Area(m^2)")

#waterlevel vs precipitation
ggplot(data = wetland.data) + geom_point(aes(x = Precipitation, y = Waterlevel, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Water level ()", x = "Precipitaiton (mm)")


#flux vs precipitation (this was the day of averafe)
ggplot(data = wetland.data) + geom_point(aes(x = Precipitation, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Precipitation (mm)")
ggplot(data = wetland.data) + geom_line(aes(x = Precipitation, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Precipitation (mm)")


#CO2 vs precipitation (this was the day of average)

ggplot(data = wetland.data) + geom_point(aes(x = Precipitation, y = CO2ppm, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "CO2 (ppm)", x = "Precipitation (mm)")
ggplot(data = wetland.data) + geom_point(aes(x = Precipitation, y = CO2ppm, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "CO2 (ppm)", x = "Precipitation (mm)") + xlim(0,5) + ylim(0,.12)


#area vs precip (precip was average CO2)
ggplot(data = wetland.data) + geom_point(aes(x = area2, y = Precipitation, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Precipitation (mm)", x = "Area (m^2)") 


#Precipitation vs CO2 wetland  *important 

ggplot(data = wetland.data, aes(x = Precipitation, y = CO2ppm, colour = wetland_factor)) + geom_point() + labs(y= "CO2 (ppm)", x = "Precipitation (mm)") + facet_grid(~wetland_factor) + facet_wrap(~wetland_factor, nrow = 4, scales = "free_y")


#Precip vs flux per wetland 
library(ggplot2)
library(gridExtra)
plot1<- ggplot(data = wetland.data) + geom_point(aes(x = Precipitation, y = CO2ppm, group = Wetland, color = factor(Wetland1)), size = 3.5 ) + labs(y= "CO2 (ppm)", x = "Precipitation (mm)")
plot2<-ggplot(data = wetland.data) + geom_point(aes(x = Precipitation, y = Flux_conc, group = Wetland, color = factor(Wetland)), size = 3.5 ) + labs(y= "Flux (μmol/s)", x = "Precipitation (mm)")
grid.arrange(plot1, plot2, ncol=2)


ggplot(data = wetland.data) + geom_point(aes(x = Precipitation, y = CO2ppm, group = wetland1, color = factor(Wetland)), size = 3.5 ) + labs(y= "CO2 (ppm)", x = "Precipitation (mm)")
ggplot(data[data$Wetland %in% "1", ], aes(Precipitation, CO2ppm)) + geom_point(color = "green", size = 3)

