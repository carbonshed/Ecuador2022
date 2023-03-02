
```{r load packages}
library(tidyverse) # includes package ggplot that we will use to visualize data
library(lubridate) # helps with formating date and time
library(dplyr) # lots of useful functions in this package
library(here) # use this when reading in data, here sets our working directory to the folder containing our R project. This makes code sharing easier.
```
wetland.data <- read.csv(file="wetlands_df.csv")         
view(wetland.data)

colnames(wetland.data) <- c("Wetland", "Location","Date","Watertemp_c","Waterlevel","ppm_NOTcorrected","Flux_mean","Flux_stdev","Time_Baro","AirPress_kpa", "AirTemp_C", "AirPress_hpa", "CO2_ppm","AirPress_atm","VaporPressure_atm","TotalAir_atm", "Total_air_MolperL","CO2_air_MolesPerLiter","CO2_air_gCO2asCPerLiter","Watertemp_K","KH_mol.L.atm", "CO2_water_gCO2asCPerLiter","deltaCO2_gCO2asCperM3", "Flux_gCO2asCperM2perDay",	"k_m.d", "Sc", "K600")


wetland.data$wetland_factor <- as.factor(wetland.data$Wetland)

wetland.data$Location <- as.character(wetland.data$Location)

wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)


wetland.data$Date <- as.Date(wetland.data$Date,"%m/%d/%Y")

view(wetland.data)

#wetland.data$Time_Baro <- as.numeric(wetland.data$Time_Baro)
#wetland.data$Time_Baro <- as.difftime(wetland.data$Time_Baro,"%M:%S")
#For the hashtags above I was struggling with the time but then i realized I didnt even need this for the graphs :)



#below is the start of the actual diagrams :)

#below is the water temp vs Water level for each wetland 
wetland.data <- read.csv(file="wetlands_df.csv")         

wetland.data$Waterlevel <- as.numeric(wetland.data$Waterlevel)

ggplot(data = wetland.data, mapping = aes(x = Watertemp_c, y = Waterlevel)) + geom_point(aes(color = Wetland))
ggplot(data = wetland.data) +  geom_point(aes(x=Watertemp_c, y=Waterlevel, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))


#below is the wetland date and waterlevel

ggplot(data = wetland.data, aes(x = Date, y = Waterlevel, group = Wetland)) + geom_line()


#This shows the date and the water level for each wetland 
ggplot(data = wetland.data, aes(x = Date, y = Waterlevel, group = Wetland)) + geom_line(aes(color=Wetland))+geom_point()
ggplot(data = wetland.data) +  geom_line(aes(x=Date, y=Waterlevel, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))


ggplot(data = wetland.data) +  geom_point(aes(x=Date, y=Waterlevel, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))

#peep for above when i try to do the geomline with color the lines become vertical


#This shows the date and the water temp for each wetland 
ggplot(data = wetland.data, aes(x = Date, y = Watertemp_c, group = Wetland)) + geom_line(aes(color=Wetland))+geom_point()
ggplot(data = wetland.data) +  geom_point(aes(x=Date, y=Watertemp_c, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))



#This shows the date and the CO2 for each wetland 
ggplot(data = wetland.data, aes(x = Date, y = CO2_ppm, group = Wetland)) + geom_line(aes(color=Wetland))+geom_point()
ggplot(data = wetland.data) +  geom_point(aes(x=Date, y=CO2_ppm, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))


#This shows CO2  & water level for each wetland 
ggplot(data = wetland.data, aes(x = Waterlevel, y = CO2_ppm, color = Wetland)) + geom_line()
ggplot(data = wetland.data) +  geom_point(aes(x=Waterlevel, y=CO2_ppm, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))


#This shows CO2  & water temp for each wetland 
ggplot(data = wetland.data, aes(x = Watertemp_c, y = CO2_ppm, group = Wetland)) + geom_line(aes(color=Wetland))+geom_point()
ggplot(data = wetland.data) +  geom_point(aes(x=Watertemp_c, y=CO2_ppm, col=Wetland)) + scale_color_gradientn(colours=rainbow(100))


#side by side c02 vs water temp and water level 
str(wetland.data)
par(mfrow = c(2,2))
plot(Water_temp,CO2_ppm)
plot(Waterlevel,CO2_ppm)

set.seed(123)                 
data1 <- wetland.data(x = rnorm(700))
plot1 <- ggplot(data1, aes(x = Waterlevel)) + geom_density()
  
  
#its treating wetland like a variable 

library(ggplot2)
#Code The bottom fit it bc it shows c02 but nit the individual wetlands
ggplot(data = wetland.data,aes(x=Location,y=CO2_ppm,color=Date))+ geom_line()+ geom_point()

#Unique regions

uni <- unique(data = wetland.data$Wetland)
#Plot func
myplot <- function(x)
{
  G <- ggplot(subset(df,Region==x),aes(x=Date,y=Crime_occurrencies,color=Crime))+
    geom_line()+
    geom_point()+
    ggtitle(x)
  G
}
#Apply
List <- lapply(uni, myplot)




ggplot(data = wetland.data, aes(x = Wetland, y = CO2ppm)) + stat_boxplot(geom = "errorbar", # Boxplot with error bars width = 0.2) + geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors  alpha = 0.9, outlier.colour = "red") +  scale_y_continuous(name = "Co2 ppm") +  # Continuous variable label scale_x_discrete(name = "Wetland") +      # Group label ggtitle("Wetland CO2") + # Plot title theme(axis.line = element_line(color = "black", # Theme customization size = 0.25))


                        
#NEW THINGS
packages <- c("tidyverse", "datasets","papaja" ,"here")                 
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

wetland.data %>% filter(Wetland %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) %>%  knitr::kable() group_by(Date, Wetland) %>%

mutate(percent = CO2_ppm/(sum(CO2_ppm)), se = sqrt((percent * (1-percent))/CO2_ppm)) %>%


  
#boxplot 
  ggplot(data = wetland.data, aes(x = Wetland, y = CO2_ppm)) + stat_boxplot(geom = "errorbar", # Boxplot with error bars width = 0.2) + geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors alpha = 0.9, outlier.colour = "red") + scale_y_continuous(name = "CO2_ppm") +  # Continuous variable scale_x_discrete(name = "Wetland") +      # Group label ggtitle("Boxplot by groups ggplot2") + # Plot title theme(axis.line = element_line(colour = "black", # Theme customization size = 0.25))
                                                                            
                                                                            
                                                                            
                                                                            
                                                                            
                                                                            
ggplot(data = wetland.data, aes(x = Wetland, y = CO2_ppm)) + stat_boxplot(geom = "errorbar", # Boxplot with error bars width = 0.2) + geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors alpha = 0.9, outlier.colour = "red") + scale_y_continuous(name = "CO2") +  # Continuous variable label scale_x_discrete(name = "Wetland") +      # Group label ggtitle("Boxplot by groups ggplot2") + # Plot title theme(axis.line = element_line(colour = "black", # Theme customizatio size = 0.25))
                                                                          # Boxplot by ggplot(data = wetland.data, aes(x = Wetland, y = CO2_ppm)) + stat_boxplot(geom = "errorbar", # Boxplot with error bars width = 0.2) + geom_boxplot(fill = "#4271AE", colour = "#1F3552", # C alpha = 0.9, outlier.colour = "red") + scale_y_continuous(name = "CO2_ppm") +  # Continuous variable label scale_x_discrete(name = "Wetland") +      # Group label ggtitle("Boxplot by groups ggplot2") + # Plot title theme(axis.line = element_line(colour = "black", # Theme customization size = 0.25))

                                                                          
      #make points bigger                                                                       
                                                                            
                                                                            
                                                                            
                                                                            
                                                                                                           