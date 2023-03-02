```{r load packages}
library(tidyverse) 
library(lubridate) 
library(dplyr) 
library(here) 

wetland.data <- read.csv(file="wetlands_df.csv")         
view(wetland.data)

colnames(wetland.data) <- c("Wetland", "Location","Date","Watertemp_c","Waterlevel","ppm_NOTcorrected","Flux_mean","Flux_stdev","Time_Baro","AirPress_kpa", "AirTemp_C", "AirPress_hpa", "CO2_ppm","AirPress_atm","VaporPressure_atm","TotalAir_atm", "Total_air_MolperL","CO2_air_MolesPerLiter","CO2_air_gCO2asCPerLiter","Watertemp_K","KH_mol.L.atm", "CO2_water_gCO2asCPerLiter","deltaCO2_gCO2asCperM3", "Flux_gCO2asCperM2perDay",	"k_m.d", "Sc", "K600")


# Convert character data to date and time.

# Be more specific.
income <- read.csv("data/ACS_13_5YR_S1903/ACS_13_5YR_S1903.csv", stringsAsFactors=FALSE, sep=",", colClasses=c("GEO.id2"="character"))




p1 <- ggplot() + geom_line(aes(y = Waterlevel, x = Watertemp_c),
                           data = wetland.data)
p1

p1 + labs(title = "Wetland's temperature  vs waterlevel", x = "Water temperature (K)", y = "Water level")



#Under the Environment tab, click the arrow to the left of your dataframe. You will see each column listed with the type of data (ex. num = numeric, chr = character). Check to make sure the data type is correct. 

#The most tricky column is the date/time/date time  column or columns. Look at code that i have written (or google it) to find a line of code that you can adjust to work for your dataframe. 

#Look for this function:
 # - as.POSIXct()
#Temp vs WL (Y)
#https://www.storybench.org/create-simple-line-chart-r/

