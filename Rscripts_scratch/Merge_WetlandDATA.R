#Wetland dataframe

library(dplyr)
library(tidyverse)

#main data frame 


#df
df <-  read.csv(here::here("Wetlands/SamplingNotes/Wetlands_data_master_excel.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
df <- df[,c("Wetland","Location","Date","CO2.NO3.time","CH4.Time","Mehane.bottle","Watertemp_mean","water.level")]
colnames(df) <- c("Wetland","Location","Date","Time_CO2","Time_CH4","Methane_Bottle","Watertemp_c","Waterlevel")
df$Time_CH4 <- NULL
df$Methane_Bottle <- NULL
df$Time_CO2 <- NULL
df <- unique(df)
#CO2
CO2 <-  read.csv(here::here("Wetlands/Wetland_CO2Data_2022-10-18.csv"), skip=0, header = TRUE, sep = ",",
                quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2$Date <- as.Date(CO2$Date, format = "%m/%d/%y")
CO2 <- CO2[,c("Wetland","Location","Date","ppm_NOTcorrected")]
CO2 <- CO2%>%drop_na(ppm_NOTcorrected)
#CO2$WetlandDate <- paste(CO2$Wetland,Flux$Date)
#CO2_pivot <- CO2%>%group_by(WetlandDate)%>%
#  summarise(CO2_mean = mean(ppm_NOTcorrected,na.rm=TRUE),
#            CO2_stdev = sd(ppm_NOTcorrected,na.rm=TRUE))

#flux
Flux <-  read.csv(here::here("Wetlands/Wetland_FluxData_2022-09-17.csv"), skip=0, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Flux$Time <- NULL
Flux$Flux <- as.numeric(Flux$Flux)
Flux$WetlandDate <- paste(Flux$Wetland,Flux$Date)
FluxData_pivot <- Flux%>%group_by(WetlandDate)%>%
  summarise(Flux_mean = mean(Flux,na.rm=TRUE),
            Flux_stdev = sd(Flux,na.rm=TRUE))
FluxData_pivot$Wetland <- gsub( " .*$", "", FluxData_pivot$WetlandDate)
FluxData_pivot$Date <- gsub(".*\\ ", "", FluxData_pivot$WetlandDate) 
FluxData_pivot$Date <- as.Date(FluxData_pivot$Date, format = "%m/%d/%y")
FluxData_pivot$WetlandDate <- NULL
FluxData_pivot$Wetland <- as.integer(FluxData_pivot$Wetland)

#Baro
Baro <- read.csv(here::here("Wetlands/Wetlands_BaroData_2022-09-29.csv"), skip=0, header = TRUE, sep = ",",
                     quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Baro$Date <- as.Date(Baro$Date, format = "%m/%d/%y")
Baro <- Baro[,c("Wetland","Date","Time_start","AirPress_kpa","AirTemp_C")]
colnames(Baro) <- c("Wetland","Date","Time_Baro","AirPress_kpa","AirTemp_C")
######methane
Vial_Vol <-  read.csv(here::here("Methane/Vial_volume_6-13-22.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Vial_Vol <- Vial_Vol[,c(1:5)]

Vial_loc <-  read.csv(here::here("Methane/Vial_sample_location.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Vial_loc <- Vial_loc[,c(1:9)]
colnames(Vial_loc) <- c("AquaticSystem","Wetland","Bottle_Number","Location","Time","Date","Rep","Notes","Notes2")

exsist <-read.csv(here::here("Methane/Vials'that'exist.csv"), skip=0, header = TRUE, sep = ",",
                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#merge
df_merge <- full_join(Vial_Vol,Vial_loc,by="Bottle_Number")
df_merge <- full_join(df_merge,exsist,by="Bottle_Number")
df_merge$Date <- as.Date(df_merge$Date,format="%m/%d/%y")

#drop_NA - data check
df_merge_2 <- df_merge[,c("Bottle_Number","Zinc_Chloride","Date","Exsist","Sample_Collected")]
df_merge_2 <-df_merge_2 %>% drop_na(Date)
df_merge_2 <-df_merge_2 %>% drop_na(Exsist)
df_merge_2 <-df_merge_2 %>% drop_na(Zinc_Chloride)
#

## Merge with data 
#sample_data <-  read.csv(here::here("Methane/Methane_Data_2022-08-04.csv"), skip=0, header = TRUE, sep = ",",
#                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
sample_data <-  read.csv(here::here("Methane/GHG_Data_2022-09-28.csv"), skip=0, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#names(GHG_df)[names(GHG_df) == "Bottle_Number"] <- "Vial_no"
#sample_data$Bottle_Number <- as.numeric(sample_data$Bottle_Number)

df_merge <- full_join(df_merge,sample_data,by="Bottle_Number")

Methane <- df_merge%>%drop_na(Bottle_Number)%>%drop_na(AquaticSystem)%>%filter(AquaticSystem=="wetland")

Methane <- Methane[c("Wetland","Location","Time","Date","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")]  
colnames(Methane) <- c("Wetland","Location","Time_CH4","Date","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")  
Methane$Wetland <- as.integer(Methane$Wetland)
Methane$Location <- as.integer(Methane$Location)

##merge 
df <- full_join(df,CO2, by=c("Wetland","Date","Location"))
df <- full_join(df,Methane, by=c("Wetland","Date","Location"))
df <- full_join(df,FluxData_pivot, by=c("Wetland","Date"))
df <- full_join(df,Baro, by=c("Wetland","Date"))

#######
##correct pressure and temperature
########

#convert baro to hpa
#1 kPa = 10 hPa
#1 kPa = 0.101972 m
df$AirPress_hpa <- df$AirPress_kpa * 10

df$CO2_ppm <- df$ppm_NOTcorrected * (1 + (1013 - df$AirPress_hpa) * 0.0015) * (1 - (25 - df$Watertemp_c) * 0.003)

#######
##calculate k600
########


#concentration of air
#from www.esrl.noaa.gov
#average of July and August
#410.86 ppm
CO2_air_ppm <- 410*10^-6
R=0.08205736608096
gCO2asC <- 12

#convert hpa to atm; 1hPa = 0.0009869233 atm
df$AirPress_atm <- df$AirPress_hpa * 0.0009869233

df$VaporPressure_atm <- 10^(5.40221-(1838.675/((df$AirTemp_C + 273.15)-31.737)))

df$TotalAir_atm <- df$AirPress_atm - df$VaporPressure_atm
df$Total_air_MolperL <- df$TotalAir_atm/(R*(df$AirTemp_C + 273.15)) 
df$CO2_air_MolesPerLiter <- df$Total_air_MolperL * CO2_air_ppm
# 12 grams of C in 1 mole of CO2
df$CO2_air_gCO2asCPerLiter <- df$CO2_air_MolesPerLiter * gCO2asC


### Calculate concentration CO2 in water (gCO2-C per Liter) using Henry's law ###
#Equation below can be used to adjust Henry's constant to the temperature of the environment
# *NOTE: we may need to do an additional calculation to adjust for low pressure in our environment?*
#**KH = KH(STP) x exp(D(1/T-1/T(STP)))**
#* KH(STP) = Henry's law constant at STP (0.035 mol/L)
#*NOTE: the cited literature say that this in mol/(kg x bar)*
#  * D = Temperature dependence constant (2400 K)
#* T = Water Temperature (K)
#* T(STP) = Temp at STP (298.15 K)

#set constants
kH_STP_mol.L.atm = .035
D_K = 2400 
df$Watertemp_K <- df$Watertemp_c + 273.15
T_STP_K = 298.15

#calculate henry's law constant using 
df$KH_mol.L.atm <- kH_STP_mol.L.atm * exp(D_K*(1/df$Watertemp_K - 1/T_STP_K))

UatmToatm <- 10^6

#calculate mass equivalence of CO2 in water
df$CO2_water_gCO2asCPerLiter <- (df$CO2_ppm / UatmToatm) * df$KH_mol.L.atm * gCO2asC

#calculate concntration difference between CO2 in the water and in the air
#for some reason, unclear to me, Mcdowell multiplies by henry's constant AGAIN. Let's try it
LiterToCubicMeter = 1000

df$deltaCO2_gCO2asCperM3 <-  (df$CO2_water_gCO2asCPerLiter - df$CO2_air_gCO2asCPerLiter)  *
  df$KH_mol.L.atm  * LiterToCubicMeter


#convert flux to mole/m2/d
#seconds per day (86,400)
SecToDay <- 86400
#umole to mole
UmoleToMole <- 10^6
#convert moles to grams of Carbon dioxide as Carbon
#gCO2asC <- 12

df$Flux_gCO2asCperM2perDay <- df$Flux_mean * SecToDay /  UmoleToMole *gCO2asC

#Fick' Law: calculate K

#**F(aq) = K[pCO2(aq) - pCO2(air)] x KH**
#  * k = F/(pCO2(aq)-pC02(air))/KH
#* F = Flux mole/m2/d
#* K = gas transfer rate m/d
#* KH = henry's constant for molar concentration of CO2 in water 

#*rearrange equation to solve for k*
#**k = F(aq) / ([pCO2(aq) - pCO2(air)] x KH)**

#convert Liters to cubic meters
#LiterToCubicMeter <- 1000
#df$deltaCO2_gCO2asCPerCubicMeter <- df$deltaCO2_gCO2asCPerLiter * LiterToCubicMeter

#calculate

df$k_m.d <- df$Flux_gCO2asCperM2perDay  / df$deltaCO2_gCO2asCperM3


#convert to k600
#k600 = kCO2 * (600/SC)^-0.5
#SC= schmidst constant, temperature dependent
#SC = 1911.1 - 118.11*T + 3.4527*T^2 - 0.04132*T^3
#T = temperature in c
df$Sc <- 1911.1 - 118.11*df$Watertemp_c + 3.4527*(df$Watertemp_c)^2 - 0.04132*(df$Watertemp_c)^3

df$K600 <- df$k_m.d * (600/df$Sc)^(-0.5)


###########
#write out#
##########

write.csv(df, here::here("wetlands_df_2022-10-18.csv"),
          row.names = FALSE)

##########

# Basic box plot
df$Wetland <- as.factor(df$Wetland)
p <- ggplot(df, aes(x=Wetland, y=log10(CO2_ppm))) + 
  geom_boxplot()
p

df$K600
p <- ggplot(df%>%
              subset(Wetland = "1"|"2"|"3"), aes(x=Date, y=log10(K600))) + 
  geom_point()

p + facet_grid(~Wetland)

p <- ggplot(df%>%
              subset(Wetland = "1"|"2"|"3"), aes(x=Date, y=log10(Flux_mean))) + 
  geom_point()

p + facet_grid(~Wetland)


####suplimentary data frame####

WaterChem1 <- read.csv(here::here("WaterSamples/WaterSamples_2022-08-15.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
WaterChem2 <- read.csv(here::here("WaterSamples/DOC_KW_08-25-2022.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
WaterChem <- full_join(WaterChem1,WaterChem2, by="sample.ID")

rm(WaterChem1,WaterChem2)

WaterChem <- WaterChem[,c("Date","site","time","DOC..mg.L.","TDN..mg.L.")]
colnames(WaterChem) <- c("Date","Site","Time","DOC_mgL","TDN_mgL")
