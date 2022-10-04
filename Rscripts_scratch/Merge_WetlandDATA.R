#Wetland dataframe

library(dplyr)

#main data frame 


#co2
CO2 <-  read.csv(here::here("Wetlands/SamplingNotes/Wetlands_data_master_excel.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CO2$Date <- as.Date(CO2$Date, format = "%m/%d/%y")
CO2 <- CO2[,c("Wetland","Location","Date","Time","ppm_NOTcorrected")]
colnames(CO2) <- c("Wetland","Location","Date","Time_CO2","ppm_NOTcorrected")

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

df <- full_join(CO2,Methane, by=c("Wetland","Date","Location"))
df <- full_join(df,FluxData_pivot, by=c("Wetland","Date"))
df <- full_join(df,Baro, by=c("Wetland","Date"))


####suplimentary data frame####

WaterChem1 <- read.csv(here::here("WaterSamples/WaterSamples_2022-08-15.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
WaterChem2 <- read.csv(here::here("WaterSamples/DOC_KW_08-25-2022.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
WaterChem <- full_join(WaterChem1,WaterChem2, by="sample.ID")

rm(WaterChem1,WaterChem2)

WaterChem <- WaterChem[,c("Date","site","time","DOC..mg.L.","TDN..mg.L.")]
colnames(WaterChem) <- c("Date","Site","Time","DOC_mgL","TDN_mgL")
