#Wetland dataframe

library(dplyr)
library(tidyverse)

#main data frame 

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

Methane <- rename(Methane, Vial_no = Bottle_Number)
Methane$Vial_no <- as.integer(Methane$Vial_no)

#methane processing
processing <-  read.csv(here::here("Methane/Methane_Processing_Info.csv"), skip=6, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#rename column names
processing <- rename(processing, Vial_no = Vial_n0_9ml)
processing$Vial_no <- as.integer(processing$Vial_no)
processing$Field.blank.Date <- NULL
processing$Field.blank.vol <- NULL
processing$Fieil.blank.vol <- NULL
processing$Field.Blank.time <- NULL


Methane <- left_join(Methane,processing, by="Vial_no")



#Methane <- Methane[c("Wetland","Location","Time","Date","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")]  
#colnames(Methane) <- c("Wetland","Location","Time_CH4","Date","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")  

Methane$Notes <- NULL
Methane$Notes2 <- NULL
Methane$CH4.area <- NULL
Methane$N2O.area <- NULL
Methane$Title <- NULL
Methane$batch <- NULL
Methane$AquaticSystem <- NULL

Methane$Wetland <- as.integer(Methane$Wetland)
Methane$Location <- as.integer(Methane$Location)



#Baro
Baro <- read.csv(here::here("Wetlands/Wetlands_BaroData_2022-09-29.csv"), skip=0, header = TRUE, sep = ",",
                 quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Baro$Date <- as.Date(Baro$Date, format = "%m/%d/%y")
Baro <- Baro[,c("Wetland","Date","Time_start","AirPress_kpa","AirTemp_C")]
colnames(Baro) <- c("Wetland","Date","Time_Baro","AirPress_kpa","AirTemp_C")

##merge 
df <- full_join(Methane,Baro, by=c("Wetland","Date"))

