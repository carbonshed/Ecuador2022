#Wetland dataframe

library(dplyr)
library(tidyverse)
library(here)
#main data frame 

######methane
Vial_Vol <-  read.csv(here::here("Methane/Vial_volume_6-13-22.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Vial_Vol <- Vial_Vol[,c(1:5)]

Vial_loc <-  read.csv(here::here("Methane/Vial_sample_location.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Vial_loc <- Vial_loc[,c(1:7)]
colnames(Vial_loc) <- c("AquaticSystem","Wetland","Bottle_Number","Location","Time","Date","Rep")

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

df_merge <- df_merge[,c("Bottle_Number","Water_Weight","Date","AquaticSystem","Wetland","Location","Rep")]
df_merge <- df_merge %>% drop_na(AquaticSystem) %>% filter(Date < as.Date("2022-10-01"))
#

## Merge with data 
#sample_data <-  read.csv(here::here("Methane/Methane_Data_2022-08-04.csv"), skip=0, header = TRUE, sep = ",",
#                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
sample_data <-  read.csv(here::here("Methane/GHG_Data_2022-09-28.csv"), skip=0, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#names(GHG_df)[names(GHG_df) == "Bottle_Number"] <- "Vial_no"
#sample_data$Bottle_Number <- as.numeric(sample_data$Bottle_Number)

sample_data <- sample_data[,c("Bottle_Number","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")]


df_merge <- left_join(df_merge,sample_data,by="Bottle_Number")

#Methane <- df_merge%>%drop_na(Bottle_Number)%>%drop_na(AquaticSystem)%>%filter(AquaticSystem=="wetland")
#Methane <- rename(Methane, Vial_no = Bottle_Number)
#Methane$Vial_no <- as.integer(Methane$Vial_no)

#methane processing
processing <-  read.csv(here::here("Methane/Methane_Processing_Info.csv"), skip=6, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")

processing <- processing[,c("Vial_n0_9ml","Date.headspace.introduced","Time.headspace.introduced",
                                                      "Extract.Headspace.Date","Extract.Headspace.time","headspace_ml")]

#rename column names

processing <- processing %>% dplyr::rename(Bottle_Number = Vial_n0_9ml)%>%
  drop_na(headspace_ml)
processing$Bottle_Number <- as.integer(processing$Bottle_Number)

df_merge <- left_join(df_merge,processing, by="Bottle_Number")

#Methane <- Methane[c("Wetland","Location","Time","Date","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")]  
#colnames(Methane) <- c("Wetland","Location","Time_CH4","Date","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")  


#environmental data
enviroData <- read.csv(here::here("wetlands_df_2022-10-18.csv"))
enviroData <- enviroData[,c("Wetland","Location","Date","Watertemp_c","AirPress_kpa","AirTemp_C")]
enviroData$Date <- as.Date(enviroData$Date, format = "%m/%d/%y")
enviroData$Location <- NULL
enviroData <- unique(enviroData)
enviroData <- enviroData%>%filter(Date< as.Date("2022-10-01"))

##merge 

df <- left_join(df_merge,enviroData, by=c("Wetland","Date"))


#read out

write.csv(df, here::here("methane_df_2023-02-13.csv"), row.names = FALSE)

