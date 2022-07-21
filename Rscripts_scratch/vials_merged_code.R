#Vial merge code
#kriddie
#7/20/2022

library(here)
library(dplyr)
library(tidyr)


Vial_Vol <-  read.csv(here::here("Methane/Vial_volume_6-13-22.csv"), skip=0, header = TRUE, sep = ",",
                  quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Vial_Vol <- Vial_Vol[,c(1:5)]


Vial_loc <-  read.csv(here::here("Methane/Vial_sample_location.csv"), skip=0, header = TRUE, sep = ",",
                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")

Vial_loc <- Vial_loc[,c(1:7)]
colnames(Vial_loc) <- c("Wetland","Bottle_Number","Location","Time","Date","Notes","Notes2")

exsist <-read.csv(here::here("Methane/Vials'that'exist.csv"), skip=0, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#merge
df_merge <- full_join(Vial_Vol,Vial_loc,by="Bottle_Number")
df_merge <- full_join(df_merge,exsist,by="Bottle_Number")
df_merge$Date <- as.Date(df_merge$Date,format="%m/%d/%Y")

#drop_NA

df_merge_2 <- df_merge[,c("Bottle_Number","Zinc_Chloride","Date","Exsist","Sample_Collected")]

df_merge_2 <-df_merge_2 %>% drop_na(Date)

df_merge_2 <-df_merge_2 %>% drop_na(Exsist)


df_merge_2 <-df_merge_2 %>% drop_na(Zinc_Chloride)
