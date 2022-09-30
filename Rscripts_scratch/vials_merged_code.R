#Vial merge code
#kriddie
#7/20/2022

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

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
df_merge$Date <- as.Date(df_merge$Date,format="%m/%d/%Y")

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

df_merge <- df_merge%>%drop_na(Bottle_Number)%>%drop_na(AquaticSystem)

#merge with processing info

processing_df <-  read.csv(here::here("Methane/Methane_Processing_Info.csv"), skip=6, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
names(processing_df)[names(processing_df) == "Sample.ID"] <- "Bottle_Number"

df_merge <- df_merge%>%drop_na(Bottle_Number)%>%drop_na(AquaticSystem)

#info we need to bind in
#site,date collected time collected. decimal time, uncorrecte headspace concntration (ppm),
#vol sample (L), vol headspace (L),
#Air pressure at elevation (atm), Air pressure at elevation (DCW barologger), Air temperature ©, Ambient concntration ppm

#convert water weight to volume
#1 milliliter (mL) of water weighs 1 gram (g). 
df_merge$vol_bottle_L <- df_merge$Filles_with_water/1000

df <- df_merge[,c("Bottle_Number","Zinc_Chloride","vol_bottle_L","AquaticSystem","Wetland","Location","Date","Time",
                  "CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")]

colnames(df) <- df[,c("Bottle_Number","Zinc_Chloride_g","vol_bottle_L","AquaticSystem","Wetland","Location","Date_collection","Time_collection",
                            "CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")]


################
#####PLOT#######
################

# Basic box plot
p <- ggplot(df_merge, aes(x=AquaticSystem, y=CH4.ppm.NOT.CORRECTED)) + 
  geom_boxplot()
p

df_wetland <- df_merge%>%subset(AquaticSystem == "wetland")

p <- ggplot(df_merge%>%
              subset(AquaticSystem == "wetland")%>%
              subset(Wetland = "1"|"2"|"3"), aes(x=Wetland, y=CH4.ppm.NOT.CORRECTED)) + 
  geom_boxplot()

#p <- ggplot(df_merge %>%subset(AquaticSystem == "wetland"), aes(x=as.factor(Date), y=CH4.ppm.NOT.CORRECTED)) + 
#  geom_boxplot()=

#n20 data
p <- ggplot(df_merge, aes(x=AquaticSystem, y=N20.ppm.NOT.CORRECTED)) + 
  geom_boxplot()
p

###############################
#### use this to show DIEGO ####
############################### 

p <- ggplot(df_merge %>%subset(AquaticSystem == "wetland")%>%
              subset(Wetland == "10"| Wetland == "11"| Wetland =="12")
          #  %>% subset(CH4.ppm.NOT.CORRECTED < 200)
            , 
            aes(x=as.factor(Date), y=CH4.ppm.NOT.CORRECTED)) + 
  geom_point(#fill="red",
    aes(fill=Location),
    shape=21, size = 3) +
  theme_linedraw()
p+ facet_wrap(~Wetland, ncol = 3)
#############################


p <- ggplot(df_merge%>%subset(AquaticSystem == "wetland"), aes(x=Rep, y=CH4.ppm.NOT.CORRECTED)) + 
  geom_point()
p+ facet_wrap(~Wetland, ncol = 3)

df_merge%>%subset(AquaticSystem == "wetland")%>%
  mutate(across(Wetland, factor, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))) %>%
  ggplot() + 
  geom_point(aes(Rep,CH4.ppm.NOT.CORRECTED))+ 
  facet_wrap(~Wetland, ncol=3, scales = "free") +
  theme_bw()


df_merge%>%subset(AquaticSystem == "river")%>%
#  mutate(across(Wetland, factor, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))) %>%
  ggplot() + 
  geom_point(aes(Wetland,CH4.ppm.NOT.CORRECTED))+ 
#  facet_wrap(~Wetland, ncol=3, scales = "free") +
  theme_bw()

