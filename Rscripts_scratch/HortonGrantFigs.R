#Horton Grant figures?


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

#

## Merge with data 
#sample_data <-  read.csv(here::here("Methane/Methane_Data_2022-08-04.csv"), skip=0, header = TRUE, sep = ",",
#                      quote = "\"",dec = ".", fill = TRUE, comment.char = "")
sample_data <-  read.csv(here::here("Methane/GHG_Data_2022-09-28.csv"), skip=0, header = TRUE, sep = ",",
                         quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#names(GHG_df)[names(GHG_df) == "Bottle_Number"] <- "Vial_no"
#sample_data$Bottle_Number <- as.numeric(sample_data$Bottle_Number)

sample_data <- sample_data[,c("Bottle_Number","CH4.ppm.NOT.CORRECTED","N20.ppm.NOT.CORRECTED")]


df_merge <- full_join(df_merge,sample_data,by="Bottle_Number")
df_merge <- df_merge %>% 
  mutate(across(where(is.character), str_trim))

river_df <- df_merge%>%filter(AquaticSystem == "river")
river_df$Location <- as.factor(river_df$Location)

ggplot(river_df,aes(x=Location,y=CH4.ppm.NOT.CORRECTED, color = Wetland)) + geom_boxplot()

ggplot(river_df%>%filter(Wetland=="Gavi-main"),aes(x=Date,y=CH4.ppm.NOT.CORRECTED, color = Location)) + geom_point()

####do