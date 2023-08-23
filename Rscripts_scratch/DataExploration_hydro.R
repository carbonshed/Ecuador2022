###
library(here)
library(dplyr)
library(tidyr)

WLData <- read.csv(here::here("Kriddie/WL_data.csv"))
WLData$DateTime <- as.POSIXct(WLData$DateTime, tz="UTC", Formats = c("%Y-%m-%d %H:%M:%S"))
WLData <- WLData%>%drop_na(Station)


precip_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Precipitación_subhorario-validado.csv"))
colnames(precip_df) <- c("DateTime","precipt_mm")
precip_df$DateTime <- as.POSIXct(precip_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
#precip_df$Date <- as.Date(precip_df$DateTime,format="%Y-%m-%d")
#precip_summary <- precip_df%>%group_by(Date)%>%summarise(PrecipAccuDay_mm = sum(precipt_mm))
#precip_summary$Date <- precip_summary$Date - 1

df_hydro <- left_join(WLData,precip_df,by="DateTime")



ggplot(df_hydro%>%filter(Station=="WL_Wetland02" & DateTime < as.POSIXct("2022-07-31 23:00:00"))) +
  geom_line(aes(x=DateTime,y=WaterLevel_m)) +
  geom_point(aes(x=DateTime,y=precipt_mm)) +
  theme(text=element_text(size=20))

#read in Holgerston & Raymond data set
df_Holg <- read.csv(here::here("Wetlands/Holgerson&Raymond_data1.csv"))
df_merge <- df
df_merge$Reference <- "This Study"
df_merge$Site.Name <- df_merge$Wetland
df_merge$Location <- "La Virgen"
df_merge$latitude <- -0.324311
df_merge$longitude <- -78.1995
df_merge$Study.Years <- 2022
df_merge$area..ha. <- NA
df_merge$ch4..umol.L. <- df_merge$CH4_umol.L
df_merge$co2..umol.L. <- df_merge$CO2_umol.L
df_merge$temp..C. <- df_merge$Watertemp_c
df_merge$temp..estimate. <- NA
df_merge$atm..ch4 <- NA
df_merge$atm..co2 <- df_merge$CO2_ppm
df_merge <- df_merge%>%select(Reference,Site.Name,Location,latitude,longitude,Study.Years,area..ha.,ch4..umol.L.,co2..umol.L.,temp..C.,atm..co2,temp..estimate.,atm..ch4)

df_Holg <- rbind(df_Holg,df_merge)
df_Holg$ch4..umol.L. <- as.numeric(df_Holg$ch4..umol.L.)
df_Holg$co2..umol.L. <- as.numeric(df_Holg$co2..umol.L.)

colnames(df_Holg) <- c("Reference","Site_Name","Location","latitude","longitude","Study_Years,","Area_ha","CH4_umol.L","CO2_umol.L",
                       "Teemp_c","temp_est","atm_CO2","atm_CH4")

ggplot(df_Holg) +
  geom_point(aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference)) 

#co2 v ch4
ggplot() +
  geom_point(data=df_Holg,aes(x=CO2_umol.L,y=CH4_umol.L),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=CO2_umol.L,y=CH4_umol.L,color=Site_Name),size=3) 
  theme_bw()

#log co2 v ch4
ggplot() +
  geom_point(data=df_Holg,aes(x=log(CO2_umol.L),y=log(CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=log(CO2_umol.L),y=log(CH4_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 

ggplot() +
  geom_point(data=df_Holg,aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=CO2_umol.L,y=CH4_umol.L,shape='Site Name'),size=3)
#log co2 v lat
ggplot() +
  geom_point(data=df_Holg ,aes(x=latitude,y=log(CO2_umol.L),color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CO2_umol.L),shape='Site Name'),size=3)
#log CH4 v lat
ggplot() + 
  geom_point(data=df_Holg ,aes(x=latitude,y=log(CH4_umol.L),color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CH4_umol.L),shape='Site Name'),size=3)
#log c02/ch4 v lat
ggplot() + 
  geom_point(data=df_Holg ,aes(x=latitude,y=log(CO2_umol.L/CH4_umol.L),color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CO2_umol.L/CH4_umol.L),shape='Site Name'),size=3)

