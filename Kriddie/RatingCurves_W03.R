#wetland depth vs surface area relationships
#Wetland 03
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

Station_name <- "WL_Wetland03"

WL_df <- read.csv(here::here("Kriddie/WL_Wetland_ALL.csv"))
WL_df <- WL_df%>%filter(Station == Station_name)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$Date <- as.Date(df$Date)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%Y-%m-%d %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)

ggplot(data = WL_df , aes(x=DateTime, y = WaterLevel_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$WaterLevel_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$WaterLevel_m), linetype="dashed", color = "red")+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted")

#percent change water level 
(max(WL_df$WaterLevel_m,na.rm = TRUE)-min(WL_df$WaterLevel_m,na.rm = TRUE))/
  (max(WL_df$WaterLevel_m,na.rm = TRUE)+min(WL_df$WaterLevel_m,na.rm = TRUE))/2 * 100

#percent surface area change
(max(df$Area,na.rm = TRUE) - min(df$Area,na.rm = TRUE))/
  (max(df$Area,na.rm = TRUE) + min(df$Area,na.rm = TRUE))/2 * 100

#percent outside rating curve
count(WL_df%>%filter(WaterLevel_m > max(df$WaterLevel_m,na.rm = TRUE) |
                       WaterLevel_m < min(df$WaterLevel_m,na.rm = TRUE))) / count(WL_df%>%na.omit(WaterLevel_m)) * 100

##
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
   geom_point(size=3)

#####Rating curve
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)

#look at highest wl area again, but for now, it looks like this should be a straight line :(
surface_area <- mean(df$Area)