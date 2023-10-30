#wetland depth vs surface area relationships
#Wetland 12
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

#I didn't take home some of the polygons that Dani made of w10. 
#so maybe I'll need to go back and do this before rating curve is complete

Station_name <- "WL_Wetland12"

WL_df <- read.csv(here::here("Kriddie/WL_Wetland_ALL.csv"))
WL_df <- WL_df%>%filter(Station == Station_name)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)
depth_diff <- WL_df[1,]$depth_diff_m
df$depth_ave_m <- df$WaterLevel_m + depth_diff



ggplot(data = WL_df , aes(x=DateTime, y = WaterLevel_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$WaterLevel_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$WaterLevel_m), linetype="dashed", color = "red")+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted")

####################
#####Rating curve ##
####################
ggplot(data = df , aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)

##
#We didn't measure depth in this wetland, nore and attmepts at measuring surface area more than once was tricky...
#Its just a different wetland, way more complex
#we will just say it is a constant surface area throughout the year



##Formula!

WL_df_2 <- WL_df
WL_df_2$surface_area_m2 <- mean(df$Area, na.rm=TRUE)

#surface are to volumn ratio. Can't do this, no depth
WL_df_2$Volumn_m3 <- NA
WL_df_2$SA_to_Vol_ratio <- NA


ggplot(data = WL_df_2, aes(x = DateTime, y = surface_area_m2)) + 
  geom_point(size=1)


#write out final data frame
#write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland12_FINAL.csv"))
