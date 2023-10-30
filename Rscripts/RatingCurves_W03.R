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
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)
depth_diff <- WL_df[1,]$depth_diff_m
df$depth_ave_m <- df$WaterLevel_m + depth_diff

ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$depth_ave_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$depth_ave_m), linetype="dashed", color = "red")+
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



#Add in DSM
#pixel size 0.0390099999999868*0.039009999999101
df_DSM <- read.csv(here::here("Wetlands/W03_DSM_20221101.csv"))

df_merge1 <- df%>%select(c(depth_ave_m,Area))
df_merge1$method <- "Manual"
df_merge2 <- df_DSM%>%select(c(WaterLevel_m,Total_Surface_aream2))
colnames(df_merge2) <- c("depth_ave_m","Area")
df_merge2$method <- "DSM"
df_merge <- rbind(df_merge1,df_merge2)
df_merge <- df_merge%>%filter(depth_ave_m<1)

ggplot(data = df_merge
       , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)
##
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
   geom_point(size=3)

#####Rating curve
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)

#look at highest wl area again, but for now, it looks like this should be a straight line :(


#formula 
WL_df$surface_area_m2 <- mean(df$Area, na.rm=TRUE)

#surface are to volumn ratio
WL_df$Volumn_m3 <- WL_df$surface_area_m2*WL_df$depth_ave_m
WL_df$SA_to_Vol_ratio <- WL_df$surface_area_m2/WL_df$Volumn_m3


ggplot(data = WL_df, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=1)

ggplot(data = WL_df, aes(x = DateTime, y = surface_area_m2)) + 
  geom_point(size=1)

ggplot(data = WL_df, aes(x = DateTime, y = Volumn_m3)) + 
  geom_point(size=1)

plot_ly(data=WL_df, x = ~DateTime, y = ~SA_to_Vol_ratio)#%>%add_markers(size=1)


#write out final data frame
#write.csv(WL_df, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland03_FINAL.csv"))


