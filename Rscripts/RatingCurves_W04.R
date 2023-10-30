#wetland depth vs surface area relationships
#Wetland 04
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

Station_name <- "WL_Wetland04"

WL_df <- read.csv(here::here("Kriddie/WL_Wetland_ALL.csv"))
WL_df <- WL_df%>%filter(Station == Station_name)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)
#depth is close, but actually, depth = 0 needs to be a little different
#change ave depth
depth_diff <- df[df$Area==0 & df$Date=="12/2/2022",]$WaterLevel_m
df$depth_ave_m <- df$WaterLevel_m - depth_diff
WL_df$depth_diff_m <- depth_diff
WL_df$depth_ave_m <- WL_df$WaterLevel_m - WL_df$depth_diff_m



#Add in DSM
df_DSM <- read.csv(here::here("Wetlands/W04_DSM_20221101.csv"))

df_merge1 <- df%>%select(c(depth_ave_m,Area))
df_merge1$method <- "Manual"
df_merge2 <- df_DSM%>%select(c(WaterLevel_m,Total_Surface_aream2))
colnames(df_merge2) <- c("depth_ave_m","Area")
df_merge2$method <- "DSM"
df_merge <- rbind(df_merge1,df_merge2)

ggplot(data = df_merge#%>%filter(WaterLevel_m<1)%>%filter(WaterLevel_m>=-.01)
       , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)

#wow that looks amazing

ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue", size=1) +
  geom_hline(yintercept=max(df$depth_ave_m), linetype="dashed", color = "red",size=1)+ 
  geom_hline(yintercept=min(df$depth_ave_m), linetype="dashed", color = "red",size=1)+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted",size=1)

###RATING CURVES 
#total difference between min and max depth = 0.4490546
#lets get rid of point that it the lower depth, with Area = 0
df <- df%>%filter(depth_ave_m>=0)

ggplot(data = df_merge%>%filter(depth_ave_m<.45)#%>%filter(WaterLevel_m>=-.01)
       , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)

#
df_merge_1 <- df_merge%>%filter(depth_ave_m<.45)

lm_poly2 <- lm(data = df_merge_1, formula = Area ~ depth_ave_m + I(depth_ave_m^2)-1)
summary(lm_poly2)

x_poly <- seq(from = 0, to = .5, by = 0.01)
y_poly =  lm_poly2$coefficients[1]*x_poly + lm_poly2$coefficients[2]*x_poly^2

plot_ly(data=df_merge_1, x = ~depth_ave_m, y = ~Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_poly, y=y_poly)


##Formula!

####THAT LOOKS GREAT
WL_df_low <- WL_df%>%filter(depth_ave_m<=0)
WL_df_low$surface_area_m2 <- 0
WL_df_high <- WL_df%>%filter(depth_ave_m > 0)
WL_df_high$surface_area_m2 <-
  lm_poly2$coefficients[1]*WL_df_high$depth_ave_m + 
  lm_poly2$coefficients[2]*WL_df_high$depth_ave_m^2

WL_df_2 <- rbind(WL_df_low,WL_df_high)

#change all depth less than 0 to zero
WL_df_depth0 <- WL_df_2%>%filter(depth_ave_m<=0)
WL_df_depth0$depth_ave_m <- 0
WL_df_depthover0 <- WL_df_2%>%filter(depth_ave_m>0)

WL_df_3 <- rbind(WL_df_depth0,WL_df_depthover0)


#surface are to volumn ratio
WL_df_3$Volumn_m3 <- WL_df_3$surface_area_m2*WL_df_3$depth_ave_m
WL_df_3$SA_to_Vol_ratio <- WL_df_3$surface_area_m2/WL_df_3$Volumn_m3

#set temperature to zero when depth == 0
WL_df_3[WL_df_3$depth_ave_m==0,]$WLTemp_c <- NA

ggplot(data = WL_df_3, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=3)

ggplot(data = WL_df_3, aes(x = DateTime, y = Volumn_m3)) + 
  geom_point(size=3)

ggplot(data = WL_df_3, aes(x = DateTime, y = SA_to_Vol_ratio)) + 
  geom_point(size=3)

ggplot(data = WL_df_3, aes(x = DateTime, y = WLTemp_c)) + 
  geom_point(size=3)


plot_ly(data=WL_df_3, x = ~DateTime, y = ~surface_area_m2)#%>%add_markers(size=1)

#write out final data frame
#write.csv(WL_df_3, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland04_FINAL.csv"))

