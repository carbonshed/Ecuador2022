#wetland depth vs surface area relationships
#Wetland 02
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

#notes
#we are going to call water_level < -0.06, depth = 0  
#total change in water_level = 0.8265639+.06 = 0.8865639
#max extent == 5836.5
#Also, Depth == 0 between "2023-02-03 13:30:00" & "2023-02-21 00:00:00"

Station_name <- "WL_Wetland02"

WL_df <- read.csv(here::here("Kriddie/WL_Wetland_ALL.csv"))
WL_df <- WL_df%>%filter(Station == Station_name)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")
#corrections for depth
WL_df$depth_diff_m <- -.06
WL_df$depth_ave_m <- WL_df$WaterLevel_m - WL_df$depth_diff_m


df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)

df_DSM <- read.csv(here::here("Wetlands/HistogramofDSM_06162022.csv"))
#df_DSM <- read.csv(here::here("Wetlands/HistogramofDSM_20230128.csv"))
#df_DSM <- read.csv(here::here("Wetlands/DSM_20230128_manual.csv"))

df_merge1 <- df%>%select(c(WaterLevel_m,Area))
df_merge1$method <- "Manual"
df_merge2 <- df_DSM%>%select(c(WaterLevel_m,Total_Surface_aream2))
colnames(df_merge2) <- c("WaterLevel_m","Area")
df_merge2$method <- "DSM"
df_merge <- rbind(df_merge1,df_merge2)

ggplot(data = df_merge%>%filter(WaterLevel_m<3)
       , aes(x = WaterLevel_m, y = Area, color=method)) + 
  geom_point(size=3)

ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue", size=1) +
  geom_hline(yintercept=max(df$WaterLevel_m), linetype="dashed", color = "red",size=1)+ 
  geom_hline(yintercept=min(df$WaterLevel_m), linetype="dashed", color = "red",size=1)+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted",size=1)

plot_ly(data = WL_df%>%filter(DateTime>as.POSIXct("2023-01-10 00:00:00")&
                                DateTime<as.POSIXct("2023-03-03 00:00:00")), x = ~DateTime, y = ~WaterLevel_m)

#percent change water level 
(max(WL_df$WaterLevel_m,na.rm = TRUE)-min(WL_df$WaterLevel_m,na.rm = TRUE))/
  (max(WL_df$WaterLevel_m,na.rm = TRUE)+min(WL_df$WaterLevel_m,na.rm = TRUE))/2 * 100

#percent surface area change
(max(df$Area,na.rm = TRUE) - min(df$Area,na.rm = TRUE))/
  (max(df$Area,na.rm = TRUE) + min(df$Area,na.rm = TRUE))/2 * 100

#percent outside rating curve
count(WL_df%>%
        filter(WaterLevel_m > max(df$WaterLevel_m,na.rm = TRUE) |
                 WaterLevel_m < min(df$WaterLevel_m,na.rm = TRUE))) / count(WL_df%>%na.omit(WaterLevel_m)) * 100

##
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)

ggplot(data = df_DSM, aes(x = WaterLevel_m, y = Total_Surface_aream2)) + 
  geom_point(size=3)



#####Rating curve
 
 
#Wetland 02
# based on looking at the long term data, we will say depth = 0 , surface area = 0 at water level = 
#max: 5836.468147


#on DSM created 6/16/2022, 4248.31 altitude = 0.1217370732 m in water level
WL_06162022 <- df%>%filter(Date=="2022-06-16")%>%select(WaterLevel_m)
WL_06162022 <- WL_06162022[1,1]
Area_06162022 <- df%>%filter(Date=="2022-06-16")%>%select(Area)
Area_06162022 <- Area_06162022[1,1]
#Based on imagery from 6/16/2022, the lowest land area not covered by water is 4248.31m

#so lets increase by increments of .1to see where it falls on the rating curve
# 4248.31m + .1m = 4248.41m




# y=a+b*ln(x) 
 #fit the model
 
df_1 <- df%>%filter(Station=="WL_Wetland02")
weight_values <- c(1,1,1,1,1,1)
df_1$WaterLevel_m_adjust <- df_1$WaterLevel_m -  df_1[5,"WaterLevel_m"]
lm_log1p <- lm(Area~log1p(WaterLevel_m_adjust)-1, data=df_1,weight=weight_values)
summary(lm_log1p)

#plot

x_exp <- seq(from = -.3, to = 0.6, by = .01)
#y_exp = lm_log1p$coefficients[1] +  lm_log1p$coefficients[2] * log1p(x_exp)
y_exp = lm_log1p$coefficients[1] * log1p(x_exp)

plot_ly(x = df_1$WaterLevel_m_adjust, y=df_1$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_exp, y=y_exp)
#

df_1$WaterLevel_m_adjust <- df_1$WaterLevel_m -  df_1[5,"WaterLevel_m"]

lm_poly2 <- lm(data = df_1, formula = WaterLevel_m ~ Area + I(Area^2)-1)
summary(lm_poly2)

x_poly <- seq(from = -10, to = 5000, by = 1)
y_poly =  lm_poly2$coefficients[1]*x_poly + lm_poly2$coefficients[2]*x_poly^2

plot_ly(x = df_1$Area, y=df_1$WaterLevel_m)%>%
  add_markers(size=4)%>%
  add_lines(x = x_poly, y=y_poly)
