#wetland depth vs surface area relationships
#Wetland 02
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

Station_name <- "WL_Wetland02"

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
count(WL_df%>%
        filter(WaterLevel_m > max(df$WaterLevel_m,na.rm = TRUE) |
                 WaterLevel_m < min(df$WaterLevel_m,na.rm = TRUE))) / count(WL_df%>%na.omit(WaterLevel_m)) * 100

##
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)

#####Rating curve
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)
 
#notes
#draw those redlines on rating curves.
 
# also, 
 
#Wetland 02
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
