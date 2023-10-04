#wetland depth vs surface area relationships
#Wetland 05
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

ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue") +
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

#####Rating curve
ggplot(data = df, aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)

###RATING CURVES 
#df$WaterLevel_m_adjust <- df$WaterLevel_m - df$WaterLevel_m[df$Date=="2022-12-02"]
#df_1 <- df%>%filter(WaterLevel_m_adjust >= 0)

lm_poly2 <- lm(data = df_1, formula = Area ~ WaterLevel_m_adjust + I(WaterLevel_m_adjust^2)-1)
summary(lm_poly2)

x_poly <- seq(from = 0, to = .5, by = 0.01)
y_poly =  lm_poly2$coefficients[1]*x_poly + lm_poly2$coefficients[2]*x_poly^2

plot_ly(x = df$WaterLevel_m_adjust, y=df$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_poly, y=y_poly)


##Formula!
lm_poly2$coefficients[1]*x_poly + lm_poly2$coefficients[2]*x_poly^2 - df$WaterLevel_m[df$Date=="2022-12-02"]

# > 0.143529
# 1190.659*waterlevel_m^2 + 1544.041*waterlevel_m - 0.143529

# <= 0.143529
# 0

