#wetland depth vs surface area relationships
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$Date <- as.Date(df$Date)

 ggplot(data = df%>%filter(Station == "WL_Wetland02"), aes(x = WaterLevel_m, y = Area, color=Date)) + 
   geom_point(size=3)

 #notes
 #Wetland 1 and 3, revisit
 #I feel certain that, at least one day, wetland 7 was completly dry, but I don't know what day that was
 # wetland 10, revisit
 # did I ever fly both wetland 7 and 11, and 11 was missed?
 
 # also, draw those redlines on rating curves.
 
 
 
 #wetland 1
 
 
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
