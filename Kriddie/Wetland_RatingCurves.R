#wetland depth vs surface area relationships
library(here)

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
 data <- df%>%filter(Station == "WL_Wetland02")
 data$WaterLevel_m_1 <- data$WaterLevel_m +1
 lm_log <- lm(WaterLevel_m_1~log(Area), data)
 plot(WaterLevel_m_1~yArea, data)
 curve(coef(lm_log)[1] + 
         coef(lm_log)[2]*log(WaterLevel_m_1), 
       add=TRUE, 
       col = "red")

fig1 <- plot_ly(data=wetland_data ,
        x=~DateTime,y=~WaterLevel_m)
#Water Temp
fig2 <- plot_ly(data=wetland_data,
        x=~DateTime,y=~WLTemp_c)
fig <- subplot(fig1, fig2,nrows = 2)
