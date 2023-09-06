#wetland depth vs surface area relationships
library(here)

df <- read.csv(here::here("Wetlands/DroneFiles/Drone_flights_2023-08-25.csv"))[,1:6]
df$WL <- as.numeric(df$WL)

 ggplot(data = df%>%filter(Wetland == "Wetland_3"), aes(x = WL, y = Area, color=Date)) + 
   geom_point(size=3)

#notes
 #Wetland 1 and 3, revisit
 #I feel certain that, at least one day, wetland 7 was completly dry, but I don't know what day that was
 # wetland 10, revisit
 # did I ever fly both wetland 7 and 11, and 11 was missed?
 
 # also, draw those redlines on rating curves.
 
p2 <- ggplot(data = df, aes(x = WL, y = Area)) + geom_point()
p2 + facet_wrap(~Wetland)




fig1 <- plot_ly(data=wetland_data ,
        x=~DateTime,y=~WaterLevel_m)
#Water Temp
fig2 <- plot_ly(data=wetland_data,
        x=~DateTime,y=~WLTemp_c)
fig <- subplot(fig1, fig2,nrows = 2)
