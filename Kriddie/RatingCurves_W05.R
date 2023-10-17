#wetland depth vs surface area relationships
#Wetland 05
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

Station_name <- "WL_Wetland05"

WL_df <- read.csv(here::here("Kriddie/WL_Wetland_ALL.csv"))
WL_df <- WL_df%>%filter(Station == Station_name)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)
depth_diff <- WL_df[1,]$depth_diff_m
df$depth_ave_m <- df$WaterLevel_m - depth_diff


#Add in DSM
df_DSM <- read.csv(here::here("Wetlands/W05_DSM_20221101.csv"))

df_merge1 <- df%>%select(c(depth_ave_m,Area))
df_merge1$method <- "Manual"
df_merge2 <- df_DSM%>%select(c(WaterLevel_m,Total_Surface_aream2))
colnames(df_merge2) <- c("depth_ave_m","Area")
df_merge2$method <- "DSM"
df_merge <- rbind(df_merge1,df_merge2)

##honestly, DSM doesn't seem so useful in this case

ggplot(data = WL_df , aes(x=DateTime, y = WaterLevel_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$WaterLevel_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$WaterLevel_m), linetype="dashed", color = "red")+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted")


#dif between max and min depth: 0.7043931-0.4101765=0.2942166

#####Rating curve
ggplot(data = df , aes(x = depth_ave_m, y = Area, color=Date)) + 
  geom_point(size=3)


#low
lm_poly2 <- lm(data = df, formula = Area ~ depth_ave_m + I(depth_ave_m^2))
summary(lm_poly2)

x_poly <- seq(from = .4, to = .5, by = 0.001)
y_poly =  lm_poly2$coefficients[1] + lm_poly2$coefficients[2]*x_poly + lm_poly2$coefficients[3]*x_poly^2

plot_ly(data=df, x = ~depth_ave_m, y = ~Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_poly, y=y_poly)

## high values would be different... maybe linear??
##I don't like linear.
#log
df_high <- df%>%filter(depth_ave_m > 0.428740)
lm_ln <- lm(data=df_high,Area ~ log(depth_ave_m))
summary(lm_ln)

x_ln <- seq(from = .3, to = .8, by = .001)
y_ln = lm_ln$coefficients[1] + lm_ln$coefficients[2]*log(x_ln)

plot_ly(x = df_high$depth_ave_m, y=df_high$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_ln, y=y_ln)


##together
plot_ly(x = df$depth_ave_m, y=df$Area)%>%
  add_lines(x=x_poly, y=y_poly)%>%
  add_lines(x = x_ln, y=y_ln)%>%
  add_markers(size=5)%>%
  layout(
    yaxis = list(title = "Surface Area (m^2)", titlefont = list(size = 20), tickfont = list(size = 20)),
    xaxis = list(title = "Depth (m)", titlefont = list(size = 20), tickfont = list(size = 20))
  )


##Formula!
intercept <- 0.4446

WL_df_low <- WL_df %>%filter(depth_ave_m <= intercept)
WL_df_low$surface_area_m2 <- lm_poly2$coefficients[1] + lm_poly2$coefficients[2]*WL_df_low$depth_ave_m + lm_poly2$coefficients[3]*WL_df_low$depth_ave_m^2 

WL_df_high <- WL_df%>%filter(depth_ave_m > intercept)
WL_df_high$surface_area_m2 <- lm_ln$coefficients[1] + lm_ln$coefficients[2]*log(WL_df_high$depth_ave_m)


WL_df_2 <- rbind(WL_df_low,WL_df_high)

#surface are to volumn ratio
WL_df_2$Volumn_m3 <- WL_df_2$surface_area_m2*WL_df_2$depth_ave_m
WL_df_2$SA_to_Vol_ratio <- WL_df_2$surface_area_m2/WL_df_2$Volumn_m3

ggplot(data = WL_df_2, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=2)

ggplot(data = WL_df_2, aes(x = DateTime, y = Volumn_m3)) + 
  geom_point(size=2)

ggplot(data = WL_df_2, aes(x = DateTime, y = SA_to_Vol_ratio)) + 
  geom_point(size=2)


plot_ly(data=WL_df_2, x = ~DateTime, y = ~surface_area_m2)#%>%add_markers(size=1)


#write out final data frame
#write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland05_FINAL.csv"))
