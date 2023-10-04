#wetland depth vs surface area relationships
#Wetland 06
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

Station_name <- "WL_Wetland06"

WL_df <- read.csv(here::here("Kriddie/WL_Wetland_ALL.csv"))
WL_df <- WL_df%>%filter(Station == Station_name)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)
depth_diff <- WL_df[1,]$depth_diff_m
df$depth_ave_m <- df$WaterLevel_m + depth_diff


#Add in DSM
df_DSM <- read.csv(here::here("Wetlands/W06_DSM_20221120.csv"))

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

#wow that looks amazing
#we do need to adjust depth, we can set it to depth = 0 at the manual reading of surface area = 0
#(this is only a cm off from our field depth measurements wich is pretty cool)

depth_diff_2 <- df_merge[df_merge$Area==0,]$depth_ave_m
df_merge$depth_diff <- depth_diff_2
df_merge$depth_ave_m <- df_merge$depth_ave_m - df_merge$depth_diff

# dif between max and min depth: 0.4061234
depth_diff_Final <- df[df$Area==0,]$WaterLevel_m
WL_df$depth_diff_m <- depth_diff_Final
WL_df$depth_ave_m <- WL_df$WaterLevel_m - WL_df$depth_diff_m

ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$WaterLevel_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$WaterLevel_m), linetype="dashed", color = "red")+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted")

#####Rating curve
ggplot(data = df , aes(x = depth_ave_m, y = Area, color=Date)) + 
  geom_point(size=3)
ggplot(data = df_merge , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)

df_merge_1 <- df_merge%>%filter(depth_ave_m<.5)

#low

lm_poly2 <- lm(data = df_merge_1, formula = Area ~ depth_ave_m + I(depth_ave_m^2)-1)
summary(lm_poly2)

x_poly <- seq(from = 0, to = .5, by = 0.01)
y_poly =  lm_poly2$coefficients[1]*x_poly + lm_poly2$coefficients[2]*x_poly^2

plot_ly(data=df_merge_1, x = ~depth_ave_m, y = ~Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_poly, y=y_poly)


lm_poly3 <- lm(data = df_merge_1, formula = Area ~ depth_ave_m + I(depth_ave_m^2) + I(depth_ave_m^3)-1)
summary(lm_poly3)

x_poly <- seq(from = 0, to = 0.4061234, by = 0.001)
y_poly =  lm_poly3$coefficients[1]*x_poly + lm_poly3$coefficients[2]*x_poly^2 + lm_poly3$coefficients[3]*x_poly^3

plot_ly(data=df_merge_1, x = ~depth_ave_m, y = ~Area)%>%
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

#log
lm_ln <- lm(data=df_merge_1,Area ~ log1p(depth_ave_m)-1)
summary(lm_ln)

x_ln <- seq(from = 0, to = .5, by = .001)
y_ln =  lm_ln$coefficients[2]*log1p(x_ln)

plot_ly(x = df_merge_1$depth_ave_m, y=df_merge_1$Area)%>%
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

ggplot(data = WL_df_2, aes(x = DateTime, y = surface_area_m2)) + 
  geom_point(size=1)

plot_ly(data=WL_df_2, x = ~DateTime, y = ~surface_area_m2)#%>%add_markers(size=1)


#write out final data frame
write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland05_FINAL.csv"))
