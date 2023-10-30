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
#WL_df$depth_diff_m <- -.06
#WL_df$depth_ave_m <- WL_df$WaterLevel_m - WL_df$depth_diff_m

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)

#df_DSM <- read.csv(here::here("Wetlands/HistogramofDSM_06162022.csv"))
df_DSM <- read.csv(here::here("Wetlands/HistogramofDSM_20230128.csv"))
#df_DSM <- read.csv(here::here("Wetlands/DSM_20230128_manual.csv"))

df_merge1 <- df%>%select(c(WaterLevel_m,Area))
df_merge1$method <- "Manual"
df_merge2 <- df_DSM%>%select(c(WaterLevel_m,Total_Surface_aream2))
colnames(df_merge2) <- c("WaterLevel_m","Area")
df_merge2$method <- "DSM"
df_merge2$WaterLevel_m <- df_merge2$WaterLevel_m - 3.1 + 0.371258303
df_merge <- rbind(df_merge1,df_merge2)

df_merge[df_merge$WaterLevel_m < 0.4 & df_merge$method=="DSM",]$WaterLevel_m <- NA

df_merge <- df_merge%>%filter(WaterLevel_m<1.5)

ggplot(data = df
       , aes(x = WaterLevel_m, y = Area, color=Date)) + 
  geom_point(size=3)+                                     
  theme_bw(base_size = 16) 

ggplot(data = df_merge%>%filter(WaterLevel_m<1)%>%filter(WaterLevel_m>=-.01)
       , aes(x = WaterLevel_m, y = Area, color=method)) + 
  geom_point(size=3)+                                     
  theme_bw(base_size = 16) 


ggplot(data = WL_df , aes(x=DateTime, y = WaterLevel_m)) + geom_line(color="blue", size=1) +
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


#####Rating curve

 
#Wetland 02
# based on looking at the long term data, we will say depth = 0 , surface area = 0 at water level = -0.061409001
#Based on imagery from 6/16/2022, the lowest land area not covered by water is 4248.31m
#max surface area: 5836.468147

df_merge$depth_ave_m <- df_merge$WaterLevel_m - (-0.061409001)

ggplot(data = df_merge%>%filter(depth_ave_m<1)%>%filter(depth_ave_m>=0)
       , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)

#square root
df_merge_low <- df_merge%>%filter(depth_ave_m<.9)
modelsr_1<-lm(data=df_merge_low, Area~sqrt(depth_ave_m)+depth_ave_m-1)
print(summary(modelsr_1))

x_sr_1 <- seq(from = -.01, to = .9, by = .01)
#y_exp = lm_log1p$coefficients[1] +  lm_log1p$coefficients[2] * log1p(x_exp)
y_sr_1 = modelsr_1$coefficients[1]*sqrt(x_sr_1) + modelsr_1$coefficients[2]*x_sr_1

plot_ly(x = df_merge_low$depth_ave_m, y=df_merge_low$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_sr_1, y=y_sr_1)


## high values would be different... maybe linear??
df_merge_high <- df_merge%>%filter(depth_ave_m>.4)
modelsr_2<-lm(data=df_merge_high, Area~sqrt(depth_ave_m)+depth_ave_m)
print(summary(modelsr_2))

x_sr_2 <- seq(from = .3, to = 1.5, by = .01)
#y_exp = lm_log1p$coefficients[1] +  lm_log1p$coefficients[2] * log1p(x_exp)
y_sr_2 = modelsr_2$coefficients[1] + modelsr_2$coefficients[2]*sqrt(x_sr_2) + modelsr_2$coefficients[3]*x_sr_2

plot_ly(x = df_merge_high$depth_ave_m, y=df_merge_high$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_sr_2, y=y_sr_2)

##together


plot_ly(x = df_merge$depth_ave_m, y=df_merge$Area)%>%
  add_lines(x=x_sr_1, y=y_sr_1)%>%
  add_lines(x = x_sr_2, y=y_sr_2)%>%
  add_markers(size=5)%>%
  layout(
    yaxis = list(title = "Surface Area (m^2)", titlefont = list(size = 20), tickfont = list(size = 20)),
    xaxis = list(title = "Depth (m)", titlefont = list(size = 20), tickfont = list(size = 20))
  )


####THAT LOOKS GREAT
intercept <- .3247
WL_df_low <- WL_df%>%filter(depth_ave_m<=intercept)
WL_df_low$surface_area_m2 <- 
  modelsr_1$coefficients[1]*sqrt(WL_df_low$depth_ave_m) +
  modelsr_1$coefficients[2]*WL_df_low$depth_ave_m
WL_df_high <- WL_df%>%filter(depth_ave_m > intercept)
WL_df_high$surface_area_m2 <- 
  modelsr_2$coefficients[1] + 
  modelsr_2$coefficients[2]*sqrt(WL_df_high$depth_ave_m) +
  modelsr_2$coefficients[3]*WL_df_high$depth_ave_m
  

WL_df_2 <- rbind(WL_df_low,WL_df_high)

#surface are to volumn ratio
WL_df_2$Volumn_m3 <- WL_df_2$surface_area_m2*WL_df_2$depth_ave_m
WL_df_2$SA_to_Vol_ratio <- WL_df_2$surface_area_m2/WL_df_2$Volumn_m3

ggplot(data = WL_df_2, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=3)

ggplot(data = WL_df_2, aes(x = DateTime, y = Volumn_m3)) + 
  geom_point(size=3)

ggplot(data = WL_df_2, aes(x = DateTime, y = SA_to_Vol_ratio)) + 
  geom_point(size=3)


plot_ly(data=WL_df_2, x = ~DateTime, y = ~surface_area_m2)#%>%add_markers(size=1)

#set temperature to na when depth == 0
WL_df_2[WL_df_2$depth_ave_m==0,]$WLTemp_c <- NA

#write out final data frame
write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland02_FINAL.csv"))
