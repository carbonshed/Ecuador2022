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

####################
#####Rating curve ##
####################
ggplot(data = df , aes(x = depth_ave_m, y = Area, color=Date)) + 
  geom_point(size=3)
ggplot(data = df_merge , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)

df_merge_1 <- df_merge%>%filter(depth_ave_m<.5)

modelsr_1<-lm(data=df_merge_1, Area~sqrt(depth_ave_m)+depth_ave_m-1)
print(summary(modelsr_1))

x_sr_1 <- seq(from = 0, to = .5, by = .01)
y_sr_1 = modelsr_1$coefficients[1]*sqrt(x_sr_1) + modelsr_1$coefficients[2]*x_sr_1

plot_ly(x = df_merge_1$depth_ave_m, y=df_merge_1$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_sr_1, y=y_sr_1)


##Formula!
WL_df_low <- WL_df%>%filter(depth_ave_m<=0)
WL_df_low$surface_area_m2 <- 0
WL_df_high <- WL_df%>%filter(depth_ave_m>0)
WL_df_high$surface_area_m2 <- 
  modelsr_1$coefficients[1]*sqrt(WL_df_high$depth_ave_m) +
  modelsr_1$coefficients[2]*WL_df_high$depth_ave_m

WL_df_2 <- rbind(WL_df_low,WL_df_high)

WL_df_low <- WL_df_2%>%filter(depth_ave_m<=0)
WL_df_low$depth_ave_m <- 0
WL_df_high <- WL_df_2%>%filter(depth_ave_m>0)

WL_df_3 <- rbind(WL_df_low,WL_df_high)

#surface are to volumn ratio
WL_df_3$Volumn_m3 <- WL_df_3$surface_area_m2*WL_df_3$depth_ave_m
WL_df_3$SA_to_Vol_ratio <- WL_df_3$surface_area_m2/WL_df_3$Volumn_m3

ggplot(data = WL_df_3, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=3)

ggplot(data = WL_df_3, aes(x = DateTime, y = Volumn_m3)) + 
  geom_point(size=3)

ggplot(data = WL_df_3, aes(x = DateTime, y = surface_area_m2)) + 
  geom_point(size=3)


plot_ly(data=WL_df_3, x = ~DateTime, y = ~SA_to_Vol_ratio)#%>%add_markers(size=1)


#write out final data frame
write.csv(WL_df_3, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland06_FINAL.csv"))
