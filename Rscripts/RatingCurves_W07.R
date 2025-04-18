#wetland depth vs surface area relationships
#Wetland 07
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

Station_name <- "WL_Wetland07"

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
df_DSM <- read.csv(here::here("Wetlands/W07_DSM_20221120.csv"))

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

#looks pretty good
#But we do need to change the depth for the DSM to line up with the Manual.
# 1. make a linear regression between the two points that fall on either side of our lowest manual measurment of area, then use that to offset
#We will use 0 based on our field measurments of depth
# Range 0 to 0.2841456
#remember to set depth < than 0 to 0

# 1. y=mx+b
slope <- (646.61771600-589.41352950)/(0.488845242-0.438845242)
b <- 646.61771600 - slope * 0.488845242
# 2. where would the lowest manually collected point fall on line?
sample_y <- 604.13267100
sample_x <- (sample_y-b)/slope
diff <- sample_x - 0.004083008

df_merge[df_merge$method=="DSM",]$depth_ave_m <- df_merge[df_merge$method=="DSM",]$depth_ave_m - diff

ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$depth_ave_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$depth_ave_m), linetype="dashed", color = "red")+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted")

####################
#####Rating curve ##
####################
ggplot(data = df , aes(x = depth_ave_m, y = Area, color=Date)) + 
  geom_point(size=3)
ggplot(data = df_merge, aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)

##
#now we take all the dsm points that are higher than the highest manual point
df_merge_1 <- df_merge%>%filter(Area > 625|depth_ave_m < -1|method=="Manual")%>%
  filter(depth_ave_m<=0.2841456
)

ggplot(data = df_merge_1 , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)



##model

modelsr_1<-lm(data=df_merge_1, Area~sqrt(depth_ave_m)+depth_ave_m-1)
print(summary(modelsr_1))

x_sr_1 <- seq(from = 0, to = .25, by = .01)
y_sr_1 = modelsr_1$coefficients[1]*sqrt(x_sr_1) + modelsr_1$coefficients[2]*x_sr_1

plot_ly(x = df_merge_1$depth_ave_m, y=df_merge_1$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_sr_1, y=y_sr_1)


##Formula!
WL_df_low <- WL_df%>%filter(depth_ave_m<=0)
WL_df_low$surface_area_m2 <- 0
WL_df_low$depth_ave_m <- 0
WL_df_high <- WL_df%>%filter(depth_ave_m>0)
WL_df_high$surface_area_m2 <- 
  modelsr_1$coefficients[1]*sqrt(WL_df_high$depth_ave_m) +
  modelsr_1$coefficients[2]*WL_df_high$depth_ave_m

WL_df_2 <- rbind(WL_df_low,WL_df_high)

#surface are to volumn ratio
WL_df_2$Volumn_m3 <- WL_df_2$surface_area_m2*WL_df_2$depth_ave_m
WL_df_2$SA_to_Vol_ratio <- WL_df_2$surface_area_m2/WL_df_2$Volumn_m3
#set temperature to na when depth == 0
WL_df_2[WL_df_2$depth_ave_m==0,]$WLTemp_c <- NA

ggplot(data = WL_df_2, aes(x = DateTime, y = WLTemp_c)) + 
  geom_point(size=1)

ggplot(data = WL_df_2, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=1)

ggplot(data = WL_df_2, aes(x = DateTime, y = Volumn_m3)) + 
  geom_point(size=1)

ggplot(data = WL_df_2, aes(x = DateTime, y = surface_area_m2)) + 
  geom_point(size=1)

ggplot(data = WL_df_2, aes(x = DateTime, y = SA_to_Vol_ratio)) + 
  geom_point(size=1)

ggplot(data = WL_df_2, aes(x = DateTime, y = WLTemp_c)) + 
  geom_point(size=1)


plot_ly(data=WL_df_2, x = ~DateTime, y = ~SA_to_Vol_ratio)#%>%add_markers(size=1)


#write out final data frame
write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland07_FINAL.csv"))
