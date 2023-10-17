#wetland depth vs surface area relationships
#Wetland 11
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

#you can use peggy's data to make another polygon

Station_name <- "WL_Wetland11"

WL_df <- read.csv(here::here("Kriddie/WL_Wetland_ALL.csv"))
WL_df <- WL_df%>%filter(Station == Station_name)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime, format="%Y-%m-%d %H:%M",tz="UTC")

df <- read.csv(here::here("Wetlands/SurfaceArea_df.csv"))
df <- df%>%select(Station,Date,Time_recoreded,Time_used,WaterLevel_m,Area,WLTemp_c)
df$DateTime <- as.POSIXct(paste(df$Date,df$Time_used),format="%m/%d/%Y %H:%M",tz="UTC")
df <- df%>%filter(Station == Station_name)
depth_diff <- WL_df[1,]$depth_diff_m
df$depth_ave_m <- df$WaterLevel_m + depth_diff


#i think, lets not worry about DSM. Too hard for this one


ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$depth_ave_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$depth_ave_m), linetype="dashed", color = "red")+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted")

####################
#####Rating curve ##
####################
ggplot(data = df , aes(x = depth_ave_m, y = Area, color=Date)) + 
  geom_point(size=3)
#ggplot(data = df_merge , aes(x = depth_ave_m, y = Area, color=method)) + 
#  geom_point(size=3)


#maybe log is best???
df_merge_1 <- df

modelln_1<-lm(data=df_merge_1, Area~log(depth_ave_m))
print(summary(modelln_1))

x_exp <- seq(from = .1, to = .5, by = .01)
y_exp = modelln_1$coefficients[1] +  modelln_1$coefficients[2] * log(x_exp)

plot_ly(x = df_merge_1$depth_ave_m, y=df_merge_1$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_exp, y=y_exp)



##Formula!

WL_df_2 <- WL_df
WL_df_2$surface_area_m2 <- 
  modelln_1$coefficients[1] +
  modelln_1$coefficients[2]*log(WL_df_2$depth_ave_m)


#surface are to volumn ratio
WL_df_2$Volumn_m3 <- WL_df_2$surface_area_m2*WL_df_2$depth_ave_m
WL_df_2$SA_to_Vol_ratio <- WL_df_2$surface_area_m2/WL_df_2$Volumn_m3

ggplot(data = WL_df_2, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=1)

ggplot(data = WL_df_2, aes(x = DateTime, y = surface_area_m2)) + 
  geom_point(size=1)

ggplot(data = WL_df_2, aes(x = DateTime, y = Volumn_m3)) + 
  geom_point(size=1)

plot_ly(data=WL_df_2, x = ~DateTime, y = ~SA_to_Vol_ratio)#%>%add_markers(size=1)


#write out final data frame
#write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland11_FINAL.csv"))
