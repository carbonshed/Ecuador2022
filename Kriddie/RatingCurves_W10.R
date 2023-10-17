#wetland depth vs surface area relationships
#Wetland 10
library(here)
library(dplyr)
library(ggplot2)
library(plotly)

#I didn't take home some of the polygons that Dani made of w10. 
#so maybe I'll need to go back and do this before rating curve is complete

Station_name <- "WL_Wetland10"

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

#raster pixel size:
#0.0368699999999658*0.0368700000022459

df_DSM <- read.csv(here::here("Wetlands/DSM_W10_20220927.csv"))

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


ggplot(data = WL_df , aes(x=DateTime, y = depth_ave_m)) + geom_line(color="blue") +
  geom_hline(yintercept=max(df$depth_ave_m), linetype="dashed", color = "red")+ 
  geom_hline(yintercept=min(df$depth_ave_m), linetype="dashed", color = "red")+
  geom_vline(xintercept = df$DateTime, color = "black",linetype="dotted")

####################
#####Rating curve ##
####################
ggplot(data = df , aes(x = depth_ave_m, y = Area, color=Date)) + 
  geom_point(size=3)
ggplot(data = df_merge , aes(x = depth_ave_m, y = Area, color=method)) + 
  geom_point(size=3)

##
#wetland never dry, so use depth measured in field
#This is kind of tricky because the change in water level is very slight.
#Judging by the DSM, I am going to do a linear regresion based on manual readings without the DSM


df_merge_1 <- df

#difference in highest and lowest measurments
# 0.3709113-0.2114131

modelln_1<-lm(data=df_merge_1, Area~depth_ave_m)
print(summary(modelln_1))

x_ln_1 <- seq(from = .2, to = .4, by = .01)
y_ln_1 = modelln_1$coefficients[1] + modelln_1$coefficients[2]*x_ln_1

plot_ly(x = df_merge_1$depth_ave_m, y=df_merge_1$Area)%>%
  add_markers(size=4)%>%
  add_lines(x = x_ln_1, y=y_ln_1)


##Formula!

WL_df_2 <- WL_df
WL_df_2$surface_area_m2 <- 
  modelln_1$coefficients[1] +
  modelln_1$coefficients[2]*WL_df_2$depth_ave_m



ggplot(data = WL_df_2, aes(x = DateTime, y = depth_ave_m)) + 
  geom_point(size=1)

plot_ly(data=WL_df_2, x = ~DateTime, y = ~surface_area_m2)#%>%add_markers(size=1)


#write out final data frame
#write.csv(WL_df_2, here::here("Wetlands/WaterLevel_FINAL/WL_Wetland10_FINAL.csv"))
