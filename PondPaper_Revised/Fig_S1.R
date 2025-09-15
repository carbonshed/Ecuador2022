
#Supplimentary Figure 1
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)

#read in data
WL_df <- read.csv(here::here("data/Pond_continuous_data.csv"))
#we don't have depth for Site but we can use water level to plot
WL_df[WL_df$Site=="Wetland",]$depth_ave_m <- WL_df[WL_df$Site=="Wetland",]$WaterLevel_m
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

#read in elevation data to merge with WL
df <- read.csv(here::here("data/predictor_variables_df.csv"))%>%dplyr::select(Site,Date, Elevation_m)
df <- unique(df)

#summarize data
Summary_df <- WL_df%>%
  group_by(Site)%>%
  summarise(
    SA_max = max(surface_area_m2, na.rm = TRUE),
    SA_min = min(surface_area_m2, na.rm = TRUE),
    SA_mean = mean(surface_area_m2, na.rm = TRUE),
    depth_max = max(depth_ave_m, na.rm = TRUE),
    depth_min = min(depth_ave_m, na.rm = TRUE),
    depth_mean = mean(depth_ave_m, na.rm = TRUE)
  )

Summary_df <- full_join(Summary_df,df,by="Site")

p1 <- ggplot(data=Summary_df,aes(x=SA_mean,y=depth_mean, fill=Elevation_m)) +
  geom_point(shape=22,size=5) +
  geom_segment(aes(x = SA_min, y = depth_mean, xend = SA_max, yend = depth_mean), color="grey25",linewidth=.5) +
  geom_segment(aes(x = SA_mean, y = depth_min, xend = SA_mean, yend = depth_max), color="grey25",linewidth=.5) +
  scale_y_log10() + scale_x_log10() +
  theme_classic() +
  theme(text=element_text(size=20)) + guides(fill=guide_legend(title="Elevation (m)")) +
  theme(
    axis.text.x = element_text(color="black"),
    axis.ticks = element_line(color = "black")) +
  xlab(expression('Surface Area ('~m^2~')')) + ylab("Depth (m)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

p2 <- p1 + annotation_logticks(
  base = 10, sides = "bl", outside = TRUE, scaled = TRUE,
  short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
  colour = "black", linewidth = 0.5, linetype = 1, alpha = 1, color = "grey40") +
  coord_cartesian(clip = "off")

