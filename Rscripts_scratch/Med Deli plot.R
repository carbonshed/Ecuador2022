#Figs
#2022-11-28
#this script is to make figs to use for AGU poster and wetland paper 
library(stringr)
library(grid)

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-10.csv"))
df$Date <- as.Date(df$Date)

df$name.code <- paste("W",as.numeric(gsub("\\D", "", df$Wetland)))
df$name.code <- str_replace_all(df$name.code, " ", "")
df$name.code <- factor(df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))

WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
#we don't have depth for wetland 12 but we can use water level to plot
WL_df[WL_df$Station=="WL_Wetland12",]$depth_ave_m <- WL_df[WL_df$Station=="WL_Wetland12",]$WaterLevel_m
WL_df <- WL_df[c("DateTime","Station","Baro_kpa","BaroTemp_c","WLTemp_c","depth_ave_m","surface_area_m2","Volumn_m3","SA_to_Vol_ratio")]
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

WL_df$name.code <- paste("W",as.numeric(gsub("\\D", "", WL_df$Station)))
WL_df$name.code <- str_replace_all(WL_df$name.code, " ", "")
WL_df$name.code <- factor(WL_df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))



#med deli plot


Summary_df <- WL_df%>%
 group_by(Station)%>%
  summarise(
    SA_max = max(surface_area_m2, na.rm = TRUE),
    SA_min = min(surface_area_m2, na.rm = TRUE),
    SA_mean = mean(surface_area_m2, na.rm = TRUE),
    
    depth_max = max(depth_ave_m, na.rm = TRUE),
    depth_min = min(depth_ave_m, na.rm = TRUE),
    depth_mean = mean(depth_ave_m, na.rm = TRUE)
  )

Summary_df$name.code <- paste("W",as.numeric(gsub("\\D", "", Summary_df$Station)))
library(stringr)
Summary_df$name.code <- str_replace_all(Summary_df$name.code, " ", "")
Summary_df$name.code <- factor(Summary_df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))


p2 <- ggplot(data=Summary_df,aes(x=SA_mean,y=depth_mean, color=name.code)) +
  geom_point(shape=7,size=5) +
  geom_segment(aes(x = SA_min, y = depth_mean, xend = SA_max, yend = depth_mean, color=name.code),linewidth=1) +
  geom_segment(aes(x = SA_mean, y = depth_min, xend = SA_mean, yend = depth_max, color=name.code),linewidth=1) +
  scale_y_log10() + scale_x_log10() +
  theme_classic() +
  theme(text=element_text(size=20)) + theme(legend.title= element_blank()) +
  theme(
    axis.text.x = element_text(color="black"),
    axis.ticks = element_line(color = "black")) +
  xlab(expression('Surface Area (' ~ m^2~')')) + ylab("Depth ( m )") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
)

p2 + annotation_logticks(
  base = 10,
  sides = "bl",
  outside = TRUE,
  scaled = TRUE,
  short = unit(0.1, "cm"),
  mid = unit(0.2, "cm"),
  long = unit(0.3, "cm"),
  colour = "black",
  size = 0.5,
  linetype = 1,
  alpha = 1,
  color = "grey40"
) + coord_cartesian(clip = "off")



