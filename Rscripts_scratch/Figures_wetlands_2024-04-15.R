#Figs
#2022-11-06
#this script is to make figs to use for AGU poster and wetland paper 
library(stringr)
library(grid)
library(ggplot2)
library(ggpubr)

#df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-10.csv"))
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2024-04-14.csv"))

df$Date <- as.Date(df$Date)

df$name.code <- paste("W",as.numeric(gsub("\\D", "", df$Site)))
df$name.code <- str_replace_all(df$name.code, " ", "")
df$name.code <- factor(df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))

pp1 <- ggplot(data=df,aes(x=CO2_umol.L,y=Flux_umol_m2_s, fill=name.code)) +
  geom_point(shape=21,size=5) +
  scale_y_log10() + scale_x_log10() +
  xlab(expression(CO[2] ~'('~mu*'mol' ~ l^-1~')')) + ylab(expression(CO[2] ~'Flux ('~mu*'mol' ~ m^2~ s^-1~')' )) +
  theme_bw() + theme(legend.title= element_blank()) +
  theme(legend.position = "top")+
  theme(text=element_text(size=20))

pp1


pp2 <- ggplot(data=df,aes(x=Elevation_m,y=CO2_umol.L, fill=name.code)) +
  geom_point(shape=21,size=5) +
  scale_y_log10() + #scale_x_log10() +
  ylab(expression(CO[2] ~'('~mu*'mol' ~ l^-1~')')) + xlab("Elevation (m)") +
  theme_bw() + theme(legend.title= element_blank(),axis.title.x = element_blank()) +
  theme(legend.position = "none")+
  theme(text=element_text(size=20))  

pp2

pp3 <- ggplot(data=df,aes(x=Elevation_m,y=CH4_umol.L, fill=name.code)) +
  geom_point(shape=21,size=5) +
  scale_y_log10() + #scale_x_log10() +
  ylab(expression(CH[4] ~'('~mu*'mol' ~ l^-1~')')) + xlab("Elevation (m)") +
  theme_bw() + theme(legend.title= element_blank()) +
  theme(legend.position = "none")+
  theme(text=element_text(size=20))

leg <- get_legend(pp1)

g <- rbind(ggplotGrob(pp2), ggplotGrob(pp3), size = "last")

ggarrange(g, leg, widths = c(2,.5))
ggarrange(pp1, leg, g, ncol=3,widths = c(2,.5,2))
ggarrange(pp1, g, nrow=1, widths = c(2,2)
          )

#I want to make a 12 panel figure with 2 axis depth and surface area

WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
#we don't have depth for wetland 12 but we can use water level to plot
WL_df[WL_df$Station=="WL_Wetland12",]$depth_ave_m <- WL_df[WL_df$Station=="WL_Wetland12",]$WaterLevel_m
WL_df <- WL_df[c("DateTime","Station","Baro_kpa","BaroTemp_c","WLTemp_c","depth_ave_m","surface_area_m2","Volumn_m3","SA_to_Vol_ratio")]
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

WL_df$name.code <- paste("W",as.numeric(gsub("\\D", "", WL_df$Station)))
WL_df$name.code <- str_replace_all(WL_df$name.code, " ", "")
WL_df$name.code <- factor(WL_df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))



p1 <- ggplot(data = WL_df%>%filter(Station=="WL_Wetland07")) + 
  geom_point(aes(x = DateTime, y = depth_ave_m),size=1)
p1


fig <- plot_ly(data = WL_df%>%filter(Station=="WL_Wetland07"),
               x = ~DateTime, y = ~depth_ave_m)


p1 + facet_wrap(~Station)+theme(text=element_text(size=21))


coeff <- 1000
#%>%filter(Station=="WL_Wetland03"|Station=="WL_Wetland09"|Station=="WL_Wetland12")
p1 <- ggplot(WL_df, aes(x=DateTime)) +
  geom_line( aes(y=depth_ave_m),color="brown") + 
  geom_line( aes(y=surface_area_m2 / coeff),color="blue") + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    name = "Average Depth", #color="brown",# Features of the first axis
    sec.axis = sec_axis(~.*coeff, name="Surface Area")     # Add a second axis and specify its features
  ) + theme_bw() +
  theme(
    # axis.text.y.left = element_text(colour = "brown"),
         axis.title.y.left = element_text(colour = "brown",face="bold"),
         #axis.line.y.right = element_line(color = "blue"), 
         #axis.text.y.right = element_line(color = "blue"),
         axis.title.y.right = element_text(color = "blue",face="bold")
        ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  theme(text=element_text(size=20))

p1 + facet_wrap(~name.code, scales = "free_y")#+theme(text=element_text(size=12))

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



