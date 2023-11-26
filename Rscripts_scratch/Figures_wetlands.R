#Figs
#2022-11-06
#this script is to make figs to use for AGU poster and wetland paper 

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-10.csv"))
df$Date <- as.Date(df$Date)


pp1 <- ggplot(data=df,aes(x=CO2_umol.L,y=Flux_umol_m2_s, fill=Wetland)) +
  geom_point(shape=21,size=5) +
  scale_y_log10() + scale_x_log10() +
  xlab(expression(CO[2] ~'('~mu*'mol' ~ l^-1~')')) + ylab(expression(CO[2] ~'Flux ('~mu*'mol' ~ m^2~ s^-1~')' )) +
  theme_bw() +
  theme(text=element_text(size=18))

pp1
#I want to make a 12 panel figure with 2 axis depth and surface area

WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
#we don't have depth for wetland 12 but we can use water level to plot
WL_df[WL_df$Station=="WL_Wetland12",]$depth_ave_m <- WL_df[WL_df$Station=="WL_Wetland12",]$WaterLevel_m
WL_df <- WL_df[c("DateTime","Station","Baro_kpa","BaroTemp_c","WLTemp_c","depth_ave_m","surface_area_m2","Volumn_m3","SA_to_Vol_ratio")]
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

p1 <- ggplot(data = WL_df%>%filter(Station=="WL_Wetland01")) + 
  geom_point(aes(x = DateTime, y = depth_ave_m),size=1)
p1
p1 + facet_wrap(~Station)+theme(text=element_text(size=21))


coeff <- 1000
#%>%filter(Station=="WL_Wetland03"|Station=="WL_Wetland09"|Station=="WL_Wetland12")
p1 <- ggplot(WL_df, aes(x=DateTime)) +
  geom_line( aes(y=depth_ave_m),color="brown") + 
  geom_line( aes(y=surface_area_m2 / coeff),color="blue") + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    name = "Average Depth", #color="brown",# Features of the first axis
    sec.axis = sec_axis(~.*coeff, name="Surface Area")     # Add a second axis and specify its features
  ) +
  theme(
    # axis.text.y.left = element_text(colour = "brown"),
         axis.title.y.left = element_text(colour = "brown",size=16,face="bold"),
         #axis.line.y.right = element_line(color = "blue"), 
         #axis.text.y.right = element_line(color = "blue"),
         axis.title.y.right = element_text(color = "blue",size=16,face="bold")
        ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1 + facet_wrap(~Station, scales = "free_y")#+theme(text=element_text(size=12))

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

p2 <- ggplot(data=Summary_df,aes(x=SA_mean,y=depth_mean, color=name.code)) +
  geom_point(shape=7,size=5) +
  geom_segment(aes(x = SA_min, y = depth_mean, xend = SA_max, yend = depth_mean, color=name.code),linewidth=1) +
  geom_segment(aes(x = SA_mean, y = depth_min, xend = SA_mean, yend = depth_max, color=name.code),linewidth=1) +
  scale_y_log10() + scale_x_log10() +
  theme_bw() +
  theme(text=element_text(size=18)) +
  theme(
    axis.text.x = element_text(color="black"),
    axis.ticks = element_line(color = "black")
  ) +
  xlab(expression('Surface Area (' ~ m^2~')')) + ylab("Depth ( m )")
p2 + annotation_logticks() 

expression(Speed ~ ms^-1 ~ by ~ impeller)