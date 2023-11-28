#Figs
#2022-11-28
#this script is to make time series to use for AGU poster and wetland paper 
library(stringr)
library(grid)

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-10.csv"))
df$Date <- as.Date(df$Date)

df$name.code <- paste("W",as.numeric(gsub("\\D", "", df$Wetland)))
df$name.code <- str_replace_all(df$name.code, " ", "")
df$name.code <- factor(df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))

#I want to make a 12 panel figure with 2 axis depth and surface area

WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
#we don't have depth for wetland 12 but we can use water level to plot
WL_df[WL_df$Station=="WL_Wetland12",]$depth_ave_m <- WL_df[WL_df$Station=="WL_Wetland12",]$WaterLevel_m
WL_df <- WL_df[c("DateTime","Station","Baro_kpa","BaroTemp_c","WLTemp_c","depth_ave_m","surface_area_m2","Volumn_m3","SA_to_Vol_ratio")]
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

WL_df$name.code <- paste("W",as.numeric(gsub("\\D", "", WL_df$Station)))
WL_df$name.code <- str_replace_all(WL_df$name.code, " ", "")
WL_df$name.code <- factor(WL_df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))



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
