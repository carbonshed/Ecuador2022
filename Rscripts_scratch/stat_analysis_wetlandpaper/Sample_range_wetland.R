#this script is to come up ways to explain
#how much our sampling covers variation

#load library
library(ggplot2)
library(plotly)
library(lubridate)
library(zoo)

#some stats for methods section

laVirgin_df <- read.csv(here::here("WeatherStation_LaVirgen/LaVirgin_dailyprecipt.csv"))
laVirgin_df$Date <- as.Date(laVirgin_df$Date,format="%m/%d/%Y")
laVirgin_df$Year <- as.numeric(format(laVirgin_df$Date,'%Y'))
laVirgin_df$Month <- as.numeric(format(laVirgin_df$Date,'%m'))

laVirgin_yearsummary <- laVirgin_df%>%group_by(Year)%>%
  summarise(Precip_yearsum = sum(precipt_mm))
laVirgin_monthsummary <- laVirgin_df%>%group_by(Year,Month)%>%
  summarise(Precip_yearsum = sum(precipt_mm))

laVirgin_df%>%
  summarise(
    precip_day_mean = mean(precipt_mm, na.rm = TRUE),
    precip_day_median = median(precipt_mm, na.rm = TRUE),
    precip_day_min = min(precipt_mm, na.rm = TRUE),
    precip_day_max = max(precipt_mm, na.rm = TRUE))

laVirgin_df_sampleperiod <- laVirgin_df%>%filter(Date>"2022-06-01"&Date<"2023-06-01")

laVirgin_df_sampleperiod%>%
summarise(
  precip_day_mean = mean(precipt_mm, na.rm = TRUE),
  precip_day_median = median(precipt_mm, na.rm = TRUE),
  precip_day_sum = sum(precipt_mm, na.rm = TRUE),
  precip_day_min = min(precipt_mm, na.rm = TRUE),
  precip_day_max = max(precipt_mm, na.rm = TRUE))


#plot the time series of water level and plot where we sampled 
#plot time series of rain fall and where we sampled

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2024-03-27.csv"))
df$X.2 <- NULL
df$X.1 <- NULL
df$X <- NULL
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
df$Date <- as.Date(df$Date)

WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
WL_df <- WL_df[c("DateTime","Station","Baro_kpa","BaroTemp_c","WLTemp_c","depth_ave_m","surface_area_m2","Volumn_m3","SA_to_Vol_ratio")]
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

WL_df$Site <- gsub(".*_","",WL_df$Station)
WL_df$Station <- NULL
WL_df$Date <- as.Date(WL_df$DateTime)

ggplot() + 
  geom_line(data=WL_df%>%filter(Site=="Wetland08"), aes(x=depth_ave_m, y=Volumn_m3), color='black') + 
  geom_point(data=df%>%filter(Site=="Wetland08"), aes(x=depth_ave_m, y=Volumn_m3), color='red')

ggplot() + 
  geom_line(data=WL_df, aes(x=depth_ave_m, y=Volumn_m3), color='black') + 
  geom_point(data=df, aes(x=depth_ave_m, y=Volumn_m3), color='red') + 
  facet_wrap(vars(Site),scales = "free_y")

ggplot() + 
  geom_line(data=WL_df%>%filter(Site!="Wetland12"), aes(x=depth_ave_m, y=surface_area_m2), color='black') + 
  geom_point(data=df%>%filter(Site!="Wetland12"), aes(x=depth_ave_m, y=surface_area_m2), color='red') + 
  facet_wrap(vars(Site),scales = "free_y")


p <- ggplot() + 
  geom_line(data=WL_df%>%filter(Site=="Wetland08"), aes(x=DateTime, y=surface_area_m2), color='green') + 
  geom_point(data=df%>%filter(Site=="Wetland08"), aes(x=DateTime, y=surface_area_m2), color='red')

ggplotly(p)

#box plot
WL_df$LaVirgin <- "LaVirgin"
df$LaVirgin <- "LaVirgin"
box1 <- ggplot() + 
  geom_boxplot(data=WL_df%>%filter(Site!="Wetland12"), aes(x=LaVirgin, y=surface_area_m2), color='black') + 
  geom_point(data=df%>%filter(Site!="Wetland12"), aes(x=LaVirgin, y=surface_area_m2), color='red') +
  facet_wrap(vars(Site),scales = "free_y") + scale_y_log10()
box1


box2 <- ggplot() + 
  geom_boxplot(data=WL_df, aes(x=Site, surface_area_m2), color='black') + 
  geom_point(data=df, aes(x=Site, y=surface_area_m2), color='red')

ful <- box + scale_y_log10()

ggplotly(box2)


##ok now for percipitation!!!!


precip_df <- read.csv(here::here("WeatherStation_LaVirgen/M5025_Precipitacion_Dato_validado.csv"))
colnames(precip_df) <- c("DateTime","precipt_mm")
precip_df$DateTime <- as.POSIXct(precip_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
precip_df$Date <- as.Date(precip_df$DateTime,format="%Y-%m-%d")
precip_df <- precip_df%>%filter(Date > "2022-05-22"&
                                  Date < "2023-06-01")
precip_summary <- precip_df%>%group_by(Date)%>%summarise(PrecipAccuDay_mm = sum(precipt_mm))
###############
##sum by week##
###############
precip_summary_week <- precip_df %>% group_by(week = cut(DateTime, "week"))%>%
  summarise(weeksum_precipt_mm = sum(precipt_mm))
precip_summary_week$Date <- as.Date(precip_summary_week$week)
precip_summary_week$LaVirgin <- "LaVirgin"
#now bind that in to both data frames so that we can plot
precip_weekAve <- transform(precip_summary, avg7 = rollmeanr(PrecipAccuDay_mm, 7, fill = NA,na.rm=TRUE))
precip_weekAve$weeksum_precipt_mm <- precip_weekAve$avg7 * 7
df_precipt <- left_join(df,precip_weekAve%>%select(Date,weeksum_precipt_mm),join_by("Date"))

# Sample data for precipitation
weeksum_precipt_mm <- precip_summary_week$weeksum_precipt_mm # Replace with your actual precipitation data
# Site names
site_names <- c("Wetland01", "Wetland02", "Wetland03", "Wetland04","Wetland05","Wetland06","Wetland07",
                "Wetland08","Wetland09","Wetland10","Wetland11","Wetland12")  # Replace with your actual site names

# Repeat precipitation data for each site
repeated_precipitation <- rep(weeksum_precipt_mm, each = length(site_names))

# Repeat site names for each entry in precipitation
repeated_site_names <- rep(site_names, times = length(weeksum_precipt_mm))

# Create a data frame with precipitation and site names
precipt_weeksum <- data.frame(Site = repeated_site_names, weeksum_precipt_mm = repeated_precipitation)


#plot
df_temp <- df%>%filter(Site=="Wetland06")
ggplot() +
  geom_bar(data=precip_summary_week, aes(x=Date, y=weeksum_precipt_mm), stat="identity") + 
  geom_vline(xintercept = as.Date(df_temp$DateTime),color="red",linetype = "dashed")

df_temp <- df_precipt%>%filter(Site=="Wetland06")
df_temp$LaVirgin <- "LaVirgin"

box2 <- ggplot() + 
  geom_boxplot(data=precipt_weeksum, aes(x=Site, y=weeksum_precipt_mm), fill='grey80') + 
  geom_point(data=df_precipt, aes(x=Site, y=weeksum_precipt_mm), color='red') +
  theme_bw() +
  theme(text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))
  
  
box2

