#Supplimentary Figure 6
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)

#read in data
WL_df <- read.csv(here::here("data/Pond_continuous_data.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

WL_df <- WL_df %>% 
  mutate(
    date = as.Date(DateTime),
    hour = hour(DateTime),
    minute = minute(DateTime),
    second = second(DateTime)
  ) %>% 
  mutate(
    format_date = format(date, "%m/%d/%Y"),
    format_hour = paste(hour, minute, second, sep = ":")
  )

WL_df_midday <- WL_df %>%filter(format_hour=="12:0:0"|format_hour=="11:0:0"|format_hour=="13:0:0")%>%rename(Date=date)
WL_df_midday <- WL_df_midday%>%group_by(Site,Date)%>%summarise(
  Baro_kpa = mean(Baro_kpa,na.rm=TRUE )
)

CO2_summary <- read.csv(here::here("data/Pond_pCO2_data_summary.csv"))
CO2_summary$Date <- as.Date(CO2_summary$Date,format="%Y-%m-%d")

predictor_variables <- read.csv(here::here("data/predictor_variables_df.csv"))%>%dplyr::select(Site,Date,Elevation_m)
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")

df <- full_join(CO2_summary,predictor_variables,by=c("Site","Date"))
df <- left_join(df,WL_df_midday,by=c("Site","Date"))

df_1 <- df%>%filter(Site=="S12"|Site=="S11"|Site=="S10")
df_1 <- df_1%>%filter(Date<"2022-10-01")
df_2 <- df%>%filter(Site!="S12")%>%
  filter(Site!="S11")%>%filter(Site!="S10")
df <- rbind(df_1,df_2)

df <- df%>%group_by(Site)%>%mutate(
  diff_ave = mean(AirPress_kpa-Baro_kpa,na.rm=TRUE)
)

#elevation of barometric pressure logger
BaroStation <- 4158.6912 

p <- ggplot(df) +
  geom_point(aes(x=AirPress_kpa-Baro_kpa,y=Elevation_m)) +
  geom_smooth(aes(x=diff_ave,y=Elevation_m),method = lm, se = FALSE) +
  geom_point(aes(x=diff_ave,y=Elevation_m),color="red",shape=3, size=3)+
  #  geom_hline(yintercept=BaroStation) +
  xlab("Air pressure difference (kPa)")+
  ylab("Site elevation (m)") +
  theme_bw(base_size = 14)+
  guides(color = guide_legend(ncol = 2)) +
  annotate("text", label = paste0("atop('y = 30.93 + -0.0074 * x',R^2==0.995)"),
           x = 1, y = 4250, size = 3, parse = TRUE)



