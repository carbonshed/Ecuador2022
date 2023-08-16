###
library(here)
library(dplyr)
library(tidyr)

WLData <- read.csv(here::here("Kriddie/WL_data.csv"))
WLData$DateTime <- as.POSIXct(WLData$DateTime, tz="UTC", Formats = c("%Y-%m-%d %H:%M:%S"))
WLData <- WLData%>%drop_na(Station)


precip_df <- read.csv(here::here("Wetlands/WeatherStation_LaVirgen/M5025_Precipitación_subhorario-validado.csv"))
colnames(precip_df) <- c("DateTime","precipt_mm")
precip_df$DateTime <- as.POSIXct(precip_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
#precip_df$Date <- as.Date(precip_df$DateTime,format="%Y-%m-%d")
#precip_summary <- precip_df%>%group_by(Date)%>%summarise(PrecipAccuDay_mm = sum(precipt_mm))
#precip_summary$Date <- precip_summary$Date - 1

df_hydro <- left_join(WLData,precip_df,by="DateTime")


ggplot(df_hydro%>%filter(Station=="WL_Wetland02" & DateTime < as.POSIXct("2022-07-31 23:00:00"))) +
  geom_line(aes(x=DateTime,y=WaterLevel_m)) +
  geom_point(aes(x=DateTime,y=precipt_mm)) +
  theme(text=element_text(size=20))

df_hydro$Station