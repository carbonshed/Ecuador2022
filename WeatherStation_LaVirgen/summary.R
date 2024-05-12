#weather summary

library(readxl)
library(dplyr)
library(readr)
library(ggplot2)

df_precip <- read_csv("WeatherStation_LaVirgen/M5025-La_Virgen_precip_2007-2024.csv")%>%rename(precip_mm=Valor)
df_precip$Fecha <- as.POSIXct(df_precip$Fecha, format="%Y-%m-%d %H:%M:%S",tz="UTC")
df_precip$month <- format(df_precip$Fecha, "%m")
df_precip$month_yr <- format(as.Date(df_precip$Fecha), "%Y-%m")
df_precip$month <- format(as.Date(df_precip$Fecha), "%m")
df_precip$yr <- format(as.Date(df_precip$Fecha), "%Y")

df_precip_summary <- df_precip%>%group_by(month_yr)%>%
  summarise(precip_sum_mo = sum(precip_mm))
df_precip_summary$month_yr <-as.Date(paste(df_precip_summary$month_yr, "-01", sep=""))
df_precip_summary$month <- format(as.Date(df_precip_summary$month_yr), "%m")
df_precip_summary$yr <- format(as.Date(df_precip_summary$month_yr), "%Y")


df_precip_summary2 <- df_precip%>%group_by(yr)%>%
  summarise(precip_sum_yr = sum(precip_mm))

df_precip_summary3 <- df_precip%>%group_by(month)%>%
  summarise(precip_ave_mo = mean(precip_mm,na.rm = TRUE))


ggplot(df_precip_summary3, aes(x=month, y=precip_ave_mo)) + 
  geom_point()

ggplot(df_precip_summary, aes(x=month, y=precip_sum_mo)) + 
  geom_boxplot()
ggplot(df_precip_summary, aes(x=month, y=precip_sum_mo,color=yr)) + 
  geom_point()

df_temp <- read_csv("WeatherStation_LaVirgen/M5025-La_Virgen_temp_2007-2024.csv")%>%rename(temp_c=Valor)
df_temp$Fecha <- as.POSIXct(df_temp$Fecha, format="%m/%d/%y %H:%M",tz="UTC")
df_temp$month <- format(df_temp$Fecha, "%m")
df_temp$month_yr <- format(as.Date(df_temp$Fecha), "%Y-%m")
df_temp$yr <- format(as.Date(df_temp$Fecha), "%Y")

df_temp_summary <- df_temp%>%group_by(month_yr)%>%
  summarise(temp_ave_mo = mean(temp_c))
df_temp_summary2 <- df_temp%>%group_by(yr)%>%
  summarise(temp_ave_yr = mean(temp_c))

df_temp_summary$month_yr <-as.Date(paste(df_temp_summary$month_yr, "-01", sep=""))

ggplot(data=df_temp_summary,aes(x=month_yr,y=temp_ave)) + geom_point()
ggplot(data=df_temp_summary2,aes(x=yr,y=temp_ave)) + geom_point()


###comapre precipitation and temp for sample period
df_precip$month_day <- format(as.Date(df_precip$Fecha), "%m-%d")
df_precip$month <- as.numeric(df_precip$month)
df_precip <- df_precip%>%filter(month == 6 |month==7)%>%
  filter(month_day=="06-21"|month_day=="06-22"|month_day=="06-23"|month_day=="06-24"|
           month_day=="06-25"|month_day=="06-26"|month_day=="06-27"|
           month_day=="06-28"|month_day=="06-29"|month_day=="06-30"|
           month_day=="07-01"|month_day=="07-02"|month_day=="07-03"|
           month_day=="07-04"|month_day=="07-05"|month_day=="07-06"|
           month_day=="07-07"|month_day=="07-08"|month_day=="07-09")


#2021 93.3
#all 128.4
summary(df_precip_summary4)


#
df_temp <- df_temp%>%filter(Fecha > "2021-06-18" & Fecha <= "2021-07-09")
df_temp$Day <- as.Date(df_temp$Fecha)
df_temp <- df_temp%>%filter(Day >= "2021-06-18" & Day <= "2021-07-09")
df_temp_summary4 <- df_temp%>%group_by(Day)%>%
  summarize(temp_ave = mean(temp_c))
