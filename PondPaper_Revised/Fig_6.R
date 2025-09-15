# Figure 6
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)

#read in data
WL_df <- read.csv(here::here("data/Pond_continuous_data.csv"))
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
WL_df$Date <- as.Date(WL_df$DateTime,format="%Y-%m-%d")

WL_df <- WL_df%>%filter(DateTime>as.POSIXct("2022-07-12 00:00:00",tz="UTC")&DateTime<as.POSIXct("2023-07-12 00:00:00",tz="UTC"))

WL_df_maxmean <- WL_df%>%group_by(Site)%>%summarise(SA_max=max(surface_area_m2,na.rm =  TRUE),SA_mean=mean(surface_area_m2,na.rm =  TRUE))

WL_df_day<-WL_df%>%group_by(Date,Site)%>%
  summarise(SA_day_ave = mean(surface_area_m2,na.rm = TRUE))

WL_df_day <- WL_df_day%>%group_by(Site)%>%
  mutate(mean_val = mean(SA_day_ave,na.rm=TRUE))

WL_df_day <- full_join(WL_df_day,WL_df_maxmean,by="Site")

WL_df_day$yr_mo <- format(WL_df_day$Date,format="%Y-%m")

#plot
p <- ggplot(WL_df_day %>%drop_na(yr_mo),
            aes(x=as.factor(yr_mo),y=SA_day_ave/SA_max,fill=yr_mo)) +
  geom_boxplot(outlier.size = .25,lwd=.5)+
  geom_hline(data= WL_df_day, aes(yintercept=mean_val/SA_max), linetype = 'solid', colour = 'red') +
  facet_wrap(~factor(Site, levels=c('S1', 'S2', 'S3','S4','S5','S6','S7','S8','S10','S11','S12')), scales = "free_y", nrow = 4, strip.position = "top") +
  xlab("") + ylab("Scaled daily surface area") +
  scale_fill_discrete(name="year-month") +
  scale_y_continuous(breaks = breaks_pretty())+
  theme_bw(base_size = 14) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 
