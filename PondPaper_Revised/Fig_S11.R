#Supplimentary Figure 11
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)

precip_summary <- read.csv(here::here("data/PrecipitationData.csv"))
precip_summary$Date <- as.Date(precip_summary$Date,format="%Y-%m-%d")

WL_df <- read.csv(here::here("data/Pond_continuous_data.csv"))%>%dplyr::select("DateTime","Site","WaterTemp_c","AirTemp_c")#%>%rename(Site=Station,AirTemp=BaroTemp_c)
WL_df$DateTime <- as.POSIXct(WL_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

df1 <- read.csv(here::here("data/predictor_variables_df.csv"))

gmc <- df1 %>%
  group_by(Site,Elevation_m) %>%
  mutate(gpm_Watertemp_c = mean(Watertemp_c)) %>% 
  ungroup()
df <- df1%>%dplyr::select(Site,Elevation_m,Watertemp_c)
df <- unique(df)

WL_df <- left_join(WL_df,df,by="Site")


start_time <- as.POSIXct("2022-06-16 11:00:00",tz="UTC")
end_time <- as.POSIXct("2023-08-24 13:15:00",tz="UTC")
timestamp_sequence <- seq.POSIXt(start_time, end_time, by = "hour")

new_df <- as.data.frame(timestamp_sequence)
new_df <- new_df%>%rename(DateTime=timestamp_sequence)

new_df$slope <- NA
new_df$adjR <- NA
new_df$n <- NA
new_df$AirTemp_c <- NA
new_df$RMSE <- NA
new_df$pvalue <- NA
new_df$slope <- NA
i <-2
for (i in 1:nrow(new_df)) {
  select_timestamp <- new_df[i,]$DateTime
  #filter 
  filter_df <-  WL_df%>%filter(DateTime==select_timestamp)
  new_df[i,]$AirTemp_c <- mean(filter_df$AirTemp)
  if(nrow(filter_df) > 5){
    model <- lm(WaterTemp_c ~ Elevation_m, data = filter_df)
    new_df[i,]$slope <- summary(model)$coefficients[2,1]
    new_df[i,]$adjR <- summary(model)$adj.r.squared
    new_df[i,]$RMSE <- sqrt(mean(model$residuals^2))
    new_df[i,]$pvalue <- summary(model)$coefficients[2,4]
    new_df[i,]$n <- nrow(filter_df)
    new_df[i,]$slope <- as.numeric(model$coefficients[2])
    
  }else{
    new_df[i,]$adjR <- NA
  }
  
} 



adjR.plot2 <- ggplot()+ geom_line(data=new_df%>%filter(n>7),aes(x=DateTime,y=adjR),color="salmon") +
  ylab(expression(paste("\nAdj. r squared"))) +
  theme_classic(base_size = 14)+
  xlim(as.POSIXct("2022-07-01 12:00:00"),as.POSIXct("2023-07-01 00:00:00"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

slope.plot <- ggplot()+ 
  geom_point(data=new_df%>%filter(DateTime>as.POSIXct("2022-07-01 12:00:00")&DateTime<as.POSIXct("2023-07-01 00:00:00"))%>%filter(slope<0),aes(x=DateTime,y=slope),color="black",shape=1)  +
  geom_point(data=new_df%>%filter(DateTime>as.POSIXct("2022-07-01 12:00:00")&DateTime<as.POSIXct("2023-07-01 00:00:00"))%>%filter(slope>0),aes(x=DateTime,y=slope),color="grey",shape=1) + ylab("Slope of\n linear regression") +
  theme_classic(base_size = 14)+
  xlim(as.POSIXct("2022-07-01 12:00:00"),as.POSIXct("2023-07-01 00:00:00"))


precip.plot <- ggplot(aes(x=Date, y=Precip_mm_ave7), data = precip_summary) + geom_point(color="steelblue")+ geom_line(color="steelblue")+
  ylab("Precipitation\n7 day ave. (mm)") +
  xlim(as.Date("2022-07-02"),as.Date("2023-07-01")) +
  theme_classic(base_size = 14)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 


p1 <- ggplot() + geom_point(data=df1,aes(x=Elevation_m,y=Watertemp_c,fill = reorder(Site,Elevation_m,median,decreasing = TRUE)),shape=21,size=2)+
  geom_point(data=gmc,aes(x=Elevation_m,y=gpm_Watertemp_c),fill="black",shape=23,size=3)+
  guides(fill=guide_legend(title="Site name",ncol=2))+ 
  xlab("Elevation (m)") + ylab("Water temp. (C)") +
  geom_smooth(data=gmc,aes(x=Elevation_m,y=gpm_Watertemp_c),method='lm',formula = "y ~ x",se=TRUE,color="black") +
  theme_bw(base_size = 14)

#zoom in 
d = data.frame(x = seq(from = as.POSIXct("2022-07-01 12:00:00"), to=as.POSIXct("2022-08-01 00:00:00"), by = "1 hour"), y = 1)
d$hour = as.numeric(format(d$x, "%H"))
d$hour_shade =ifelse(d$hour >= 18 | d$hour <= 6, "gray80", "gray95")


zoom.plot <- ggplot(d, aes(x, y)) + 
  geom_rect(aes(xmin = x, xmax = lead(x), ymin = -Inf, ymax = Inf,
                fill = hour_shade)) +
  scale_fill_identity() +
  geom_line(data=new_df,aes(x=DateTime,y=adjR),color="salmon",linewidth=1) +
  ylab(expression(paste("Adj. r squared"))) + xlab("")  +
  theme_classic(base_size = 14)+
  xlim(as.POSIXct("2022-07-01 12:00:00"),as.POSIXct("2022-08-01 00:00:00"))

###plot all

plot_column1 <- 
  cowplot::plot_grid(NULL,
    precip.plot+ rremove("x.axis") + rremove("xlab"), NULL,
    adjR.plot2+ rremove("x.axis") + rremove("xlab"), NULL,
    slope.plot, 
    ncol = 2, rel_widths = c(.02,1),
    labels = c("","A","","B","","C"),
    hjust = 1,align = "v",label_size = 13, label_fontface = "bold")

plot_column2 <- cowplot::plot_grid(
  p1, 
  zoom.plot,
  ncol = 2,
  labels = c("D","E"),
  label_size = 14,
  label_fontface = "bold")


# then combine with the top row for final plot
full_plot <- plot_grid(plot_column1,plot_column2,nrow=2,rel_heights = c(1.5,1))
