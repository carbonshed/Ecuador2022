#Figure 4
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(cowplot)

#read data
CO2_df <- read.csv(here::here("data/Pond_pCO2_data_summary.csv"))
CO2_df$Date <- as.Date(CO2_df$Date,format="%Y-%m-%d")

predictor_variables <- read.csv(here::here("data/predictor_variables_df.csv"))%>%dplyr::select(!Watertemp_c)
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")

#join
CO2_df <- full_join(CO2_df,predictor_variables,by=c("Site","Date"))

#summarise data
CO2_summary <- CO2_df %>%
  group_by(Site,Elevation_m,percent_DTW, WS_area_minus_pond) %>%
  mutate(gpm_Watertemp_c = mean(Watertemp_c),
         gpm_WaterLevel_m = mean(WaterLevel_m),
         mean_pCO2_uatm = mean(pCO2_w_uatm),
         Watertemp_c_centered = Watertemp_c - gpm_Watertemp_c,
         WaterLevel_m_centered = WaterLevel_m -gpm_Watertemp_c) %>% 
  ungroup()


# get list of residuals
model <- lm(log(CO2_summary$pCO2_w_uatm)~CO2_summary$Elevation_m)
CO2_summary$res <- resid(model) 

#plot
xlab <- "Water temperature (°C)"
p.co2 <- ggplot() + 
  geom_smooth(data=CO2_summary,aes(x=gpm_Watertemp_c,y=mean_pCO2_uatm),method='lm',formula = "y ~ x",se=FALSE,color="black") +
  geom_point(data=CO2_summary,aes(x=Watertemp_c,y=pCO2_w_uatm,color=reorder(Site,Elevation_m,median,decreasing = TRUE)),size=2) +
  geom_point(data=CO2_summary,aes(x=Watertemp_c,y=pCO2_w_uatm),color="black",shape=1, size=2) +
  geom_point(data=CO2_summary,aes(x=gpm_Watertemp_c,y=mean_pCO2_uatm,fill=Elevation_m),shape=23,size=4)+
  geom_point(data=CO2_summary,aes(x=gpm_Watertemp_c,y=mean_pCO2_uatm),color="black",shape=3,size=1)+
  ylab(expression(italic(p)*paste("CO"[2] ," (",mu,"atm)"))) +
  xlab(xlab) +
  scale_color_viridis(name="Site",discrete = TRUE) +
  scale_y_continuous(transform="log",breaks=c(300,1000,3000,10000)) + 
  theme_bw(base_size = 14) +
  scale_fill_continuous(name = "Elevation (m)") +
  theme(legend.box = "horizontal") +
  guides(fill = guide_legend(order = 2),color = guide_legend(order = 1,ncol = 2)) + 
  guides(color = guide_legend(ncol = 2)) +
  annotate("text", label = paste0("atop('marg. '* R^2==0.43,RMSE==0.25)"),
           x = 5, y = 5000, size = 2.5, parse = TRUE)


p.co2.ele <- ggplot() +
  geom_smooth(data=CO2_summary,aes(x=Elevation_m,y=pCO2_w_uatm), method='lm',formula = "y ~ x",se=FALSE,color="black") +
  geom_point(data=CO2_summary,aes(x=Elevation_m,y=pCO2_w_uatm,color=reorder(Site,Elevation_m,median,decreasing = TRUE)),size=2) +
  geom_point(data=CO2_summary,aes(x=Elevation_m,y=pCO2_w_uatm),shape=1,size=2) +
  ylab(expression(italic(p)*paste("CO"[2] ," (",mu,"atm)"))) +
  xlab('Elevation (m)') +
  scale_color_viridis(name="Site",discrete = TRUE) +
  scale_y_continuous(transform="log",breaks=c(300,1000,3000,10000)) +
  theme_bw(base_size = 14) +
  annotate("text", label = paste0("atop('marg. '* R^2==0.74,RMSE==0.25)"),
           x = 4200, y = 5000, size = 2.5, parse = TRUE)


p.dtw.co2 <-  ggplot() + 
  geom_smooth(data=CO2_summary,aes(x=percent_DTW,y=res),method='lm',formula = "y ~ x",se=FALSE,color="black") +
  geom_point(data=CO2_summary,aes(x=percent_DTW,y=res,color=reorder(Site,Elevation_m,median,decreasing = TRUE)),size=2) +
  geom_point(data=CO2_summary,aes(x=percent_DTW,y=res),color="black",shape=1, size=2) +
  ylab('Residuals') + xlab('% DTW < 1-m') +
  scale_x_continuous(transform="log",breaks=c(1,3,10,30,100)) +
  scale_color_viridis(name="Site",discrete = TRUE) +
  theme_bw(base_size = 14)+ 
  guides(color = guide_legend(ncol = 2)) +
  annotate("text", label = paste0("atop('marg. '* R^2==0.29,RMSE==0.26)"),
           x = 5, y = 1, size = 2.5, parse = TRUE)

###plots together
leg <- ggpubr::get_legend(p.co2)
leg_plot <- ggarrange(leg,nrow=1)

full_1 <- plot_grid(p.co2.ele  + rremove("legend") , p.co2+ rremove("legend"), p.dtw.co2+ rremove("legend"),leg_plot,ncol = 2, align = "v",labels=c("(a)","(b)","(c)"))
