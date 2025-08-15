#Figure 3
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(cowplot)

CH4_summary <- read.csv(here::here("PondPaper_Revised/Pond_pCH4_data_summary.csv"))
CH4_summary$Date <- as.Date(CH4_summary$Date,format="%Y-%m-%d")

predictor_variables <- read.csv(here::here("PondPaper_Revised/predictor_variables_df.csv"))
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")

df <- left_join(CH4_summary%>%filter(pCH4_w_uatm>3),predictor_variables,by=c("Site","Date"))


# get list of residuals
model <- lm(log(df$pCH4_w_uatm)~df$Elevation_m)
df$res <- resid(model) 



gmc_ch4 <- df %>%
  dplyr::select(pCH4_w_uatm,pCH4_ppm,Site,Elevation_m,Watertemp_c)%>%
  group_by(Elevation_m) %>%
  mutate(mean_pCH4_uatm=mean(pCH4_w_uatm))%>%
  mutate( gpm_Watertemp_c = mean(Watertemp_c),
          Watertemp_c_centered = Watertemp_c - gpm_Watertemp_c)  %>% 
  ungroup()

#CH4 plots
xlab <- "Water temperature (°C)"
p.ch4 <- ggplot(gmc_ch4,aes(x=Watertemp_c,y=pCH4_w_uatm,color = reorder(Site,Elevation_m,median,decreasing = TRUE))) +
  geom_point(size=2)+
  geom_point(data=gmc_ch4,aes(x=Watertemp_c,y=pCH4_w_uatm,),shape=1,size=2,color="black")+
  geom_smooth(method='lm',formula = "y ~ x",se=FALSE) +
  geom_point(data=gmc_ch4,aes(x=gpm_Watertemp_c,y=mean_pCH4_uatm,fill=Elevation_m),color="black",shape=23,size=4)+
  geom_point(data=gmc_ch4,aes(x=gpm_Watertemp_c,mean_pCH4_uatm),shape=3,size=1,color="black") +
  guides(color=guide_legend(ncol=2)) +
  ylab(expression(italic(p)*paste("CH"[4] ," (",mu,"atm)"))) +
  xlab(xlab) +
  scale_color_viridis(name="Site",discrete = TRUE) +
  scale_fill_continuous(name = "Elevation (m)") +
  scale_y_continuous(transform="log",breaks=c(10,100,1000,10000)) +
  theme_bw(base_size = 14) +
  annotate("text", label = paste0("atop('marg. '* R^2==0.81,RMSE==0.43)"),
           x = 5, y = 3800, size = 2.5, parse = TRUE)


p.ch4.ele <- ggplot() +
  geom_smooth(data=gmc_ch4,aes(x=Elevation_m,y=pCH4_w_uatm), method='lm',formula = "y ~ x",se=FALSE,color="black") +
  geom_point(data=gmc_ch4,aes(x=Elevation_m,y=pCH4_w_uatm,color=reorder(Site,Elevation_m,median,decreasing = TRUE)),size=2) +
  geom_point(data=gmc_ch4,aes(x=Elevation_m,y=pCH4_w_uatm),shape=1,size=2) +
  ylab(expression(italic(p)*paste("CH"[4] ," (",mu,"atm)"))) +  xlab('Elevation (m)') +
  scale_color_viridis(name="Site",discrete = TRUE) +
  scale_y_continuous(transform="log",breaks=c(10,100,1000,10000)) +
  theme_bw(base_size = 14)+
  annotate("text", label = paste0("atop('marg. '* R^2==0.64,RMSE==0.67)"),
           x = 4040, y = 100, size = 2.5, parse = TRUE)


full_1 <- plot_grid(p.ch4 + 
                      rremove("legend") ,p.ch4.ele+ 
                      rremove("legend"),nrow=2,labels=c("(a)","(b)"))

leg <- ggpubr::get_legend(p.ch4)
full_4 <- ggarrange(leg,nrow=1)

full_5 <- ggarrange(full_1,full_4,widths = c(4,3), ncol = 2)

