#supplementary figure 9
library(here)
library(dplyr)
library(ggplot2)

k_df <- read.csv(here::here("data/Pond_discrete_data.csv"))
k_df$Date <- as.Date(k_df$Date,format="%Y-%m-%d")

k_df_summer <- k_df%>%filter(Date<"2022-10-01")


CO2_air_max <- max(k_df_summer$pCO2_air_uatm,na.rm = TRUE)
CO2_air_min <- min(k_df_summer$pCO2_air_uatm,na.rm = TRUE)
CO2_air_ave <- mean(k_df_summer$pCO2_air_uatm,na.rm = TRUE)

#plot
p1 <- ggplot(k_df_summer%>%filter(K600_m_d>0) 
             %>%drop_na(Flux_CO2_mol_m2_d),aes(x=pCO2_w_uatm,y=Flux_CO2_mol_m2_d)) + geom_point(aes(fill=Site),shape=21,size=3) +  
  geom_smooth(method='lm', color='black') + #guides(fill=guide_legend(title=expression(paste(italic('k')[600]," (m ", d^-1,")"))))+
  labs(fill="Site")+
  annotate("text", label = paste0("atop( R^2==0.78,RMSE==0.57)"),
           x = 3500, y = 0.001, size = 4, parse = TRUE) +
  geom_rect(aes(xmin=CO2_air_min, xmax=CO2_air_max, ymin=0, ymax=Inf),fill="red",alpha=.5) +
  ylab(expression(italic(p)~paste("CO"[2] ,"  Evasion (mol ", m^-2," ", d^-1,")"))) +
  xlab(expression(italic(p)*paste("CO"[2] ," (",mu,"atm)"))) +
  scale_x_continuous(transform = "log10") + scale_y_continuous(transform = "log10") +
  theme_bw(base_size = 14) 

