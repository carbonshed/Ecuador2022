#supplementary figure 10
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)

k_df <- read.csv(here::here("PondPaper_Revised/Pond_discrete_data.csv"))
k_df$Date <- as.Date(k_df$Date,format="%Y-%m-%d")
k_df_summer <- k_df%>%filter(Date<"2022-10-01")
k_df_summer_positvevalues <- k_df%>%filter(Date<"2022-10-01")%>%filter(k_CO2_m_d>0)

predictor_variables <- read.csv(here::here("PondPaper_Revised/predictor_variables_df.csv"))
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")
predictor_variables <- unique(predictor_variables)

k_df_summer_positvevalues <- left_join(k_df_summer_positvevalues,predictor_variables,by=c("Site","Date"))


#plot failed predictors of k

p2.1 <- ggplot(k_df_summer_positvevalues%>%drop_na(K600_m_d)) + geom_point(
  aes(x=surface_area_m2,y=K600_m_d),fill="#31a354",
  shape=21,size=3)  + 
  geom_hline(aes(yintercept = median(K600_m_d)), color="blue", linetype="dashed") +
  theme_bw(base_size = 14) +
  scale_x_log10() + scale_y_log10() +
  xlab(expression(paste("Surface Area (",m^2, ")"))) +
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) 

p2.2 <- ggplot(k_df_summer_positvevalues%>%drop_na(K600_m_d)) + geom_point(
  aes(x=windspeed_m_s,y=K600_m_d),fill="#31a354",
  shape=21,size=3)  + 
  geom_hline(aes(yintercept = median(K600_m_d)), color="blue", linetype="dashed") +
  theme_bw(base_size = 14) +
  scale_x_log10() + scale_y_log10() +
  xlab(expression(paste("Windspeed (m ",s^-1,")"))) + 
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) 


p2.3 <- ggplot(k_df_summer_positvevalues %>%drop_na(K600_m_d)) + geom_point(
  aes(x=PrecipAccuDay_mm,y=K600_m_d),fill="#31a354",
  shape=21,size=3)  + 
  geom_hline(aes(yintercept = median(K600_m_d)), color="blue", linetype="dashed") +
  theme_bw(base_size = 14) +
  scale_x_log10() + scale_y_log10() +
  xlab(expression(paste("Precipitation Accumulation (mm)"))) + 
  ylab(expression(paste(italic('k')[600]," (m ", d^-1,")"))) 

p2.full <- ggarrange(p2.1,p2.2,p2.3,labels="AUTO",nrow=2,ncol=2)

