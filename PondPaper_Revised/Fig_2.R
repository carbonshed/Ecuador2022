# Figure 2
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)

#read in CH4 data
CH4_summary <- read.csv(here::here("PondPaper_Revised/Pond_pCH4_data_summary.csv"))
CH4_summary$Date <- as.Date(CH4_summary$Date,format="%Y-%m-%d")

#read in CO2 data
CO2_summary <- read.csv(here::here("PondPaper_Revised/Pond_pCO2_data_summary.csv"))
CO2_summary$Date <- as.Date(CO2_summary$Date,format="%Y-%m-%d")
CO2_summary <- CO2_summary %>% mutate(pCO2_w_uatm_sd = ifelse(is.na(pCO2_w_uatm_sd), 0, pCO2_w_uatm_sd))

CO2_air_max <- max(CO2_summary$pCO2_air_uatm,na.rm = TRUE)
CO2_air_min <- min(CO2_summary$pCO2_air_uatm,na.rm = TRUE)

CH4_air_max <- max(CH4_summary$CH4_air_atm*10^6,na.rm = TRUE)
CH4_air_min <- min(CH4_summary$CH4_air_atm*10^6,na.rm = TRUE)

#plot
options(scipen = 100)
p.ch4 <- ggplot(CH4_summary,aes(x=reorder(Site,pCH4_w_uatm,mean,decreasing = FALSE),y=pCH4_w_uatm)) +
  geom_rect(aes(ymin=CH4_air_min, ymax=CH4_air_max, xmin=0, xmax=Inf),fill="red",alpha=.5) +
  stat_summary(geom = "point", fun = "mean", col = "red", size = 2, shape = 24, fill = "red") +
  geom_errorbar(aes(ymin = pCH4_w_uatm-pCH4_w_uatm_sd, ymax = pCH4_w_uatm+pCH4_w_uatm_sd), position = position_dodge2(width = 0.1), width = 0.3) +
  scale_y_log10() +
  ylab(expression(italic(p)*paste("CH"[4] ," (",mu,"atm)"))) +  xlab("") +
  theme_classic2(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  annotate("text", x = "S1", y = 15000, label = "A",size = 9/.pt) +
  annotate("text", x = "S2", y = 15000, label = "A",size = 9/.pt) +
  annotate("text", x = "S3", y = 15000, label = "AB",size = 9/.pt) +
  annotate("text", x = "S4", y = 15000, label = "CD",size = 9/.pt) +
  annotate("text", x = "S5", y = 15000, label = "BCD",size = 9/.pt) +
  annotate("text", x = "S6", y = 15000, label = "ABC",size = 9/.pt) +
  annotate("text", x = "S7", y = 15000, label = "AB",size = 9/.pt) +
  annotate("text", x = "S8", y = 15000, label = "CDE",size = 9/.pt) +
  annotate("text", x = "Wetland", y = 15000, label = "ABC",size = 9/.pt) +
  annotate("text", x = "S10", y = 15000, label = "E",size = 9/.pt) +
  annotate("text", x = "S11", y = 15000, label = "E",size = 9/.pt) +
  annotate("text", x = "S12", y = 15000, label = "DE",size = 9/.pt)


p.co2 <- ggplot(CO2_summary,aes(x=reorder(Site,pCO2_w_uatm,mean,decreasing = FALSE),y=pCO2_w_uatm)) +
  geom_rect(aes(ymin=CO2_air_min, ymax=CO2_air_max, xmin=0, xmax=Inf),fill="red",alpha=.5) +
  stat_summary(geom = "point", fun = "mean", col = "red", size = 2, shape = 24, fill = "red") +
  geom_errorbar(aes(ymin = pCO2_w_uatm-pCO2_w_uatm_sd, ymax = pCO2_w_uatm+pCO2_w_uatm_sd), position = position_dodge2(width = 0.1), width = 0.3) +
  scale_y_log10() +
  ylab(expression(italic(p)*paste("CO"[2] ," (",mu,"atm)"))) +  xlab("Site") +
  theme_classic2(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  annotate("text", x = "S1", y = 15000, label = "AB",size = 9/.pt) + 
  annotate("text", x = "S2", y = 15000, label = "A",size = 9/.pt) +
  annotate("text", x = "S3", y = 15000, label = "AB",size = 9/.pt) +
  annotate("text", x = "S4", y = 15000, label = "B",size = 9/.pt) +
  annotate("text", x = "S5", y = 15000, label = "AB",size = 9/.pt) +
  annotate("text", x = "S6", y = 15000, label = "AB",size = 9/.pt) +
  annotate("text", x = "S7", y = 15000, label = "AB",size = 9/.pt) +
  annotate("text", x = "S8", y = 15000, label = "AB",size = 9/.pt) +
  annotate("text", x = "Wetland", y = 15000, label = "C",size = 9/.pt) +
  annotate("text", x = "S10", y = 15000, label = "DE",size = 9/.pt) +
  annotate("text", x = "S11", y = 15000, label = "CD",size = 9/.pt) +
  annotate("text", x = "S12", y = 15000, label = "E",size = 9/.pt)


p.all <- ggarrange(p.ch4, p.co2, labels = c("(a)", "(b)"),heights=c(2,2), ncol = 1, nrow = 2)

