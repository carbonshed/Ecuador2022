# Figure 7
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)

#read in wetland center data
df_CO2_full <- read.csv(here::here("PondPaper_Revised/Pond_pCO2_data.csv"))%>%
  filter(Site=="Wetland")%>%
  dplyr::select(pCO2_w_uatm)%>%
  rename(partialpressure=pCO2_w_uatm)
df_CO2_full$gas <- "pCO2"

df_CH4_full <- read.csv(here::here("PondPaper_Revised/Pond_pCH4_data.csv"))%>%
  filter(Site=="Wetland")%>%
  dplyr::select(pCH4_w_uatm)%>%
  rename(partialpressure=pCH4_w_uatm)
df_CH4_full$gas <- "pCH4"

df_center <- rbind(df_CO2_full,df_CH4_full)
df_center$Location <- "center"

#read in wetland margins data
drone_co2 <- read.csv(here::here("PondPaper_Revised/Wetland_edges_df.csv"))%>%
  dplyr::select(pCO2_w_uatm)%>%rename(partialpressure=pCO2_w_uatm)
drone_co2$gas <- "pCO2"

drone_ch4 <- read.csv(here::here("PondPaper_Revised/Wetland_edges_df.csv"))%>%
  dplyr::select(pCH4_w_uatm)%>%rename(partialpressure=pCH4_w_uatm)
drone_ch4$gas <- "pCH4"

df_margins <- rbind(drone_co2,drone_ch4)
df_margins$Location <- "margins"

df_all <- rbind(df_center,df_margins)

###


p2 <- ggplot(df_all,aes(x=gas, y=partialpressure,fill=Location)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(shape=21,color="black",position = position_jitterdodge(jitter.width = .25, jitter.height = 0, dodge.width = 0.75)) + ylab("") + xlab("") +
  scale_fill_grey(labels=c("open-water \ntransect","wetland \nmargins")) + 
  scale_x_discrete(labels=c("pCH4" = expression(italic(p)*paste("CH"[4] ," (",mu,"atm)")), "pCO2" = expression(italic(p)*paste("CO"[2]~" (",mu,"atm)" )))) +
  scale_y_continuous(transform = "log1p",breaks=c(1,10,100,1000,10000)) + theme_bw(base_size = 16) + theme(legend.position="top")

