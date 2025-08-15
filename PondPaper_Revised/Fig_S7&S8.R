#Supplimentary Figure 7 and 8

#ch4 
df_CH4_full <- read.csv(here::here("PondPaper_Revised/Pond_pCH4_data.csv"))
df_CH4_full$Date <- as.Date(df_CH4_full$Date,format="%Y-%m-%d")
df_CH4_full$Date_2 <-format(df_CH4_full$Date, "%m-%d")

#co2
df_CO2_full <- read.csv(here::here("PondPaper_Revised/Pond_pCO2_data.csv"))%>%drop_na(Date)%>%drop_na(pCO2_w_uatm)
df_CO2_full$Date <- as.Date(df_CO2_full$Date,format="%Y-%m-%d")
df_CO2_full$Date_2 <-format(df_CO2_full$Date, "%m-%d")

#add letters of significance
sig_letters <- read.csv(here::here("PondPaper_Revised/signifacantLetters_df.csv"))
sig_letters$Date <- as.Date(sig_letters$Date,format="%m/%d/%y")

sig_letters_ch4 <- full_join(df_CH4_full%>%dplyr::select(Site,Date,Date_2,pCH4_w_uatm),sig_letters,by=c("Site","Date"))

sig_letters_co2 <- full_join(df_CO2_full%>%dplyr::select(Site,Date,Date_2,pCO2_w_uatm),sig_letters,by=c("Site","Date"))

sig_letters_co2 <- sig_letters_co2%>%
  group_by(Site)%>%
  mutate(
    max_co2 = max(pCO2_w_uatm),
    sd_co2 = sd(pCO2_w_uatm)
  )

sig_letters_ch4 <- sig_letters_ch4%>%
  group_by(Site)%>%
  mutate(
    max_ch4 = max(pCH4_w_uatm),
    sd_ch4 = sd(pCH4_w_uatm)
  )

df_CH4_full$Site <- factor(df_CH4_full$Site, levels = c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Wetland","S10","S11","S12"))

df_CO2_full$Site <- factor(df_CO2_full$Site, levels = c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Wetland","S10","S11","S12"))

sig_letters_ch4$Site <- factor(sig_letters_ch4$Site, levels = c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Wetland","S10","S11","S12"))

sig_letters_co2$Site <- factor(sig_letters_co2$Site, levels = c("S1", "S2", "S3", "S4", "S5", "S6","S7","S8","Wetland","S10","S11","S12"))

CO2_air_max <- max(df_CO2_full$pCO2_air_uatm,na.rm = TRUE)
CO2_air_min <- min(df_CO2_full$pCO2_air_uatm,na.rm = TRUE)

CH4_air_max <- max(df_CH4_full$CH4_air_atm*10^6,na.rm = TRUE)
CH4_air_min <- min(df_CH4_full$CH4_air_atm*10^6,na.rm = TRUE)

#ch4
p <- ggplot(df_CH4_full,aes(x=as.factor(Date_2),y=pCH4_w_uatm)) +
  geom_point() +
  facet_wrap(Site ~ ., scales = "free") +
  theme_bw(base_size = 10) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  ylab(expression(italic(p)*paste("CH"[4] ," (",mu,"atm)"))) +  xlab("")

p1 <- p + 
  geom_text(size=3,data = sig_letters_ch4, aes(x = as.factor(Date_2), y = max_ch4+sd_ch4, label = Letter_ch4)) + scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) 

#Figure S8: CO2

p2 <- ggplot(df_CO2_full%>%drop_na(Site),aes(x=as.factor(Date_2),y=pCO2_w_uatm)) +
  geom_point() +
  facet_wrap(Site ~ ., scales = "free") +
  theme_bw(base_size = 10) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  ylab(expression(italic(p)*paste("CO"[2] ," (",mu,"atm)"))) +  xlab("")

p3 <- p2 + 
  geom_text(size=3,data = sig_letters_co2%>%drop_na(Date), aes(x = as.factor(Date_2), y = max_co2+sd_co2, label = Letter_co2)) + scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) 



