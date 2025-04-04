WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))
WL_df <- WL_df%>%
  select(DateTime,Station,Baro_kpa,BaroTemp_c,WLTemp_c,WaterLevel_m,depth_ave_m,surface_area_m2,Volumn_m3)%>%
  rename(Site=Station)%>%rename(WaterTemp_c=WLTemp_c,AirPressure_kPa=Baro_kpa,AirTemp_c=BaroTemp_c)
WL_df$Site <- gsub(".*_","",WL_df$Site)
WL_df$WaterTemp_c <- ifelse(WL_df$WaterTemp_c < 0, 0, WL_df$WaterTemp_c)

WL_df$Site_ID <- WL_df$Site
#make column with new name to reflect elevation. S stands for "site"
WL_df <- WL_df%>%
  mutate(Site_ID = case_match(Site_ID, 
                               'Wetland01' ~ 'S1', 
                               'Wetland02' ~ 'S2',
                               'Wetland03' ~ 'S3',
                               'Wetland04' ~ 'S4',
                               'Wetland05' ~ 'S8',
                               'Wetland06' ~ 'S5',
                               'Wetland07' ~ 'S7',
                               'Wetland08' ~ 'S12',
                               'Wetland09' ~ 'S10',
                               'Wetland10' ~ 'S11',
                               'Wetland11' ~ 'S6',
                               'Wetland12' ~ 'Wetland'))
WL_df$Site <- NULL
WL_df <- WL_df%>%rename(AirTemp_c=AirTemp)%>%
  select(DateTime,Site_ID,AirPressure_kPa,AirTemp_c,WaterTemp_c,WaterLevel_m,depth_ave_m,surface_area_m2,Volumn_m3)

#write.csv(WL_df,here::here("PondPaper_Repository/continuous_data.csv"),row.names = FALSE)

## GHG_sampleing

sum_table <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))%>%
  select(Date,DateTime,Site,DOC_mg.L,TDN_mg.L,Elevation_m,percent_DTW,WS_area_minus_pond,
         AirPress_kpa,AirTemp_c,Watertemp_c,depth_ave_m,surface_area_m2,Volumn_m3,
         SA_to_Vol_ratio,PrecipAccuDay_mm,windspeed_m_s,
         pCO2_ppm,CO2_umol.L,CO2_sat_precent,pCH4_ppm,CH4_umol.L,CH4_sat,
         pCO2_air_atm,
         Flux_umol_m2_s,KH_mol.m3.atm, KH_CH4_mol.L.atm,k_m_d,k_ch4,K600)%>%rename(
           AirPressure_kPa=AirPress_kpa,WaterTemp_c=Watertemp_c,CH4_sat_precent=CH4_sat,
           Flux_CO2_umol.m2.s=Flux_umol_m2_s,KH_CO2_mol.m3.atm=KH_mol.m3.atm,
           k_CO2_m.d=k_m_d,k_ch4_m.d=k_ch4
         )
sum_table$Date <- as.Date(sum_table$Date)

sum_table_before <- sum_table%>%filter(Date<"2022-09-01")
sum_table_after <- sum_table%>%filter(Date>"2022-09-01")
sum_table_after$Flux_CO2_umol.m2.s <- NA
sum_table_after$KH_CO2_mol.m3.atm <- NA
sum_table_after$KH_CH4_mol.L.atm <- NA
sum_table_after$k_ch4_m.d <- NA
sum_table_after$k_CO2_m.d <- NA

sum_table <- rbind(sum_table_before,sum_table_after)


sum_table <- sum_table%>%
  mutate(Site = case_match(Site, 
                              'Wetland01' ~ 'S1', 
                              'Wetland02' ~ 'S2',
                              'Wetland03' ~ 'S3',
                              'Wetland04' ~ 'S4',
                              'Wetland05' ~ 'S8',
                              'Wetland06' ~ 'S5',
                              'Wetland07' ~ 'S7',
                              'Wetland08' ~ 'S12',
                              'Wetland09' ~ 'S10',
                              'Wetland10' ~ 'S11',
                              'Wetland11' ~ 'S6',
                              'Wetland12' ~ 'Wetland'))%>%rename(
                                Site_ID=Site
                              )

#write.csv(sum_table,here::here("PondPaper_Repository/GHGsampling_discrete_data.csv"),row.names = FALSE)

####################

modeled_GHG_df <- read.csv(here::here("ProcessedData/modeled_GHG_df_nov6.csv"))
modeled_GHG_df$DateTime <- as.POSIXct(modeled_GHG_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
modeled_GHG_df <- modeled_GHG_df%>%filter(DateTime > "2022-07-06 00:00:00" & DateTime < "2023-07-06 00:00:00")
modeled_GHG_df <- modeled_GHG_df%>%rename(Site_ID=Site)

modeled_GHG_df <- modeled_GHG_df%>%
  mutate(Site_ID = case_match(Site_ID, 
                           'Wetland01' ~ 'S1', 
                           'Wetland02' ~ 'S2',
                           'Wetland03' ~ 'S3',
                           'Wetland04' ~ 'S4',
                           'Wetland05' ~ 'S8',
                           'Wetland06' ~ 'S5',
                           'Wetland07' ~ 'S7',
                           'Wetland08' ~ 'S12',
                           'Wetland09' ~ 'S10',
                           'Wetland10' ~ 'S11',
                           'Wetland11' ~ 'S6',
                           'Wetland12' ~ 'Wetland'))


seq_df <- seq(as.POSIXct("2022-07-06 00:00:00"), as.POSIXct("2023-07-06 00:00:00"), by="hour")
seq_df <- as.data.frame(seq_df)
colnames(seq_df) <- "DateTime"

modeled_GHG_df_2 <- left_join(seq_df,modeled_GHG_df,by=c("DateTime"))
modeled_GHG_df_2 <- modeled_GHG_df_2%>%
  drop_na(Site_ID)%>%
  select(Site_ID, DateTime,WaterTemp_c,                                    
         flux_mol.sec.m2_co2_fixed,flux_mol.sec.m2_co2_fluctuate,flux_mol.sec.m2_ch4_fixed,flux_mol.sec.m2_ch4_fluctuate,
         F_mol.s_CH4,F_mol.s_CH4_fixedSA,F_mol.s_CH4_fixedTemp,F_mol.s_CH4_fixedTempfixedSA,
         F_mol.s_CO2,F_mol.s_CO2_fixedSA,F_mol.s_CO2_fixedTemp,F_mol.s_CO2_fixedTempfixedSA)



#write.csv(modeled_GHG_df_2,here::here("PondPaper_Repository/modeled_GHG_data.csv"),row.names = FALSE)


#####
drone_GHG <- read.csv(here::here("ProcessedData/Drone_Gavi_sampling_GHG.csv"))%>%
  select(Lat,Lon,Site, Site2, Date, AirPress_kpa,Watertemp_c,CO2_ppm,CH4_umol.L, CO2_umol.L)%>%
  rename(pCO2_ppm=CO2_ppm,WaterTemp_c=Watertemp_c,AirPressure_kPa=AirPress_kpa)

##pecent diff of dups

drone_GHG_summ <- drone_GHG%>%group_by(Lat,Lon,Site,Date,Site2)%>%summarise(
  CH4_umol.L = mean(CH4_umol.L,rm.na=TRUE),
  CO2_umol.L = mean(CO2_umol.L,rm.na=TRUE),
  pCO2_ppm = mean(pCO2_ppm,rm.na=TRUE),
  AirPressure_kPa = mean(AirPressure_kPa,rm.na=TRUE),
  WaterTemp_c = mean(WaterTemp_c,rm.na=TRUE)
)

drone_GHG_summ$Date <- as.Date(drone_GHG_summ$Date,format = "%m/%d/%y")
drone_GHG_summ$Location <- "margins"

df <- drone_GHG_summ

#add henry's constant CH4
kH_STP_mol.L.atm = .0014182
dlnHcppersperK = 1600
T_STP_K = 298.15
df$KH_CH4_mol.L.atm <- kH_STP_mol.L.atm*exp(dlnHcppersperK*(1/(df$WaterTemp_c+273.15)-1/T_STP_K))

#Ambient CH4 concentration ppb	1910.97 	
#average 2021 from Moaa
CH4_air_ppb <- 1910.97 
df$CH4_air_atm <- (df$AirPressure_kPa/101.3) * CH4_air_ppb * 10^-9
df$pCH4_ppm <- df$CH4_umol.L /df$KH_CH4_mol.L.atm

df$CH4_air_atm <- NULL
df$KH_CH4_mol.L.atm <- NULL

df <- df%>%select(Date,Lat,Lon,Site,Site2,Location,AirPressure_kPa,WaterTemp_c,pCO2_ppm,pCH4_ppm,CO2_umol.L,CH4_umol.L)
#write.csv(df,here::here("PondPaper_Repository/wetland_GHG_data.csv"),row.names = FALSE)

