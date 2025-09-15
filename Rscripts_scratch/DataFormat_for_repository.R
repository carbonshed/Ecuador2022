#This rscript is intended to format data frames for the PondPaper repository

WL_df <- read.csv(here::here("Wetlands/WaterLevel_FINAL/WL_Wetland_all_FINAL.csv"))

WL_df$trashit <- str_sub(WL_df$Station,4)

WL_df <- WL_df%>%mutate(Site = case_match(trashit, 'Wetland01' ~ 'S1', 'Wetland02' ~ 'S2','Wetland03' ~ 'S3','Wetland04' ~ 'S4','Wetland05' ~ 'S8','Wetland06' ~ 'S5','Wetland07' ~ 'S7','Wetland08' ~ 'S12','Wetland09' ~ 'S10','Wetland10' ~ 'S11','Wetland11' ~ 'S6','Wetland12' ~ 'Wetland'))
WL_df$Station <- NULL
WL_df$X <- NULL
WL_df$trashit <- NULL
WL_df$depth_diff_m <- NULL
WL_df$WLPres_kpa <- NULL
WL_df <- WL_df%>%rename(AirTemp_c=BaroTemp_c,WaterTemp_c=WLTemp_c)

#write.csv(WL_df,here::here("PondPaper_Revised/Pond_continuous_data.csv"))

##just CO2
df_CO2_full <- read.csv(here::here("ProcessedData/All_CO2_samples_June24.csv"))%>%rename(Site=New.Name,pCO2_ppm=ppm_Corrected)
df_CO2_full$Date <- as.Date(df_CO2_full$Date,format="%Y-%m-%d")
df_CO2_full$X <- NULL

#write.csv(df_CO2_full,here::here("PondPaper_Revised/Pond_pCO2_data.csv"))




##just CH4##just CH4##just CH4
df_CH4_full <- read.csv(here::here("ProcessedData/CH4samples_allsamples_June24.csv"))
df_CH4_full$Site <- df_CH4_full$New.Name
df_CH4_full$New.Name <- NULL
df_CH4_full$X <- NULL
df_CH4_full$Date <- as.Date(df_CH4_full$Date,format="%Y-%m-%d")

#write.csv(df_CH4_full,here::here("PondPaper_Revised/Pond_pCH4_data.csv"))

#summarys 

CH4_summary <- read.csv(here::here("ProcessedData/CH4samples_summary_June24.csv"))%>%rename(Site=New.Name)
CH4_summary$Date <- as.Date(CH4_summary$Date,format="%Y-%m-%d")
CH4_summary$X <- NULL
#write.csv(CH4_summary,here::here("PondPaper_Revised/Pond_pCH4_data_summary.csv"))

CO2_summary <- read.csv(here::here("ProcessedData/CO2_summary_June24.csv"))%>%rename(Site=New.Name)
CO2_summary$Date <- as.Date(CO2_summary$Date,format="%Y-%m-%d")
CO2_summary$X <- NULL
#write.csv(CO2_summary,here::here("PondPaper_Revised/Pond_pCO2_data_summary.csv"))

###pond paper discrete data
df <- read.csv(here::here("ProcessedData/df_summary_June24.csv"))%>%rename(Site=New.Name)
df$X <- NULL
#write.csv(df,here::here("PondPaper_Revised/Pond_discrete_data.csv"))

#preditor variables
df_merge <- df%>%dplyr::select(Site,Date,Watertemp_c)
predictor_variables <- read.csv(here::here("ProcessedData/predictor_variables_July2.csv"))%>%rename(Site=New.Name,WaterTemp_c_yearly=waterTemp_c_yearly)
predictor_variables$X <- NULL
predictor_variables <- left_join(predictor_variables,df_merge,by=c("Date","Site"))
predictor_variables <- predictor_variables%>%dplyr::select(
  Site,Date,DOC_mg.L,TDN_mg.L,Elevation_m,AirTemp_c,Watertemp_c,WaterTemp_c_ave3,WaterTemp_c_yearly,depth_ave_m,WaterLevel_m,surface_area_m2,Volumn_m3,SA_to_Vol_ratio,WS_area_minus_pond,percentpond,percent_DTW,PrecipAccuDay_mm,PrecipAccu_mm_PreviousDay,precip_mm_ave2,Precip_mm_ave7,solarrad_W_m2,solarrad_Wm2_daymean,windspeed_m_s,winddirecion
)
#write.csv(predictor_variables,here::here("PondPaper_Revised/predictor_variables_df.csv"))


##precipitation 
precip_df <- read.csv(here::here("WeatherStation_LaVirgen/M5025_Precipitacion_Dato_validado.csv"))
colnames(precip_df) <- c("DateTime","precipt_mm")
precip_df$DateTime <- as.POSIXct(precip_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
precip_df$Date <- as.Date(precip_df$DateTime,format="%Y-%m-%d")
precip_summary <- precip_df%>%
  filter(DateTime>as.POSIXct("2022-06-16 11:00:00",tz="UTC") & DateTime < as.POSIXct("2023-08-24 13:15:00",tz="UTC"))%>%
  group_by(Date)%>%
  summarise(PrecipAccuDay_mm = sum(precipt_mm))
#Calculate 7 day average 
precip_weekAve <- transform(precip_summary, avg7 = rollmeanr(PrecipAccuDay_mm, 7, fill = NA,na.rm=TRUE))
colnames(precip_weekAve) <- c("Date","PrecipAccuDay_mm","Precip_mm_ave7")

#write.csv(precip_weekAve,here::here("PondPaper_Revised/PrecipitationData.csv"))

#letters of significance
sig_letters <- read.csv(here::here("Methane/signifacantLetters.csv"))%>%rename(Site=New.Name)
#write.csv(sig_letters,here::here("PondPaper_Revised/signifacantLetters_df.csv"))

##modeled data
modeled_GHG_df <-read.csv(here::here("ProcessedData/modeled_GHG_df_revisionJuly07.csv"))
modeled_GHG_df$Station <- modeled_GHG_df$New.Name
modeled_GHG_df$New.Name <- NULL
modeled_GHG_df$Site <- NULL
modeled_GHG_df <- modeled_GHG_df%>%rename(Site=Station,WaterTemp_c=WLTemp_c)
modeled_GHG_df$X <- NULL
#write.csv(modeled_GHG_df,here::here("PondPaper_Revised/Modeled_flux_df.csv"))

#holgerson data
df_Holg <- read.csv(here::here("Wetlands/Holgerson&Raymond_data1.csv"))%>%rename("Site_Name"="Site.Name","CH4_umol.L"="ch4..umol.L.","CO2_umol.L"="co2..umol.L.","Area_ha"="area..ha.","Watertemp_c"="temp..C.")%>%dplyr::select(Reference,Site_Name,Location,latitude,longitude,Study.Years,CH4_umol.L,CO2_umol.L,Area_ha,Watertemp_c)
#write.csv(df_Holg,here::here("PondPaper_Revised/Holgerson_and_Raymond2016_df.csv"))

#wetland data
drone_co2 <- read.csv(here::here("ProcessedData/Drone_Gavi_forImporttoArcPro_june30.csv"))

#write.csv(drone_co2,here::here("PondPaper_Revised/Wetland_edges_df.csv"))
