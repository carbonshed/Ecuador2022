CO2_summary <- read.csv(here::here("ProcessedData/CO2_summary_June24.csv"))
CO2_summary$Date <- as.Date(CO2_summary$Date,format="%Y-%m-%d")

predictor_variables <- read.csv(here::here("ProcessedData/predictor_variables_July2.csv"))
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")

df <- full_join(CO2_summary,predictor_variables,by=c("New.Name","Date"))


##add elevation levels
df$log_SA <- log(df$surface_area_m2)
df$log_WS <- log(df$WS_area_minus_pond)
df$log_waterlevel <- log(df$WaterLevel_m)

# get list of residuals
model <- lm(log(df$pCO2_w_uatm)~df$Elevation_m)
df$res <- resid(model) 


gmc_co2 <- df %>%
  dplyr::select(res,pCO2_w_uatm,New.Name,Elevation_m,percent_DTW,WS_area_minus_pond,Watertemp_c,Volumn_m3,surface_area_m2,SA_to_Vol_ratio,WaterLevel_m,depth_ave_m,log_SA,log_WS)%>%
  group_by(Elevation_m,percent_DTW,
           WS_area_minus_pond,log_WS) %>%
  mutate(mean_res = mean(res),
         median_res = median(res)) %>%
  mutate( gpm_WaterTemp = mean(Watertemp_c),
          WaterTemp_centered = Watertemp_c - gpm_WaterTemp) %>%
  mutate( gpm_SA = mean(surface_area_m2),
          SA_centered = surface_area_m2 - gpm_SA) %>%
  mutate( gpm_depth = mean(depth_ave_m),
          depth_centered = depth_ave_m - gpm_depth) %>%
  mutate(gpm_wl = mean(WaterLevel_m),
         WaterLevel_centered = WaterLevel_m - gpm_wl) %>%
  mutate( gpm_log_SA = mean(log_SA),
          log_SA_centered = log_SA - gpm_log_SA) %>% 
  mutate( gpm_vol = mean(Volumn_m3),
          vol_centered = Volumn_m3 - gpm_vol) %>% 
  mutate( gpm_SAtovol = mean(SA_to_Vol_ratio),
          SAtovol_centered = SA_to_Vol_ratio - gpm_SAtovol) %>% 
  ungroup()

co2_m1 <- lmer(log(pCO2_w_uatm) ~ 
                 scale(gpm_WaterTemp)+ #GROUP mean centered!
                 WaterTemp_centered + #GROUP means
               #  scale(Elevation_m)+
                 (1 |New.Name), data =gmc_co2#%>%filter(New.Name!="Wetland")
)
summary(co2_m1)


co2_m2 <- lmer(log(pCO2_w_uatm) ~ 
                 scale(gpm_depth)+ #GROUP mean centered!
                 depth_centered + #GROUP means
               #  scale(Elevation_m)+
                   (1 |New.Name), data =gmc_co2#%>%filter(New.Name!="Wetland")
)

summary(co2_m2)


co2_m3 <- lmer(log(pCO2_w_uatm) ~ 
                 scale(WS_area_minus_pond) +
               #  scale(Elevation_m)+
                 (1 |New.Name), data =gmc_co2#%>%filter(New.Name!="Wetland")
)
summary(co2_m3)

co2_res <- lmer(res ~ 
                #     scale(Elevation_m)+
                   log(percent_DTW) +
                   (1 |New.Name), data =gmc_co2#%>%filter(New.Name!="Wetland")
)
summary(co2_res)
performance::model_performance(co2_res)


co2_dtw <- lmer(log(pCO2_w_uatm) ~ 
                       scale(Elevation_m)+
                  log(percent_DTW) +
                  (1 |New.Name), data =gmc_co2#%>%filter(New.Name!="Wetland")
)
summary(co2_dtw)
performance::model_performance(co2_dtw)

modelsummary::modelsummary(list(co2_m1,co2_m2,co2_m3,co2_res),stars=T)



####DOC


DOC_df <- read.csv(here::here("ProcessedData/predictor_variables_July2.csv"))
#just the days when DOC was sample
DOC_df$Date <- as.Date(DOC_df$Date)
DOC_df1 <- DOC_df%>%filter(Date==as.Date("2022-07-27")|Date==as.Date("2022-07-25"))
DOC_df2 <- DOC_df%>%filter(Date==as.Date("2022-07-22"))%>%filter(New.Name!="Wetland")
DOC_df3 <- DOC_df%>%filter(Date==as.Date("2022-07-19"))%>%filter(New.Name!="Wetland06")%>%filter(New.Name!="Wetland08")
DOC_df4 <- DOC_df%>%filter(Date==as.Date("2022-07-18"))

DOC_df_all <- rbind(DOC_df1,DOC_df2,DOC_df3,DOC_df4)
DOC_df_all <- left_join(DOC_df_all,CO2_summary,by=c("New.Name","Date"))

# get list of residuals
model <- lm(log(DOC_df_all$pCO2_w_uatm)~DOC_df_all$Elevation_m)
DOC_df_all$res <- resid(model) 


M1 <- lm(log(pCO2_w_uatm) ~ 
           scale(DOC_mg.L), 
         #    scale(Elevation_m),
         data =DOC_df_all)
summary(M1)


M1 <- lm(log(pCO2_w_uatm) ~ 
           log(DOC_mg.L)+ 
           scale(Elevation_m), 
         data =DOC_df_all)
summary(M1)

M1 <- lm(log(pCO2_w_uatm) ~ 
           log(DOC_mg.L)+ 
           scale(Elevation_m), 
         data =DOC_df_all)
summary(M1)

M1 <- lm(log(pCO2_w_uatm) ~ 
           log(TDN_mg.L),#+ 
         #            scale(Elevation_m), 
         data =DOC_df_all)
summary(M1)

M1 <- lm(DOC_mg.L ~ 
           res,#+ 
         #            scale(Elevation_m), 
         data =DOC_df_all)
summary(M1)
