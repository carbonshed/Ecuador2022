#plot water temp as driver of CO2
CH4_summary <- read.csv(here::here("ProcessedData/CH4samples_summary_June24.csv"))
CH4_summary$Date <- as.Date(CH4_summary$Date,format="%Y-%m-%d")

predictor_variables <- read.csv(here::here("ProcessedData/predictor_variables_July2.csv"))#%>%select(New.Name,Date,Elevation_m)
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")

df <- left_join(CH4_summary%>%filter(pCH4_w_uatm>3),predictor_variables,by=c("New.Name","Date"))


##add elevation levels
df$log_SA <- log(df$surface_area_m2)
df$log_WS <- log(df$WS_area_minus_pond)
df$log_waterlevel <- log(df$WaterLevel_m)

# get list of residuals
model <- lm(log(df$pCH4_w_uatm)~df$Elevation_m)
df$res <- resid(model) 



gmc_ch4 <- df %>%
  dplyr::select(res,pCH4_w_uatm,pCH4_ppm,New.Name,Elevation_m,percent_DTW,WS_area_minus_pond,Watertemp_c,Volumn_m3,surface_area_m2,SA_to_Vol_ratio,WaterLevel_m,depth_ave_m,log_SA,log_WS)%>%
  group_by(Elevation_m,percent_DTW,
           WS_area_minus_pond,log_WS) %>%
  mutate(mean_res = mean(res),
         median_res = median(res)) %>%
  mutate( gpm_Watertemp_c = mean(Watertemp_c),
          Watertemp_c_centered = Watertemp_c - gpm_Watertemp_c) %>%
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

ch4_mod1 <- lmer(log(pCH4_w_uatm) ~ 
                   #Level 1 (time) 
                   Watertemp_c_centered+ #GROUP mean centered!
                   #Level 2 (pond-level)
                   scale(gpm_Watertemp_c)+ #GROUP means
               #    scale(Elevation_m)+
                   (1 |New.Name), data =gmc_ch4)
summary(ch4_mod1)
modelPerformance(ch4_mod1)


ch4_m2.1 <- lmer(log(pCH4_w_uatm) ~ 
                scale(gpm_SA)+ #GROUP means
                  SA_centered + #GROUP mean centered!
                 scale(gpm_Watertemp_c)+
                 (1 |New.Name), data =gmc_ch4 %>%
                  filter(New.Name!="Wetland")
)
summary(ch4_m2.1)

ch4_m2.2 <- lmer(log(pCH4_w_uatm) ~ 
                 depth_ave_m + #GROUP mean centered!
                 scale(gpm_Watertemp_c)+
                 (1 |New.Name), data =gmc_ch4 %>%filter(pCH4_w_uatm>3)%>%filter(New.Name!="Wetland")
)
summary(ch4_m2.2)
modelsummary::modelsummary(list(ch4_mod1,ch4_m2.2),stars=T)


#significant: depth_centered,SAtovol_centered
#insignificant: gpm_SA,gpm_depth, gpm_wl,gpm_vol,SA_centered,WaterLevel_centered,vol_centered,gpm_SAtovol

ch4_m3 <- lmer(log(pCH4_w_uatm) ~ 
                 scale(WS_area_minus_pond) +
                 scale(gpm_Watertemp_c)+
                 (1 |New.Name), data =gmc_ch4 #%>%filter(New.Name!="Wetland")
)
summary(ch4_m3)

ch4_res <- lmer(res ~ 
                  #     scale(Elevation_m)+
                  depth_ave_m +
                  (1 |New.Name), data =gmc_ch4#%>%filter(New.Name!="Wetland")
)
summary(ch4_res)


