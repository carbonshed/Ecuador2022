df <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))

#without wetland 12 
df_1 <- df%>%filter(Site!="Wetland12")

df_high <- df%>%filter(Site=="Wetland01"| Site=="Wetland02"| Site=="Wetland03")
df_mid <- df%>%filter(Site!="Wetland01")%>%
  filter(Site!="Wetland02")%>%
  filter(Site!="Wetland03")%>%
  filter(Site!="Wetland08")%>%
  filter(Site!="Wetland09")%>%
  filter(Site!="Wetland10")%>%
  filter(Site!="Wetland12")
df_low <- df%>%filter(Site=="Wetland08"| Site=="Wetland09"| Site=="Wetland10")


ggplot(df%>%filter(Site!="Wetland01")%>%
         filter(Site!="Wetland02")%>%
         filter(Site!="Wetland03")%>%
         filter(Site!="Wetland08")%>%
         filter(Site!="Wetland09")%>%
         filter(Site!="Wetland10")%>%
         filter(Site!="Wetland12")) + 
  geom_point(aes(x=depth_ave_m,y=pCO2_ppm,color=Site)) + 
  scale_y_continuous(transform = "log")



M_AICc_CO2 <- lmer(log(pCO2_ppm) ~ 
                     scale(Watertemp_c)+ 
                     #  scale(waterTemp_c_yearly) +
                      #   scale(log(surface_area_m2)) +
                    #   scale(log(WS_area_minus_pond)) +
                     #     scale(percent_DTW) +
                    #   scale(Elevation_m) +
                    #    scale(depth_ave_m) +
                     (1 |Site), data =df_high,REML = FALSE)
sum_AICc_CO2 <- summary(M_AICc_CO2)
sum_AICc_CO2
sum_AICc_CO2_coefficients <- sum_AICc_CO2$coefficients
modelPerformance(M_AICc_CO2)
vif(M_AICc_CO2)

forward_model <- step(M_AICc_CO2, direction = "forward", scope = formula(~ .))
backward_model <- step(M_AICc_CO2, direction = "backward")
both_model <- step(M_AICc_CO2, direction = "both")

ggplot(df_test) + 
  geom_point(aes(x=percent_DTW,y=pCO2_ppm,color=Watertemp_c,shape=Site)) + 
  scale_y_continuous(transform = "log")



M_AICc_CH4 <- lmer(log(pCH4_ppm) ~ 
                     scale(Watertemp_c)+ 
                     #  scale(waterTemp_c_yearly) +
                     #   scale(log(surface_area_m2)) +
                     # scale(log(WS_area_minus_pond)) +
                    # scale(percent_DTW) +
                     #  scale(Elevation_m) +
                     #    scale(depth_ave_m) +
                     (1 |Site), data =df_high,REML = FALSE)
sum_AICc_CH4 <- summary(M_AICc_CH4)
sum_AICc_CH4
sum_AICc_CH4_coefficients <- sum_AICc_CH4$coefficients
modelPerformance(M_AICc_CH4)
vif(M_AICc_CH4)


#pCH4_ppm
#pCO2_ppm
M_AICc_CH4 <- lmer(log(pCO2_ppm) ~
                     WaterTemp_c_ave3 +
                    # Watertemp_c+ 
                    # waterTemp_c_yearly +
                     #   log(surface_area_m2) +
                     # log(WS_area_minus_pond) +
                    # percent_DTW +
                    # Elevation_m +
                     # (depth_ave_m) +
                     (1 |Site), data =df_low,REML = FALSE)

summary(M_AICc_CH4)
mp <- modelPerformance(M_AICc_CH4)
mp$Performance$MarginalR2


#significant df_test for pCO2:percent_DTW
#significant df_test for pCH4:percent_DTW and Elevation_m

group_sum <- df_test%>%
  group_by(Site)%>%
  summarise(
    mean_CO2 = mean(pCO2_ppm,na.rm=TRUE),
    mean_watertemp = mean(Watertemp_c,na.rm=TRUE)
  )

ggplot(df_test,aes(x=WaterTemp_c_ave3,y=pCO2_ppm,color=Site,group=Site)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
#  geom_point(data=group_sum,aes(x=mean_watertemp,y=mean_CO2,color=Site),size=4,shape=3) +
  scale_y_continuous(transform = "log") 



#water temp

ggplot(df,aes(x=Watertemp_c,y=pCO2_ppm,color=Site,group=Site)) + 
  geom_point() + facet_wrap(~Site,scales="free_y") + 
  geom_smooth(method = "lm", se = FALSE) +
  #  geom_point(data=group_sum,aes(x=mean_watertemp,y=mean_CO2,color=Site),size=4,shape=3) +
  scale_y_continuous(transform = "log") 

M_AICc_CH4 <- lm(log(pCO2_ppm) ~
                     #WaterTemp_c_ave3 
                   #waterTemp_c_day
                     Watertemp_c
                     , data =df%>%filter(Site=="Wetland09"))

summary(M_AICc_CH4)
mp <- modelPerformance(M_AICc_CH4)
mp$Performance$MarginalR2

