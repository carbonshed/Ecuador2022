df <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
df$Time <- format(as.POSIXct(df$DateTime), format = "%H:%M:%S")
df$Time <- hms(df$Time)        # format to 'hours:minutes:seconds'
df$Time <- (hour(df$Time)*60 + minute(df$Time))
 

#without wetland 12 
df_1 <- df%>%filter(Site!="Wetland12")

df_high <- df%>%filter(Site=="Wetland01"| Site=="Wetland02"| Site=="Wetland03")
df_mid <- df%>%filter(Site!="Wetland01")%>%
  filter(Site!="Wetland02")%>%
  filter(Site!="Wetland03")%>%
  filter(Site!="Wetland08")%>%
  filter(Site!="Wetland09")%>%
  filter(Site!="Wetland10")
df_low <- df%>%filter(Site=="Wetland08"| Site=="Wetland09"| Site=="Wetland10")


#What is the relationship between water temp and pco2?
M_CO2_temp <- lmer(log(pCO2_ppm) ~ 
                    Watertemp_c+ 
                     (1 |Site), data =df,REML = FALSE)
summary(M_CO2_temp)
modelPerformance(M_CO2_temp)

M_CO2_temp <- lm(log(pCO2_ppm) ~ 
                     Watertemp_c, data =df%>%filter(Site=="Wetland12"))
summary(M_CO2_temp)

#Site.    | Wetland01 | Wetland02 | Wetland03 | Wetland04 | Wetland05 | Wetland06 | Wetland07 | W08 | W09 | W10 | W11 | W12
#p-value  | 0.02656   | 0.04243   | 0.1127    | 0.1137    | 0.588     |
#r2.      |0.9965.    | 0.9911    | 0.969     | 0.6783    | -0.2727   | 0.5982
#now plot temp and co2

ggplot(df,aes(x=Watertemp_c,y=pCO2_ppm,color=Site) ) + 
  geom_point()+
  geom_smooth(method='lm',formula = "y ~ x",se=FALSE) +
  scale_y_continuous(transform="log") +scale_x_continuous(transform="log")


M_CO2_ele <- lmer(log(pCO2_ppm) ~ 
                    Elevation_m+ Watertemp_c+percent_DTW +
                     (1 |Site), data =df,REML = FALSE)
summary(M_CO2_ele)
modelPerformance(M_CO2_ele)

M_CO2_ele <- lmer(log(pCO2_ppm) ~ 
                    Elevation_m+ 
                    Watertemp_c+
                    percent_DTW +
                    (1 |Site), data =df_mid,REML = FALSE)
summary(M_CO2_ele)
modelPerformance(M_CO2_ele)

M_CO2_ele <- lmer(percent_DTW ~ 
                    scale(Elevation_m)+ 
                   # scale(Watertemp_c)+
                    (1 |Site), data =df_mid,REML = FALSE)
summary(M_CO2_ele)
modelPerformance(M_CO2_ele)









M_CO2_temp <- lm(log(pCO2_ppm) ~ 
                   Watertemp_c, data =df)
summary(M_CO2_temp)



M_temp_c <- lm(Watertemp_c ~ 
                  log(surface_area_m2) +
                   Elevation_m 
                     , data =df)
summary(M_temp_c)

M_temp_year <- lm(waterTemp_c_yearly ~ 
               #log(surface_area_m2) # + 
                 Volumn_m3
             #   Elevation_m 
            , data =df_mid)
summary(M_temp_year)


ggplot(df_mid#%>%filter(Site=="Wetland11")
       ) + 
  geom_point(aes(x=percent_DTW,y=Elevation_m,color=Site) )


ggplot(df#%>%filter(Site!="Wetland04")
) + 
  geom_point(aes(x=Watertemp_c,y=waterTemp_c_yearly,color=Site)) 








mem_rt_2 <- lmer(log(pCO2_ppm) ~ Watertemp_c + (1+Watertemp_c|Site) + (1|Site), data = df)
sum_mem_rt_2 <- summary(mem_rt_2)




ggplot(df) + 
  geom_point(aes(x=Time,y=pCO2_ppm,color=Site)) + 
  scale_y_continuous(transform = "log")



M_AICc_CO2 <- lmer(log(pCH4_ppm) ~ 
                     scale(Watertemp_c)+ 
                    #   scale(waterTemp_c_yearly) +
                    #     scale(log(surface_area_m2)) +
                    #   scale(log(WS_area_minus_pond)) +
                     #     scale(percent_DTW) +
                    #   scale(Elevation_m) +
                    #    scale(depth_ave_m) +
                     (1 |Site), data =df,REML = FALSE)
sum_AICc_CO2 <- summary(M_AICc_CO2)
sum_AICc_CO2
sum_AICc_CO2_coefficients <- sum_AICc_CO2$coefficients
modelPerformance(M_AICc_CO2)
vif(M_AICc_CO2)

forward_model <- step(M_AICc_CO2, direction = "forward", scope = formula(~ .))
backward_model <- step(M_AICc_CO2, direction = "backward")
both_model <- step(M_AICc_CO2, direction = "both")

ggplot(df%>%filter(Watertemp_c<8)) + 
  geom_point(aes(x=Elevation_m,y=pCO2_ppm,color=Watertemp_c)) + 
 # geom_line(aes(x=Elevation_m,y=pCO2_ppm,color=Site)) +
  scale_y_continuous(transform = "log")

ggplot(df#%>%filter(Watertemp_c<8)
       ) + 
  geom_point(aes(x=Watertemp_c,y=pCO2_ppm,color=Site))# + 
  # geom_line(aes(x=Elevation_m,y=pCO2_ppm,color=Site)) +
  scale_y_continuous(transform = "log") #+
  scale_color_gradient(low = "black",
                       high = "red")
  
  
  ggplot(df%>%filter(Site=="Wetland09")
  ) + 
    geom_point(aes(x=Watertemp_c,y=pCO2_ppm,color=as.factor(DateTime))) + 
    # geom_line(aes(x=Elevation_m,y=pCO2_ppm,color=Site)) +
    scale_y_continuous(transform = "log") #+
  scale_color_gradient(low = "black",
                       high = "red")


df$pCO2_ppm

M_AICc_CH4 <- lmer(log(pCO2_ppm) ~ 
                     scale(Watertemp_c)+ 
                     #  scale(waterTemp_c_yearly) +
                     #   scale(log(surface_area_m2)) +
                     # scale(log(WS_area_minus_pond)) +
                     #scale(percent_DTW) +
                       scale(Elevation_m) +
                     #    scale(depth_ave_m) +
                     (1 |Site), data =df%>%filter(Watertemp_c<8),REML = FALSE)
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

ggplot(df,aes(x=Watertemp_c,y=pCO2_ppm,color=Time,group=Site)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
#  geom_point(data=group_sum,aes(x=mean_watertemp,y=mean_CO2,color=Site),size=4,shape=3) +
  scale_y_continuous(transform = "log") 



#water temp

ggplot(df,aes(x=Watertemp_c,y=pCO2_ppm,color=Date,group=Site)) + 
  geom_point() + facet_wrap(~Site,scales="free_y") + 
  geom_smooth(method = "lm", se = FALSE) +
  #  geom_point(data=group_sum,aes(x=mean_watertemp,y=mean_CO2,color=Site),size=4,shape=3) +
  scale_y_continuous(transform = "log") 

M_AICc_CH4 <- lm(log(pCO2_ppm) ~
                     #WaterTemp_c_ave3 
                   #waterTemp_c_day
                     Watertemp_c
                     , data =df%>%filter(Site=="Wetland10"))

summary(M_AICc_CH4)
mp <- modelPerformance(M_AICc_CH4)
mp$Performance$MarginalR2

