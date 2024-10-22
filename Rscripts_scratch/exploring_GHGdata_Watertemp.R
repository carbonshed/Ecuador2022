#question to ask
  #how do I talk about my results regarding lm vs lmer being significant?
  #how do I talk
  #how do i tlak about elevation being so significant and water tmeperature too?

library(olsrr)
library(MASS)

df <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))
df$New.Name <- df$Site
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")
df$Time <- format(as.POSIXct(df$DateTime), format = "%H:%M:%S")
df$Time <- hms(df$Time)        # format to 'hours:minutes:seconds'
df$Time <- (hour(df$Time)*60 + minute(df$Time))
df$dtw_binary <- NA 

df$dtw_binary <- ifelse(df$percent_DTW < mean(df$percent_DTW),"low","high")


df_1 <- df%>%filter(Site!="Wetland12")

df_high <- df%>%filter(Site=="Wetland01"| Site=="Wetland02"| Site=="Wetland03")
df_mid <- df%>%filter(Site!="Wetland01")%>%filter(Site!="Wetland02")%>%filter(Site!="Wetland03")%>%filter(Site!="Wetland08")%>%filter(Site!="Wetland09")%>%filter(Site!="Wetland10")
df_low <- df%>%filter(Site=="Wetland08"| Site=="Wetland09"| Site=="Wetland10")


GHGsummary_df<-df %>%group_by(New.Name,Site,Elevation_m,percent_DTW,dtw_binary,WS_area_minus_pond,waterTemp_c_yearly)%>%
  summarise(mean_ch4 = mean(pCH4_ppm),
            mean_co2 = mean(pCO2_ppm),
            mean_watertemp = mean(Watertemp_c),
            mean_airtemp = mean(AirTemp_c),
            mean_SA = mean(surface_area_m2),
            mean_vol = mean(Volumn_m3),
            mean_vol_to_SA = mean(SA_to_Vol_ratio),
            mean_depth = mean(depth_ave_m))
GHGsummary_high <- GHGsummary_df%>%filter(Site=="Wetland01"| Site=="Wetland02"| Site=="Wetland03")
GHGsummary_mid <- GHGsummary_df%>%filter(Site!="Wetland01")%>%filter(Site!="Wetland02")%>%filter(Site!="Wetland03")%>%filter(Site!="Wetland08")%>%filter(Site!="Wetland09")%>%filter(Site!="Wetland10")
GHGsummary_low <- GHGsummary_df%>%filter(Site=="Wetland08"| Site=="Wetland09"| Site=="Wetland10")


###

m1 <- lmer(log(pCO2_ppm) ~ 
             scale(Watertemp_c) +
           #  scale(AirTemp_c) +
           #  scale(log(surface_area_m2)) +
          #   scale(log(Volumn_m3)) +
          #   scale(SA_to_Vol_ratio) +
          #   scale(depth_ave_m) +
             scale(Elevation_m)+
             scale(percent_DTW) +
          #   scale(WS_area_minus_pond)+
             (1 |Site), data =df)

summary(m1)
modelPerformance(m1)

m2 <- lm(log(mean_co2) ~ 
          scale(mean_watertemp) 
         +
         #   scale(mean_airtemp) +
          #      scale(log(mean_SA))+
      #         scale(log(mean_vol)) +
      #         scale(mean_vol_to_SA) + 
              # scale(mean_depth)+
    #  +
           scale(percent_DTW) +
      #      scale(WS_area_minus_pond) +
    scale(Elevation_m)
         ,
         data =GHGsummary_df)

summary(m2)



###

m3 <- lmer(log(pCH4_ppm) ~ 
             scale(Watertemp_c) +
             #   scale(AirTemp_c) +
             #   scale(log(surface_area_m2)) +
             #   scale(log(Volumn_m3)) +
             #   scale(SA_to_Vol_ratio) +
             #   scale(depth_ave_m) +
             scale(Elevation_m)+
             #  scale(percent_DTW) +
             #   scale(WS_area_minus_pond)+
             (1 |Site), data =df)

summary(m3)
modelPerformance(m3)

m4 <- lm(log(mean_ch4) ~ 
           #      scale(mean_airtemp) +
           #          scale(log(mean_SA)) +
           #         scale(log(mean_vol)) +
           #         scale(mean_vol_to_SA) + 
           #scale(mean_depth) +
          #scale(percent_DTW)+
          #scale(WS_area_minus_pond)+
         scale(Elevation_m) +
         scale(mean_watertemp) 
         ,
         data =GHGsummary_df)

summary(m4)

##
m4 <- lm(mean_watertemp ~ 
              #   scale(mean_airtemp) +
              #       scale(log(mean_SA)) +
           #         scale(log(mean_vol)) 
         #+
              #      scale(mean_vol_to_SA) + 
         #  scale(mean_depth)
          #  +
             scale(Elevation_m)
            +
           scale(percent_DTW)
          #+
          #       scale(WS_area_minus_pond)
         ,
         data =GHGsummary_df)

summary(m4)


##
#plot
ggplot(GHGsummary_df) +
  geom_point(aes(x=Elevation_m,y=log(mean_co2),color=percent_DTW))

ggplot(GHGsummary_df) +
  geom_point(aes(x=Elevation_m,y=log(mean_ch4),color=dtw_binary))

# Stepwise regression model
step.model <- stepAIC(m2, direction = "both", 
                      trace = FALSE)

summary(step.model)



#What is the relationship between water temp and pco2?
M_CO2_temp <- lmer(log(pCO2_ppm) ~ 
                    Watertemp_c+ 
                     (1 |Site), data =df,REML = FALSE)
summary(M_CO2_temp)
modelPerformance(M_CO2_temp)

M_CO2_temp <- lm(log(pCO2_ppm) ~ 
                     Watertemp_c, data =df%>%filter(Site=="Wetland12"))
summary(M_CO2_temp)


library(MASS)



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


