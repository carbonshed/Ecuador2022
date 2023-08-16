#explore data
#this script is to explore data by ploting 
library(ggplot2)

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-08-01.csv"))
df$X <- NULL
df$Date <- as.Date.character(df$Date,format="%Y-%m-%d")

# try to find hydrology thing

ggplot(df ,aes(x=PrecipAccuDay_mm,y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth",
              se=F, aes(color=Wetland))
df$Flux_umol_m2_s

ggplot(df ,aes(x=PrecipAccuDay_mm,y=log(CH4_sat))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth",
              se=F, aes(color=Wetland))

ggplot(df ,aes(x=PrecipAccuDay_mm,y=k_m.d)) +
  geom_point(aes(color=Wetland),size=5)+                                     
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth",
              se=F, aes(color=Wetland))


ggplot(df ,aes(x=Waterlevel_m,y=PrecipAccuDay_mm)) +
  geom_point(aes(color=Wetland),size=5)+                                     
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth",
              se=F, aes(color=Wetland))


ggplot(df ,aes(x=Waterlevel_m,y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se=F,
              aes(color=Wetland))

ggplot(df ,aes(x=Date,y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se=F,
              aes(color=Wetland))

ggplot(df,aes(x=log(CH4_umol.L),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5) +
  theme(text=element_text(size=20))

ggplot(df ,aes(x=CO2_umol.L,y=DOC_mg.L)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=DOC_mg.L,y=TDN_mg.L)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=Watertemp_c,y=k_m.d)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=AirTemp_c,y=k_m.d)) +
  geom_point(aes(color=Wetland),size=5)

ggplot(df ,aes(x=(Watertemp_c-AirTemp_c),y=k_m.d)) +
  geom_point(aes(color=Wetland),size=5)+                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se=F,
              aes(color=Wetland))

ggplot(df ,aes(x=AirPress_kpa,y=K600)) +
  geom_point(aes(color=Wetland),size=5)


#plot data real quick

ggplot(CH4_df, aes(x=Site, y=CH4_umol.L)) + 
  geom_point(#fill="red",
    aes(fill=Date_collected),
    shape=21, size = 3)

ggplot(CH4_df, aes(x=Date_collected, y=CH4_umol.L)) + 
  geom_point(#fill="red",
    aes(fill=Date_collected),
    shape=21, size = 3)+
  facet_wrap(~Site)



