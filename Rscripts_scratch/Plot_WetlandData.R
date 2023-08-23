#explore data
#this script is to explore data by ploting 
library(ggplot2)

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-08-01.csv"))
df$X <- NULL
df$Date <- as.Date.character(df$Date,format="%Y-%m-%d")

#########################
#### Drivers of CO2 and CH4 ####
##########################

#look at average temp for each site

#temp
ggplot(df ,aes(x=log(Watertemp_c),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df ,aes(x=log(Watertemp_c),y=log(CH4_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

#water chem
ggplot(df ,aes(x=log(DOC_mg.L),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
ggplot(df ,aes(x=log(TDN_mg.L),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df,aes(x=log(DOC_mg.L),y=log(CH4_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
ggplot(df,aes(x=log(TDN_mg.L),y=log(CH4_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df ,aes(x=DOC_mg.L,y=TDN_mg.L)) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

#########################
#### Drivers of flux & K ####
##########################

ggplot(df ,aes(x=log(CO2_umol.L),y=log(Flux_umol_m2_s))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

#temp
df$AirWaterTemp_dif <- df$AirTemp_c - df$Watertemp_c
df_melt <- dfs
ggplot(df ,aes(x=log(Watertemp_c),y=log(k_m.d))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
ggplot(df ,aes(x=log(AirTemp_c),y=log(k_m.d))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
ggplot(df ,aes(x=log(AirTemp_c-Watertemp_c+1),y=log(k_m.d))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

#####################
##### Holgersten ####
#####################

#read in Holgerston & Raymond data set
df_Holg <- read.csv(here::here("Wetlands/Holgerson&Raymond_data1.csv"))
df_merge <- df
df_merge$Reference <- "This Study"
df_merge$Site.Name <- df_merge$Wetland
df_merge$Location <- "La Virgen"
df_merge$latitude <- -0.324311
df_merge$longitude <- -78.1995
df_merge$Study.Years <- 2022
df_merge$area..ha. <- NA
df_merge$ch4..umol.L. <- df_merge$CH4_umol.L
df_merge$co2..umol.L. <- df_merge$CO2_umol.L
df_merge$temp..C. <- df_merge$Watertemp_c
df_merge$temp..estimate. <- NA
df_merge$atm..ch4 <- NA
df_merge$atm..co2 <- df_merge$CO2_ppm
df_merge <- df_merge%>%select(Reference,Site.Name,Location,latitude,longitude,Study.Years,area..ha.,ch4..umol.L.,co2..umol.L.,temp..C.,atm..co2,temp..estimate.,atm..ch4)

df_Holg <- rbind(df_Holg,df_merge)
df_Holg$ch4..umol.L. <- as.numeric(df_Holg$ch4..umol.L.)
df_Holg$co2..umol.L. <- as.numeric(df_Holg$co2..umol.L.)

colnames(df_Holg) <- c("Reference","Site_Name","Location","latitude","longitude","Study_Years,","Area_ha","CH4_umol.L","CO2_umol.L",
                       "Teemp_c","temp_est","atm_CO2","atm_CH4")

ggplot(df_Holg) +
  geom_point(aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference)) 

#co2 v ch4
ggplot() +
  geom_point(data=df_Holg,aes(x=CO2_umol.L,y=CH4_umol.L),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=CO2_umol.L,y=CH4_umol.L,color=Site_Name),size=3) 
theme_bw()

#log co2 v ch4
ggplot() +
  geom_point(data=df_Holg,aes(x=log(CO2_umol.L),y=log(CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=log(CO2_umol.L),y=log(CH4_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 

ggplot() +
  geom_point(data=df_Holg,aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=CO2_umol.L,y=CH4_umol.L,shape='Site Name'),size=3)
#log co2 v lat
ggplot() +
  geom_point(data=df_Holg ,aes(x=latitude,y=log(CO2_umol.L),color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CO2_umol.L),shape='Site Name'),size=3)
#log CH4 v lat
ggplot() + 
  geom_point(data=df_Holg ,aes(x=latitude,y=log(CH4_umol.L),color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CH4_umol.L),shape='Site Name'),size=3)
#log c02/ch4 v lat
ggplot() + 
  geom_point(data=df_Holg ,aes(x=latitude,y=log(CO2_umol.L/CH4_umol.L),color=Reference),size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CO2_umol.L/CH4_umol.L),shape='Site Name'),size=3)

#################################


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

