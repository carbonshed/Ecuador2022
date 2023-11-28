#explore data
#this script is to explore data by ploting 
library(ggplot2)

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-10-28.csv"))
df$X <- NULL
df$Date <- as.Date.character(df$Date,format="%Y-%m-%d")
df$surface_area_ha <- df$surface_area_m2 /10000

#########################
#### Drivers of CO2 and CH4 ####
##########################

##Temperature
ggplot(df ,aes(x=log(Watertemp_c),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df%>%filter(Date<"2022-09-01") ,aes(x=log(waterTemp_c_summer),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df%>%filter(Date>"2022-09-01") ,aes(x=log(waterTemp_c_fall),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df ,aes(x=log(waterTemp_c_yearly),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df ,aes(x=log(Watertemp_c),y=log(CH4_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

###this one is good
ggplot(df ,aes(x=log(waterTemp_c_summer),y=log(CH4_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
####

#airpressure
ggplot(df ,aes(x=log(AirPress_kpa),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

##precipitation
ggplot(df ,aes(x=PrecipAccu_mm_PreviousDay,y=CO2_umol.L)) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df ,aes(x=log(waterTemp_c_summer),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

#volumn
ggplot(df %>%filter(Date<"2022-09-01")  ,aes(x=log(Volumn_m3_summer),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
ggplot(df %>%filter(Date>"2022-09-01")  ,aes(x=log(Volumn_m3_fall),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

#watershed sized
ggplot(df%>%filter(Date<"2022-09-01")  ,aes(x=log(Watershed_m2),y=log(CO2_umol.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df%>%filter(Wetland!="Wetland01"&Wetland!="Wetland02"&Wetland!="Wetland12"&Wetland!="Wetland12")
       #%>%filter(Date>"2022-09-01")
       , aes(x=log(SA_to_Vol_ratio),y=log(CO2_umol.L))) +
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

ggplot(df ,aes(x=log(Watershed_m2),y=log(DOC_mg.L))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

#########################
#### Drivers of flux & K ####
##########################

#build a model for flux


ggplot(df ,aes(x=log(CO2_umol.L),y=log(Flux_umol_m2_s))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 

ggplot(df ,aes(x=log(AirPress_kpa),y=log(Flux_umol_m2_s))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
#temp
#df$AirWaterTemp_dif <- df$AirTemp_c - df$Watertemp_c
#df_melt <- dfs
ggplot(df ,aes(x=log(Watertemp_c),y=log(k_m.d))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
ggplot(df ,aes(x=log(AirTemp_c),y=log(k_m.d))) +
  geom_point(aes(color=Wetland),size=5)+                                     
  theme_bw(base_size = 16) 
ggplot(df ,aes(x=log1p(AirTemp_c-Watertemp_c),y=log1p(k_m.d))) +
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
df_merge$area..ha. <- df_merge$surface_area_ha
df_merge$ch4..umol.L. <- df_merge$CH4_umol.L
df_merge$co2..umol.L. <- df_merge$CO2_umol.L
df_merge$temp..C. <- df_merge$Watertemp_c
df_merge$temp..estimate. <- NA
df_merge$atm..ch4 <- NA
df_merge$atm..co2 <- df_merge$CO2_ppm
df_merge <- df_merge[
  c("Reference","Site.Name","Location","latitude","longitude","Study.Years","area..ha.","ch4..umol.L.","co2..umol.L.","temp..C.","atm..co2","temp..estimate.","atm..ch4"
    )]

df_Holg <- rbind(df_Holg,df_merge)
df_Holg$ch4..umol.L. <- as.numeric(df_Holg$ch4..umol.L.)
df_Holg$co2..umol.L. <- as.numeric(df_Holg$co2..umol.L.)
df_Holg$Area_ha <- as.numeric(df_Holg$Area_ha)

colnames(df_Holg) <- c("Reference","Site_Name","Location","latitude","longitude","Study_Years,","Area_ha","CH4_umol.L","CO2_umol.L",
                       "Teemp_c","temp_est","atm_CO2","atm_CH4")

df_Holg$Reference2<-sub(" or ", " \n ", df_Holg$Reference) 

ggplot(df_Holg) +
  geom_point(aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference2)) 


#log co2 v c h4
ggplot() +
  geom_point(data=df_Holg,aes(x=log(CO2_umol.L),y=log(CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=log(CO2_umol.L),y=log(CH4_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 

p <- ggplot() +
  geom_point(data=df_Holg%>%filter(Reference!="This Study"),
             aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference2),size=5) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=CO2_umol.L,y=CH4_umol.L,shape=Reference2),size=5)+ 
  scale_colour_discrete(name  ="Referance",
                       ) +
  scale_shape_discrete(name  = NULL,
                       labels="This Study") +
   
  theme_bw(base_size = 20) +
  xlab(expression(CO[2] ~'('~mu*'mol' ~ l^-1~')')) +
  ylab(expression(CH[4] ~'('~mu*'mol' ~ l^-1~')')) +
  scale_y_log10() + scale_x_log10() +
  guides(color=guide_legend(ncol =1))

p + annotation_logticks() 

#co2 and ch4 vs surface extent
ggplot() +
  geom_point(data=df_Holg,aes(x=log(Area_ha),y=log(CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=log(Area_ha),y=log(CH4_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 
ggplot() +
  geom_point(data=df_Holg,aes(x=log(Area_ha),y=log(CO2_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=log(Area_ha),y=log(CO2_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 
ggplot() +
  geom_point(data=df_Holg,aes(x=log(Area_ha),y=log(CO2_umol.L/CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=log(Area_ha),y=log(CO2_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 


#co2 and ch4 vs latitude

ggplot() +
  geom_point(data=df_Holg,aes(x=latitude,y=log(CO2_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CO2_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 

ggplot() +
  geom_point(data=df_Holg,aes(x=latitude,y=log(CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CH4_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 

#log c02/ch4 v lat
ggplot() +
  geom_point(data=df_Holg,aes(x=latitude,y=log(CO2_umol.L/CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=latitude,y=log(CO2_umol.L/CH4_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 


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


####keepers

#log co2 v ch4 (holgerston)
ggplot() +
  geom_point(data=df_Holg,aes(x=log(CO2_umol.L),y=log(CH4_umol.L)),color='grey',size=3) +
  geom_point(data=df_Holg%>%filter(Reference=="This Study"),
             aes(x=log(CO2_umol.L),y=log(CH4_umol.L),color=Site_Name),size=3) + 
  theme_bw(base_size = 16) 
