library(ggnewscale)

#holgerson and Raymond plot


CO2_summary <- read.csv(here::here("ProcessedData/CO2_summary_June24.csv"))%>%dplyr::select(New.Name,Date,CO2_umol.L,Watertemp_c)
CO2_summary$Date <- as.Date(CO2_summary$Date,format="%Y-%m-%d")

CH4_summary <- read.csv(here::here("ProcessedData/CH4samples_summary_June24.csv"))%>%dplyr::select(New.Name,Date,CH4_umol.L)
CH4_summary$Date <- as.Date(CH4_summary$Date,format="%Y-%m-%d")

predictor_variables <- read.csv(here::here("ProcessedData/predictor_variables_July2.csv"))%>%dplyr::select(New.Name,Date,surface_area_m2)
predictor_variables$Date <- as.Date(predictor_variables$Date,format="%Y-%m-%d")

df_merge <- full_join(CO2_summary,CH4_summary,by=c("New.Name","Date"))
df_merge <- left_join(df_merge,predictor_variables,by=c("New.Name","Date"))


df_merge$Area_ha <-  df_merge$surface_area_m2 / 10000
df_merge$Reference <- "This Study"
df_merge$color_code <- 23
df_merge$Location <- "La Virgen"
df_merge$Ecosystem <- "Peatland"
df_merge$latitude <- -0.324311
df_merge$longitude <- -78.1995
df_merge$Study.Years <- 2022
df_merge <- df_merge%>%dplyr::select(Reference,Ecosystem,Location,latitude,longitude,Study.Years,CH4_umol.L,CO2_umol.L,Area_ha,Watertemp_c)

#read in Holgerston & Raymond data set
df_Holg <- read.csv(here::here("Wetlands/Holgerson&Raymond_data1.csv"))%>%
  rename("CH4_umol.L"="ch4..umol.L.","CO2_umol.L"="co2..umol.L.","Area_ha"="area..ha.","Watertemp_c"="temp..C.")%>%
  dplyr::select(Reference,Ecosystem,Location,latitude,longitude,Study.Years,CH4_umol.L,CO2_umol.L,Area_ha,Watertemp_c)%>%
  mutate(CH4_umol.L = as.numeric(CH4_umol.L),
         CO2_umol.L = as.numeric(CO2_umol.L),
         Watertemp_c = as.numeric(Watertemp_c)
         )
df_Holg <- rbind(df_Holg,df_merge)

#summarize for discussion
df_Holg_test <- df_Holg%>%filter(Area_ha < .5)
df_Holg_test2 <- df_Holg%>%filter(Area_ha < .5)%>%filter(Reference != "This Study")


df_Holg_peat <- df_Holg%>%filter(Reference=="Hamilton et al. (1994)"|Reference=="Kankaala et al. (2013)"|Reference=="Laurion et al. (2010)"|Reference=="Shirokova et al. (2013)"|Reference=="Pelletier et al. (2014)"|Reference=="This Study")

df_Holg_melt <- df_Holg%>%dplyr::select(
  Reference, Area_ha,Watertemp_c,CH4_umol.L,CO2_umol.L
)%>% #pivot_long() to give each observation it's own row in the data.frame
  pivot_longer( cols = CH4_umol.L:CO2_umol.L, names_to = 'gas', values_to = 'mmol.L')%>%
  mutate(Reference=as.factor(Reference))
df_Holg_melt_ThisStudy <- df_Holg_melt%>%filter(Reference=="This Study")

#all the data
blues_color <- c("#00008B","#190D8F","#271994","#322398","#3C2D9C",
                 "#4436A0","#4C3FA4","#5B51AD","#625BB1","#6864B5",
                 "#6F6DB9", "#7576BD", "#7B80C2", "#8189C6", "#92A6D2",
                 "#98B0D6", "#9DBADA", "#A2C4DE", "#A8CEE2", "#ADD8E6"
                 )
red_color <- c("#FF0000", "#FF4029", "#FF4B34", "#FF5E48", "#FF6752",
               "#FF7865", "#FF8F83", "#FF968D", "#FFB9C0", "black")
options(scipen = 999)
p1 <- ggplot() +
  geom_point(data=df_Holg_melt%>%filter(Area_ha>.5)%>%filter(Reference!="This Study"),aes(x=gas,y=mmol.L,color=Reference),size=3,#alpha=.7,
             position = "jitter") +
    scale_color_manual(name= expression(paste("> 5,000"~m^2)),values = blues_color[1:22]) +
  guides(color=guide_legend(ncol=2)) +

  # start a new scale
  new_scale_colour() +
  geom_point(data=df_Holg_melt%>%filter(Area_ha<.5)#%>%filter(Reference!="This Study")
             ,aes(x=gas,y=mmol.L,color=Reference),size=3,#alpha=.7,
             position = "jitter") +
  scale_color_manual(name= expression(paste("< 5,000"~m^2)),values = red_color[1:10]) +
  
#  geom_point(data=df_Holg_melt_ThisStudy,aes(x=gas,y=mmol.L,color="This Study"),color="black",size=3,position = "jitter")+
  scale_y_log10()  +
  theme_bw() +
  theme(legend.text=element_text(size=9),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14)) +
  ylab(expression(paste(" (",mu,"mol ", L^-1,")"))) +
  scale_x_discrete(
    "", labels = c(expression(paste("CH"[4])),
                   expression(paste("CO"[2]))
    )) +
  guides(color=guide_legend(ncol=2))

p1  
ld <- layer_data(last_plot())
(head(ld))

######
#co2 v ch4

blues_color2 <- c("#00008B","#322398","#4436A0","#625BB1",
                 "#6F6DB9", "#7B80C2",  "#92A6D2","#9DBADA",  "#ADD8E6")
red_color2 <- c("#FF0000",  "#FF4B34", "#FF5E48", 
               "#FF7865", "#FF968D", "#FFB9C0", "black")

df_Holg_dropna <- df_Holg%>%drop_na(CO2_umol.L)%>%drop_na(CH4_umol.L)
#log co2 v ch4
p <- ggplot() +
  geom_point(data=df_Holg_dropna%>%filter(Area_ha > .5),
             aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference),alpha=.7,size=3) +
  
  scale_color_manual(name= expression(paste("> 5,000"~m^2)),values = blues_color2[1:9]) +
  guides(color=guide_legend(ncol=2)) +
  
  # start a new scale
  new_scale_colour() +
  geom_point(data=df_Holg_dropna%>%filter(Area_ha<.5)#%>%filter(Reference!="This Study")
             ,aes(x=CO2_umol.L,y=CH4_umol.L,color=Reference),alpha=.7,size=3) +
  scale_color_manual(name= expression(paste("< 5,000"~m^2)),values = red_color2[1:7]) +
  
  xlab(expression(paste("CO"[2] ," (",mu,"mol ", L^-1,")"))) +
  ylab(expression(paste("CH"[4] ," (",mu,"mol ", L^-1,")"))) +
  scale_y_log10() + scale_x_log10() + scale_fill_discrete(name = "Site") +
  theme_bw() +
  theme(legend.text=element_text(size=9),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14)) 


#####

# read in Arsenault-et-al_All
df_Arsenault <- read.csv(here::here("Wetlands/Arsenault_Inter-regional_spatial_data_peatland pools.csv"))%>%rename(Site_Name=Peatland.Pools,latitude=Latitude,longitude=Longitude,Area_m2=Area..m2.,CO2.C_mgL=CO2.C..mgL.1.,CH4_ugL=CH4..μgL.1.)%>%dplyr::select(Site_Name,latitude,longitude,Area_m2,CO2.C_mgL,CH4_ugL)
df_Arsenault$Reference <- "Arsenault et al. 2023"
df_Arsenault$Location <- ifelse(df_Arsenault$latitude < 0, "Chile","Canada")
df_Arsenault$Study.Years <- "2011-2021"
df_Arsenault$CO2_umol.L <- df_Arsenault$CO2.C_mgL / 1000 * 12 * 10^6 
df_Arsenault$CH4_umol.L <- df_Arsenault$CH4_ugL * 16
df_Arsenault$Area_ha <- df_Arsenault$Area_m2 / 10000
df_Arsenault$Watertemp_c <- NA
df_Arsenault$New.Name <- NA
df_Arsenault <- df_Arsenault%>%dplyr::select(Reference,Site_Name,Location,latitude,longitude,Study.Years,CO2_umol.L,CH4_umol.L,Area_ha,Watertemp_c,New.Name)

#df_Holg <- rbind(df_Holg,df_Arsenault)
