#supplementary figures 12,13,14,15
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)

modeled_GHG_df <- read.csv(here::here("PondPaper_Revised/Modeled_flux_df.csv"))
modeled_GHG_df$DateTime <- as.POSIXct(modeled_GHG_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

####
options(scipen=10000)
p.F.co2 <- ggplot(modeled_GHG_df %>%filter(DateTime > as.POSIXct("2022-07-12 00:00:00",tz='UTC') & DateTime < as.POSIXct("2022-08-12 00:00:00",tz='UTC'))) + 
  geom_point(aes(x=DateTime,y=F_CO2_mol_m2_d*12
  ),size=.1,color="#d95f02") +
  facet_wrap(~factor(Site, levels=c('S1', 'S2', 'S3','S4','S5','S6','S7','S8','S10','S11','S12','Wetland')), scales = "free_y") +
  ylab(Emission~("g"~CO[2]*"-C"~m^-2~d^-1)) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

p.F.ch4  <- ggplot(modeled_GHG_df %>%filter(DateTime > as.POSIXct("2022-07-12 00:00:00",tz='UTC') & DateTime < as.POSIXct("2022-08-12 00:00:00",tz='UTC'))) + 
  geom_point(aes(x=DateTime,y=F_CH4_mol_m2_d*12
  ),size=.1,color="#d95f02") +
  facet_wrap(~factor(Site, levels=c('S1', 'S2', 'S3','S4','S5','S6','S7','S8','S10','S11','S12','Wetland')), scales = "free_y") +
  ylab(Emission~("g"~CH[4]*"-C"~m^-2~d^-1)) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

###### now for total flux

p.F.co2_2 <-ggplot(modeled_GHG_df %>%filter(DateTime > as.POSIXct("2022-07-12 00:00:00",tz='UTC') & DateTime < as.POSIXct("2022-08-12 00:00:00",tz='UTC'))) + 
  geom_point(aes(x=DateTime,y=F_mol.d_CO2*12
  ),size=.1,color="#1b9e77") +
  facet_wrap(~factor(Site, levels=c('S1', 'S2', 'S3','S4','S5','S6','S7','S8','S10','S11','S12','Wetland')), scales = "free_y") +
  ylab("Total emissions"~("g"~CO[2]*"-C"~~d^-1)) +  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

p.F.ch4_2 <- ggplot(modeled_GHG_df %>%filter(DateTime > as.POSIXct("2022-07-12 00:00:00",tz='UTC') & DateTime < as.POSIXct("2022-08-12 00:00:00",tz='UTC'))) + 
  geom_point(aes(x=DateTime,y=F_mol.d_CH4*12
  ),size=.1,color="#1b9e77") +
  facet_wrap(~factor(Site, levels=c('S1', 'S2', 'S3','S4','S5','S6','S7','S8','S10','S11','S12','Wetland')), scales = "free_y") +
  ylab("Total emissions"~("g"~CH[4]*"-C"~d^-1)) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
