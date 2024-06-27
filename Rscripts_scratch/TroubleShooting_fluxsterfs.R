test <- modeled_GHG_df %>%
  filter(Site=="Wetland03")%>%
  select(Site,DateTime,k_ch4_m.sec,Sc_ch4,F_umol.m2.s_AICc_CH4,CO2_umol.L_mean,M_BIC_CO2,M_AICc_CO2,CH4_umol.L_mean,M_BIC_CH4,M_AICc_CH4,waterTemp_c_yearly,Elevation_m,percent_DTW,WaterTemp_c,SA_to_Vol_ratio)
modeled_GHG_df$DateTime <- as.POSIXct(modeled_GHG_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")




ggplot( df_full %>%filter(Site=="Wetland02")) + 
  geom_line(aes(x=DateTime,y=F_umol.m2.s_AICc_CH4),linetype = "solid",color="#1b9e77") 

ggplot( df_full %>%filter(Site=="Wetland11")) + 
  geom_line(aes(x=DateTime,y=k_ch4_m.sec),linetype="solid",color="#d95f02")

ggplot( df_full %>%filter(Site=="Wetland03")) + 
  geom_line(aes(x=DateTime,y=Sc_ch4),color="#1b9e77") 

ggplot( df_full
        %>%filter(Site=="Wetland01")) + 
  geom_line(aes(x=DateTime,y=WaterTemp_c),color="#7570b3")
ggplot( df_full
        %>%filter(Site=="Wetland03")) + 
  geom_line(aes(x=DateTime,y=WaterTemp_c),color="#7570b3")
ggplot( df_full
        %>%filter(Site=="Wetland02")) + 
  geom_line(aes(x=DateTime,y=WaterTemp_c),color="#7570b3")
ggplot( df_full
        %>%filter(Site=="Wetland04")) + 
  geom_line(aes(x=DateTime,y=WaterTemp_c),color="#7570b3")
ggplot( df_full
        %>%filter(Site=="Wetland07")) + 
  geom_line(aes(x=DateTime,y=WaterTemp_c),color="#7570b3")


####
ggplot( df_full
        %>%filter(Site=="Wetland03")%>%filter(WaterTemp_c<15)
        ) + 
  geom_line(aes(x=DateTime,y=F_umol.m2.s_AICc_CH4),color="#7570b3")

library(plotly)
fig <- plot_ly(data = df_full%>%filter(Site=="Wetland03"),
               x = ~WaterTemp_c, y = ~k_ch4_m.sec)

fig <- plot_ly(data = WL_df%>%filter(Site=="Wetland07"),
               x = ~DateTime, y = ~WaterTemp_c)

fig
###
ggplot( df_full %>%filter(Site=="Wetland01")) + 
  geom_line(aes(x=WaterTemp_c,y=k_ch4_m.sec),color="#d95f02")
ggplot( df_full %>%filter(Site=="Wetland02")) + 
  geom_line(aes(x=WaterTemp_c,y=k_ch4_m.sec),color="#d95f02")
ggplot( df_full %>%filter(Site=="Wetland03")) + 
  geom_line(aes(x=WaterTemp_c,y=k_ch4_m.sec),linetype="solid",color="#d95f02")
ggplot( df_full %>%filter(Site=="Wetland04")) + 
  geom_line(aes(x=WaterTemp_c,y=k_ch4_m.sec),linetype="solid",color="#d95f02")
ggplot( df_full %>%filter(Site=="Wetland05")) + 
  geom_line(aes(x=WaterTemp_c,y=k_ch4_m.sec),linetype="solid",color="#d95f02")

test <- df_full %>%filter(Site=="Wetland02")
df$Sc_ch4
ggplot( df_full %>%filter(Site=="Wetland03")) + 
  geom_line(aes(x=WaterTemp_c,y=k_ch4_m.sec),linetype="solid",color="#d95f02")

ggplot( df_full %>%filter(Site=="Wetland03")%>%filter(WaterTemp_c>15.2)
        ) + 
  geom_line(aes(x=WaterTemp_c,y=k_ch4_m.sec),linetype="solid",color="#d95f02")

ggplot( df_full %>%filter(Site=="Wetland03")#%>%filter(WaterTemp_c<15)
        ) + 
  geom_line(aes(x=DateTime,y=k_ch4_m.sec),linetype="solid",color="#d95f02")

#water temp all sires
ggplot(df_full%>%filter(Site=="Wetland03")#|
              #            Site=="Wetland02"|
              #            Site=="Wetland03"|
              #       Site=="Wetland04"|
              #         Site=="Wetland05"|
              #         Site=="Wetland06")
       ) + 
  geom_line(aes(x=DateTime,y=WaterTemp_c,color=Site),linetype = "solid") 

####
ggplot( modeled_GHG_df %>%filter(Site=="Wetland03")) + 
  geom_line(aes(x=DateTime,y=KH_mol.L.atm_CH4),linetype="dashed",color="#7570b3")

ggplot( modeled_GHG_df %>%filter(Site=="Wetland03")) +  
 geom_line(aes(x=DateTime,y=M_AICc_CH4),linetype="dashed",color="#7570b3")




