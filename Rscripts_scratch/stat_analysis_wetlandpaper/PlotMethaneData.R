

df1 <- read.csv(here::here("Wetlands/Wetland_df_2023-11-10.csv"))%>%select(Wetland,Date,CH4_umol.L,CH4_sat)
df2 <- read.csv(here::here("Methane/Methane_fall2022.csv"))%>%select(Site,Date_collected,CH4_umol.L,CH4_sat)%>%
  filter(Site!="Gavilan")%>%filter(Site!="Chakanas")%>%rename(Wetland=Site,Date=Date_collected)

df <- rbind(df1,df2)
df$Date <- as.Date(df$Date,format="%m/%d/%y")

df$CH4_umol.L <- as.numeric(df$CH4_umol.L)
df$CH4_sat <- as.numeric(df$CH4_sat)

ggplot(df,aes(x=Wetland,y=log(CH4_umol.L), color=Date)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(df,aes(x=Wetland,y=log(CH4_sat), color=Date)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
