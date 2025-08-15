CH4_summary <- read.csv(here::here("ProcessedData/CH4samples_summary_June24.csv"))
CH4_summary$Date <- as.Date(CH4_summary$Date,format="%Y-%m-%d")

CO2_summary <- read.csv(here::here("ProcessedData/CO2_summary_June24.csv"))
CO2_summary$Date <- as.Date(CO2_summary$Date,format="%Y-%m-%d")

df <- read.csv(here::here("ProcessedData/PondPaper_k600_May29.csv"))%>%dplyr::select(Date,Site,New.Name,DOC_mg.L,TDN_mg.L,percent_DTW,Elevation_m)
#just the days when DOC was sample
df$Date <- as.Date(df$Date)
df1 <- df%>%filter(Date==as.Date("2022-07-27")|Date==as.Date("2022-07-25"))
df2 <- df%>%filter(Date==as.Date("2022-07-22"))%>%filter(Site!="Wetland12")
df3 <- df%>%filter(Date==as.Date("2022-07-19"))%>%filter(Site!="Wetland11")%>%filter(Site!="Wetland05")
df4 <- df%>%filter(Date==as.Date("2022-07-18"))
df_doc <- rbind(df1,df2,df3,df4)

CH4_summary_sum <-CH4_summary %>%
  group_by(New.Name)%>%summarise(
    pCH4_w_uatm = mean(pCH4_w_uatm,na.rm=TRUE))
CO2_summary_sum <-CO2_summary %>%
  group_by(New.Name)%>%summarise(
    pCO2_w_uatm = mean(pCO2_w_uatm,na.rm=TRUE))

df_doc_ch4 <- left_join(df_doc,CH4_summary,by=c("Date","New.Name"))
df_doc_co2 <- left_join(df_doc,CO2_summary,by=c("Date","New.Name"))


df_doc_ch4_2 <- left_join(df_doc,CH4_summary_sum,by=c("New.Name"))
df_doc_co2_2 <- left_join(df_doc,CO2_summary_sum,by=c("New.Name"))

df_doc_ch4_3 <- left_join(df_doc,CH4_summary,by=c("New.Name"))
df_doc_co2_3 <- left_join(df_doc,CO2_summary,by=c("New.Name"))

#model
M1 <- lmer(log(pCO2_w_uatm) ~ 
             log(DOC_mg.L) #+ Elevation_m 
           +1|New.Name
           , 
           data =df_doc_co2_3)
summary(M1)
performance::model_performance(M1)
M2 <- lmer(log(pCO2_w_uatm) ~ 
             log(TDN_mg.L) + 1|New.Name, 
           data =df_doc_co2_3)
summary(M2)

M3 <- lm(log(pCH4_w_uatm) ~ 
           log(DOC_mg.L), 
         data =df_doc_ch4_2)
summary(M3)
