#let's try 
library(VisCollin)
library(dplyr)
library(tidyr)
library(car)
library(corrplot)

#Download Data and defining df and omitting zeros
df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2024-04-14.csv"))
df$New.Name <- df$Site
df$site.date <- paste(df$Site,df$Date)
#make column with new name to reflect elevation. S stands for "site"
df <- df %>%
  mutate(New.Name = recode(New.Name, 'Wetland01'= 'S1', 'Wetland02' = 'S2','Wetland03' = 'S3',
                           'Wetland04' = 'S4','Wetland05' = 'S8','Wetland06' = 'S5', 
                           'Wetland07' = 'S7','Wetland08' = 'S12','Wetland09' = 'S10',
                           'Wetland10' = 'S11', 'Wetland11' = 'S6','Wetland12' = 'S9',))%>%
  filter(Site!="Wetland12")
df2 <- df%>%select(Site,New.Name,
                   CO2_ppm,CO2_umol.L,CH4_umol.L,CH4_sat,
  Watertemp_c, waterTemp_c_yearly, Elevation_m,# AirTemp_c,
  #BaroTemp_c_day, BaroTemp_c_yearly, waterTemp_c_day,
  SA_to_Vol_ratio, surface_area_m2,#Volumn_m3,
  WS_area_minus_pond, #     Watershed_m2,
  precip_mm_ave2, #percentpond,# percent_DTW, 
  solarrad_Wm2_daymean
)

df2 <- df2%>%select(!CO2_ppm)%>%
  select(!CO2_umol.L)%>%
  select(!CH4_umol.L)%>%
  select(!CH4_sat)


#df2$Vol_log <- log(df2$Volumn_m3)
df2$SAtoVOL_log <- log(df2$SA_to_Vol_ratio)
df2$precip_log <- log1p(df2$precip_mm_ave2)
df2$SA_log <- log(df2$surface_area_m2)
df2$WS_log <- log(df2$WS_area_minus_pond)
#df2$precentPond_log <- log(df2$percentpond)


df3 <- df2 %>%select(!New.Name)%>%#select(!Volumn_m3)
  select(!SA_to_Vol_ratio)%>%
  select(!precip_mm_ave2)%>%select(!surface_area_m2)%>%select(!WS_area_minus_pond)



df3 <- df3[,-1]
#rownames(df3) <- df3[,1]
cor(df3, method = c("pearson", "kendall", "spearman"))
res <- cor(df3)
round(res, 2)
cor(df3, use = "complete.obs")

chart.Correlation(df3, histogram=FALSE, pch=19)


corrplot.mixed(df3, lower = "square", upper = "ellipse", tl.col = "black")

library(Hmisc)
res2 <- rcorr(as.matrix(df3))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2<-rcorr(as.matrix(mtcars[,1:7]))
flattenCorrMatrix(res2$r, res2$P)


install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(df3, histogram=TRUE, pch=19)
