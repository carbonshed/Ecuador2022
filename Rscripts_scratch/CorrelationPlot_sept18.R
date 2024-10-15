#run the combo watershed data script first
#then this
library(here)
library(dplyr)
library(Hmisc)
library(corrplot)

df <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))
df <- df%>%filter(Site!='Wetland12')
  
df$log10CO2<-log10(df$pCO2_ppm)
df$log10ch<-log10(df$pCH4_ppm)
df$log10SA<-log10(df$surface_area_m2)
df$log10WS<-log10(df$WS_area_minus_pond)

#df$log1pprecip<-log1p(df$precip_mm_ave2)
df$satows <- df$surface_area_m2/ (df$WS_area_minus_pond + df$surface_area_m2)

#hist(sites$meanTN)
#sites$log10TN<-log10(sites$meanTN)

names(df)

#sub<-sites[,c(2,3,9,11:12,42, 37,46,43:45, 18)]
sub <- df%>%select(Elevation_m,
                   log10SA,#waterTemp_c_day,waterTemp_c_yearly,
                   SA_to_Vol_ratio,log10WS,percent_DTW,depth_ave_m,
                   #log1pprecip,# precip_mm_ave2,Precip_mm_ave7,PrecipAccuDay_mm,PrecipAccu_mm_PreviousDay,
                  # K600,
                   DOC_mg.L,TDN_mg.L,log10CO2,log10ch)
  

 
names(sub)<-c('elevation', 'surface area', #'mean water temp. (day)', 'mean water temp. (year)', 
              'surface area to volume', 'watershed area', 'DTW %', 'SA to Vol ratio',
              #'precip_mm_ave2','Precip_mm_ave7','PrecipAccuDay_mm','PrecipAccu_mm_PreviousDay',
              #'K600',
              'DOC', 'TDN','pCO2', 'pCH4')

res<-cor(sub, use='complete.obs')
res1<-cor.mtest(sub, conf.level=0.95)
#write.csv(res, 'correlationtable.csv')
#write.csv(res1, 'corsignificance.csv')

pal1<-colorRampPalette(c('grey50', 'white', 'coral2'))

corrplot(res, type = 'lower', order = 'original', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10))

## leave blank on non-significant coefficient
## add significant correlation coefficients
corrplot(res, #p.mat = testRes$p, 
         method = 'circle', type = 'lower', insig='blank', col = COL2('PuOr', 10),
         addCoef.col ='black', number.cex = 0.8, order = 'original', diag=FALSE)

#getwd()
#tiff('correlationplot.tif', width=6, height=6, res=390, units='in')
corrplot(res, type = "lower", order = "original", p.mat = res1$p,
         tl.col = "black", tl.srt = 90, method='color', diag=FALSE,
       #  addCoef.col = "black",
       col=pal1(20),
         sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig',
       pch.cex = 1, pch.col='grey20')
#dev.off()


#now lets run some models
library(lme4)
library(lmerTest)
library(jtools)
library(extraoperators)
library(car)
library(JWileymisc)
library(multilevelTools)
library("Hmisc")
library(GGally)
library(MCMCglmm)
library(effects)
library(MuMIn)
#check vif

#ch4

M1 <- lmer(log(pCH4_ppm) ~ 
             scale(Elevation_m)+ 
             scale(log(surface_area_m2)) +
             scale(waterTemp_c_day) +
             scale(waterTemp_c_yearly) +
             scale(log(SA_to_Vol_ratio)) + 
             #scale(log(WS_area_minus_pond)) +
             #scale(percent_DTW) +
             #scale(precip_mm_ave2) + 
             #scale(depth_ave_m) + 
             (1 |Site), data =df )

vif(M1)

M_AICc <- lmer(log(pCH4_ppm) ~ 
                 scale(Elevation_m)+ 
             #    scale(log(surface_area_m2)) +
                 scale(waterTemp_c_day) +
              #   scale(waterTemp_c_yearly) +
                 scale(log(SA_to_Vol_ratio)) + 
                 (1 |Site), data =df,REML = FALSE)
summary(M_AICc)
modelPerformance(M_AICc)

#co2

M1 <- lmer(log(pCO2_ppm) ~ 
             scale(Elevation_m)+ 
             scale(log(surface_area_m2)) +
             scale(waterTemp_c_day) +
             #scale(waterTemp_c_yearly) +
             #  scale(log(SA_to_Vol_ratio)) + 
             #scale(log(WS_area_minus_pond)) +
             scale(percent_DTW) +
             #scale(precip_mm_ave2) + 
             #scale(depth_ave_m) + 
             (1 |Site), data =df )

vif(M1)


M_AICc <- lmer(log(pCO2_ppm) ~ 
                 scale(Elevation_m)+ 
                 scale(log(surface_area_m2)) +
                 scale(waterTemp_c_day) +
                 scale(percent_DTW) +
                 (1 |Site), data =df,REML = FALSE)
summary(M_AICc)
modelPerformance(M_AICc)

#okay why the heck is this different from the other temp plot?  double check it all!  Oh yeah bc the other is partial pressure i think
