#run the combo watershed data script first
#then this
df <- read.csv(here::here("ProcessedData/PondPaper_k600.csv"))

df$log10CO2<-log10(df$pCO2_ppm)
df$log10ch<-log10(df$pCH4_ppm)

#hist(sites$meanTN)
#sites$log10TN<-log10(sites$meanTN)

names(df)

#sub<-sites[,c(2,3,9,11:12,42, 37,46,43:45, 18)]
sub <- df%>%select(DOC_mg.L,TDN_mg.L,waterTemp_c_day,waterTemp_c_yearly,Elevation_m,
                   surface_area_m2,SA_to_Vol_ratio,WS_area_minus_pond,percent_DTW,
                   precip_mm_ave2,solarrad_Wm2_daymean,log10CO2,log10ch)
library(Hmisc)
library(corrplot)


#names(sub)<-c('Temp.', 'Precip.', 'WS Area', 'Catch. slope', 'Str. slope', 'Str. Order',
#              'DOC', 'TN', 'DO',
#              'CO2', 'CH4', 'N2O')

res<-cor(sub, use='complete.obs')
res1<-cor.mtest(sub, conf.level=0.95)
#write.csv(res, 'correlationtable.csv')
#write.csv(res1, 'corsignificance.csv')

pal1<-colorRampPalette(c('grey50', 'white', 'coral2'))

getwd()
tiff('correlationplot.tif', width=6, height=6, res=390, units='in')
corrplot(res, type = "lower", order = "original", p.mat = res1$p,
         tl.col = "black", tl.srt = 90, method='color', diag=FALSE,
       #  addCoef.col = "black",
       col=pal1(20),
         sig.level = c(0.01, 0.05, 0.1), insig = 'label_sig',
       pch.cex = 1, pch.col='grey20')
dev.off()

plot(N2O~Temp., data=sub)
modtest<-lm(N2O~Temp., data=sub)
abline(modtest)
summary(modtest)

#okay why the heck is this different from the other temp plot?  double check it all!  Oh yeah bc the other is partial pressure i think
