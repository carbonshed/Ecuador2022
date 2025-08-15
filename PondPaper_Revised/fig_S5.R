#supplementary figure 5
library(here)
library(dplyr)
library(ggplot2)

df <- read.csv(here::here("PondPaper_Revised/predictor_variables_df.csv"))%>%dplyr::select(Site,Elevation_m,percent_DTW,DOC_mg.L,TDN_mg.L)
df <- unique(df)

p3 <- ggplot(data=df) +
  geom_point(aes(x=DOC_mg.L,y=TDN_mg.L,color=reorder(Site,Elevation_m,median,decreasing = TRUE))) +
  geom_smooth(aes(x=DOC_mg.L,y=TDN_mg.L),method='lm',formula = "y ~ x") +
  xlab(expression(paste("DOC (mg"~L^-1*")"))) +
  ylab(expression(paste("TDN (mg"~L^-1*")")))  +
  scale_color_viridis(name="Site",discrete = TRUE) +
  theme_bw(base_size = 14) +
  guides(color = guide_legend(ncol = 2)) +
  annotate("text", label = paste0("atop(R^2==0.60,p-value==0.002)"), x = 4, y = .5, size = 3, parse = TRUE)

