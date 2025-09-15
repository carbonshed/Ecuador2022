#FIGURE 5
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggbreak)
library(cowplot)

modeled_GHG_df <- read.csv(here::here("data/Modeled_flux_df.csv"))
modeled_GHG_df$DateTime <- as.POSIXct(modeled_GHG_df$DateTime,format="%Y-%m-%d %H:%M:%S",tz="UTC")

modeled_GHG_sum_df <- modeled_GHG_df %>%
  dplyr::select(Site,DateTime,F_mol.d_CH4,F_mol.d_CO2)

modeled_GHG_sum_df$Date <- as.Date(modeled_GHG_sum_df$DateTime)
modeled_GHG_sum_df$month_Day <-  format(as.Date(modeled_GHG_sum_df$DateTime,format="%Y-%m-%d"), format = "%m-%d")

modeled_GHG_sum_df_2 <- modeled_GHG_sum_df%>%group_by(Site,Date)%>%
  summarise(
    F_CH4_mol.d_dayave = mean(F_mol.d_CH4,na.rm=TRUE),
    F_CO2_mol.d_dayave = mean(F_mol.d_CO2,na.rm=TRUE))

GHG_df_filter <- modeled_GHG_sum_df_2%>%filter(
  Date >= as.Date("2022-07-12") & Date <= as.Date("2022-08-12")
)

GHG_df_filter_sum <- GHG_df_filter%>%group_by(Site)%>%summarise(
  F_CH4_mol_4week = sum(F_CH4_mol.d_dayave),
  F_CO2_mol_4week = sum(F_CO2_mol.d_dayave)
)

#plot
options(scipen = 10)
p.ch4 <- ggplot(data=GHG_df_filter_sum, aes(x=factor(Site, levels=c('S10', 'S6', 'S11','S12','S5','S4','S7','S8','S3','S2','Wetland','S1')), y=F_CH4_mol_4week*12)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  geom_bar(stat="identity", fill="#feb24c",color="black", position=position_dodge()) +
  theme_bw(base_size = 12) +
  ylab(expression("Total g of"~CH[4]~"as C")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

options(scipen = 10)
p.co2<- ggplot(GHG_df_filter_sum) + aes(x = factor(Site, levels=c('S10', 'S6', 'S11','S12','S5','S4','S7','S8','S3','S2','Wetland','S1')), y = F_CO2_mol_4week*12) +
  geom_bar(stat="identity", fill="#dd1c77",color="black", position=position_dodge())+
  ylab(expression("Total g of"~CO[2]~"as C"~~yr^-1)) +
  xlab("Site \n(ordered by increasing surface area)") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_y_cut(breaks=c(1.5*10^4), which=c(1, 2), scales=c(0.5, 3)) +
  theme_bw(base_size = 12) 

p_full <- ggarrange(p.ch4,p.co2,nrow=2)

save.co2 <- print(p.co2)
save.ch4 <- print(p.ch4)

full <- plot_grid(save.ch4,save.co2,nrow=2,labels=c("(a)","(b)"),rel_heights = c(1,1.5))
