#Figs
#2022-11-06
#this script is to make figs to use for AGU poster and wetland paper 
library(stringr)
library(grid)

df <- read.csv(here::here("Wetlands/Wetland_df_MERGE_2023-11-10.csv"))
df$Date <- as.Date(df$Date)

df$name.code <- paste("W",as.numeric(gsub("\\D", "", df$Wetland)))
df$name.code <- str_replace_all(df$name.code, " ", "")
df$name.code <- factor(df$name.code, levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","W11","W12"))

pp1 <- ggplot(data=df,aes(x=CO2_umol.L,y=Flux_umol_m2_s, fill=name.code)) +
  geom_point(shape=21,size=5) +
  scale_y_log10() + scale_x_log10() +
  xlab(expression(CO[2] ~'('~mu*'mol' ~ l^-1~')')) + ylab(expression(CO[2] ~'Flux ('~mu*'mol' ~ m^2~ s^-1~')' )) +
  theme_bw() + theme(legend.title= element_blank()) +
  theme(legend.position = "top")+
  theme(text=element_text(size=20))

pp1


pp2 <- ggplot(data=df,aes(x=Elevation_m,y=CO2_umol.L, fill=name.code)) +
  geom_point(shape=21,size=5) +
  scale_y_log10() + #scale_x_log10() +
  ylab(expression(CO[2] ~'('~mu*'mol' ~ l^-1~')')) + xlab("Elevation (m)") +
  theme_bw() + theme(legend.title= element_blank(),axis.title.x = element_blank()) +
  theme(legend.position = "none")+
  theme(text=element_text(size=20))  

pp2

pp3 <- ggplot(data=df,aes(x=Elevation_m,y=CH4_umol.L, fill=name.code)) +
  geom_point(shape=21,size=5) +
  scale_y_log10() + #scale_x_log10() +
  ylab(expression(CH[4] ~'('~mu*'mol' ~ l^-1~')')) + xlab("Elevation (m)") +
  theme_bw() + theme(legend.title= element_blank()) +
  theme(legend.position = "none")+
  theme(text=element_text(size=20))

leg <- get_legend(pp1)

g <- rbind(ggplotGrob(pp2), ggplotGrob(pp3), size = "last")

#ggarrange(g, leg, widths = c(2,.5))
#ggarrange(pp1, leg, g, ncol=3,widths = c(2,.5,2))
ggarrange(pp1, g, nrow=1, widths = c(2,2)
          )
