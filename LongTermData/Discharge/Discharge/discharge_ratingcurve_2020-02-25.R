
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

#rating curves with KW 


rating_curve <- read.csv("C:/Users/kriddie/OneDrive - University of North Carolina at Chapel Hill/Ecuador/Discharge/Discharge_KWCalc.csv", 
                         header=T, na.strings=c("","NA"))
rating_curve$Level_m_compensated <-  as.numeric( as.character (rating_curve$Level_m_compensated))
rating_curve$Date <- as.POSIXct(rating_curve$Date, format='%m/%d/%Y')
rating_curve$Station <- as.factor(as.numeric(as.factor(rating_curve$Station)))


#station 1
stn1 <- rating_curve %>%
  filter(Station == "1") #%>%
#  filter(Date != "2019-07-24") %>%
#  filter(Date != "2019-08-13")
mdl1_1 <- lm(stn1$Q_m2.s_Trapazoid ~ stn1$Level_m_compensated)
mdl2_1 <- lm(stn1$Q_m2.s_Trapazoid ~ stn1$Level_m_compensated + I(stn1$Level_m_compensated^2))
mdl3_1 <- lm(stn1$Q_m2.s_Trapazoid ~ stn1$Level_m_compensated + I(stn1$Level_m_compensated^2) + I(stn1$Level_m_compensated^3))
mdl4_1 <- lm(stn1$Q_m2.s_Trapazoid ~ I(stn1$Level_m_compensated^2))

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_1$coefficients[1] + mdl1_1$coefficients[2]*q
y_mdl2 <- mdl2_1$coefficients[1] + mdl2_1$coefficients[2]*q + mdl2_1$coefficients[3]*q^2
y_mdl3 <- mdl3_1$coefficients[1] + mdl3_1$coefficients[2]*q + mdl3_1$coefficients[3]*q^2 + mdl3_1$coefficients[4]*q^3
y_mdl4 <- mdl4_1$coefficients[1] + mdl4_1$coefficients[2]*q^2


plot(stn1$Q_m2.s_Trapazoid ~ stn1$Level_m_compensated)
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.8823  - 10.3227*x + 32.2560*x^2 ; adjR-squared .9898", side=3)

#station 2
stn2 <- rating_curve %>%
  filter(Station == "2") #%>%
#  filter(Date != "2019-07-24") %>%
#  filter(Date != "2019-08-13")
mdl1_2 <- lm(stn2$Q_m2.s_Trapazoid ~ stn2$Level_m_compensated)
mdl2_2 <- lm(stn2$Q_m2.s_Trapazoid ~ stn2$Level_m_compensated + I(stn2$Level_m_compensated^2))
mdl3_2 <- lm(stn2$Q_m2.s_Trapazoid ~ stn2$Level_m_compensated + I(stn2$Level_m_compensated^2) + I(stn2$Level_m_compensated^3))
mdl4_2 <- lm(stn2$Q_m2.s_Trapazoid ~ I(stn2$Level_m_compensated^2))

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_2$coefficients[1] + mdl1_2$coefficients[2]*q
y_mdl2 <- mdl2_2$coefficients[1] + mdl2_2$coefficients[2]*q + mdl2_2$coefficients[3]*q^2
y_mdl3 <- mdl3_2$coefficients[1] + mdl3_2$coefficients[2]*q + mdl3_2$coefficients[3]*q^2 + mdl3_2$coefficients[4]*q^3
y_mdl4 <- mdl4_2$coefficients[1] + mdl4_2$coefficients[2]*q^2


plot(stn2$Q_m2.s_Trapazoid ~ stn2$Level_m_compensated)
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.8823  - 10.3227*x + 32.2560*x^2 ; adjR-squared .9898", side=3)



#station 3
stn3 <- rating_curve %>%
  filter(Station == "3")

mdl1_3 <- lm(stn3$Q_m2.s_Trapazoid ~ stn3$Level_m_compensated)
mdl2_3 <- lm(stn3$Q_m2.s_Trapazoid ~ stn3$Level_m_compensated + I(stn3$Level_m_compensated^2))
mdl3_3 <- lm(stn3$Q_m2.s_Trapazoid ~ stn3$Level_m_compensated + I(stn3$Level_m_compensated^2) + I(stn3$Level_m_compensated^3))
mdl4_3 <- lm(stn3$Q_m2.s_Trapazoid ~ I(stn3$Level_m_compensated^2))

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_3$coefficients[1] + mdl1_3$coefficients[2]*q
y_mdl2 <- mdl2_3$coefficients[1] + mdl2_3$coefficients[2]*q + mdl2_3$coefficients[3]*q^2
y_mdl3 <- mdl3_3$coefficients[1] + mdl3_3$coefficients[2]*q + mdl3_3$coefficients[3]*q^2 + mdl3_3$coefficients[4]*q^3
y_mdl4 <- mdl4_3$coefficients[1] + mdl4_3$coefficients[2]*q^2

plot(stn3$Q_m2.s_Trapazoid ~ stn3$Level_m_compensated)
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  0.3427  - 4.52*x + 17.02*x^2 ; adjR-squared 0.8756", side=3)

#station 4
stn4 <- rating_curve %>%
  filter(Station == "4")

mdl1_4 <- lm(stn4$Q_m2.s_Trapazoid ~ stn4$Level_m_compensated)
mdl2_4 <- lm(stn4$Q_m2.s_Trapazoid ~ stn4$Level_m_compensated + I(stn4$Level_m_compensated^2))
mdl3_4 <- lm(stn4$Q_m2.s_Trapazoid ~ stn4$Level_m_compensated + I(stn4$Level_m_compensated^2) + I(stn4$Level_m_compensated^3))
mdl4_4 <- lm(stn4$Q_m2.s_Trapazoid ~ I(stn4$Level_m_compensated^2))

set.seed(20)
q <- seq(from=0, to=.5, by=0.005)
y_mdl1 <- mdl1_4$coefficients[1] + mdl1_4$coefficients[2]*q
y_mdl2 <- mdl2_4$coefficients[1] + mdl2_4$coefficients[2]*q + mdl2_4$coefficients[3]*q^2
y_mdl3 <- mdl3_4$coefficients[1] + mdl3_4$coefficients[2]*q + mdl3_4$coefficients[3]*q^2 + mdl3_4$coefficients[4]*q^3
y_mdl4 <- mdl4_4$coefficients[1] + mdl4_4$coefficients[2]*q^2



plot(stn4$Q_m2.s_Trapazoid ~ stn4$Level_m_compensated)
#lines(q,y_mdl1,col='firebrick1',lwd=1)
lines(q,y_mdl2,col='blue',lwd=1)
#lines(q,y_mdl3,col='yellow',lwd=1)
#lines(q,y_mdl4,col='green',lwd=1)
mtext("y=  .0276  - 1.43*x + 9.85*x^2 ; adjR-squared 0.9358", side=3)

