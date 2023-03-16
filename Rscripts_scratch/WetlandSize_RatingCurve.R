###rgression lines for wetland size

library(here)
library(ggplot2)
library(dplyr)

df <- read.csv(here::here("Wetlands/DroneFlights_2023-03-16.csv"))
df$WL <- as.numeric(df$WL)
df$Wetland <- as.factor(df$Wetland)
df <- df%>%drop_na(Area)

#wetland 02
Wetland02 <- df%>%filter(Wetland=="wetland_2")
Wetland04 <- df%>%filter(Wetland=="wetland_4")
ggplot(df%>%filter(Wetland=="wetland_1"),aes(x=WL,y=Area)) + geom_point()

x <- Wetland02$WL 
y <- Wetland02$Area

d <- data.frame(x,y)  ## need to use data in a data.frame for predict()
logEstimate <- lm(y~log(x),data=d)

plot(x,y)
xvec <- seq(-.1,.4,length=101)
logpred <- predict(logEstimate,newdata=data.frame(x=xvec))
lines(xvec,logpred)

####

x <- Wetland02$WL  +1
y <- Wetland02$Area 

Estimate = lm(y ~ x)
logEstimate = lm(y ~ log(x))

plot(x,predict(Estimate),type='l',col='blue')
lines(x,predict(logEstimate),col='red')
points(x,y)

###squre root data
#create limited data
x <- Wetland02$WL
y <- Wetland02$Area

plot(y~x)
model<-lm(y~x)
#plot a linear fit
abline(model, col="blue")
print(summary(model))

#model with the square of x
#  I() inhibit interpretation see help(I) for more information
modelsr<-lm(y~I(sqrt(x)))
print(summary(modelsr))

#generate the data to the model
xbar<-seq(-.1, .4, .01)
ybar<-modelsr$coefficients[1]+sqrt(xbar)*modelsr$coefficients[2]
#plot model
lines(y=ybar, x=xbar, pch=19, col="green") 
