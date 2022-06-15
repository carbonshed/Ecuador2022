<<<<<<< HEAD
#binding lux data

#dissolved oxygen code
#Amy Madrigal 
#6-14-22

library(lubridate) #package for our date parsing
library(dplyr)
library(here)
library(ggplot)
setwd(here::here("DissolvedO2"))
##set folder for site ##

#read in Data 


###plot Dissolved o2 data
DOData_stn1 <- read.csv("DO_01_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                    quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn1 <- DOData_stn1[,c(2,3,4)]
names(DOData_stn1)[names(DOData_stn1)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn1)[names(DOData_stn1)=="DO.conc..mg.L..LGR.S.N..21223042..SEN.S.N..21223042."] <- "DO_mgL"
names(DOData_stn1)[names(DOData_stn1)=="Temp...C..LGR.S.N..21223042..SEN.S.N..21223042."] <- "Temperature_C"
DOData_stn1$DateTime <- as.POSIXct(DOData_stn1$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")


DOData_stn2 <- read.csv("DO_02_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn2 <- DOData_stn2[,c(2,3,4)]
names(DOData_stn2)[names(DOData_stn2)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn2)[names(DOData_stn2)=="DO.conc..mg.L..LGR.S.N..20645540..SEN.S.N..20645540."] <- "DO_mgL"
names(DOData_stn2)[names(DOData_stn2)=="Temp...C..LGR.S.N..20645540..SEN.S.N..20645540."] <- "Temperature_C"
DOData_stn2$DateTime <- as.POSIXct(DOData_stn2$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")



DOData_stn3 <- read.csv("DO_03_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn3 <- DOData_stn3[,c(2,3,4)]
names(DOData_stn3)[names(DOData_stn3)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn3)[names(DOData_stn3)=="DO.conc..mg.L..LGR.S.N..20645538..SEN.S.N..20645538."] <- "DO_mgL"
names(DOData_stn3)[names(DOData_stn3)=="Temp...C..LGR.S.N..20645538..SEN.S.N..20645538."] <- "Temperature_C"
DOData_stn3$DateTime <- as.POSIXct(DOData_stn3$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")


DOData_stn4 <- read.csv("DO_04_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn4 <- DOData_stn4[,c(2,3,4)]
names(DOData_stn4)[names(DOData_stn4)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn4)[names(DOData_stn4)=="DO.conc..mg.L..LGR.S.N..20645539..SEN.S.N..20645539."] <- "DO_mgL"
names(DOData_stn4)[names(DOData_stn4)=="Temp...C..LGR.S.N..20645539..SEN.S.N..20645539."] <- "Temperature_C"
DOData_stn4$DateTime <- as.POSIXct(DOData_stn4$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")






=======
#binding lux data

#dissolved oxygen code
#Amy Madrigal 
#6-14-22

library(lubridate) #package for our date parsing
library(dplyr)
library(here)
library(ggplot)
setwd(here::here("DissolvedO2"))
##set folder for site ##

#read in Data 


###plot Dissolved o2 data
DOData_stn1 <- read.csv("DO_01_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                    quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn1 <- DOData_stn1[,c(2,3,4)]
names(DOData_stn1)[names(DOData_stn1)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn1)[names(DOData_stn1)=="DO.conc..mg.L..LGR.S.N..21223042..SEN.S.N..21223042."] <- "DO_mgL"
names(DOData_stn1)[names(DOData_stn1)=="Temp...C..LGR.S.N..21223042..SEN.S.N..21223042."] <- "Temperature_C"
DOData_stn1$DateTime <- as.POSIXct(DOData_stn1$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")


DOData_stn2 <- read.csv("DO_02_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn2 <- DOData_stn2[,c(2,3,4)]
names(DOData_stn2)[names(DOData_stn2)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn2)[names(DOData_stn2)=="DO.conc..mg.L..LGR.S.N..20645540..SEN.S.N..20645540."] <- "DO_mgL"
names(DOData_stn2)[names(DOData_stn2)=="Temp...C..LGR.S.N..20645540..SEN.S.N..20645540."] <- "Temperature_C"
DOData_stn2$DateTime <- as.POSIXct(DOData_stn2$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")



DOData_stn3 <- read.csv("DO_03_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn3 <- DOData_stn3[,c(2,3,4)]
names(DOData_stn3)[names(DOData_stn3)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn3)[names(DOData_stn3)=="DO.conc..mg.L..LGR.S.N..20645538..SEN.S.N..20645538."] <- "DO_mgL"
names(DOData_stn3)[names(DOData_stn3)=="Temp...C..LGR.S.N..20645538..SEN.S.N..20645538."] <- "Temperature_C"
DOData_stn3$DateTime <- as.POSIXct(DOData_stn3$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")


DOData_stn4 <- read.csv("DO_04_2022-06-02.csv", skip=1, header = TRUE, sep = ",",
                        quote = "\"",dec = ".", fill = TRUE, comment.char = "")
DOData_stn4 <- DOData_stn4[,c(2,3,4)]
names(DOData_stn4)[names(DOData_stn4)=="Date.Time..GMT.05.00"] <- "DateTime"
names(DOData_stn4)[names(DOData_stn4)=="DO.conc..mg.L..LGR.S.N..20645539..SEN.S.N..20645539."] <- "DO_mgL"
names(DOData_stn4)[names(DOData_stn4)=="Temp...C..LGR.S.N..20645539..SEN.S.N..20645539."] <- "Temperature_C"
DOData_stn4$DateTime <- as.POSIXct(DOData_stn4$DateTime, format="%m/%d/%y %I:%M:%S %p", tz="UTC")






>>>>>>> 5e0b2504c6ae069c36af14225523ad4169a895e0
