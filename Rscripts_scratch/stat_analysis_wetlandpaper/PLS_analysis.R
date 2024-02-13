#Partial Least Squares  
#author: Kriddie
#date start: jan 18th 2024

#about: this script is to explore partial least squares as a way to analyse my data for the wetland paper

library(pls) #link to vinnete: https://cran.r-project.org/web/packages/pls/vignettes/pls-manual.pdf
data(yarn)
data(oliveoil)
data(gasoline)
options(digits = 4)

gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]

gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")
 
summary(gas1)
plot(RMSEP(gas1), legendpos = "topright")        
plot(gas1, ncomp = 2, asp = 1, line = TRUE)
plot(gas1, plottype = "scores", comps = 1:3) 
#The numbers in parentheses after the component labels are the relative amount of X variance explained by each component. 

explvar(gas1)#The explained variances can be extracted explicitly with

#The loading plot (Figure 5) is much used for interpretation purposes, for instance to look for known spectral peaks or profiles:
#The labels = "numbers" argument makes the plot function try to interpret the variable names as numbers, and use them as x axis labels
plot(gas1, "loadings", comps = 1:2, legendpos = "topleft",
     labels = "numbers", xlab = "nm")
abline(h = 0)

#A fitted model is often used to predict the response values of new observations. The following predicts the responses for the ten observations in gasTest, using two components:
predict(gas1, ncomp = 2, newdata = gasTest)

#Because we know the true response values for these samples, we can calculate the test set RMSEP:
#For two components, we get 0.244, which is quite close to the cross-validated estimate above (0.297).
RMSEP(gas1, newdata = gasTest)
