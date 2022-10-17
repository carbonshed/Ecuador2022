## Use this code to start exploring the possibilities of ggplot

#first: go to the link below

# https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html

#Work your way through the code on that page. 

#copy and paste the code from the the grayed boxes into this R file so that you can test out the code yourself.

#I found reading in the data to be a little tricky, so I did that part for you, below. 

#The data that they want you to download is already in your folder.

library(tidyverse)
library(here)

surveys_complete <- read_csv(here::here("Amy/combined.csv"))

ggplot(data = surveys_complete)

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))
