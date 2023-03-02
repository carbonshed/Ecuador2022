library(dplyr)
library(ggplot2)


Amy <- read.csv('wetlands_df.csv')

print(Amy)


ggplot()+
  geom_line(data =  Amy, mapping =  aes(x-Date, y-Watertemp_c))




ggplot (Amy, aes(Date.,Watertemp_c))+ geom_point(
)

ggplot (Amy, aes(Date.,Watertemp_c))+ geom_line(
)
