## Discharge plots
library(lubridate)
library(here)
library(plotly)
library(dplyr)
library(ggplot2)

df <- read.csv(here::here("data_4_analysis/All_Stream_Data.csv"))%>%
  select(DateTime,stn1_Q,stn2_Q,stn3_Q,stn4_Q)
df$DateTime <- ymd_hms(df$DateTime)


df%>%
  plot_ly()%>%
  add_markers(x=~DateTime, y =~stn1_Q)%>%
  add_markers(x=~DateTime, y =~stn2_Q)%>%
  add_markers(x=~DateTime, y =~stn3_Q)%>%
  add_markers(x=~DateTime, y =~stn4_Q)
