setwd("/Users/Joy/Desktop/STAT306_lab9/proj")

library(readxl)
X306 <- read_excel("~/Desktop/STAT306_lab9/proj/306.xlsx")

fit_con<-lm(X306$AQI~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h)