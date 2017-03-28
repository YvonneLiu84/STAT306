setwd("/Users/Joy/Desktop/STAT306_lab9/proj")

library(readxl)
X306 <- read_excel("~/Desktop/STAT306_lab9/proj/306.xlsx")

View(X306)
X306$AL[X306$AQI_Level=="Excellent"]<-1
X306$AL[X306$AQI_Level=="Moderate"]<-2
X306$AL[X306$AQI_Level=="Unhealthy for sensitive groups"]<-3
X306$AL[X306$AQI_Level=="Unhealthy"]<-4
X306$AL[X306$AQI_Level=="Very unhealthy"]<-5
X306$AL[X306$AQI_Level=="Hazardous"]<-6
fit<-glm(X306$AL~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h, family = "poisson")
summary(fit)
#exhaustive method
library(leaps)
out.exh<-regsubsets(X306$AL~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h, data=X306,nbest=1,nvmax=6)
summ.exh<-summary(out.exh)
print(summ.exh)
cat("Cp and adjr\n")
print(summ.forw$cp)
print(summ.forw$adjr)
#The best variable would be PM2.5_24h, PM10_24h, NO2_24h, SO2_24h, CO_24h, O3_24h
#forward method
out.forw<-regsubsets(X306$AL~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h, data=X306,method="forward",nvmax=6)
summ.forw<-summary(out.forw)
print(summ.forw$outmat)
print(summ.forw$outmat)
cat("Cp and adjr\n")
print(summ.forw$cp)
print(summ.forw$adjr)
#variable selection result is the same as the exhaustive method
#print the correlation table
d<-data.frame(X306$AL, X306$CO_24h, X306$NO2_24h, X306$O3_24h, X306$PM10_24h, X306$PM2.5_24h, X306$SO2_24h)
cor(d)
