setwd("/Users/Joy/Desktop/STAT306_lab9/proj")

library(readxl)
X306 <- read_excel("~/Desktop/STAT306_lab9/proj/306.xlsx")

plot(X306$AQI~X306$CO_24h)
plot(X306$AQI~X306$NO2_24h)
plot(X306$AQI~X306$O3_24h)
plot(X306$AQI~X306$PM2.5_24h)
plot(X306$AQI~X306$SO2_24h)

fit_con<-lm(X306$AQI~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h)

sumFit <- summary(fit_con)
sigma <- sumFit$sigma
pred<-predict(fit_con)
res<-resid(fit_con) 
qqnorm(res,main="normal Q-Q plot of residuals")
qqline(res)
plot(pred,res,xlab="predicted value",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("const spread or pattern as yhat increases?",cex.main=0.8)

ls.diag(fit_con)

