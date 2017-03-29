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

*******************************
library(readxl)
X306 <- read_excel("~/Desktop/306.xlsx")
View(X306)
#correlation table
d<-data.frame(X306$AQI, X306$CO_24h, X306$NO2_24h, X306$O3_24h, X306$PM10_24h, X306$PM2.5_24h, X306$SO2_24h)
cor(d)
#PM10_24h and PM10_24h are highly correlated, so might not need to include both of them

#fit1
fit1<-lm(X306$AQI~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h)
sumFit <- summary(fit1)
sigma <- sumFit$sigma
pred<-predict(fit1)
res<-resid(fit1) 
qqnorm(res,main="normal Q-Q plot of residuals")
qqline(res)
plot(X306$AQI,res,xlab="AQL",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("residual VS.AQL ",cex.main=0.8)
#There are lots of points outside the 2*residual range, so we need to do some transformation

#plot residuals VS. each explanatory variable
plot(X306$CO_24h, res)
plot(X306$NO2_24h, res)
plot(X306$O3_24h, res)
plot(X306$PM10_24h, res) #spread out pattern-> try log first?
plot(X306$PM2.5_24h, res) #spread out pattern->try log first?
plot(X306$SO2_24h, res)
plot(X306$AQI, res) #increasing trend

#log transfromation on PM10_24h, PM2.5_24h and AQI
PM10<-log(X306$PM10_24h)
PM2.5<-log(X306$PM2.5_24h)
aql<-log(X306$AQI)
fit2<-lm(aql~X306$CO_24h+X306$NO2_24h+X306$O3_24h+PM10+PM2.5+X306$SO2_24h)
summary(fit2) #res:0.4713  adjR^2: 0.4579  NO2_24h and PM10 (***) PM2.5 (.)
sigma2 <- summary(fit2)$sigma
res2<-resid(fit2)
plot(aql, res2)
abline(h=2*sigma2); abline(h=-2*sigma2); abline(h=0) # better than fit1 but still not quite desirable

#add a scaler of 10 on CO_24h, log(PM10_24h), log(PM2.5_24h), and a scaler of 100 on sqrt(AQI)
CO<-10*X306$CO_24h
PM10<-log(X306$PM10_24h)*10
PM2.5<-log(X306$PM2.5_24h)*10
aql<-sqrt(X306$AQI)*100
fit3<-lm(aql~CO+X306$NO2_24h+X306$O3_24h+PM10+PM2.5+X306$SO2_24h)
summary(fit3) #res:230.4 adjR^2: 0.5019 
sigma3 <- summary(fit3)$sigma
res3<-resid(fit3)
plot(aql, res3)
abline(h=2*sigma3); abline(h=-2*sigma3); abline(h=0) #the betas are bigger than fit1 which would be easier to do analysis

#variable selection
library(leaps)
out.exh<-regsubsets(X306$AQI~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h, data=X306,nbest=1,nvmax=6)
summ.exh<-summary(out.exh)
print(summ.exh)
cat("Cp and adjr\n")
print(summ.exh$cp)
print(summ.exh$adjr) #delete CO_24h first and try with fit3
fit3_star<-fit3<-lm(aql~X306$NO2_24h+X306$O3_24h+PM10+PM2.5+X306$SO2_24h)
summary(fit3_star) #res:230.5 adjR^2: 0.5016 
sigma3_star <- summary(fit3_star)$sigma
res3_star<-resid(fit3_star)
plot(aql, res3_star)
abline(h=2*sigma3_star); abline(h=-2*sigma3_star); abline(h=0) 
#maybe fit3_star is better than fit3 since there's one more significant variable NO2_24h







