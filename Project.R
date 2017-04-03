
library(readxl)
X306 <- read_excel("C:/Users/Frances/Desktop/Geeksquad Backup/users/Desktop/Chun fang/homework2016-2017/Term 2/stat 306/ProjectData.xlsx")

#correlation table
d<-data.frame(X306$AQI, X306$CO_24h, X306$NO2_24h, X306$O3_24h, X306$PM10_24h, X306$PM2.5_24h, X306$SO2_24h)
cor(d)
#PM10_24h and PM10_24h are highly correlated, so might not need to include both of them

#fit1
fit1<-lm(X306$AQI~X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h)
sumFit1 <- summary(fit1)  #adjR2 = 0.5569
sigma <- sumFit$sigma
pred<-predict(fit1)
res<-resid(fit1) 
qqnorm(res,main="normal Q-Q plot of residuals")
qqline(res)
plot(X306$AQI,res,xlab="AQL",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("residual VS.AQL ",cex.main=0.8)
#There are lots of points outside the 2*residual range, so we need to do some transformation

plot(X306$CO_24h,X306$AQI)
plot(X306$NO2_24h, X306$AQI)
#plot(X306$O3_24h, X306$AQI)
plot(X306$PM10_24h, X306$AQI) 
plot(X306$PM2.5_24h, X306$AQI)
plot(X306$SO2_24h, X306$AQI)

sqCO <- X306$CO_24h^2
sqNO2 <- X306$NO2_24h^2
sqPM10 <- X306$PM10_24h^2
sqPM25 <- X306$PM2.5_24h^2
sqSO2 <- X306$SO2_24h^2

CONO <- X306$CO_24h*X306$NO2_24h
COPM10 <- X306$CO_24h*X306$PM10_24h
COPM25 <- X306$CO_24h*X306$PM2.5_24h
COSO <- X306$CO_24h*X306$SO2_24h
NOPM10 <- X306$NO2_24h*X306$PM10_24h
NOPM25 <- X306$NO2_24h*X306$PM2.5_24h
NOSO <- X306$NO2_24h*X306$SO2_24h
PM10PM25 <- X306$PM10_24h*X306$PM2.5_24h
PM10SO <- X306$PM10_24h*X306$SO2_24h
PM25SO <- X306$PM2.5_24h*X306$SO2_24h


fit4 <- lm(X306$AQI~PM10SO+PM25SO+CONO+COPM10+COPM25+COSO+NOPM10+NOPM25+NOSO+PM10PM25+X306$NO2_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h)
sumFit4 <- summary(fit4)

out.exh4<-regsubsets(X306$AQI~PM10SO+PM25SO+CONO+COPM10+COPM25+COSO+NOPM10+NOPM25+NOSO+PM10PM25+X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h, data=X306,nbest=1, nvmax=16)
summ.exh4<-summary(out.exh4)
cat("Cp and adjr\n")
print(summ.exh4$cp)
print(summ.exh4$adjr)


fit5 <- lm(X306$AQI~sqCO+sqNO2+sqPM10+sqPM25+sqSO2+X306$NO2_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h)
sumFit5 <- summary(fit5)

out.exh5<-regsubsets(X306$AQI~sqCO+sqNO2+sqPM10+sqPM25+sqSO2+PM10SO+PM25SO+CONO+COPM10+COPM25+COSO+NOPM10+NOPM25+NOSO+PM10PM25+X306$CO_24h+X306$NO2_24h+X306$O3_24h+X306$PM10_24h+X306$PM2.5_24h+X306$SO2_24h, data=X306,nbest=1, nvmax=21)
summ.exh5<-summary(out.exh5)
cat("Cp and adjr\n")
print(summ.exh5$cp)
print(summ.exh5$adjr) #max adjr = 0.60607





#########################
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

