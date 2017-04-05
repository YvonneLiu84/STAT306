setwd("/Users/Joy/Desktop/STAT306_lab9/proj")

library(readxl)
library(leaps)

X306 <- read_excel("~/Desktop/STAT306_lab9/proj/306.xlsx")

## Requirment 1 (Summary of each variable and their corr)
summary(X306$AQI)
summary(X306$CO_24h)
summary(X306$NO2_24h)
summary(X306$O3_24h)
summary(X306$PM10_24h)
summary(X306$PM2.5_24h)
summary(X306$SO2_24h)
#correlation matrix
d<-data.frame(X306$AQI, X306$CO_24h, X306$NO2_24h, X306$O3_24h, X306$PM10_24h, X306$PM2.5_24h, X306$SO2_24h)
cor(d)
## END Reuirement 1 (Summary of each variable and their corr)

## Requirment 2 (Create a model)
X306 <- X306[-478,] #delete the row with AQI = 0
newO3 <- X306$O3_24h^0.3
fit6 <- lm(log(X306$AQI)~sqrt(X306$CO_24h)+X306$NO2_24h+newO3+log(X306$SO2_24h)+log(X306$PM10_24h)+log(X306$PM2.5_24h), data = X306)
summary(fit)

## END Requirment 2 (Create a model)

## Requirment 3 (Print out the residual)
#plot residuals vs fitted response variable
sigma <- sumFit$sigma
plot(fit6$fitted.values, fit6$residuals, main = "Residual Plot", ylab = "residuals", xlab = "Fitted AQI")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)

plot(sqrt(X306$CO_24h), fit6$residuals, main = "Residual Plot", ylab = "residuals", xlab = "CO_24h")
plot(X306$NO2_24h, fit6$residuals, main = "Residual Plot", ylab = "residuals", xlab = "NO2_24h")
plot(newO3, fit6$residuals, main = "Residual Plot", ylab = "residuals", xlab = "O3_24h")
plot(log(X306$PM10_24h), fit6$residuals, main = "Residual Plot", ylab = "residuals", xlab = "PM10_24h") 
plot(log(X306$PM2.5_24h), fit6$residuals, main = "Residual Plot", ylab = "residuals", xlab = "PM2.5_24h")
plot(log(X306$SO2_24h), fit6$residuals, main = "Residual Plot", ylab = "residuals", xlab = "SO2_24h")
## END Requirment 3 (Print out the residual)


# Requirement 4 (Use variable selection)
out.exh6<-regsubsets(log(X306$AQI)~sqrt(X306$CO_24h)+X306$NO2_24h+newO3+log(X306$SO2_24h)+log(X306$PM10_24h)+log(X306$PM2.5_24h), data = X306, nbest=1, nvmax=6)
summ.exh6<-summary(out.exh6)
cat("Cp and adjr\n")
print(summ.exh6$cp)
print(summ.exh6$adjr)
# Based on Cp and adjR^2, we decide to compare 3, 4, 5, 6 variables since their Cp and adjR^2 are really close
# Perform DW test
dwtest(log(X306$AQI)~sqrt(X306$CO_24h)+X306$NO2_24h+newO3+log(X306$SO2_24h)+log(X306$PM10_24h)+log(X306$PM2.5_24h), data = X306)
#Conclude that the assumption of residuals are independent is valid here based on DW 
# END Requirement 4 (Use variable selection)


# Requirement 5 ( Set training and hold-out set.)
## Make Train and hold out set
n <- nrow(X306)
set.seed(1)
# here select 1/5 of the data randomly to be the hold out set. 
holdoutIndex <- sort(sample(1:n, round(n/5), replace = FALSE))

# Initially subset1 is the training set and subset2 is the hold-out set 
holdoutSet <- X306[holdoutIndex,]
trainingSet <- X306[-holdoutIndex,] 

#based on the exhaustive selection; compare Cp and adjR^2 with 6, 3, 4, 5 variables
model.trainSet_6var <- lm(log(AQI)~sqrt(CO_24h)+NO2_24h+I(O3_24h^0.3)+log(SO2_24h)+log(PM10_24h)+log(PM2.5_24h), data = trainingSet)
model.trainSet_3var <- lm(log(AQI)~NO2_24h+log(PM10_24h)+log(PM2.5_24h), data = trainingSet)
model.trainSet_4var <- lm(log(AQI)~NO2_24h+log(SO2_24h)+log(PM10_24h)+log(PM2.5_24h), data = trainingSet)
model.trainSet_5var <- lm(log(AQI)~NO2_24h+I(O3_24h^0.3)+log(SO2_24h)+log(PM10_24h)+log(PM2.5_24h), data = trainingSet)                      


# Make predictions at the each hold-out data set.
pred.holdout_6var <- predict(model.trainSet_6var, holdoutSet)
holdout.err_6var <- sqrt(sum((holdoutSet$AQI - exp(pred.holdout_6var))^2)/length(pred.holdout_6var))
plot(holdoutIndex, (holdoutSet$AQI - exp(pred.holdout_6var)))
                          
pred.holdout_3var <- predict(model.trainSet_3var, holdoutSet)
holdout.err_3var <- sqrt(sum((holdoutSet$AQI - exp(pred.holdout_3var))^2)/length(pred.holdout_3var))
plot(holdoutIndex, (holdoutSet$AQI - exp(pred.holdout_3var))) 
                          
pred.holdout_4var <- predict(model.trainSet_4var, holdoutSet)
holdout.err_4var <- sqrt(sum((holdoutSet$AQI - exp(pred.holdout_4var))^2)/length(pred.holdout_4var))
plot(holdoutIndex, (holdoutSet$AQI - exp(pred.holdout_4var)))
                          
pred.holdout_5var <- predict(model.trainSet_5var, holdoutSet)
holdout.err_5var <- sqrt(sum((holdoutSet$AQI - exp(pred.holdout_5var))^2)/length(pred.holdout_5var))
plot(holdoutIndex, (holdoutSet$AQI - exp(pred.holdout_5var)))
#from the residual plot of the holdoutSet actual data vs predicted values based on training set
#is approx. random
#Compare the prediction errors and choose the best model: 3 variables with NO2_24h+log(PM10_24h)+log(PM2.5_24h)                      

#predicted valus and actual values based on the best model chosed from requirement 4
plot(holdoutIndex, holdoutSet$AQI, type = "l")
lines(holdoutIndex, exp(pred.holdout_3var), type = "l", col = 'red')
# END Requirement 5 ( Set training and hold-out set.)

## Requirment 6 (ls.diag(fit) Cook&dfit)
#cooks distance / dfits
fit6_new<-lm(log(AQI)~NO2_24h+log(PM10_24h)+log(PM2.5_24h), data=X306)
diagnostics <- ls.diag(fit6_new)
cookDis <- diagnostics$cooks
dfits <- diagnostics$dfits

#plot cooks distance and dfits
index <- seq(1:nrow(X306))
plot(index, cookDis, main = "Cooks Distance Plot", xlab = "Index", ylab = "Cooks Distance")
plot(index, dfits, main = "dfits Plot", xlab = "Index", ylab = "dfits")
## END Requirment 6 (ls.diag(fit) Cook&dfit)
