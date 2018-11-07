#######################################################################
#   
#   University of Edinburgh Business School
#   MSc Business Analytics
#   Predictive Analytics and Modelling Data
#
#   Forecasting assignment
#
#   Please read attached 'Report' file for the analysis and more information
#
#   Copyright© of this project is with the authors
#
#######################################################################

# Load libraries

library(zoo);
library(taRifx);
library(lubridate);
library(xts);
library(tidyquant);
library(tseries);

library(fpp);
library(dplyr);
library(corrplot);
library(car);
library(caret);

library(AppliedPredictiveModeling);
library(MASS);
library(pls);
library(gdata);
library(TTR);
library(vars);
library(lmtest);

#######################################################################
#
#                      Exponential Smoothing
#
#######################################################################

#Read data 
dataset <- read.csv('1.5yearweekDLC.csv',header=TRUE,sep=',')

#Dataframe with only dates and closing price
NewData <- as.data.frame( dataset[,1:2], drop=false)
NewData <- NewData[,-1]

#Plotting the data as timeseries
plot.ts(NewData,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices")

#Holt method
FinalData <- window(NewData,start=1,end=80)
fit1 <- holt(FinalData, alpha=0.8, beta=0.2, initial="simple", h=20) 

##Explonential trend method
fit2 <- holt(FinalData, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=20) 

# Results for first model:
fit1$model$state
fitted(fit1)
summary(fit1)
summary(fit2)

##Check accuracy using accuracy()
fit1$mean

#Damped Trend Method
fit3 <- holt(FinalData, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=20) 
summary(fit3)
plot(fit2, type="o", ylab="Weekly closing price", xlab="Weeks", 
     fcol="white", plot.conf=FALSE,main='Holts method forecast(0.8,0.2)')
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topright", lty=1, cex=0.60, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))

# Model 2
FinalData <- window(NewData,start=1,end=80)
fit1 <- holt(FinalData, alpha=0.2, beta=0.1, initial="simple", h=20) 
fit2 <- holt(FinalData, alpha=0.2, beta=0.1, initial="simple", exponential=TRUE, h=20) 

# Results for first model and plot:
fit1$model$state
fitted(fit1)
fit1$mean
summary(fit1)
fit3 <- holt(FinalData, alpha=0.2, beta=0.1, damped=TRUE, initial="simple", h=20) 
plot(fit2, type="o", ylab="Weekly closing price", xlab="Weeks", 
     fcol="white", plot.conf=FALSE, main='Holts method forecast(0.2,0.1)')
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topright", lty=1, cex=0.60, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))


#######################################################################
#
#                             ARIMA models
#
#######################################################################

#caTools
install.packages('caTools')

#Read data 
dataset <- read.csv('1.5yearweekDLC.csv',header=TRUE,sep=',')

#Dataframe with only dates and closing price
NewData <- as.data.frame( dataset[,1:2], drop=false)
NewData <- NewData[,-1]

#Creating Training and testing dataset
split=sample.split(NewData,SplitRatio = 0.7)
TrainingData = subset(NewData,split==TRUE)
TestData = subset(NewData,split==FALSE)

#Plotting the data as timeseries
plot.ts(TrainingData,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices")

#Plot time series, ACF and PACF
tsdisplay(TrainingData,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices")

##Unit root test (Augmented Dickey-Fuller test)
adf.test(TrainingData,alternative="stationary");
adf.test(diff(TrainingData),alternative="stationary");

##Frist difference of data
##ACF, PACF of differenced data to determine possible candidate models
tsdisplay(diff(TrainingData),xlab = "Weeks", ylab = "Weekly closing prices", main = "Time series after 1st Differencing");

##Residual diagnostics (Box-Ljung test)
##Box-Ljung test for autocorrelation up until r_10
##The higher the p-value, the better (-> H_0 rejected: no independence)
Box.test(TrainingData, type = "L", lag = 10);

##Automatically fit an ARIMA-model

## auto.arimaT (for training and testing)
fitTT=auto.arima(TrainingData,seasonal=F);
summary(fitTT);
FV1=forecast(fitTT, h=24);
FFV1=as.numeric(FV1$mean)
plot(forecast(fitTT, h=24), include=95,xlab = "Weeks", ylab = "Weekly closing prices",col="red");

##ACF, PACF of the residuals of chosen model
tsdisplay(fitTT$residuals,xlab = "Weeks", ylab = "Weekly closing prices", main = "Time plot of Residuals from ARIMA method");

##Portmanteau test of the residuals of chosen model
Box.test(fitTT$residuals, type = "L", lag = 10);

##Histogram of residuals
hist(fitTT$residuals, nclass="FD", main="Histogram of residuals")
accuracy(fitTT)

df1<- data.frame(TestData,FFV1)
col_headings <- c("Actual Price","Forecasted Price")
names(df1) <- col_headings
AP1<-df1$'Actual Price'
FP1<-df1$'Forecasted Price'
percentage_error=(AP1-FP1)/AP1
percentage_error
mean(percentage_error)


####################################################################################### 

#Forecasting for data after implementing a training testing methodology


#Plotting the data as timeseries
plot.ts(NewData,xlab = "Weeks", ylab = "Weekly closing prices", plot.type = c("multiple"), main = "DC Stock Prices",col="green")

#Plot time series, ACF and PACF
tsdisplay(NewData,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices")

##Unit root test (Augmented Dickey-Fuller test)
adf.test(NewData,alternative="stationary");
adf.test(diff(NewData),alternative="stationary");

##Frist difference of data
##ACF, PACF of differenced data to determine possible candidate models
tsdisplay(diff(NewData),xlab = "Weeks", ylab = "Weekly closing prices", main = "Time series after 1st Differencing");

##Residual diagnostics (Box-Ljung test)
##Box-Ljung test for autocorrelation up until r_10
##The higher the p-value, the better (-> H_0 rejected: no independence)
Box.test(NewData, type = "L", lag = 10);

##Automatically fit an ARIMA-model

## auto.arima1
fitT1=auto.arima(NewData,seasonal=F);
summary(fitT1);
accuracy(fitT1);
plot(forecast(fitT1, h=20), include=95,xlab = "Weeks", ylab = "Weekly closing prices");

##ACF, PACF of the residuals of chosen model
tsdisplay(fitT1$residuals,xlab = "Weeks", ylab = "Weekly closing prices", main = "Time plot of Residuals from ARIMA method");

##Portmanteau test of the residuals of chosen model
Box.test(fitT1$residuals, type = "L", lag = 10);

##Histogram of residuals
hist(fitT1$residuals, nclass="FD", main="Histogram of residuals")

## auto.arima2
fitT2=auto.arima(NewData,seasonal=F,stepwise = FALSE,approximation = FALSE);
summary(fitT2);
plot(forecast(fitT2, h=20),  include=95,xlab = "Weeks", ylab = "Weekly closing prices");

##ACF, PACF of the residuals of chosen model
tsdisplay(fitT2$residuals,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices");

##Portmanteau test of the residuals of chosen model
Box.test(fitT2$residuals, type = "L", lag = 10)

##Histogram of residuals
hist(fitT2$residuals, nclass="FD", main="Histogram of residuals")

##Try out different ARIMA models
fit1 = Arima(NewData, c(3, 1, 0));
fit2 = Arima(NewData, c(3, 1, 1));
fit3 = Arima(NewData, c(4, 1, 0));
fit4 = Arima(NewData, c(2, 1, 0));
fit5 = Arima(NewData, c(1, 1, 0));

tsdisplay(fit1$residuals,xlab = "Weeks", ylab = "Weekly closing prices");
Box.test(residuals(fit1), lag=24, fitdf=4, type="Ljung");
plot(forecast(fit1),xlab = "Weeks", ylab = "Weekly closing prices");

tsdisplay(fit2$residuals,xlab = "Weeks", ylab = "Weekly closing prices");
Box.test(residuals(fit2), lag=24, fitdf=4, type="Ljung");
plot(forecast(fit2),xlab = "Weeks", ylab = "Weekly closing prices");

tsdisplay(fit3$residuals,xlab = "Weeks", ylab = "Weekly closing prices");
Box.test(residuals(fit3), lag=24, fitdf=4, type="Ljung");
plot(forecast(fit3),xlab = "Weeks", ylab = "Weekly closing prices");

tsdisplay(fit4$residuals,xlab = "Weeks", ylab = "Weekly closing prices");
Box.test(residuals(fit4), lag=24, fitdf=4, type="Ljung");
plot(forecast(fit4),xlab = "Weeks", ylab = "Weekly closing prices");

tsdisplay(fit5$residuals,xlab = "Weeks", ylab = "Weekly closing prices");
Box.test(residuals(fit5), lag=24, fitdf=4, type="Ljung");
plot(forecast(fit5),xlab = "Weeks", ylab = "Weekly closing prices");


##Compare
summary(fit1);
summary(fit2);
summary(fit3);
summary(fit4);
summary(fit5);



#######################################################################
#
#                       Implementing GARCH Models
#
#######################################################################

library(quantmod);
library(rugarch);
library(fGarch);

#Load weekly data 
weeklong <-read.csv('5yeardailyDLC.csv');

#remove any NaN values
weeklong[weeklong=="null"] <- NA;
weeklong <- na.omit(weeklong);

#Convert into xts time series
close <- as.numeric(as.matrix(weeklong)[ ,5]);
dates <- as.Date(weeklong[,1], format="%Y-%m-%d");
stockp <- data.frame("week" = dates, "close" = close);
stockp <- xts(stockp$close, order.by=stockp$week);

#Plot
chartSeries(stockp)

#Specify standard (1,1) GARCH models with different ARMA parameters
#ARMA order for past 30 days
stockp1 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),mean.model=list(armaOrder=c(365,365)),distribution.model="std");
#ARMA order for past 14 days
stockp2 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),mean.model=list(armaOrder=c(152,152)),distribution.model="std");
#ARMA order for past 7 days
stockp3 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),mean.model=list(armaOrder=c(30,30)),distribution.model="std");
#Different GARCH (2,2) model for last 7 days
stockp4 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,2)),mean.model=list(armaOrder=c(30,30)),distribution.model="std");
#GARCH model with std distribution
stockp5 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),mean.model=list(armaOrder=c(30,30)),distribution.model="norm");

#Fit above GARCH models
stockpGarch1 <- ugarchfit(spec=stockp1, data=stockp);
stockpGarch2 <- ugarchfit(spec=stockp2, data=stockp);
stockpGarch3 <- ugarchfit(spec=stockp3, data=stockp);
stockpGarch4 <- ugarchfit(spec=stockp4, data=stockp);
stockpGarch5 <- ugarchfit(spec=stockp5, data=stockp);

#Obtain model results and check Information Criteria values
stockpGarch1
stockpGarch2
stockpGarch3
stockpGarch4
stockpGarch5

#Use ugarchforecast to see full plot of GARCH models to predict n periods ahead
#Plot predictions and select 1: Time Series Prediction (unconditional)
prediction1 <- ugarchforecast(stockpGarch1, n.ahead=140, method=c("Partial","Full")[1]);
plot(prediction1,weight = 1)

prediction2 <- ugarchforecast(stockpGarch2, n.ahead=140, method=c("Partial","Full")[1]);
plot(prediction2,weight = 1)

prediction3 <- ugarchforecast(stockpGarch3, n.ahead=140, method=c("Partial","Full")[1]);
plot(prediction3,weight = 1)

prediction4 <- ugarchforecast(stockpGarch4, n.ahead=140, method=c("Partial","Full")[1]);
plot(prediction4,weight = 1)

prediction5 <- ugarchforecast(stockpGarch5, n.ahead=140, method=c("Partial","Full")[1]);
plot(prediction5,weight = 1)

#Use ugarchbootto see plot of GARCH models to predict n periods ahead from T0
predictionb1 <- ugarchboot(stockpGarch1, n.ahead=30, method=c("Partial","Full")[1])
#Plot predictions and select 2: Series Standard Error Plots
plot(predictionb1,weight = 1)

predictionb2 <- ugarchboot(stockpGarch2, n.ahead=30, method=c("Partial","Full")[1])
#Plot predictions and select 2: Series Standard Error Plots
plot(predictionb2,weight = 1)

predictionb3 <- ugarchboot(stockpGarch3, n.ahead=30, method=c("Partial","Full")[1])
#Plot predictions and select 2: Series Standard Error Plots
plot(predictionb3,weight = 1)

predictionb4 <- ugarchboot(stockpGarch4, n.ahead=30, method=c("Partial","Full")[1])
#Plot predictions and select 2: Series Standard Error Plots
plot(predictionb4,weight = 1)

predictionb5 <- ugarchboot(stockpGarch5, n.ahead=30, method=c("Partial","Full")[1])
#Plot predictions and select 2: Series Standard Error Plots
plot(predictionb5,weight = 1)

#Autodetect GARCH model
source("garchAuto.R")
rets = ROC(Cl(stockp), na.pad=FALSE)
fit = garchAuto(rets, trace=TRUE)


#######################################################################
#
#                      Finding Granger Causality
#
#######################################################################

#Read data files and select appropriate intervals (Dec-2014 - Nov 2017)

#Select DLC stock's monthly closing price
stock <- read.csv('3yearmonthDLC.csv')
stock <- ts(stock[(2:(nrow(stock)-1)), c(5)])

#Perform Augmented Dickey-Fuller test to test for stationality
adf.test(stock)

#Consumer Price Inflation 
CPI <- read.csv('5yearmonthCONSUMERPRICEINFLATION.csv')
CPI <- CPI[((nrow(CPI)-34):nrow(CPI)), c(2)]

#Perform Augmented Dickey-Fuller test to test for stationality
adf.test(CPI)

#Consumer Confidence
CC <- read.csv('5yearmonthUKCONSUMERCONFIDENCE.csv')
CC <- CC[(27:nrow(CC)), c(7)]

#Perform Augmented Dickey-Fuller test to test for stationality
adf.test(CC)

#Obtain Google Trends 'dixons carphone' data and convert from weekly into monthly
GoogleTrends <- read.csv('3yearweekGT.csv')
GoogleTrends <- GoogleTrends[3:(nrow(GoogleTrends)-2), c(1,2)]
amount <- as.numeric(as.matrix(GoogleTrends)[,2])
dates <- as.Date(GoogleTrends[,1], format="%Y-%m-%d")
GoogleTrends <- data.frame("Month" = dates, "Amount" = amount)
GoogleTrends.xts <- xts(GoogleTrends$Amount, order.by=GoogleTrends$Month)
GoogleTrends.xts <- apply.monthly(GoogleTrends.xts, "sum")
GoogleTrends <- data.frame(index(GoogleTrends.xts), GoogleTrends.xts)
GoogleTrends <- ts(GoogleTrends[1:(nrow(GoogleTrends)-1), c(2)])

#Perform Augmented Dickey-Fuller test to test for stationality
adf.test(GoogleTrends)

#Obtain Google Trends 'dixons carphone share' data and convert from weekly into monthly
GoogleTrendsshare <- read.csv('3yearweekGTshare.csv')
GoogleTrendsshare <- GoogleTrendsshare[3:(nrow(GoogleTrendsshare)-2), c(1,2)]
amount <- as.numeric(as.matrix(GoogleTrendsshare)[,2])
dates <- as.Date(GoogleTrendsshare[,1], format="%Y-%m-%d")
GoogleTrendsshare <- data.frame("Month" = dates, "Amount" = amount)
GoogleTrendsshare.xts <- xts(GoogleTrendsshare$Amount, order.by=GoogleTrendsshare$Month)
GoogleTrendsshare.xts <- apply.monthly(GoogleTrendsshare.xts, "sum")
GoogleTrendsshare <- data.frame(index(GoogleTrendsshare.xts), GoogleTrendsshare.xts)
GoogleTrendsshare <- ts(GoogleTrendsshare[1:(nrow(GoogleTrendsshare)-1), c(2)])

#Perform Augmented Dickey-Fuller test to test for stationality
adf.test(GoogleTrendsshare)

#Apply differencing to all datasets from ADF tests
stock <- diff(stock)
CC <- ts(diff(CC))
CPI <- ts(diff(CPI))
GoogleTrends <- ts(diff(GoogleTrends))
GoogleTrendsshare <- ts(diff(GoogleTrendsshare))

#Create dataframe to merge the variables together
CCstock <- data.frame("CC" = CC, "stock" = stock)
CPIstock <- data.frame("CPI" = CPI, "stock" = stock)
GTstock <- data.frame("GoogleTrends" = GoogleTrends, "stock" = stock)
GTshare <- data.frame("GoogleTrendsshare" = GoogleTrendsshare, "stock" = stock)

#Use tsdisplay to see time series + ACF + PACF
tsdisplay(stock)
tsdisplay(CPI)
tsdisplay(CC)
tsdisplay(GoogleTrends)
tsdisplay(GoogleTrendsshare)

# Find an vector autoregressive model for 2 significant relations:
VARselect(CC, lag.max=5, type="const")$selection;
#Test Granger causality CC on stock
var <- VAR(CCstock, p = 2, type = "const");
causality(var, cause = "CC")$Granger

# Find an vector autoregressive model for 2 significant relations:
VARselect(CPIstock, lag.max=5, type="const")$selection;
#Test Granger causality CPI on stock
var <- VAR(CPIstock, p = 1, type = "const");
causality(var, cause = "CPI")$Granger

# Find an vector autoregressive model for 2 significant relations:
VARselect(GTstock, lag.max=7, type="const")$selection;
#Test Granger causality CoogleTrends on stock
var <- VAR(GTstock, p = 7, type = "const");
causality(var, cause = "GoogleTrends")$Granger

# Find an vector autoregressive model for 2 significant relations:
VARselect(GTshare, lag.max=7, type="const")$selection;
#Test Granger causality CoogleTrends on stock
var <- VAR(GTshare, p = 7, type = "const");
causality(var, cause = "GoogleTrendsshare")$Granger

#Use second Granger casuality test to confirm results
grangertest(CC ~ stock, order = 1, data = CCstock)
grangertest(CPI ~ stock, order = 1, data = CPIstock)
grangertest(GoogleTrends ~ stock, order = 1, data = GTstock)
grangertest(GoogleTrendsshare ~ stock, order = 1, data = GTshare)

# Conclusion
# No Granger causality for any of the 4 tested variables



#######################################################################
#
#            ARIMA random walk with drift for long-term
#
#######################################################################


#caTools
install.packages('caTools')

#Read data 
dataset <- read.csv('3yearweekDLC.csv',header=TRUE,sep=',')

#Dataframe with only dates and closing price
NewData <- as.data.frame( dataset[,1:2], drop=false)
NewData <- NewData[,-1]

#Plotting the data as timeseries
plot.ts(NewData,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices")
#Plot time series, ACF and PACF
tsdisplay(NewData,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices")

##Unit root test (Augmented Dickey-Fuller test)
adf.test(NewData,alternative="stationary");
adf.test(diff(NewData),alternative="stationary")

#Creating Training and testing dataset
split=sample.split(NewData,SplitRatio = 0.7)
TrainingData = subset(NewData,split==TRUE)
TestData = subset(NewData,split==FALSE)

## auto.arimaT (for training and testing)
fitTT=auto.arima(TrainingData,seasonal=F);
summary(fitTT);
FV1=forecast(fitTT, h=48);
FFV1=as.numeric(FV1$mean)
plot(forecast(fitTT, h=48), include=95,xlab = "Weeks", ylab = "Weekly closing prices",col="red");

##ACF, PACF of the residuals of chosen model
tsdisplay(fitTT$residuals,xlab = "Weeks", ylab = "Weekly closing prices", main = "Time plot of Residuals from ARIMA method");

##Portmanteau test of the residuals of chosen model
Box.test(fitTT$residuals, type = "L", lag = 10);

df1<- data.frame(TestData,FFV1)
col_headings <- c("Actual Price","Forecasted Price")
names(df1) <- col_headings
AP1<-df1$'Actual Price'
FP1<-df1$'Forecasted Price'
percentage_error=(AP1-FP1)/AP1
percentage_error
mean(percentage_error)

#################################################################

##Residual diagnostics (Box-Ljung test)
##Box-Ljung test for autocorrelation up until r_10
##The higher the p-value, the better (-> H_0 rejected: no independence)
Box.test(NewData, type = "L", lag = 10);

##Automatically fit an ARIMA-model

## auto.arima1
fitT1=auto.arima(NewData,seasonal=F);
summary(fitT1);
accuracy(fitT1);
plot(forecast(fitT1, h=48), include=95,xlab = "Weeks", ylab = "Weekly closing prices");

##ACF, PACF of the residuals of chosen model
tsdisplay(fitT1$residuals,xlab = "Weeks", ylab = "Weekly closing prices", main = "Time plot of Residuals from ARIMA method");

##Portmanteau test of the residuals of chosen model
Box.test(fitT1$residuals, type = "L", lag = 10);

##Histogram of residuals
hist(fitT1$residuals, nclass="FD", main="Histogram of residuals")

## auto.arima2
fitT2=auto.arima(NewData,seasonal=F,stepwise = FALSE,approximation = FALSE);
summary(fitT2);
plot(forecast(fitT2, h=48),  include=95,xlab = "Weeks", ylab = "Weekly closing prices");

##ACF, PACF of the residuals of chosen model
tsdisplay(fitT2$residuals,xlab = "Weeks", ylab = "Weekly closing prices", main = "DC Stock Prices");

##Portmanteau test of the residuals of chosen model
Box.test(fitT2$residuals, type = "L", lag = 10)


##################################################################
# End of document

