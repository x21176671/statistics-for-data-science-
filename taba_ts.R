# stats CA assignment
# multiple linear regression analysis
# Peter McEnroe
library(ggplot2)
library(ggthemes)
library(scales)
library(Amelia)
library(dplyr)
library(leaps)
library(MASS)
library(stats)
library(car)
library(haven)
library(smooth)
library(ggplot2)
library(fpp2)
library(tseries)
library(Metrics)
library(forecast)
library(plotrix)
library(stats)

setwd("college/nci/semester1/statistics_for_data_science/TABA") 
carreg_data <- read.csv("CarRegistrations.csv")
names(carreg_data)[names(carreg_data) == "ï..1995M01"] <- "year_month"
names(carreg_data)[names(carreg_data) == "X10817"] <- "registrations"
# PART A TIME SERIES ANALYSIS
# assess nature & components of raw TS (using visualisations )
# turn csv into ts usable object 
carreg <- carreg_data[,2]  # col 2 contains the data, col 1 is YYYYM** format of time (eg jan 1995=1995M01)
ts_carreg <- ts(carreg, start =c(1995,1), frequency = 12) 
ts_carreg # season trends (everyone wants brand new 221/222 etc), cylical component around recession 
# upward trend until recession and starts again in 2010s 
autoplot(ts_carreg,ylab= "# of cars")
# check cycle
boxplot(ts_carreg ~ cycle(ts_carreg))
# check trend
boxplot(ts_carreg ~ floor(time(ts_carreg)))
# check seasonality 
monthplot(ts_carreg)
ggseasonplot(ts_carreg, polar = T)
#highlight the 132 change 2 new reg.s to buy, oooh shiney 
ts_carreg_solo <- window(ts_carreg, start = c(1995,1), frequency = 12, end = c(2012, 12))
ts_carreg_double <- window(ts_carreg, start = c(2013,1), frequency = 12, end = c(2021, 12))
monthplot(ts_carreg_solo)
monthplot(ts_carreg_double, add = T, lwd = 3, col = 'blue') # doesn't seem to have "add" as a parameter 
ggseasonplot(ts_carreg, polar = F)

# does the model need differentiating?
ndiffs(ts_carreg) # d = 1 , get first derivative to get a stationary time series 


# find candidate ts for : 
# split data into train/test 80/20
years <- end(ts_carreg)[1] - start(ts_carreg)[1] # get the number of years of the time series 
train <- window(ts_carreg, start = c(1995,1), frequency = 12, end= c(2016, 12) ) # set to 80% of the series, also includes 4 years of 
test <- window(ts_carreg, start = c(2017,1), frequency = 12, end= c(2021, 12) ) # set to latest 20% of the series

# i exp. smoothing /ETS models 
exp_fit <- ets(train, model = "ZZZ", ic = "aic") # ets(M,A,M)
# plot the model
plot(exp_fit)
# check model plot over data
plot.new
autoplot(train, series = "training data") +
  autolayer(forecast(exp_fit, h = 60), series= "exponential fit ets(M,A,M)") + # PI=F removes the range on the predicted series 
  autolayer(test, series = "test series data")
plot.new
autoplot(test, series = "test data") +
  autolayer(forecast(exp_fit, h = 60), series= "exponential fit ets(M,A,M)")# PI=F removes the range on the predicted series 

# ii arima/sarima models
#check ndiffs again for train (should be: d = 1, D =1) :D
ndiffs(train) # find the no. of ordinary differences to use = 1 
nsdiffs(train) # find the no. of seasonal differences to use = 1 
d_train <- diff(train)
#assess stability using Aug Dickey-Fuller test
adf.test(d_train) #stationary
par(mfrow= c(2,2))
Acf(train)
Pacf(train)
Acf(d_train, main = "Series diff(train)")
Pacf(d_train, main = "Series diff(train)")
#auto arima 
aarima_fit <- auto.arima(train, ic = "aic")
aarima_fit_nostep <- auto.arima(train, stepwise = F , ic = "aic")
aarima_fitd <- auto.arima(train, d =1, stepwise = F , ic = "aic")
aarima_fitd <- auto.arima(train, d =1, D=1, stepwise = F , ic = "aic")
aarima_fit
aarima_fit_nostep
aarima_fitd
aarima_fitdd

# dickey-fuller 
adf.test(aarima_fitd$fitted)

par(mfrow= c(1,2))
Acf(forecast(aarima_fitd, h=60))
Pacf(forecast(aarima_fitd, h=60))
plot.new
autoplot(train, series = "training data") +
  autolayer(forecast(aarima_fitd, h = 60), series= "arima(1,1,1)(2,1,1)[12] ") + # PI=F removes the range on the predicted series 
  autolayer(test, series = "test series data")
accuracy(as.vector(test), as.vector(forecast(exp_fit, h = 60)))
# check residuals 
res <- residuals(aarima_fitd)
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from ARIMA method")
par(mfrow= c(5,2))
res <- residuals(aarima_fit)
Acf(res, main ="raw ")
Pacf(res)
Box.test(predict(aarima_fitd, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
res <- residuals(aarima_fit_nostep)
Acf(res,main = "no step")
Pacf(res)
Box.test(predict(aarima_fitd, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
res <- residuals(aarima_fitd)
Acf(res, main = "d")
Pacf(res)
Box.test(predict(aarima_fitd, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
res <- residuals(aarima_fitdd)
Acf(res, main = "dd")
Pacf(res)
Box.test(predict(aarima_fitd, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
# we know d = 1, D =1, and period =12, try all p&q = 0,1,2,3 plots 
# loop through all possibilities
psnqs <- expand.grid(0:3,0:3,0:3,0:3)
count <- 0
good_arma_index <- psnqs*0
good_arma_index[good_arma_index==0] <- NA
for(i in 1:256){ #  find best box-ljung scores 
  arma_itr <- arima(train, order = c(psnqs[i,1],1,psnqs[i,2]),seasonal = c(psnqs[i,3],1,psnqs[i,4]))
  res <- residuals(arma_itr)
  Box.test(predict(arma_itr, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
  if(Box.test(predict(arma_itr, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))$p.value > 0.10)
  {count <- count +1
  good_arma_index[count,] <- psnqs[i,]
  cat(i)
  cat("\n") 
  cat(Box.test(predict(arma_itr, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))$p.value) 
  cat("\n")}
}

res <- residuals(arima(train, order = c(good_arma_index[1,1],1,good_arma_index[1,2]),
                       seasonal = c(good_arma_index[1,3],1,good_arma_index[1,4])))
Acf(res, main = "loop")
Pacf(res)
Box.test(predict(arima(train, order = c(good_arma_index[1,1],1,good_arma_index[1,2]),
                       seasonal = c(good_arma_index[1,3],1,good_arma_index[1,4])),
                 n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))

# RESIDUALacf PLOTS FOR THE REPORT
par(mfrow= c(2,2))
res <- residuals(aarima_fit_nostep)
Acf(res,main = "arima(1,0,1)(0,1,2)[12]")
Box.test(predict(aarima_fitd, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
res <- residuals(aarima_fitd)
Acf(res, main = "arima(1,1,1)(2,1,1)[12]")
Box.test(predict(aarima_fitd, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
res <- residuals(aarima_fitdd)
Acf(res, main = "arima(1,1,1)(0,1,2)[12]")
Box.test(predict(aarima_fitd, n.ahead = length(test))$pred, lag = 1, type = c("Ljung-Box"))
res <- residuals(arima(train, order = c(good_arma_index[1,1],1,good_arma_index[1,2]),
                       seasonal = c(good_arma_index[1,3],1,good_arma_index[1,4])))
Acf(res, main = "arima(1,1,3)(2,1,0)[12]")
# check residueals 
checkresiduals(aarima_fitd)

#iii simple ts models 
#seasonal naive method 
snaive_fit <- snaive(train, h = 60)
plot.new
autoplot(train, series = "training data") +
  autolayer(snaive_fit, series= "snaive") + # PI=F removes the range on the predicted series 
  autolayer(test, series = "test series data")
accuracy(forecast(snaive_fit))

# choose optimal model 
#RMSE of each model 
RMSE_ets <- rmse(test, forecast(exp_fit, h =60))

f <- predict(aarima_fitd, n.ahead = length(test))
Performance = function(pred, val) { # function to find the performance of a prediction (pred) from a model over a test period (val)
  res = pred - val
  MAE = sum(abs(res))/length(val)
  RSS = sum(res^2)
  MSE = RSS/length(val)
  RMSE = sqrt(MSE)
  perf = data.frame(MAE, RSS, MSE, RMSE)
}
# find the predicted values for each model
pred_ets <- ts(forecast(exp_fit, h = 60)$mean, start = c(2017,1), frequency = 12, end= c(2021, 12))
pred_arima <- predict(aarima_fitd, n.ahead = length(test))$pred
pred_sn <- ts(forecast(snaive_fit, h = 60)$mean, start = c(2017,1), frequency = 12, end= c(2021, 12))
ets_perf <- cbind(Type = c("ETS(M,Ad,M) "), Performance(as.vector(pred_ets), test))
arima_perf <- cbind(Type = c("arima(1,1,1)(0,1,2)[12] "), Performance(as.vector(pred_arima), test))
snaive_perf <- cbind(Type = c("Seasonal Naive"), Performance(as.vector(pred_sn), test))
perf = rbind(ets_perf, arima_perf, snaive_perf)
#plot all models together over test period  
ts.plot(test, pred_ets, pred_arima, pred_sn,  col = c("black", "deeppink", "dodgerblue4", "forestgreen"), lty = c(1,2,2,2),
        lwd = 3)
legend("topleft", c("test","ETS", "ARIMA", "Seasonal Naive"), lty = c(1,2,2,2), 
           col = c("black", "deeppink", "dodgerblue4", "forestgreen"))


