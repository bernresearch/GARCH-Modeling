library(httr)
library(highfrequency)
library(magrittr)
library(tidyverse)
library(xts)

## Turn VPN on
## Collect 5min candles of BTCUSDT

base_url <- "https://fapi.binance.com"
path1 <- "/fapi/v1/klines?symbol=BTCUSDT&interval=1m&startTime="
path2 <- "&limit=1500"
fxn <- get("GET")

## Total hours
total_hours = as.integer(as.Date("2023-03-15") - as.Date("2019-09-09")) * 24

## API requests needed
requests =  (total_hours * 60) / 1500

## Available data starts 2019-09-09
startTime = (as.numeric(as.POSIXct("2019-09-09 00:00:00 EST", origin = "1970-01-01")) * 1000)

## Create data frame of opening prices
prices = data.frame(matrix(ncol = 1))
colnames(prices) = "Open"

## Fill prices data frame
for(i in 1:requests){
  url <- paste0(base_url, path1, startTime, path2)
  call <- fxn(url)
  response <- content(call)
  prices %<>% add_row(Open = as.double(sapply(response, "[[", 2)))
  startTime = startTime + (1500*60000)
  
  print(i)
}

## Re-index
prices %<>% tail(-1)
rownames(prices) =  NULL

## Add times
startTime = (as.numeric(as.POSIXct("2019-09-09 00:00:00 EST", origin = "1970-01-01")) * 1000)
dates = as.POSIXct(seq(from = startTime, by = 60000, length.out = nrow(prices)) / 1000, origin = "1970-01-01")
blah = prices
prices$t = dates

## Save as csv
write.csv(prices, "C:\\Users\\closj\\Documents\\BTC_5m_Price_20190909-20230315.csv")

## Convert to xts
prices %<>% as.xts()

## Realized Variance and Realized Volatility
rv = rCov(rData = prices, alignBy = "minutes", alignPeriod = 1, makeReturns = TRUE)
rvol = sqrt(rv)

## Plot BTCUSDT Daily Realized volatility
plot(fortify(rvol), type = "l", main = "BTC Daily Realized Volatility")

## Preliminary analysis on variance and volatility
library(tseries)
acf(rvol)
adf.test(rvol)
kpss.test(rvol, null = "Level")

## Get BTC daily returns
r_url <- "https://fapi.binance.com/fapi/v1/klines?symbol=BTCUSDT&interval=1d&startTime=1568001600000&limit=1283"
r_call <- fxn(r_url)
r_response <- content(r_call)
daily_prices <- as.double(sapply(r_response, "[[", 5))
ret <- makeReturns(daily_prices)

## Preliminary analysis on daily returns
library(moments)
acf(ret)
Box.test(ret, lag = 1, type = "Ljung-Box")
shapiro.test(ret)
plot(density(ret))
kurtosis(ret)

## Model returns by t-distribution
library(MASS)
t.pars <- fitdistr(ret, densfun = "t", start = list(m = 0, s = 0.01, df = 1))
plot(density(ret), xlim = c(-.2, .2), ylim = c(-1, 20), xlab = "", main = "Returns are leptokurtic")
par(new = TRUE)
curve(dt((x - t.pars$estimate[1])/t.pars$estimate[2], df = t.pars$estimate[3])/t.pars$estimate[2],
      from = -.2,
      to = .2, xlim = c(-.2,.2), 
      ylim = c (-1, 20),
      ylab = "Density", xlab = "Daily Return",
      col = "green")

## Benchmark
library(forecast)
library(rugarch)
auto.arima(ret)
sgarch <- ugarchspec(mean.model = list(armaOrder = c(0,1), include.mean = TRUE),
                     distribution.model = "std")
sgarch_fitted <- ugarchfit(sgarch, data =  ret)
whole_len <- length(ret)
burning_len <- 300
forecast_len <- whole_len - burning_len
returns <- data.frame(btcr = ret)
rownames(returns)<- rownames(ret)

# Rolling estimation and forecasting
sgarch_roll <- ugarchroll(spec = sgarch, data = returns, n.ahead = 1, forecast.length = forecast_len,
                          refit.every = 2)

## Plot the predicted vol and realized vol
library(ggplot2)
library(reshape2)
x <- tail(ymd(seq(as.Date("2019-09-09"), as.Date("2023-03-15"), by = "days")), forecast_len)
realized_vol <- sqrt(tail(rv, forecast_len))
sgarch.predicted_vol <- sgarch_roll@forecast$density[,'Sigma']
tmp_df <- data.frame(x, realized_vol, sgarch.predicted_vol)
sgarch.g <- ggplot(melt(tmp_df, id.var = "x"), aes(x = x, y = value)) +
  geom_line(aes(colour = variable, group = variable)) +
  scale_color_manual(values = c('grey', 'red') )+
  ylab('daily volatility') +
  xlab('date index') +
  theme(legend.title = element_blank()) +
  ggtitle('ARMA(0,1) - GARCH(1,1) vol prediction')
sgarch.g

## SGARCH Analysis
sgarch.MSE <- mean((realized_vol - sgarch.predicted_vol)^2)
summary((realized_vol - sgarch.predicted_vol)^2)
sgarch.MSE
cor(realized_vol, sgarch.predicted_vol)
summary(lm(realized_vol ~ sgarch.predicted_vol))

## Save SGARCH model
sgarch_model <- list()
sgarch_model$spec<- sgarch
sgarch_model$roll<- sgarch_roll
sgarch_model$plot<- sgarch.g
sgarch_model$MSE<- sgarch.MSE
sgarch_model$roll.pred<- tmp_df
save(sgarch_model, file = 'sgarch_model')

## EGARCH estimation
egarch <- ugarchspec(variance.model = list( model = "eGARCH"),
                     mean.model = list(armaOrder= c(1,2)),
                     distribution.model = "std")

egarch_fitted <- ugarchfit(egarch, data = returns)
egarch_roll <- ugarchroll(spec = egarch,
                          data = returns,
                          n.ahead = 1,
                          forecast.length = forecast_len,
                          refit.every = 2)

egarch.predicted_vol <- egarch_roll@forecast$density[,'Sigma']
temp_df<- data.frame(x, realized_vol, egarch.predicted_vol)
egarch.g <- ggplot(melt(temp_df, id.var = "x"), aes(x = x, y = value)) +
  geom_line(aes(colour = variable, group = variable))+
  scale_color_manual(values = c('grey', 'blue'))+
  ylab('daily volatility')+
  xlab('date index')+
  theme(legend.title= element_blank())+
  ggtitle('ARMA(0,2) - EGARCH(1,1) vol prediction')

egarch.g

## EGARCH Analysis
egarch.MSE<- mean((realized_vol - egarch.predicted_vol)^2)
summary((realized_vol- egarch.predicted_vol)^2)
egarch.MSE
cor(realized_vol, egarch.predicted_vol)
summary(lm(realized_vol ~ egarch.predicted_vol))

## Save EGARCH model
egarch_model <- list()
egarch_model$spec<- egarch
egarch_model$roll<- egarch_roll
egarch_model$plot<- egarch.g
egarch_model$MSE<- egarch.MSE
egarch_model$roll.pred<- temp_df
save(egarch_model, file = 'egarch_model')

## RealGARCH Estimation
library(rugarch)
rgarch.model <- ugarchspec(mean.model = list(armaOrder = c(0,1)),
                           variance.model = list(model = 'realGARCH',
                                                 garchOrder= c(2,1)))

setbounds(rgarch.model) <- list(alpha2 = c(-1,1))

ret_xts <- xts(as.vector(returns$btcr), order.by = seq(as.Date("2019-09-09"), as.Date("2023-03-14"), by = "day"))
rvol <- xts(rvol, order.by = seq(as.Date("2019-09-09"), as.Date("2023-03-14"), by = "day"))

btc.xts <- merge.xts(rvol, ret_xts)
colnames(btc.xts) <- c("rvol", "return")

rgarch.fit<- ugarchfit(spec = rgarch.model, 
                       data = btc.xts$return, 
                       solver= 'hybrid', 
                       realizedVol= btc.xts$rvol)

whole_len <- dim(btc.xts)[1]
burning <- 300
forecast_len <- whole_len - burning
rgarch.roll <- ugarchroll(spec = rgarch.model,
                          data = btc.xts$return,
                          n.ahead = 1, 
                          forecast.length = forecast_len, 
                          refit.every = 15,
                          solver = "hybrid",
                          realizedVol = btc.xts$rvol)

tmp_df <- data.frame(x = tail(ymd(index(btc.xts)) , forecast_len), 
                     realized_vol = tail(btc.xts$rvol, forecast_len),
                     rgarch.prediction_vol = rgarch.roll@forecast$density$RVolForecast)

rgarch.g <- ggplot(data = melt(tmp_df, id.var= 'x'), aes( x=x, y= value))+
  geom_line(aes(colour= variable, group= variable))+
  scale_colour_manual(values= c( 'grey', 'orange'))+
  ylab('daily volatility')+
  xlab( 'date')+
  theme( legend.title= element_blank())+
  ggtitle( 'realizedGARCH(ARMA(0,2)-rGARCH(2,1)) rolling prediction')

rgarch.g




