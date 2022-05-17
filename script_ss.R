###

###
if(!("rio" %in% installed.packages()[,1])){install.packages("rio")}
rio::install_formats()
if(!("lubridate" %in% installed.packages()[,1])){install.packages("lubridate")}
if(!("ggplot2" %in% installed.packages()[,1])){install.packages("ggplot2")}
if(!("aTSA" %in% installed.packages()[,1])){install.packages("aTSA")}
if(!("urca" %in% installed.packages()[,1])){install.packages("urca")}
if(!("xts" %in% installed.packages()[,1])){install.packages("xts")}
if(!("forecast" %in% installed.packages()[,1])){install.packages("forecast")}
###
library(rio)
library(lubridate)
library(ggplot2)
library(aTSA)
library(urca)
library(lmtest)
library(xts)
library(forecast)
library(zoo)
###
data_ss_norm <- import("SS.xlsx")
colnames(data_ss_norm)[2] <- "sales.iPhone"
data_ss_norm$date <- gsub("\\.","-",data_ss_norm$date)
data_ss_norm$date <- as.Date(as.yearqtr(data_ss_norm$date), frac = 1)
data_ss_log <- data.frame(date = data_ss_norm$date,
                          sales.iPhone_log = log(data_ss_norm$sales.iPhone))
###
##
ggplot(data_ss_norm, aes(x=date)) + 
  geom_line(aes(y=sales.iPhone)) + 
  labs(title="", 
       subtitle="", 
       caption="", 
       y="sales.iPhone") +  
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  
        panel.grid.minor = element_blank())  
##
ggplot(data_ss_log, aes(x=date)) + 
  geom_line(aes(y=sales.iPhone_log)) + 
  labs(title="", 
       subtitle="", 
       caption="", 
       y="sales.iPhone log") +  
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  
        panel.grid.minor = element_blank()) 

### Bialy Szum
## Test Ljung-Box
Box.test(data_ss_norm$sales.iPhone, lag=12, type="Ljung-Box")
Box.test(data_ss_log$sales.iPhone_log, lag=12, type="Ljung-Box")
## Test Box-Pierce
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem
Box.test(data_ss_norm$sales.iPhone, lag=12, type="Box-Pierce")
Box.test(data_ss_log$sales.iPhone_log, lag=12, type="Box-Pierce")

### Testy Stacjonarnosc 
## ADF
adf.test(data_ss_norm$sales.iPhone, nlag = NULL, output = TRUE)
adf.test(data_ss_log$sales.iPhone_log, nlag = NULL, output = TRUE)
## Kwiatkowski-Phillips-Schmidt-Shin Test
kpss.test(data_ss_norm$sales.iPhone, lag.short = TRUE, output = TRUE)
kpss.test(data_ss_log$sales.iPhone_log, lag.short = TRUE, output = TRUE)
## Test Phillips-Perron Unit Root  
pp.test(data_ss_norm$sales.iPhone, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)
pp.test(data_ss_log$sales.iPhone_log, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)
## Test Augmented Dickey-Fuller
test.df.sales.iPhone <- ur.df(data_ss_norm$sales.iPhone, type = c("none"), lags = 0)
summary(test.df.sales.iPhone)
test.df.sales.iPhone_log <- ur.df(data_ss_log$sales.iPhone_log, type = c("none"), lags = 0)
summary(test.df.sales.iPhone_log)
## Test Breusch-Godfrey
bgtest(test.df.sales.iPhone@testreg$residuals ~ 1, order = 1)
bgtest(test.df.sales.iPhone_log@testreg$residuals ~ 1, order = 1)

### Split Train & Test
## Train
sales.iPhone_log.ts.train <- window(data_ss_log$sales.iPhone_log, end = 50)
## Test
sales.iPhone_log.ts.test <- window(data_ss_log$sales.iPhone_log, start = 51)

### ACF i PACF
## Plot ACF i PACF
par(mfrow = c(1, 2))
acf(sales.iPhone_log.ts.train, lag.max = 4,
    ylim = c(-1, 1),
    xlim = c(1,12),
    lwd = 4, col = "red")
pacf(sales.iPhone_log.ts.train, lag.max = 4,
     title = "xXx",
     ylim = c(-1, 1),
     xlim = c(1,12),     
     lwd = 4, col = "red")
## Value
acf(sales.iPhone_log.ts.train, lag.max = 4, plot=FALSE)
pacf(sales.iPhone_log.ts.train, lag.max = 4, plot=FALSE)
## 
tsdisplay(data_ss_log$sales.iPhone_log, lag.max=4)

###
nobs <- length(sales.iPhone_log.ts.train)
## SARIMA(0,1,0)(1,1,1)
arima010111 <- arima(sales.iPhone_log.ts.train,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(1, 1, 1), period = 4),
                     xreg = 1:nobs)
arima010111
coeftest(arima010111)
##
par(mfrow = c(2, 1))
acf(resid(arima010111), lag.max = 4,
    ylim = c(-0.4, 0.4), lwd = 4, col = "red")
pacf(resid(arima010111), lag.max = 4,
     lwd = 4, col = "red")

## SARIMA(0,1,0)(0,1,1)
arima010011 <- arima(sales.iPhone_log.ts.train,
                     order = c(0, 1, 0),
                     seasonal = list(order = c(0, 1, 1), period = 12),
                     xreg = 1:nobs)
arima010011
coeftest(arima010011)
## 
par(mfrow = c(2, 1))
acf(resid(arima010011), lag.max = 4,
    ylim = c(-0.4, 0.4), lwd = 4, col = "red")
pacf(resid(arima010011), lag.max = 4,
     lwd = 4, col = "red")
## Test LR
teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010011)))
teststat
pchisq(teststat, df=1, lower.tail = FALSE )
## Wartoœci AIC
AIC(arima010111, arima010011)
## wartosci BIC
BIC(arima010111, arima010011) 
## Test Ljung-Boxa
Box.test(resid(arima010011), type = "Ljung-Box", lag = 4)

### V. Prognoza   
## SARIMA(0,1,1)(0,1,1)
arima011011 <- arima(sales.iPhone_log.ts.train,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 4))
forecast <- predict(arima011011, n.ahead = 4)
forecast
## Wykres prognoz
ts.plot(sales.iPhone_log.ts.train, 
        main = "4 months forecast of sales iPhone")
# pocztek okresu prognozy
abline(v = 51, lty = 2, col = "gray")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col = "red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col = "red", lty = 3)

# obejrzyjmy z bli¿eniu
ts.plot(sales.iPhone_log.ts.train,
        main = "4 months forecast of sales iPhone", 
        xlim = c(50, 54), 
        ylim=c(2.5,4.5))
abline(v = 51, lty = 2, col = "gray")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col ="red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col ="red", lty = 3)

## 
iPhone_forecast <- data.frame(forecast = forecast$pred, 
                              window(data_ss_log$sales.iPhone_log, start = 51))
colnames(iPhone_forecast) <- c("forecast",
                               "actual")
## sprawdzamy jakoœæ prognozy
iPhone_forecast$mae <- abs(iPhone_forecast$actual -
                          iPhone_forecast$forecast)
iPhone_forecast$mse <- (iPhone_forecast$actual -
                       iPhone_forecast$forecast)^2
iPhone_forecast$mape <- abs((iPhone_forecast$actual -
                            iPhone_forecast$forecast) /
                           iPhone_forecast$actual)
iPhone_forecast$amape <- abs((iPhone_forecast$actual -
                             iPhone_forecast$forecast) /
                            (iPhone_forecast$actual +
                               iPhone_forecast$forecast))
colMeans(iPhone_forecast[, 3:6])
##
options(scipen = 5)
round(colMeans(iPhone_forecast[, 3:6]), 3)