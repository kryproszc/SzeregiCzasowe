###
###
if(!("rio" %in% installed.packages()[,1])){install.packages("rio")}
rio::install_formats()
if(!("ggplot2" %in% installed.packages()[,1])){install.packages("ggplot2")}
if(!("aTSA" %in% installed.packages()[,1])){install.packages("aTSA")}
if(!("urca" %in% installed.packages()[,1])){install.packages("urca")}
if(!("xts" %in% installed.packages()[,1])){install.packages("xts")}
if(!("forecast" %in% installed.packages()[,1])){install.packages("forecast")}
###

library(rio)
library(ggplot2)
library(aTSA)
library(urca)
library(lmtest)
library(xts)
library(forecast)
###
data_sn_norm <- import("SN.xlsx")
colnames(data_sn_norm)[2] <- "chfpln"
data_sn_diff <- data.frame(date = data_sn_norm$date[-1],
                           chfpln_diff = diff(data_sn_norm$chfpln, lag = 1, differences = 1)) 
data_sn_log <- data.frame(date = data_sn_norm$date,
                          chfpln_log = log(data_sn_norm$chfpln))

###
##
ggplot(data_sn_norm, aes(x=date)) + 
  geom_line(aes(y=chfpln)) + 
  labs(title="", 
       subtitle="", 
       caption="", 
       y="chfpln") +  
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  
        panel.grid.minor = element_blank())  
##
ggplot(data_sn_diff, aes(x=date)) + 
  geom_line(aes(y=chfpln_diff)) + 
  labs(title="", 
       subtitle="", 
       caption="", 
       y="chfpln diff") +  
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  
        panel.grid.minor = element_blank()) 
##
ggplot(data_sn_log, aes(x=date)) + 
  geom_line(aes(y=chfpln_log)) + 
  labs(title="", 
       subtitle="", 
       caption="", 
       y="chfpln log") +  
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  
        panel.grid.minor = element_blank()) 

### Bialy Szum
## Test Ljung-Box
Box.test(data_sn_norm$chfpln, lag=12, type="Ljung-Box")
Box.test(data_sn_diff$chfpln_diff, lag=12, type="Ljung-Box")
Box.test(data_sn_log$chfpln_log, lag=12, type="Ljung-Box")
## Test Box-Pierce
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem
Box.test(data_sn_norm$chfpln, lag=12, type="Box-Pierce")
Box.test(data_sn_diff$chfpln_diff, lag=12, type="Box-Pierce")
Box.test(data_sn_log$chfpln_log, lag=12, type="Box-Pierce")

### Testy Stacjonarnosc 
## ADF
adf.test(data_sn_norm$chfpln, nlag = NULL, output = TRUE)
adf.test(data_sn_diff$chfpln_diff, nlag = NULL, output = TRUE)
adf.test(data_sn_log$chfpln_log, nlag = NULL, output = TRUE)
## Kwiatkowski-Phillips-Schmidt-Shin Test
kpss.test(data_sn_norm$chfpln, lag.short = TRUE, output = TRUE)
kpss.test(data_sn_diff$chfpln_diff, lag.short = TRUE, output = TRUE)
kpss.test(data_sn_log$chfpln_log, lag.short = TRUE, output = TRUE)
## Test Phillips-Perron Unit Root  
pp.test(data_sn_norm$chfpln, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)
pp.test(data_sn_diff$chfpln_diff, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)
pp.test(data_sn_log$chfpln_log, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)
## Test Augmented Dickey-Fuller
test.df.chfpln <- ur.df(data_sn_norm$chfpln, type = c("none"), lags = 0)
summary(test.df.chfpln)
test.df.chfpln_diff <- ur.df(data_sn_diff$chfpln_diff, type = c("none"), lags = 0)
summary(test.df.chfpln_diff)
test.df.chfpln_log <- ur.df(data_sn_log$chfpln_log, type = c("none"), lags = 0)
summary(test.df.chfpln_log)
## Test Breusch-Godfrey
bgtest(test.df.chfpln@testreg$residuals ~ 1, order = 1)
bgtest(test.df.chfpln_diff@testreg$residuals ~ 1, order = 1)
bgtest(test.df.chfpln_log@testreg$residuals ~ 1, order = 1)

### Split Train & Test
## Train
chfpln_log.ts.train <- window(data_sn_log$chfpln_log, end = 96)
## Test
chfpln_log.ts.test <- window(data_sn_log$chfpln_log, start = 97)

### Holt-Winters Filtering [SES]
##
chfpln_log.SES <- HoltWinters(chfpln_log.ts.train,
                              alpha = 0.8,
                              beta  = FALSE, # beta jest czynnikiem trendu
                              gamma = FALSE) # gamma jest czynnikiem sezonowym
plot(chfpln_log.SES)
##
chfpln_log.SES.summary <- window(chfpln_log.SES$fitted[, 1], end = 96, extend = TRUE)
##
chfpln_log.SES.forecast <- predict(chfpln_log.SES,
                                   n.ahead = 12,
                                   prediction.interval = TRUE)
##
plot(chfpln_log.SES)
lines(chfpln_log.SES.forecast[, 1], col = "blue") # prognozy 
lines(chfpln_log.SES.forecast[, 2], col = "red", lty = 2) # dolna granica przedzia?u ufno?ci dla prognozy
lines(chfpln_log.SES.forecast[, 3], col = "red", lty = 2) # g?rna granica przedzia?u ufno?ci dla prognozy
abline(v = 96, lty = 12)

### Holt-Winters Filtering [Holt]
##
chfpln_log.Holt <- HoltWinters(chfpln_log.ts.train,
                               alpha = 0.8,
                               beta  = 0.1,
                               gamma = FALSE)
plot(chfpln_log.Holt)
##
chfpln_log.Holt.summary <- window(chfpln_log.Holt$fitted[, 1], end = 96, extend = TRUE)
##
bsales.Holt.forecast <- predict(chfpln_log.Holt,
                                n.ahead = 12,
                                prediction.interval = TRUE)
plot(bsales.Holt.forecast)

### LOG_CHFPLN
chfpln_log.ts.summary <- ts(data_sn_log$chfpln_log)
chfpln_log.ts <- ts(data_sn_log$chfpln_log)

### Error Forecast Statistics
##
chfpln_log.summary <- ts.union(chfpln_log.ts.summary,
                               chfpln_log.SES.summary,
                               chfpln_log.Holt.summary)
chfpln_log.summary = as.xts(chfpln_log.summary)
ifelse(index(chfpln_log.summary) < "0096-01-01", 0, 1)
sample_period <- ts(ifelse(index(chfpln_log.summary) < "0096-01-01", 0, 1), start  =1, freq = 1)
names(chfpln_log.summary) <- c("CHFPLN","SES","Holt")
chfpln_log.summary$date <- index(chfpln_log.ts)
chfpln_log.summary$sample_period <- sample_period
##
chfpln_log.summary$mae_SES     <- abs(chfpln_log.summary$SES-chfpln_log.summary$CHFPLN)
chfpln_log.summary$mse_SES     <- (chfpln_log.summary$SES-chfpln_log.summary$CHFPLN)^2
chfpln_log.summary$mape_SES    <- abs((chfpln_log.summary$SES-chfpln_log.summary$CHFPLN)/chfpln_log.summary$CHFPLN)
chfpln_log.summary$amape_SES   <- abs((chfpln_log.summary$SES-chfpln_log.summary$CHFPLN)/(chfpln_log.summary$SES+chfpln_log.summary$CHFPLN))
##
chfpln_log.summary$mae_Holt     <- abs(chfpln_log.summary$Holt-chfpln_log.summary$CHFPLN)
chfpln_log.summary$mse_Holt     <- (chfpln_log.summary$Holt-chfpln_log.summary$CHFPLN)^2
chfpln_log.summary$mape_Holt    <- abs((chfpln_log.summary$Holt-chfpln_log.summary$CHFPLN)/chfpln_log.summary$CHFPLN)
chfpln_log.summary$amape_Holt   <- abs((chfpln_log.summary$Holt-chfpln_log.summary$CHFPLN)/(chfpln_log.summary$Holt+chfpln_log.summary$CHFPLN))
##
aggregate(chfpln_log.summary[,6:13],
          by = list(chfpln_log.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))

### ACF i PACF
## Plot ACF i PACF
par(mfrow = c(1, 2))
acf(data_sn_log$chfpln_log, lag.max = 12,
    ylim = c(-1, 1),
    xlim = c(1,12),
    lwd = 4, col = "red")
pacf(data_sn_log$chfpln_log, lag.max = 12,
     title = "xXx",
     ylim = c(-1, 1),
     xlim = c(1,12),     
     lwd = 4, col = "red")
## Value
acf(data_sn_log$chfpln_log, lag.max = 12, plot=FALSE)
pacf(data_sn_log$chfpln_log, lag.max = 12, plot=FALSE)
## 
tsdisplay(data_sn_log$chfpln_log, lag.max=24)

### ARIMA
## Estimation Model ARIMA [2,0,4]
nobs <- length(chfpln_log.ts.train)
arima.chfpln_log204 <- arima(chfpln_log.ts.train,  
                             order = c(2, 0, 4),
                             xreg = 1:nobs)
arima.chfpln_log204
coeftest(arima.chfpln_log204)
AIC(arima.chfpln_log204)
BIC(arima.chfpln_log204)
## Estimation Model ARIMA [2,0,3]
nobs <- length(chfpln_log.ts.train)
arima.chfpln_log203 <- arima(chfpln_log.ts.train,  
                             order = c(2, 0, 3),
                             xreg = 1:nobs)
arima.chfpln_log203
coeftest(arima.chfpln_log203)
AIC(arima.chfpln_log203)
BIC(arima.chfpln_log203)
## Test LR
teststat<- 2*(as.numeric(logLik(arima.chfpln_log204))-as.numeric(logLik(arima.chfpln_log203)))
teststat
pchisq(teststat, df=1, lower.tail = FALSE )

### Diagnostic ARIMA
## Czy reszty s? bia?ym szumem?
par(mfrow = c(1, 2))
Acf(resid(arima.chfpln_log203), lag.max = 12,
    ylim = c(-1, 1),
    xlim=c(1,12),
    lwd = 4, col = "red")
Pacf(resid(arima.chfpln_log203), lag.max = 12 ,
     lwd = 4, col = "red")
## Test Ljung-Boxa 
Box.test(resid(arima.chfpln_log204), type = "Ljung-Box", lag = 12)
Box.test(resid(arima.chfpln_log203), type = "Box-Pierce", lag = 12)

### Forecast ARIMA
forecast.chfpln_log <- predict(arima.chfpln_log204, 
                               n.ahead = 12,
                               newxreg = (nobs + 1) : (nobs + 12))
forecast.chfpln_log
## Visualisation 
plot(ts(chfpln_log.ts.train), main = "12-miesieczna prognoza dla chfpln_log")
abline(v = 96, lty = 2, col = "gray")
lines(forecast.chfpln_log$pred, col = "red", lwd = 2)
lines(forecast.chfpln_log$pred + 2 * forecast.chfpln_log$se, col = "red", lty = 3)
lines(forecast.chfpln_log$pred - 2 * forecast.chfpln_log$se, col = "red", lty = 3)
## Visualisation
plot(ts(chfpln_log.ts.train), main = "12-miesieczna prognoza dla chfpln_log",
     xlim = c(96, 108), ylim = c(1.30, 1.60))
abline(v = 96, lty = 2, col = "gray")
lines(forecast.chfpln_log$pred, col = "red", lwd = 2)
lines(forecast.chfpln_log$pred + 2 * forecast.chfpln_log$se, col = "red", lty = 3)
lines(forecast.chfpln_log$pred - 2 * forecast.chfpln_log$se, col = "red", lty = 3)
## ??czymy prognozy z oryginalnym szeregiem
chfpln_log.forecast <- data.frame(forecast = forecast.chfpln_log$pred,
                                  window(chfpln_log.ts.summary, start = 97))
colnames(chfpln_log.forecast) <- c("forecast",
                                   "actual")
## Quality Forecast
chfpln_log.forecast$mae <- abs(as.numeric(chfpln_log.forecast$actual) - chfpln_log.forecast$forecast)
chfpln_log.forecast$mse <- (as.numeric(chfpln_log.forecast$actual) - chfpln_log.forecast$forecast) ^ 2
chfpln_log.forecast$mape <- abs((as.numeric(chfpln_log.forecast$actual) - chfpln_log.forecast$forecast) /
                                  as.numeric(chfpln_log.forecast$actual))
chfpln_log.forecast$amape <- abs((as.numeric(chfpln_log.forecast$actual) - chfpln_log.forecast$forecast) /
                                   (as.numeric(chfpln_log.forecast$actual) + chfpln_log.forecast$forecast))
##
colMeans(chfpln_log.forecast[,3:6])
##
options(scipen = 5)
round(colMeans(chfpln_log.forecast[, 3:6]), 3)