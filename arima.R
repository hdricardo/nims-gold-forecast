source("load.R")
data <- gold.trans
par(mfrow=c(1,1))

# OBJECTIVE: find best ARIMA and SEASONAL ARIMA models

################################################################################
# ARIMA
################################################################################

# look into ACF/PACF
par(mfrow=c(1,2))
acf(data)
pacf(data)
acf(data, lag.max=200)
pacf(data, lag.max=200)

# run autoarima
auto.arima(data)
# output: arima(0,0,1)

# test some ad-hoc models
arima(data,c(0,0,1))
arima(data,c(1,0,0))
arima(data,c(1,0,1))
arima(data,c(0,0,6))
arima(data,c(1,0,6))
arima(data,c(0,0,5))
# none of them has significance

# best fit (residuals analysis?)
par(mfrow=c(1,2))
fit <- arima(data, order=c(0,0,1))
fit
#tsdiag(fit)
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(residuals(fit), lag.max=200)

# parameters are insignificant. Why?
# http://stats.stackexchange.com/questions/176704/arima2-1-3-insignificant-coefficients


################################################################################
# SEASONAL ARIMA
################################################################################

# diff for data
ndiffs(data)  #output=0

# diff for seasonality
ndiffs(data)
## output = 0 ???? meaning?

## TODO: test seasonal arima models

