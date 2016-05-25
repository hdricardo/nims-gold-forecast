source("load.R")
data <- diff(log(gold.ts))
par(mfrow=c(1,1))

# OBJECTIVE: find best ARIMA and SEASONAL ARIMA models

################################################################################
# ARIMA
################################################################################

# look into ACF/PACF
par(mfrow=c(1,2))
Acf(data)
Pacf(data)
Acf(data, lag.max=200)
Pacf(data, lag.max=200)

# run autoarima
auto.arima(data)
# output: arima(0,0,1) with non-zero mean

# test some ad-hoc models
arima(data,c(0,0,1))
arima(data,c(1,0,0))
arima(data,c(1,0,1))
arima(data,c(0,0,6))
arima(data,c(1,0,6))
arima(data,c(0,0,5))
# none of them has significance

# best fit + residuals analysis
fit <- Arima(gold.trans, order=c(0,0,1), include.mean = T)
fit
qqnorm(residuals(fit))
qqline(residuals(fit))
Acf(residuals(fit), lag.max=30)
# less than 5% of obervations are within bounds?

# test if residuals are white noise: 
# p-value > 0.05 is desirable
res <- residuals(fit)
Box.test(res, lag=10, fitdf=0 )
Box.test(res, lag=10,fitdf=0, type="Lj")
Box.test(res, lag=21,fitdf=0, type="Lj") #oops


# parameters are insignificant. Why?
# http://stats.stackexchange.com/questions/176704/arima2-1-3-insignificant-coefficients
# http://stats.stackexchange.com/questions/109412/getting-residuals-to-be-white-noise

################################################################################
# SEASONAL ARIMA
################################################################################

# diff for data
ndiffs(data)  #output=0

# diff for seasonality
ndiffs(data)
## output = 0 ???? meaning?

## TODO: test seasonal arima models


################################################################################
# LOOPS
################################################################################

cat("\014") # clear console

x <- diff(sqrt(gold.ts))
  
par.p <- c(0,1,2)
par.q <- c(0,1,2)

par.P <- c(0,1,2)
par.D <- c(0,1)
par.Q <- c(0,1,2)

# non-seasonal ARIMA
p <- 0
q <- 0
d <- 0
for (p in par.p) {
  for (q in par.q) {
    fit <- Arima(x, order=c(p,0,q))
    summary(fit)
    cat('\n')
    cat('\n')
    cat('\n')
  }
}

# seasonal ARIMA
P <- 0
Q <- 0
D <- 0
for (p in par.p) {
  for (q in par.q) {
    for (Q in par.P) {
      for (P in par.P) {
        for (D in par.D) {
          fit <- Arima(x, order=c(p,d,q), seasonal=list(order=c(P, D, Q), period=12), method="CSS")
          summary(fit)
          cat('\n')
          cat('\n')
          cat('\n')
          res <- residuals(fit)
          s <- paste(p,d,q,";", P, D, Q)
          tsdisplay(res, main = s) 
        }
      }
    }
  }
}

