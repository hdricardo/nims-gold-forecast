setwd("/home/dario/MEOCloud/Master/FORECASTING/Project")
library(forecast)
library(fpp)

################################################################################
# LOAD
################################################################################

# load data, jan 2004
gold <- read.csv("gold2004month.txt", header=F)

# Set as time series
gold.ts <- ts(gold[,1], freq=12)

# checking if the data is consistent 1
g <- read.csv("project_gold_lbma_monthly.txt", skip=4, header=F, sep=",")
g.ts <- ts(g[,2], freq=12, start=c(1968,4))
lbma <- window(g.ts, start=c(2004,1))
plot(lbma)

# checking if the data is consistent 2
gld <- read.csv("project_gold_gld_monthly.txt", header=T, sep=",")
gld.ts <- ts(gld[,2], freq=12, start=c(2004,11))
plot(gld.ts)

################################################################################
# TRANSFORM
################################################################################

# First differences
gold.dif <- diff(gold.ts)

# Log
gold.log <- log(gold.ts)

# First differences of log
gold.dif.log <- diff(gold.log)

# Box-Cox Transformation
lambda <- BoxCox.lambda(gold.ts)
gold.bct <- ((gold.ts^lambda)-1)/lambda
gold.dif.bct <- diff(gold.bct)

# set dataset for next studies
gold.trans <- gold.dif.bct


################################################################################
# LAMBDA TESTS
################################################################################

lambda <- -2
x <- ((gold.ts^lambda)-1)/lambda
plot(x)
Acf(x)
auto.arima(x)

fit <- Arima(x, order=c(2, 1, 0), seasonal=list(order=c(2, 1, 0), period=12))
res <- residuals(fit)
Acf(res, lag.max = 30)
Pacf(res)  #???

Box.test(res, lag=12, fitdf=0 )
Box.test(res, lag=12, fitdf=0, type="Lj")


