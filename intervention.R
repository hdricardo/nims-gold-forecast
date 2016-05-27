source("load.R")
library(foreign)
library(TSA)

################################################################################
# INTERVENTION ANALYSIS
################################################################################

# http://www.marketwatch.com/story/the-biggest-potential-driver-for-gold-prices-is-a-double-edged-sword-2016-01-28

gint <- read.csv("project_gold_lbma_monthly_intervention.csv", header=T, sep=",")
gi.ts <- ts(gint[,2], freq=12, start=c(2004,1))
plot(gi.ts)

# setting an example ARIMA model
x <- diff(log(gint[,2]))
f <- Arima(x, c(1,0,0))
r <- residuals(f)
Acf(r)
Pacf(r)
Box.test(r,20,"Ljung-Box")

# estimate intervention analysis
x <- diff(log(gint[,2]))
i <- gint[-1, 3]  #remove first row 
int.1 <- arimax(x, order=c(1,0,0), xtransf=i, transfer=list(c(1,0)))
int.1

#graph the intervention
g.ts <- gint[-1,2]
i.ts <- i
t.ts <- 1:length(i)
y.ts <- 0.0078 + 0.0224*i.ts + 0.0224*(-1.0197^(t.ts-139))*as.numeric(t>139)
plot(y=g.ts, x=t.ts, type='l')
lines(y=y.ts, x=t.ts, lty=2)
