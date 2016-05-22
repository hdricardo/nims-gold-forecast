source("load.R")
par(mfrow=c(1,1))

# OBJECTIVE: DECOMPOSE THE TIME SERIES 

################################################################################
# ADDITIVE AND MULTIPLICATIVE DECOMPOSITION
################################################################################

# which one to choose?

## http://r-statistics.co/Time-Series-Analysis-With-R.html
dm <- decompose(gold.ts, type="mult")
plot(dm)
# additive 
da <- decompose(gold.ts, type="additive")
plot(da) # see plot below

## ARIMA OF RESIDUALS ????
auto.arima(dm$random)


################################################################################
# TIME SERIES DECOMPOSITION
################################################################################

## http://r-statistics.co/Time-Series-Analysis-With-R.html
par(mfrow=c(1,2))
gold.decomp <- stl(gold.ts,"periodic")  # decompose the TS
plot(gold.decomp)  # plot decomposition
gold.sa <- seasadj(gold.decomp)  # de-seasonalize
plot(gold.ts)  # plot original
plot(gold.sa)  # plot seasonal adjusted


################################################################################
# SEASONALITY PLOTS
################################################################################

par(mfrow=c(1,2))

#season plot
seasonplot(gold.sa, 12, col=rainbow(12), year.labels=TRUE, main="Gold") # seasonal frequency set as 12 for monthly data.
# the season plot is for gold.sa or gold.trans????

# Box plot of monthly data
boxplot(gold.trans ~ cycle(gold.trans))

## IS SEASONALITY SIGNIFICATIVE?
## http://www.r-bloggers.com/detecting-seasonality/
## http://robjhyndman.com/hyndsight/tscharacteristics/
## install.packages("fma")
library(fma)
fit1 <- ets(gold.ts)
fit2 <- ets(gold.ts,model="ANN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df)
# output: seasonal significant
# BUT author says it is better to check differences in AIC between seasonal and non-seasonal models


## IS SEASONALITY SIGNIFICATIVE?
## TODO: ANOVA


## EXTRAS
## https://journal.r-project.org/archive/2012-1/RJournal_2012-1_G~Barnett~et~al.pdf


################################################################################
# TREND
################################################################################

## detrended time series
## http://r-statistics.co/Time-Series-Analysis-With-R.html
## forecast::seasonaldummy, forecast::fourier ???
trModel <- lm(gold.ts ~ c(1:length(gold.ts)))
plot(resid(trModel), type="l")  # resid(trModel) contains the de-trended series.


## HP Filter
# https://ionides.github.io/531w16/midterm_project/project1/Stats_531_Midterm_Project.html



################################################################################
# CYCLES
################################################################################
