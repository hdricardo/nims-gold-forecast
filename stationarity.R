source("load.R")
par(mfrow=c(1,1))

# OBJECTIVE: STATIONARITY AND VISUAL ANALYSIS OF THE TIME SERIES

################################################################################
# EXPLORATORY PLOTS FOR STATIONARITY ETC
################################################################################

# gold prices and log(prices)
par(mfrow=c(1,2))
plot(gold.ts)
plot(log(gold.ts))

# first differences and first differences of log
par(mfrow=c(1,2))
plot(gold.dif)
plot(gold.dif.log)

# Box-Cox transform and differences of BCT
par(mfrow=c(1,2))
plot(gold.bct)
plot(gold.dif.bct)

# same as above, just comparing differences plots
par(mfrow=c(1,3))
plot(gold.dif)
plot(gold.dif.log)
plot(gold.dif.bct)


################################################################################
# STATISTICAL TESTS
################################################################################

# ADF tests
# The null-hypothesis for an ADF test is that the data are non-stationary
# So large p-values are indicative of non-stationarity, and small p-values suggest stationarity. 
# Using the usual 5% threshold, differencing is required if the p-value is greater than 0.05. 
adf.test(gold.ts)
adf.test(gold.dif)
adf.test(gold.trans)


################################################################################
# OUTLIERS
################################################################################

# checking for the presence of outliers
# http://www.inside-r.org/packages/cran/forecast/docs/tsoutliers
tsoutliers(gold.ts)
tsoutliers(gold.dif)
tsoutliers(gold.trans)
