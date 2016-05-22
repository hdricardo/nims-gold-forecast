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

