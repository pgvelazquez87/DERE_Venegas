###########################
# Markov regime-switching #
# 09 de diciembre de 2018 #
###########################

library(readr)
library(ggplot2)
library(MSwM)
library(dplyr)
setwd('/Users/pablo/Documents/DERE/Analisis matematico')

amx <- read_csv(file = 'AMX.csv')
amx <- as.data.frame(amx[, c('Date', 'Close')])
amx <-transform(amx, diff = c(NA, diff(amx$Close)))
 

par(mar=c(3,3,3,3))

mod = lm(diff ~ 1, data=amx)
summary(mod)

mod.mswm <- msmFit(mod, k=2, p=1, sw=c(rep(TRUE, 3)))
summary(mod.mswm)

plot(mod.mswm)


