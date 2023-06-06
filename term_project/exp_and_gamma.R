# RData and rda files are in the same directory as this script
library(stats4)

# Load data
load('Data/fairml/adult.rda')
load('Data/qeML/prgeng.RData')
exp_data <- adult$capital_gain
exp_size <- length(exp_data)
gam_data <- prgeng$age
gam_size <- length(gam_data)

# Exponential Family Plots
png(filename='images/capital_gain_density_estimates.png', bg='white')
hist(exp_data, probability=TRUE, main='Density Estimates of Capital Gain', xlab='Capital Gain', ylim=c(0, 0.45))
lines(density(exp_data), lwd=1, col='blue')
dev.off()


# Gamma Family Plots
png(filename='images/prgeng_age_density_estimates.png', bg='white')
hist(gam_data, probability=TRUE, main='Density Estimates of Programmer and Engineer Age', xlab='Age')
lines(density(gam_data), lwd=1, col='blue')
dev.off()


# Analysis

# MM
MM_exp_lamb <- 1 / mean(exp_data)
MM_gam_lamb <- mean(gam_data) / var(gam_data)
MM_gam_r <- MM_gam_lamb * mean(gam_data)
cat("MM Exponential Lambda:", MM_exp_lamb, '\n')
cat("MM Gamma Lambda:", MM_gam_lamb, '\n')
cat("MM Gamma r:", MM_gam_r, '\n')

png(filename='images/capital_gain_mm.png', bg='white')
curve(dexp(x, rate=MM_exp_lamb), from=0, to=100, col='red', main='Density Estimates of Capital Gain with MM', xlab='Capital Gain', ylab='Density')
lines(density(exp_data), col='blue')
dev.off()

png(filename='images/prgeng_age_mm.png', bg='white')
curve(dgamma(x, shape=MM_gam_r, rate=MM_gam_lamb), from=0, to=100, col='red', main='Density Estimates of Programmer and Engineer Age with MM', xlab='Age', ylab='Density')
lines(density(gam_data), col='blue')
dev.off()

# MLE
llexp <- function(lamb) {
  loglik <- exp_size * log(lamb) - lamb * sum(exp_data)
  return (-loglik)
}

llgamma <- function(lamb, r) {
  loglik <- gam_size * r * log(lamb) + (r - 1) * sum(log(gam_data)) - gam_size * log(gamma(r)) - lamb * sum(gam_data)
  return (-loglik)
}

zexp <- mle(minuslogl=llexp, start=list(lamb=1))
MLE_exp_lamb <- coef(summary(zexp))[1]
cat("MLE Exponential Lambda:", MLE_exp_lamb, '\n')
png(filename='images/capital_gain_mle.png', bg='white')
curve(dexp(x, rate=MLE_exp_lamb), from=0, to=100, col='red', main='Density Estimates of Captial Gain with MLE', xlab='Capital Gain', ylab='Density')
lines(density(exp_data), col='blue')
dev.off()

zgam <- mle(minuslogl=llgamma, start=list(lamb=1, r=1))
MLE_gam_lamb <- coef(summary(zgam))[1]
MLE_gam_r <- coef(summary(zgam))[2]
cat("MLE Gamma Lambda:", MLE_gam_lamb, '\n')
cat("MLE Gamma r:", MLE_gam_r, '\n')
png(filename='images/prgeng_age_mle.png', bg='white')
curve(dgamma(x, shape=MLE_gam_r, rate=MLE_gam_lamb), from=0, to=100, col='red', main='Density Estimates of Programmer and Engineer Age with MLE', xlab='Age', ylab='Density')
lines(density(gam_data), col='blue')
dev.off()

#MLE vs density with a bandwidth of 4
png(filename='images/prgeng_age_bandwidth_4.png', bg='white')
curve(dgamma(x, shape=MLE_gam_r, rate=MLE_gam_lamb), from=0, to=100, col='red', main='Density Estimates with Bandwidth 4 and MLE', xlab='Age', ylab='Density')
lines(density(gam_data, bw=4), col='blue')
dev.off()
