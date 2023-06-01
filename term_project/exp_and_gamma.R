# RData and rda files are in the same directory as this script
library(stats4)

# Load data
load('Data/fairml/national.longitudinal.survey.rda')
load('Data/qeML/nyctaxi.RData')
exp_data <- temp
exp_size <- length(exp_data)
gam_data <- nyctaxi$trip_distance
gam_size <- length(gam_data)

# Exponential Family Plots
png(filename='images/jobsnum90_density_estimates.png', bg='white')
hist(national.longitudinal.survey$jobsnum90, probability=TRUE)
lines(density(national.longitudinal.survey$jobsnum90), lwd=1, col='blue')
dev.off()


# Gamma Family Plots
png(filename='images/trip_distance_density_estimates.png', bg='white')
hist(gam_data, probability=TRUE, ylim=c(0, 0.5))
lines(density(gam_data), lwd=1, col='blue')
dev.off()


# Analysis

# MM
MM_exp_lamb <- 1 / mean(exp_data)
MM_gam_lamb <- mean(gam_data) / variance(gam_data)
MM_gam_r <- lamb * mean(gam_data)

# MLE
llexp <- function(lamb) {
  loglik <- exp_size * log(lamb) - lamb * sum(exp_data)
  return (-loglik)
}

llgamma <- function(lamb, r) {
  loglik <- gam_size * r * log(lamb) + (r - 1) * sum(log(gam_data)) - gam_size * log(gamma(r)) - lamb * sum(gam_data)
  return (-loglik)
}
