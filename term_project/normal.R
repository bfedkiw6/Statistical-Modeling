# RData and rda files are in the same directory as this script
library(stats4)

# Load data
load('Data/qeML/mlb1.RData', verbose=T)
norm_data <- mlb1$Weight
norm_size <- length(norm_data)

# Plot the histogram + density
png(filename='images/MLB_Weight_Density.png', bg='white')
hist(norm_data, probability=TRUE,main='Density Estimates of MLB Weight', xlab='Credit Score', ylim=c(0, 0.020))
lines(density(norm_data), lwd=1, col='blue')
dev.off()

# Play with Bin width
png(filename='images/MLB_Weight_Density_bw.png', bg='white')
hist(norm_data, probability=TRUE,main='Density Estimates of MLB Weight with Bin Width of 6', xlab='Credit Score', ylim=c(0, 0.020))
lines(density(norm_data, bw=6), lwd=1, col='blue')
dev.off()


# Analysis

# MM
MM_norm_mu <- mean(norm_data)
MM_norm_sigma <- sqrt(mean((norm_data-MM_norm_mu)^2))
cat("MM Normal Mu:", MM_norm_mu, '\n')
cat("MM Normal Sigma:", MM_norm_sigma, '\n')

png(filename='images/MLB_Weight_Density_mm.png', bg='white')
curve(dnorm(x,MM_norm_mu,MM_norm_sigma), from=0, to=300, col='red', main='Density Estimates of MLB Weight with MM', xlab='Credit Score', ylab='Density', ylim=c(0, 0.020))
lines(density(norm_data), lwd=1, col='blue')
dev.off()

png(filename='images/MLB_Weight_Density_mm_bw.png', bg='white')
curve(dnorm(x,MM_norm_mu,MM_norm_sigma), from=0, to=300, col='red', main='Density Estimates of MLB Weight with MM and BW of 6', xlab='Credit Score', ylab='Density', ylim=c(0, 0.020))
lines(density(norm_data, bw=6), lwd=1, col='blue')
dev.off()


# MLE
llnorm <- function(mu,sigma) {
  loglik <- -(norm_size/2)*(log(2*pi)) - (norm_size * log(sigma)) - (1/(2*sigma^2)) * sum((norm_data-mu)^2)
  return (-loglik)  
}

znorm <- mle(minuslogl=llnorm, start=list(mu=1,sigma=1))
summary(znorm)
MLE_norm_mu <- coef(summary(znorm))[1]
MLE_norm_sigma <- coef(summary(znorm))[2]
cat("MLE Normal Mu:", MLE_norm_mu, '\n')
cat("MLE Normal Sigma:", MLE_norm_sigma, '\n')
png(filename='images/MLB_Weight_Density_mle.png', bg='white')
curve(dnorm(x,MLE_norm_mu,MLE_norm_sigma), from=0, to=300, col='red', main='Density Estimates of MLB Weight with MLE', xlab='Credit Score', ylab='Density', ylim=c(0, 0.020))
lines(density(norm_data), lwd=1, col='blue')
dev.off()

