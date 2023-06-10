# RData and rda files are in the same directory as this script
library(stats4)

# Load data
load('Data/WAMfair/LawschoolBrief.RData', verbose=T)
norm_data <- lawschoolbrief$LSAT
norm_size <- length(norm_data)

# Plot the histogram + density
png(filename='images/Lawschool_LSAT_Density.png', bg='white')
hist(norm_data, probability=TRUE,main='Density Estimates of Law School LSAT', xlab='LSAT', ylim=c(0, 0.050))
lines(density(norm_data), lwd=1, col='blue')
dev.off()

# Play with Bin width
png(filename='images/Lawschool_LSAT_Density_bw.png', bg='white')
hist(norm_data, probability=TRUE,main='Density Estimates of Law School LSAT with Bandwidth of 2', xlab='LSAT', ylim=c(0, 0.050))
lines(density(norm_data, bw=2), lwd=1, col='blue')
dev.off()

# Analysis

# MM
MM_norm_mu <- mean(norm_data)
MM_norm_sigma <- sqrt(mean((norm_data-MM_norm_mu)^2))
cat("MM Normal Mu:", MM_norm_mu, '\n')
cat("MM Normal Sigma:", MM_norm_sigma, '\n')
print(sd(norm_data))

png(filename='images/Lawschool_LSAT_Density_mm.png', bg='white')
curve(dnorm(x,MM_norm_mu,MM_norm_sigma), from=0, to=300, col='red', main='Density Estimates of Law School LSAT with MM', xlab='LSAT', ylab='Density', ylim=c(0, 0.050))
lines(density(norm_data), lwd=1, col='blue')
dev.off()

png(filename='images/Lawschool_LSAT_Density_mmbw.png', bg='white')
curve(dnorm(x,MM_norm_mu,MM_norm_sigma), from=0, to=300, col='red', main='Density Estimates of Law School LSAT with MM and BW of 2', xlab='LSAT', ylab='Density', ylim=c(0, 0.050))
lines(density(norm_data, bw=2), lwd=1, col='blue')
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

png(filename='images/Lawschool_LSAT_Density_mle.png', bg='white')
curve(dnorm(x,MLE_norm_mu,MLE_norm_sigma), from=0, to=300, col='red', main='Density Estimates of MLB Weight with MLE', xlab='LSAT', ylab='Density', ylim=c(0, 0.050))
lines(density(norm_data), lwd=1, col='blue')
dev.off()

png(filename='images/Lawschool_LSAT_Density_mlebw.png', bg='white')
curve(dnorm(x,MLE_norm_mu,MLE_norm_sigma), from=0, to=300, col='red', main='Density Estimates of MLB Weight with MLE and BW of 2', xlab='LSAT', ylab='Density', ylim=c(0, 0.050))
lines(density(norm_data, bw = 2), lwd=1, col='blue')
dev.off()

