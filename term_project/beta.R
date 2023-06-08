library(stats4)
#load data
weather <- read.csv("/Users/priyal/Desktop/ecs132/austinweather.csv")
weather_data <- weather$HumidityLowPercent
weather_data <- weather_data[which(weather_data != "-")]
beta_data <- as.numeric(weather_data)/100
beta_size <- length(weatherdata)

#print(c(max(betadata),min(betadata)))

# Beta Family Plots
png(filename='images/austinweather_densityestimates.png', bg='white')
png(filename='images/austinweatherdensity_bw0.065.png', bg='white')
hist(beta_data, probability=TRUE, main='Density Estimates of Low Humidity Percentage', xlab='Low Humidity Percentage', ylim=c(0, 2.5),xlim=c(0,1))
lines(density(beta_data,bw=0.065), lwd=1, col='blue')
dev.off()

#Analysis

#MM
m <- mean(beta_data)
v <- var(beta_data)
#solve for MM_beta_alpha
#m = MM_beta_alpha / (MM_beta_alpha + MM_beta_beta)
#MM_beta_beta = (MM_beta_alpha - m*MM_beta_alpha)/m
#MM_beta_beta = MM_beta_alpha/m - MM_beta_alpha
#v = MM_beta_alpha*MM_beta_beta /(MM_beta_alpha+MM_beta_beta)^2*(MM_beta_alpha+MM_beta_beta+1)
#v = MM_beta_alpha(MM_beta_alpha/m - MM_beta_alpha)/(MM_beta_alpha+MM_beta_alpha/m - MM_beta_alpha)^2 * (MM_beta_alpha + MM_beta_alpha/m - MM_beta_alpha + 1)
MM_beta_alpha <- 3.41157
MM_beta_beta <- (MM_beta_alpha/m)-MM_beta_alpha

png(filename='images/austinweather_mm.png', bg='white')
curve(dbeta(x,MM_beta_alpha,MM_beta_beta), col='red', main='Density Estimates of Low Humidity Percentage with MM', xlab='Low Humidity Percentage alpha=3.41157 and beta=4.176611', ylab='Density')
lines(density(beta_data,bw=0.065), col='blue')
dev.off()

#MLE
llbeta <- function(alpha,beta) {
  loglik <- (beta_size*log(gamma(alpha+beta))) - (beta_size*log(gamma(alpha))) - (beta_size*log(gamma(beta))) + ((alpha-1)*sum(log(beta_data))) + ((beta-1)*sum(log(1-beta_data)))
  return (-loglik)
}

zbeta <- mle(minuslogl=llbeta, start=list(alpha=1,beta=1))
summary(zbeta)
MLE_beta_alpha <- coef(summary(zbeta))[1]
MLE_beta_beta <- coef(summary(zbeta))[2]
cat("MLE Beta alpha:", MLE_beta_alpha, '\n')
cat("MLE Beta beta:", MLE_beta_beta, '\n')

png(filename='images/austinweather_mle.png', bg='white')
curve(dbeta(x,MLE_beta_alpha,MLE_beta_beta),col="red", main='Density Estimates of Low Humidity Percentage with MLE', xlab='Low Humidity Percentage alpha=3.447276 and beta=4.150187', ylab='Density')
lines(density(beta_data,bw=0.065), col='blue')
dev.off()