# RData and rda files are in the same directory as this script

# Exponential Family Plots
load('Data/fairml/national.longitudinal.survey.rda')
png(filename='images/jobsnum90_hist.png', bg='white')
hist(national.longitudinal.survey$jobsnum90, probability=TRUE)
dev.off()

png(filename='images/jobsnum90_densityplot.png')
plot(density(national.longitudinal.survey$jobsnum90))
dev.off()


# Gamma Family Plots
load('Data/fairml/compas.rda')
png(filename='images/priors_count_hist.png', bg='white')
hist(compas$priors_count, probability=TRUE)
dev.off()

png(filename='images/priors_count_densityplot.png')
plot(density(compas$priors_count))
dev.off()
