load('/Users/priyal/Desktop/ecs132/ECS132-HW/term_project/Data/fairml/communities.and.crime.rda',verbose=T)
head(communities.and.crime)
a <- mean(communities.and.crime$PctWorkMomYoungKids)
b <- var(communities.and.crime$PctWorkMomYoungKids)
beta <- (3.91366/0.50121381)-3.91366 #beta = 3.894704
x <- communities.and.crime$PctWorkMomYoungKids
plot(density(x),col="red",xlab=NA,ylab=NA,main=NA)
curve(dbeta(x,3.91366,beta),col="blue",add=TRUE)
title(main="Percent of Working Moms with Young Kids vs Density",xlab="Percent of Working Moms with Young Kids",ylab="Density")


#logic
#a <- mean(data)
#b <- var(data)
#print(c(a,b))
#mean(data) = a / (a + b)
#b = (a - mean(data)a)/mean(data)
#b = a/mean(data) - a
#var(data) = ab /(a+b)^2*(a+b+1)
#var(data) = a(a/mean(data)-a)/(a+a/mean(data) - a)^2 * (a+a/mean(data)-a+1)