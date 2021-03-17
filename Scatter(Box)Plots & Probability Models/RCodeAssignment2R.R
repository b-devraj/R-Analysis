###################################################################################
# Run this code only once 
skewness<-function(x) {(sum((x-mean(x))^3)/length(x))/(sum((x-mean(x))^2)/length(x))^(3/2)}
kurtosis<- function(x) {(sum((x-mean(x))^4)/length(x))/(sum((x-mean(x))^2)/length(x))^2}
library(MASS)     # truehist is in the library MASS
###################################################################################

#################################################################################
# Problem 1: R code for Exponential data
id<-20842360
set.seed(id)
mu<-max(1,id-10*trunc(id/10))           # mu = last digit of ID unless it is zero
cat("mu = ", mu,"\n")                   # display value of mu
# 200 observations from Exponential(1/mu)
ye<-sort(round(rexp(200,1/mu),digits=2))   
ye[1:5]                              # display first 5 numbers in the data set
# display numerical summaries
summary(ye)
cat("sample standard deviation = ", sd(ye),"\n")  # sample standard deviation
cat("sample skewness = ", skewness(ye),"\n")      # sample skewness
cat("sample kurtosis = ", kurtosis(ye),"\n")      # sample kurtosis
# plot relative frequency histogram and superimpose Exponential pdf
truehist(ye,ymax=1/mean(ye),ylab="density",main="Relative Frequency Histogram of Exponential Data")
curve(dexp(x,1/mean(ye)),from=0.001,to=max(ye),col="red",add=TRUE,lwd=2)
# plot Empirical and Exponential cdf's
plot(ecdf(ye),verticals=T,do.points=F,xlab="ye",ylab="ecdf",main="",lwd=2)
title(main="Empirical cdf and Exponential cdf")
curve(pexp(x,1/mean(ye)),col="red",add=TRUE,lwd=1.5)
###############################################################################


#################################################################################
# Problem 2: R code for Gamma data
id<-20842360
set.seed(id)
# Use value of mu from Problem 1 to generate 200 observations from Gamma(3,1/mu)
yg<-sort(round(rgamma(200,3,1/mu),digits=2))   
yg[1:5]                              # display first 5 numbers in the data set
# display numerical summaries
summary(yg)
cat("sample standard deviation = ", sd(yg),"\n")  # sample standard deviation
cat("sample skewness = ", skewness(yg),"\n")      # sample skewness
cat("sample kurtosis = ", kurtosis(yg),"\n")      # sample kurtosis
# plot relative frequency histogram and superimpose Gaussian pdf
truehist(yg,ymax=1/mean(yg),ylab="density",main="Relative Frequency Histogram of Gamma Data")
curve(dnorm(x,mean(yg),sd(yg)),col="red",add=TRUE,lwd=2)
# plot Empirical cdf and superimpose Gaussian cdf
plot(ecdf(yg),verticals=T,do.points=F,xlab="y",ylab="ecdf",main="",lwd=2)
title(main="Empirical and Gaussian CDFs")
curve(pnorm(x,mean(yg),sd(yg)),add=TRUE,col="red",lwd=1.5) # superimpose Gaussian cdf
#Plot side by side boxplots for Gamma data (yg) and Exponential data (ye) from Problem 1 
boxplot(yg,ye,col="cyan",names=c("Gamma Data","Exponential Data"))
###############################################################################


#################################################################################
# Problem 3: R code for bivariate data
id<-20842360
set.seed(id)
x<-round(runif(100,0,5),digits=1)
alpha<-runif(1,-2,2)
beta<-runif(1,-5,5)    
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta,"\n")       
y<-round(alpha+beta*x+rnorm(100,0,1),digits=1)
# display sample correlation
cat("sample correlation = ", cor(x,y),"\n")        
plot(x,y,col="blue",main="Scatterplot of Data")
# generate new x and y values
x2<-x-2.5
y2<-round(alpha+beta*x2^2+rnorm(100,0,1.5),digits=1)
# display sample correlation
cat("sample correlation = ", cor(x2,y2),"\n")        
plot(x2,y2,col="blue",main="Scatterplot of Data")

#################################################################################


###################################################################################
# Problem 4: Binomial relative likelihood function 
id<-20842360
set.seed(id)
# generate a random value of theta from a Uniform(0.1,0.3) distribution
theta<-runif(1,0.1,0.3) 
n<-25
y<-rbinom(1,n,theta)    # observation from Binomial(n,theta) distribution
while (y==0) {y<-rbinom(1,n,theta)} # avoid case y=0
thetahat<-y/n                 # maximum likelihood estimate of theta
# display values
cat("n = ", n, ", theta = ", theta, ", y = ", y, ", thetahat = ",thetahat, "\n")
#determine an interval of values for plotting relative likelihood function
s<-(thetahat*(1-thetahat)/n)^0.5
th<-seq(max(0,thetahat-4*s), min(1,thetahat+4*s),0.001)
# create function to calculate Binomial relative likelihood function
BinRLF <- function(x,y,n,thetahat) {exp(y*log(x/thetahat)+(n-y)*log((1-x)/(1-thetahat)))}
# plot relative likelihood function for n=25
plot(th,BinRLF(th,y,n,thetahat) , xlab=expression(theta),ylab=expression(paste("R(",theta,")")),type="l",lwd=2)    
# superimpose the relative likelihood function for the same thetahat but n=100
lines(th, BinRLF(th,y*4,100,thetahat), col="red",lwd=2)
title(main="Binomial Relative Likelihood Function")
# plot log relative likelihood for n=25 and n=100 on same graph
plot(th,log(BinRLF(th,y,n,thetahat)) , xlab=expression(theta),ylab=expression(paste("r(",theta,")")),type="l",lwd=2)    
lines(th, log(BinRLF(th,y*4,100,thetahat)), col="red",lwd=2)
title(main="Binomial Log Relative Likelihood Function")
###################################################################################



###################################################################################
# Problem 5: Poisson relative likelihood function 
id<-20842360
set.seed(id)
# generate a random value of theta from a Uniform(0.1,0.3) distribution
theta<-round(runif(1,5,10) ,digits=1)
n<-30
y<-rpois(n,theta)    # n observation from Poisson(theta) distribution
thetahat<-mean(y)                 # maximum likelihood estimate of theta
# display values
cat("n = ", n, ", theta = ", theta,  ", thetahat = ",thetahat, "\n")
#determine an interval of values for plotting relative likelihood function
s<-(thetahat/n)^0.5
th<-seq(max(0,thetahat-4*s), thetahat+4*s,0.001)
# create function to calculate Poisson relative likelihood function
PoisRLF <- function(x,n,thetahat) {exp(n*thetahat*log(x/thetahat)+n*(thetahat-x))}
# plot relative likelihood function for n=30
plot(th,PoisRLF(th,n,thetahat) , xlab=expression(theta),ylab=expression(paste("R(",theta,")")),type="l",lwd=2)    
# superimpose the relative likelihood function for the same thetahat but n=90
lines(th, PoisRLF(th,90,thetahat), col="red",lwd=2)
title(main="Poisson Relative Likelihood Function")
# plot log relative likelihood for n=30 and n=90 on same graph
plot(th,log(PoisRLF(th,n,thetahat)) , xlab=expression(theta),ylab=expression(paste("r(",theta,")")),type="l",lwd=2)    
lines(th, log(PoisRLF(th,90,thetahat)), col="red",lwd=2)
title(main="Poisson Log Relative Likelihood Function")
###################################################################################



###################################################################################
# Problem 6: Exponential relative likelihood function 
id<-20842360
set.seed(id)
theta<-runif(1,1,10)    #generate a random value of theta
n<-30
# generate random sample of n observation from Exponential(theta) distribution 
y<-rexp(n,1/theta)   
thetahat<-mean(y)    #maximum likelihood estimate of theta
cat("n = ",n," theta = ",theta,", thetahat = ",thetahat, "\n")   # display values
s<-thetahat/(n^0.5)
# interval of values for plotting relative likelihood function
th<-seq(max(0,thetahat-3*s), thetahat+4*s,0.001)
# create function to calculate Exponential relative likelihood function 
ExpRLF<-function(x,n,thetahat) {exp(n*log(thetahat/x)+n*(1-thetahat/x))}
# plot relative likelihood function for n=30
plot(th,ExpRLF(th,n,thetahat) , xlab=expression(theta),ylab=expression(paste("R(",theta,")")),type="l",lwd=2)    
# superimpose the relative likelihood function for the same thetahat but n=90
lines(th, ExpRLF(th,90,thetahat), col="red",lwd=2)
title(main="Exponential Relative Likelihood Function")
# plot log relative likelihood for n=30 and n=90 on same graph
plot(th,log(ExpRLF(th,n,thetahat)) , xlab=expression(theta),ylab=expression(paste("r(",theta,")")),type="l",lwd=2)    
lines(th, log(ExpRLF(th,90,thetahat)), col="red",lwd=2)
title(main="Exponential Log Relative Likelihood Function")
###################################################################################

