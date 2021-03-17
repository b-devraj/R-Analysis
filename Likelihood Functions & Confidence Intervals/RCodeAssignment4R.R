###################################################################################
# Problem 1: Binomial relative likelihood function 
id<-20842360
set.seed(id)
# generate a random value of theta from a Uniform(0.1,0.3) distribution
theta<-runif(1,0.1,0.3) 
n<-30
y<-rbinom(1,n,theta)    # observation from Binomial(n,theta) distribution
while (y==0) {y<-rbinom(1,n,theta)}    # avoid case y=0
thetahat<-y/n                 # maximum likelihood estimate of theta
# display values
cat("n = ", n, ", theta = ", theta, ", y = ", y, ", thetahat = ",thetahat, "\n")
#determine an interval of values for plotting relative likelihood function
s<-(thetahat*(1-thetahat)/n)^0.5
th<-seq(max(0,thetahat-4*s), min(1,thetahat+4*s),0.001)
# create function to calculate Binomial relative likelihood function
BinRLF <- function(x,y,n,thetahat) {exp(y*log(x/thetahat)+(n-y)*log((1-x)/(1-thetahat)))}
# plot relative likelihood function for n=30
plot(th,BinRLF(th,y,n,thetahat) , xlab=expression(theta),ylab=expression(paste("R(",theta,")")),type="l",lwd=2)
# draw a horizontal line at 0.15
abline(a=0.15,b=0,col="red",lwd=2)
title(main="Binomial Relative Likelihood Function") 
###################################################################################

###################################################################################
# R code for finding the endpoints of a 15% likelihood interval for Binomial example
uniroot(function(x) BinRLF(x,y,n,thetahat)-0.15,lower=0.0,upper=0.1)$root
uniroot(function(x) BinRLF(x,y,n,thetahat)-0.15,lower=0.3,upper=0.4)$root
###################################################################################

###################################################################################
# Problem 2: Exponential relative likelihood function 
id<-20842360
set.seed(id)
theta<-runif(1,1,10)    # generate a random value of theta
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
# draw a horizontal line at 0.15
abline(a=0.15,b=0,col="red",lwd=2)
title(main="Exponential Relative Likelihood Function") 
###################################################################################

###################################################################################
# R code for finding the endpoints of a 15% likelihood interval for Exponential example
uniroot(function(x)  ExpRLF(x,n,thetahat)-0.15,lower=2.5,upper=3.2)$root
uniroot(function(x)  ExpRLF(x,n,thetahat)-0.15,lower=5.9,upper=6)$root
###################################################################################

###################################################################################
# Problem 3: Ch-squared distribution
# Plot 1
x<-seq(0.14,5,by=0.001)
plot(x,dchisq(x,1),col="blue",lwd=2,type="l",ylab="f(x;k)",main=bquote(chi^2~"(k) pdfs for k=1,2,3"))
curve(dchisq(x,2),from=0,to=5,col="red",lwd=2,add=T)
curve(dchisq(x,3),from=0,to=5,col="black",lwd=2,add=T)
text(1.5,0.34,expression(paste(chi^2)(2)),col="red")
text(0.6,0.6,expression(paste(chi^2)(1)),col="blue")
text(3.5,0.22,expression(paste(chi^2)(3)),col="black")
# Plot 2
x<-seq(5,90,by=0.1)
plot(x,dchisq(x,40),col="blue",lwd=2,type="l",ylab="f(x;k)",main=bquote(chi^2~"(k) pdfs for k=40,45,50"))
curve(dchisq(x,45),col="red",lwd=2,add=T)
curve(dchisq(x,50),col="black",lwd=2,add=T)
text(29,0.04,expression(paste(chi^2)(40)),col="blue")
text(48,0.045,expression(paste(chi^2)(45)),col="red")
text(59,0.035,expression(paste(chi^2)(50)),col="black")
# Plot 3
x<-seq(10,75,by=0.1)
plot(x,dchisq(x,40),col="blue",lwd=2,type="l",ylab="f(x;40)",main=bquote(chi^2~"(40) and G(40,"~sqrt(80)~") pdfs"))
curve(dnorm(x,40,sqrt(80)),col="red",lwd=2,add=T)
text(30,0.04,expression(paste(chi^2)(40)),col="blue")
text(49,0.04,expression(paste(G(40,sqrt(80)))),col="red")
###################################################################################

###################################################################################
# Problem 4: t distribution
# Plot 1
x<-seq(-4,4,by=0.001)
plot(x,dt(x,5),col="blue",lwd=2,type="l",xlab="t",ylab="f(t;k)",main="t(k) pdfs for k=5,10")
curve(dt(x,10),col="red",lwd=2,add=T)
text(0,0.36,"t(5)",col="blue")
text(0.8,0.35,"t(10)",col="red")
#
# Plot 2
plot(x,dnorm(x),col="red",lwd=2,type="l",xlab="t",ylab="f(t;k)",main="t(1) and G(0,1) pdfs")
curve(dt(x,1),col="black",lwd=2,add=T)
text(-0.77,0.15,"t(1)",col="black")
text(1.5,0.25,"G(0,1)",col="red")
#
# Plot 3
plot(x,dnorm(x),col="red",lwd=2,type="l",xlab="t",ylab="f(t;k)",main="t(15) and G(0,1) pdfs")
curve(dt(x,15),col="black",lwd=2,add=T)
text(2,0.1,"t(15)",col="black")
text(1.5,0.25,"G(0,1)",col="red")
###################################################################################

###################################################################################
# Problem 5: Binomial confidence intervals and sampling distribution of likelihood ratio statistic
library(MASS)     # truehist is in the library MASS
id<-20456458
set.seed(id)
#   generate a random value of theta
theta<-runif(1,0.1,0.3)
for (n in c(30,100))  {
cat("n = ",n," theta = ",theta,"\n")   # display values
# vector of observations for 5000 simulations from a Binomial(n,theta) distribution
yobs<- rbinom(5000,n,theta)
# corresponding vector of thetahat values
that<-yobs/n
# values used to construct an approximate 95% confidence interval  based on Gaussian approximation                               
pm<-1.96*sqrt(that*(1-that)/n)       
# each approximate 95% confidence interval is stored in a row of matrix cibi
cibi<-matrix(c(that-pm,that+pm),nrow=5000,byrow=F)
print(cibi[1:10,1:2])         # Look at first 10 approximate 95% confidence intervals
# display proportion of approximate 95% confidence intervals which contain true value of theta
prop<- mean(abs(theta-that)<pm)
cat("proportion of approximate 95% confidence intervals which contain true value of theta is",prop,"\n")
#
# create function to calculate Binomial relative likelihood function
BinRLF <- function(x) {dbinom(y,n,x)/dbinom(y,n,thetahat)}
li<-rep(0,2*5000)
li<- matrix(li,ncol=2,byrow=TRUE)                   # initialize matrix to store likelihood intervals
# For the 5000 simulations determine 15% likelihood intervals which are also 
# approximate 95% likelihood intervals 
for (i in 1:5000) {
y<-yobs[i]
thetahat<-that[i]
if (thetahat==0) { li[i,1]<-0}        # if thetahat=0 then likelihood interval has left endpoint = 0
else {result<-uniroot(function(x) BinRLF(x)-0.15,lower=0,upper=thetahat)
li[i,1]<-result$root}
if (thetahat==1) { li[i,2]<-1}       # if thetahat=1 then likelihood interval has right endpoint = 1
else {result<-uniroot(function(x) BinRLF(x)-0.15,lower=thetahat,upper=1)
li[i,2]<-result$root}
}
print(li[1:10,1:2])         # Look at first ten 15% likelihood intervals 
# display proportion of 15% likelihood intervals which contain the true value of theta
prop<- mean(theta>=li[,1] & theta<=li[,2])
cat("proportion of 15% likelihood intervals which contain true value of theta is",prop,"\n")
#
# calculate the likelihood ratio statistic for all 5000 simulations and plot a relative histogram of values
# the histogram approximates the sampling distribution of the likelihood ratio statistic
lambda<-(-2*log(dbinom(yobs,n,theta)/dbinom(yobs,n,that)))
truehist(lambda,h=0.5,xlab="Likelihood Ratio Statistic",main="Sampling Distribution of Likelihood Ratio Statistic")
curve(dchisq(x,1), from=0.001,to=12,add=TRUE,col="red",lwd=2) # superimpose Chi-squared (1) pdf
}
###################################################################################


###################################################################################
# Problem 6: Exponential confidence intervals and sampling distribution of likelihood ratio statistic
id<-20842360
set.seed(id)
theta<-max(1,id-10*trunc(id/10))                   # theta = last digit of ID unless it is zero
n<-20
cat("n = ",n," theta = ",theta,"\n")   # display values
ye<-rexp(5000*n,1/theta)    
# each of the 5000 rows of the matrix ye contains n independent observations from 
# Exponential(theta) distribution
ye<-matrix(ye,ncol=n,byrow=TRUE) 
that<-apply(ye,1,mean)                      # vector of 5000 means
pm<-1.96*that/sqrt(n)                     # used to get approximate 95% confidence interval 
# each approximate 95% confidence interval is stored in a row of matrix ciexp
ciexp<-matrix(c(that-pm,that+pm),nrow=5000,byrow=F)
ciexp[1:10,1:2]          # Look at first 10 approximate 95% confidence intervals 
# display proportion of approximate 95% confidence intervals which contain true value of theta
prop<- mean(abs(theta-that)<pm)
cat("proportion of approximate 95% confidence intervals which contain true value of theta is",prop,"\n")
#
# create function to calculate Exponential relative likelihood function
ExpRLF<-function(x) {(thetahat/x)^n*exp(n*(1-thetahat/x))}
li<-rep(0,2*5000)
li<- matrix(li,ncol=2,byrow=TRUE)                   # initialize matrix to store likelihood intervals
# For the 5000 simulations determine 15% likelihood intervals which are also
# approximate 95% likelihood intervals 
for (i in 1:5000) {
thetahat<-that[i]
result<-uniroot(function(x) ExpRLF(x)-0.15,lower=max(0,thetahat-4*thetahat/(n^0.5)),upper=thetahat)
li[i,1]<-result$root
result<-uniroot(function(x) ExpRLF(x)-0.15,lower=thetahat,upper= thetahat+4*thetahat/(n^0.5))
li[i,2]<-result$root
}
li[1:10,1:2]         # Look at first ten 15% likelihood intervals 
# display proportion of 15% likelihood intervals which contain the value of theta
prop<- mean(theta>=li[,1] & theta<=li[,2])
cat("proportion of 15% likelihood intervals which contain true value of theta is",prop,"\n")
#
# calculate the likelihood ratio statistic for all 5000 simulations and plot a relative histogram of values
# the histogram approximates the sampling distribution of the likelihood ratio statistic
lambda<- -2*log((that/theta)^n*exp(n*(1-that/theta)))
truehist(lambda,h=0.5,xlab="Likelihood Ratio Statistic",main="Sampling Distribution of Likelihood Ratio Statistic")
curve(dchisq(x,1), from=0.001,to=12,add=TRUE,col="red",lwd=2) # superimpose Chi-squared (1) pdf
###################################################################################





