###################################################################################
# Problem 1: Run this R code that produces Gaussian qqplots for the Gaussian 
# data sets that you generated on Assignment 1. 
id<-20842360
set.seed(id)
for (n in c(50,50,100,100,200,200,300,300)) {
y<-rnorm(n,0,1)
qqnorm(y,xlab='G(0,1) Theoretical Quantiles',main=paste("sample size = ",n))
qqline(y,col="red",lwd=2)
}
###################################################################################


###################################################################################
# Problem 2: Run this code which plots the qqplots for data sets generated from 
# four different distributions: Exponential, Gamma, Uniform, and t(3)
id<-20842360
set.seed(id)
mu<-max(1,id-10*trunc(id/10))              # mu = last digit of ID unless it is zero
ye<-sort(round(rexp(75,1/mu),digits=2))   # 75 observations from Exponential(1/mu) distribution
qqnorm(ye,xlab='G(0,1) Theoretical Quantiles',main='Exponential Data')
qqline(ye,col="red",lwd=2)
#
mu<-max(1,id-10*trunc(id/10))              # mu = last digit of ID unless it is zero
yg<-sort(round(rgamma(75,3,1/mu),digits=2))   # 75 observations from Gamma(3,1/mu) distribution
qqnorm(yg,xlab='G(0,1) Theoretical Quantiles',main='Gamma Data')
qqline(yg,col="red",lwd=2)
#
yu<-sort(round(runif(75),digits=2))           # 75 observations from Uniform(0,1) distribution
qqnorm(yu,xlab='G(0,1) Theoretical Quantiles',main='Uniform Data')
qqline(yu,col="red",lwd=2)
#
yt<-sort(round(rt(75,3),digits=2))           # 75 observations from t(3) distribution
qqnorm(yt,xlab='G(0,1) Theoretical Quantiles',main='t(3) Data')
qqline(yt,col="red",lwd=2)
###################################################################################


###################################################################################
# Problem 4: Sampling distribution of sample mean: Run this code. 
library(MASS)     # truehist is in the library MASS
id<-20842360
set.seed(id)
# pop is a vector of 500 variate values for a finite population
pop<-rpois(500,max(1,id-10*trunc(id/10)))
mu<-mean(pop)             # population mean
popsd<-(499*var(pop)/500)^0.5
cat("population mean  = ",mu, " population standard deviation  = ",popsd,"\n")
par(mfrow=c(1,1))
truehist(pop,h=1,main="Population 1 Histogram",xlab="Variate Value"		)
k<-10000           # number of simulations
n<-15              # sample size
sim<-rep(0,k)      # vector to store sample means
# Calculate k sample means for samples of size n drawn from population pop
for (i in 1:k)
sim[i]=mean(sample(pop,n,replace=F)) 
# display percentage of times sample mean is within 0.5 of true mean mu
prop<-mean(abs(sim-mu)<0.5)
cat("percentage of times sample mean is within 0.5 of true mean for n=15 = ",prop,"\n")
truehist(sim,h=0.25,xlab="Sample Mean",main="Sampling Distribution of Sample Mean, Population 1, n=15")
# Simulation for sample size n=30
n<-30              # sample size
sim<-rep(0,k)      # vector to store sample means
# Calculate k sample means for samples of size n drawn from population pop
for (i in 1:k)
sim[i]=mean(sample(pop,n,replace=F)) 
# display percentage of times sample mean is within 0.5 of true mean mu
prop<-mean(abs(sim-mu)<0.5)
cat("percentage of times sample mean is within 0.5 of true mean for n=30 = ",prop,"\n")
truehist(sim,h=0.25,xlab="Sample Mean",main="Sampling Distribution of Sample Mean, Population 1, n=30")
#
# sampling distribution for a different population
# pop is a vector of 500 variate values for a finite population
pop<-rexp(500,1/5)
mu<-mean(pop)             # population mean
popsd<-(499*var(pop)/500)^0.5
cat("population mean  = ",mu, " population standard deviation  = ",popsd,"\n")
truehist(pop,h=5,main="Population 2 Histogram",xlab="Variate Value"		)
k<-10000           # number of simulations
n<-15              # sample size
sim<-rep(0,k)      # vector to store sample means
# Calculate k sample means for samples of size n drawn from population pop
for (i in 1:k)
sim[i]=mean(sample(pop,n,replace=F)) 
# display percentage of times sample mean is within 2 of true mean mu
prop<-mean(abs(sim-mu)<2)
cat("percentage of times sample mean is within 2 of true mean for n=15 = ",prop,"\n")
truehist(sim,h=1,xlab="Sample Mean",main="Sampling Distribution of Sample Mean, Population 2, n=15")
# Simulation for sample size n=30
n<-30              # sample size
sim<-rep(0,k)      # vector to store sample means
# Calculate k sample means for samples of size n drawn from population pop
for (i in 1:k)
sim[i]=mean(sample(pop,n,replace=F)) 
# display percentage of times sample mean is within 2 of true mean mu
prop<-mean(abs(sim-mu)<2)
cat("percentage of times sample mean is within 0.5 of true mean for n=30 = ",prop,"\n")
truehist(sim,h=1,xlab="Sample Mean",main="Sampling Distribution of Sample Mean, Population 2, n=30")
#	
# sampling distribution for a different population
# pop is a vector of 500 variate values for a finite population
pop<-runif(500,2.5,7.5)
mu<-mean(pop)             # population mean
popsd<-(499*var(pop)/500)^0.5
cat("population mean  = ",mu, " population standard deviation  = ",popsd,"\n")
truehist(pop,h=1,main="Population 3 Histogram",xlab="Variate Value"		)
k<-10000           # number of simulations
n<-15              # sample size
sim<-rep(0,k)      # vector to store sample means
# Calculate k sample means for samples of size n drawn from population pop
for (i in 1:k)
sim[i]=mean(sample(pop,n,replace=F)) 
# display percentage of times sample mean is within 0.5 of true mean mu
prop<-mean(abs(sim-mu)<0.5)
cat("percentage of times sample mean is within 0.5 of true mean for n=15 = ",prop,"\n")
truehist(sim,h=0.25,xlab="Sample Mean",main="Sampling Distribution of Sample Mean, Population 3, n=15")
# Simulation for sample size n=30
n<-30              # sample size
sim<-rep(0,k)      # vector to store sample means
# Calculate k sample means for samples of size n drawn from population pop
for (i in 1:k)
sim[i]=mean(sample(pop,n,replace=F)) 
# display percentage of times sample mean is within 0.5 of true mean mu
prop<-mean(abs(sim-mu)<0.5)
cat("percentage of times sample mean is within 0.5 of true mean for n=30 = ",prop,"\n")
truehist(sim,h=0.25,xlab="Sample Mean",main="Sampling Distribution of Sample Mean, Population 3, n=30")
#######################################################################################################



