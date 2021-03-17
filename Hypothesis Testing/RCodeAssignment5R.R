###################################################################################
# Problem 1: Tests of hypothesis for Binomial data
id<-20842360
set.seed(id)
# generate a random value of theta from a Uniform(0.3,0.35) distribution
theta<-round(runif(1,0.3,0.35),digits=3) 
theta0<-round(theta+0.1,digits=1)   # value for theta0
n<-90
# determine the maximum likelihood estimate based on a single random observation 
# generated from a Binomial(n,theta) distribution
thetahat<-0.3  
# display values
cat('theta = ', theta, ', theta0  = ',theta0, "\n")
cat('n = ', n,  ', thetahat = ',thetahat, "\n")
# observed value of likelihood ratio test for testing the null hypothesis theta=theta0
lambda<--2*log((theta0/thetahat)^(n*thetahat)*((1-theta0)/(1-thetahat))^(n-n*thetahat))
cat('observed value of likelihood ratio statistic = ',lambda, "\n")
pvalue<-2*(1-pnorm(sqrt(lambda),0,1))
cat('approximate p-value for testing hypothesis theta = theta0: ',pvalue, "\n")
# observed value of approximate Gaussian test statistic for testing the null hypothesis theta=theta0
d<-abs(thetahat-theta0)/sqrt(theta0*(1-theta0)/n) 
cat('observed value of approximate Gaussian test statistic = ',d, "\n")
pvalue<-2*(1-pnorm(d,0,1))
cat('approximate p-value for testing hypothesis theta = theta0: ',pvalue, "\n")
# exact p-value 
d1<-abs(n*thetahat-n*theta0)
pvalue<-pbinom(n*theta0-d1,n,theta0)+1-pbinom(n*theta0+d1-as.numeric(d1 > 0),n,theta0)
cat('exact p-value for testing hypothesis theta = theta0: ',pvalue, "\n")
###################################################################################

###################################################################################
# Problem 2: Tests of hypothesis for Poisson data 
id<-20842360
set.seed(id)
# generate a random value of theta from a Uniform(3,4) distribution
theta<-round(runif(1,3,4),digits=3) 
theta0<-round(theta+0.6,digits=1)   # value for theta0
n<-30
# determine the maximum likelihood estimate based on a random sample 
# generated from a Poisson(theta) distribution
thetahat<-mean(rpois(n,theta))   
# display values
cat('theta = ', theta, ', theta0  = ',theta0, "\n")
cat('n = ', n,  ', thetahat = ',thetahat, "\n")
# observed value of likelihood ratio test for testing the null hypothesis theta=theta0
lambda<--2*log((theta0/thetahat)^(n*thetahat)*exp(n*(thetahat-theta0)))
cat('observed value of likelihood ratio statistic = ',lambda, "\n")
pvalue<-2*(1-pnorm(sqrt(lambda),0,1))
cat('approximate p-value for testing hypothesis theta = theta0: ',pvalue, "\n")
# observed value of approximate Gaussian test statistic for testing the null hypothesis theta=theta0
d<-abs(thetahat-theta0)/sqrt(theta0/n)
cat('observed value of approximate Gaussian test statistic = ',d, "\n")
pvalue<-2*(1-pnorm(d,0,1))
cat('approximate p-value for testing hypothesis theta = theta0: ',pvalue, "\n")
# exact p-value 
d1<-n*abs(thetahat-theta0)
pvalue<-ppois(n*theta0-d1,n*theta0)+1-ppois(n*theta0+d1-as.numeric(d1>0),n*theta0)
cat('exact p-value for testing hypothesis theta = theta0: ',pvalue, "\n")
###################################################################################


###################################################################################
# Problem 3: Tests of hypothesis for Exponential data
id<-20842360
set.seed(id)
# generate a random value of theta from a Uniform(9,10) distribution
theta<-round(runif(1,9,10),digits=3)
theta0<-round(theta*1.3,digits=1)   # value for theta0
n<-70
# determine the maximum likelihood estimate based on a random sample 
# generated from a Exponential(theta) distribution
thetahat<-mean(rexp(35,1/theta))   
# display values
cat('theta = ', theta, ', theta0  = ',theta0, "\n")
cat('n = ', n,  ', thetahat = ',thetahat, "\n")
# observed value of likelihood ratio test for testing the null hypothesis theta=theta0
lambda<--2*log((thetahat/theta0)^n*exp(n*(1-thetahat/theta0)))
cat('observed value of likelihood ratio statistic = ',lambda, "\n")
pvalue<-2*(1-pnorm(sqrt(lambda),0,1))
cat('approximate p-value for testing hypothesis theta = theta0: ',pvalue, "\n")
# observed value of approximate Gaussian test statistic for testing the null hypothesis theta=theta0
d<-abs(thetahat-theta0)/(theta0/sqrt(n))
cat('observed value of approximate Gaussian test statistic = ',d, "\n")
pvalue<-2*(1-pnorm(d,0,1))
# exact p-value using test statistic D1=2n*thetatilde/theta0
cat('approximate p-value for testing hypothesis theta = theta0: ',pvalue, "\n")    
d1<-2*n*thetahat/theta0
pvalue<-min(2*pchisq(d1,2*n),2*(1-pchisq(d1,2*n)))
cat('exact p-value for testing hypothesis theta = theta0 using D1: ',pvalue, "\n")
###################################################################################


###################################################################################
# Problem 4: Tests of hypothesis for Gaussian data
id<-20842360
set.seed(id)
# generate a random value of mu from a Uniform(4,5) distribution
mu<-round(runif(1,4,5),digits=3)
mu0<-round(mu+1.25,digits=2)   # value for mu0
# generate a random value of sigma from a Uniform(2,3) distribution
sigma<-round(runif(1,3,4),digits=3)
sigma0<-round(sigma*1.2,digits=1)   # value for sigma0
n<-50
# determine the estimates based on a random sample 
# generated from a G(mu,sigma) distribution
y<-rnorm(n,mu,sigma)
muhat<-mean(y)   
s<-sd(y)
# display values
cat('mu = ', mu, ', mu0  = ',mu0, "\n")
cat('sigma = ', sigma, ', sigma0  = ',sigma0, "\n")
cat('n = ', n,  ', muhat = ',muhat,', s = ',s, "\n")
# Use t.test fo test hypothesis mu=mu0
t.test(y,mu=mu0)$statistic
t.test(y,mu=mu0)$parameter
t.test(y,mu=mu0)$p.value 
df<-length(y)-1
d<-df*s^2/sigma0^2
# observed value of statistic for testing the null hypothesis sigma=sigma0
cat('observed value of test statistic = ',d, "\n")
q<-pchisq(d,df)
# p-value for testing sigma=sigma0
cat('p-value for testing hypothesis sigma = sigma0 using D: ',min(2*q,2*(1-q)), "\n")
###################################################################################
