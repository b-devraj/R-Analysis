###################################################################################
# Problem 1: Two sample Gaussian model
id<-20456458
set.seed(id)
mu1<-round(runif(1,2,3),digits=1)
mu2<-round(runif(1,2.5,3.5),digits=1)
sigma<-round(runif(1,1,2),digits=2)
n1<-sample(c(seq(100,200)),1)
n2<-sample(c(seq(100,200)),1)
# display values 
cat("mu1 = ", mu1, ", mu2 = ", mu2,", sigma = ", sigma,"\n") 
cat("n1 = ", n1, ", n2 = ", n2,"\n") 
# Generate data      
y1<-sort(round(rnorm(n1,mu1,sigma),digits=2))  # n1 observations from G(mu1,sig)
y2<-sort(round(rnorm(n2,mu2,sigma),digits=2))  # n2 observations from G(mu2,sig)
s1<-sd(y1)
s2<-sd(y2)
sp<-(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))^0.5
cat("sample sd for sample 1 = ", s1,"\n")
cat("sample sd for sample 2 = ", s2,"\n")
cat("pooled estimate of sigma = ", sp,"\n") 
# display values 
# Check Gaussian assumption using qqplots
qqnorm(y1,xlab="Standard Normal Quantiles",main="Qqplot of Dataset 1")
qqline(y1,col="red",lwd=1.5)  # add line for comparison
qqnorm(y2,xlab="Standard Normal Quantiles",main="Qqplot of Dataset 2")
qqline(y2,col="red",lwd=1.5)  # add line for comparison
# Test hypothesis of no difference in the means
t.test(y1,y2,mu=0,var.equal=TRUE,conf.level=0.95)
###################################################################################
#
###################################################################################
# Problem 2: Testing Hypothesis of Equal Probabilities in a Multinomial Model
library(MASS)      # You only need to run this command once 
id<-20456458
set.seed(id)
# generate parameter values for Multinomial
k<-sample(5:8,1)   # randomly choose number of categories for Multinomial data
n<-sample(c(seq(150,200)),1)    # randomly choose number of observations
# display values
cat("k = ", k, ", n = ", n,"\n")
# randomly choose probabilities which must sum to one 
p<-sample(3:5,k,replace=TRUE)
p<-p/sum(p) 
# generate observations from Multinomial      
y<-rmultinom(1,n,p)
# calculate expected frequencies assuming equal probabilities for each category
e<- rep(n/k, k)  
# print table of observed and expected frequencies for the k categories
cat("Table of Observed and Expected Frequencies ","\n")
print(data.frame("Cat" = rbind(y[,1],e), row.names = c("Observed", "Expected")),digits=4) 
# observed values of likelihood ratio test statistic and Goodness of Fit test statistic
# and corresponding p-values
df<-k-1      # degrees of freedom for the Chi-squared distribution
lambda<-2*sum(y*log(y/e))
pvalue<-1-pchisq(lambda,df)
cat("Observed value of likelihood ratio statistic = ", lambda,"\n")
cat("with p-value = ",pvalue, "and degrees of freedom = ",df,"\n")
pearson<-sum(((y-e)^2)/e) 
pvalue<-1-pchisq(pearson,df)  
cat("Observed value of Goodness of Fit statistic = ", pearson,"\n")
cat("with p-value = ", pvalue, "and degrees of freedom = ",df,"\n")
###################################################################################
#
###################################################################################
# Problem 3: Goodness of Fit Test for a Poisson Model
id<-20456458
set.seed(id)
n<-sample(c(seq(200,250)),1)    # randomly choose number of observations
model<-sample(c(1:4),1)
# display values
cat("Model = ", model, ", n = ", n,"\n")
# Data are randomly generated from one of four different models all with mean 4
# Model=1: Poisson(4) distribution 
# Model=2:  Negative Binomial(3,3/7)
# Model=3:  G(4,1) distribution and discretized 
# Model=4:  Gamma(3,4/3) distribution and discretized 
if (model==1) {
  y<-rpois(n,4)    # n observations from Poisson(4)
} else if (model==2) {
  y<-rnbinom(n,3,3/7)   # n observations from NB(3,3/7)
} else if (model==3) {
  y<-round( rnorm(n,4,1))   # n observations from G(4,1) rounded
  y[y<0]<-0   # convert any negative observations to 0
} else if (model==4) {
  y<-round(rgamma(n,3,3/4))    # n observations from Gamma(3,4/3) rounded
} 
ymin<-min(y)
ymax<-max(y)
# determine categories and frequencies for the data
data<-table(c(y, ymin:ymax))-1      # Done to ensure all categories are accounted for
f<-as.numeric(data)     # frequencies
cat<-as.numeric(names(data),"\n")
# determine the maximum likelihood estimate of theta which is the sample mean calculated 
# from the frequency table
thetahat<-sum(cat*f)/n 
# determine the expected frequencies assuming Poisson model
e<-dpois(cat,thetahat)*n   
#frequency for ymin must be sum for y<=ymin
e[1]<-ppois(ymin,thetahat)*n
ncat<-length(e)
# frequency for ymax must be sum of frequencies for y>=ymax
e[ncat]<- ppois(ymax- 1,thetahat, lower = F)*n     
# Table of Observed and expected frequencies
data<-rbind("y" = ymin:ymax, "observed" = f, "expected" = e)    
# print table of observed and expected frequencies
cat("Table of Observed and Expected Frequencies ","\n")
print(data,digits=4) 
# Expected frequencies must all be at least 5 to apply tests. Collapse categories if necessary.
nbins<-ncol(data)
while(data[3, nbins] < 5){
  data[2:3, nbins - 1]<-data[2:3, nbins - 1] + data[2:3, nbins]
  data<-data[, -nbins]
  nbins<-nbins - 1}
nbins<-1
while(data[3, nbins] < 5){
  data[2:3, nbins + 1]<-data[2:3, nbins + 1] + data[2:3, nbins]
  data<-data[, -nbins]}
# Observed frequencies must not be zero. Collapse categories if necessary.
nbins<-ncol(data)
while (nbins>1){
if (data[2, nbins]==0){
data[2:3, nbins - 1]<-data[2:3, nbins - 1] + data[2:3, nbins]
  data<-data[, -nbins]}
  nbins<-nbins - 1}
if (data[2,1]==0){
data[2:3,2]<-data[2:3,2]+data[2:3,1]
data<-data[,-1]}
cat("Table of Observed and Expected Frequencies ","\n")
# print table of observed and expected frequencies
print(data,digits=4) 
# observed values of likelihood ratio test statistic and Goodness of Fit test statistic
# and corresponding p-values
df = ncol(data)-2      # degress of freedom for the Chi-squared distribution
f<-data[2,]
e<-data[3,]
lambda<-2*sum(f*log(f/e))
pvalue<-1-pchisq(lambda,df)
cat("Observed value of likelihood ratio statistic = ", lambda,"\n")
cat("with p-value = ",pvalue, "and degrees of freedom = ",df,"\n")
pearson<-sum(((f-e)^2)/e) 
pvalue<-1-pchisq(pearson,df)  
cat("Observed value of Goodness of Fit statistic = ", pearson,"\n")
cat("with p-value = ", pvalue, "and degrees of freedom = ",df,"\n")
###################################################################################
#
###################################################################################
# Problem 4: Test of Independence in a Two Way Table
id<-20456458
set.seed(id)
# generate data for a two way table by first simulating bivariate data  
# from the Bivariate Normal distribution and then discretize the data
# Random uniform between -0.75 and 0.75
corrCoef<-runif(1, -0.75, 0.75)
cat("correlation coefficient = ",corrCoef,"\n")
sigma<-max(id %% 10, 1)
# Last digit of UWID using modulo, minimum value of 1.
mu1<-max(id %% 100 - id %% 10, 20)
# (Second last digit*10) is extracted here, minimum value of 20
mu2<-max(id %% 1000 - id %% 100, 30)
# (Third last digit*100) is extracted here, minimum value of 30
VarCovar<-cbind(c(sigma^2, corrCoef*sigma^2), c(corrCoef*sigma^2, sigma^2))
# Simulate data from a bivariate Normal
n<-sample(c(100:200),1)    # n =  sample size
cat("Number of observations = ",n,"\n")
data2<-mvrnorm(n, mu = c(mu1, mu2), Sigma = VarCovar)
# Create smoker/non-smoker variable by mapping 1 to smoker and 2 to non-smoker
data3 = as.data.frame(data2)
data3[, 1]<-ifelse(data3[,1] < median(data3[,1]), 1, 2)
data3[, 1]<-c("Smoker", "Non-smoker") [data3[,1]]
# Create tall/avg/short height variable by mapping 1 to tall, 2 to average and 3 to short
data3[, 2]<-floor((rank(data3[, 2])-0.1)/nrow(data3)*3) + 1
data3[, 2]<-c("Tall", "Average", "Short")[data3[, 2]]
data3[, 1]<-factor(data3[, 1])
data3[, 2]<-factor(data3[, 2])
colnames(data3)<-c("Smoker Indicator", "Height Indicator")
f<-table(data3)
cat("Table of Observed Frequencies:","\n")
f
r<-margin.table(f,1)     # row totals
c<-margin.table(f,2)     # column totals
e<-outer(r,c)/sum(f)   # matrix of expected frequencies
cat("Table of Expected Frequencies:","\n")
print(e,digits=4)
lambda<-2*sum(f*log(f/e))  # observed value of likelihood ratio statistic
df<-(length(r)-1)*(length(c)-1)  # degrees of freedom
pvalue<-1-pchisq(lambda,df)
cat("Observed value of likelihood ratio statistic = ", lambda,"\n")
cat("with p-value = ",pvalue, "and degrees of freedom = ",df,"\n")
pearson<-sum(((f-e)^2)/e) 
pvalue<-1-pchisq(pearson,df)  
cat("Observed value of Goodness of Fit statistic = ", pearson,"\n")
cat("with p-value = ", pvalue, "and degrees of freedom = ",df,"\n")
###################################################################################