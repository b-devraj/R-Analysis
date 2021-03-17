###################################################################################
# Problem 1 
id<-20842360
set.seed(id)
# generate x
x<-round(runif(100,1,20),digits=1)
# generate random values for alpha and beta 
alpha<-round(runif(1,0,5),digit=2)
beta<- round(runif(1,-5,-1),digits=2)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta,"\n")       
# generate y
y<-round(alpha+beta*x+rnorm(100,0,10),digits=1)
# display mean of x and y
cat("xbar = ", mean(x),", ybar = ", mean(y),"\n")
# display sample correlation
cat("sample correlation = ", cor(x,y),"\n") 
# run regression y = alpha+beta*x
RegModel<-lm(y~x)
summary(RegModel)     # parameter estimates and p-value for test of no relationship
alphahat<-RegModel$coefficients[1] # estimate of intercept
betahat<-RegModel$coefficients[2]  # estimate of slope
cat("alphahat = ",alphahat,"   betahat = ",betahat,"\n")
se<-summary(RegModel)$sigma        # estimate of sigma
cat("estimate of sigma = ",se,"\n")
muhat<-RegModel$fitted.values      # fitted responses
r<- RegModel$residuals             # residuals
rstar <- r/se                      # standardized residuals
# Scatterplot of data with fitted line
plot(x,y,col="blue")
title(main="Scatterplot with Fitted Line")
abline(a=alphahat,b=betahat,col="red",lwd=2)
# Residual plots
plot(x,rstar,xlab="x",ylab="Standardized Residual")
title(main="Residual vs x")
abline(0,0,col="red",lwd=1.5)
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
title(main="Residual vs Muhat")
abline(0,0,col="red",lwd=1.5)
qqnorm(rstar,main="")
qqline(rstar,col="red",lwd=1.5)  # add line for comparison
title(main="Qqplot of Residuals")
# 95% Confidence interval for slope
confint(RegModel,level=0.95)
# 90% confidence interval for mean response at x=5
predict(RegModel,data.frame("x"=5),interval="confidence",lev=0.90)
# 99% Prediction interval for response at x=2
predict(RegModel,data.frame("x"=2),interval="prediction",lev=0.99)
# 95% confidence interval for sigma
df<-length(y)-2
a<-qchisq(0.025,df)
b<-qchisq(0.975,df)
cat("95% confidence interval for sigma: ",c(se*sqrt(df/b),se*sqrt(df/a)),"\n")
###################################################################################
#

###################################################################################
# Problem 1 Modified
id<-20842360
set.seed(id)
# generate x
x<-round(runif(100,1,20),digits=1)
# generate random values for alpha and beta 
alpha<-round(runif(1,0,5),digit=2)
beta<- round(runif(1,-5,-1),digits=2)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta,"\n")       
# generate y
y<-round(alpha+beta*x+rnorm(100,0,10),digits=1)
# display mean of x and y
cat("xbar = ", mean(x),", ybar = ", mean(y),"\n")
# display sample correlation
cat("sample correlation = ", cor(x,y),"\n") 
# run regression y = alpha+beta*x
RegModel<-lm(x~y)
summary(RegModel)     # parameter estimates and p-value for test of no relationship
alphahat<-RegModel$coefficients[1] # estimate of intercept
betahat<-RegModel$coefficients[2]  # estimate of slope
cat("alphahat = ",alphahat,"   betahat = ",betahat,"\n")
se<-summary(RegModel)$sigma        # estimate of sigma
cat("estimate of sigma = ",se,"\n")
muhat<-RegModel$fitted.values      # fitted responses
r<- RegModel$residuals             # residuals
rstar <- r/se                      # standardized residuals
# Scatterplot of data with fitted line
plot(y,x,col="blue")
title(main="Scatterplot with Fitted Lines")
abline(a=alphahat,b=betahat,col="red",lwd=2)
abline(a=6.164372,b=-1.281534,col="green",lwd=2)
# Residual plots
plot(x,rstar,xlab="x",ylab="Standardized Residual")
title(main="Residual vs x")
abline(0,0,col="red",lwd=1.5)
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
title(main="Residual vs Muhat")
abline(0,0,col="red",lwd=1.5)
qqnorm(rstar,main="")
qqline(rstar,col="red",lwd=1.5)  # add line for comparison
title(main="Qqplot of Residuals")
# 95% Confidence interval for slope
confint(RegModel,level=0.95)
# 90% confidence interval for mean response at x=5
predict(RegModel,data.frame("x"=5),interval="confidence",lev=0.90)
# 99% Prediction interval for response at x=2
predict(RegModel,data.frame("x"=2),interval="prediction",lev=0.99)
# 95% confidence interval for sigma
df<-length(y)-2
a<-qchisq(0.025,df)
b<-qchisq(0.975,df)
cat("95% confidence interval for sigma: ",c(se*sqrt(df/b),se*sqrt(df/a)),"\n")
###################################################################################
#

###################################################################################
# Problem 2 
id<-20456458
set.seed(id+82630)
# generate x
x<-round(runif(95,1,20),digits=1)
# generate random values for alpha and beta 
alpha<-round(runif(1,0,5),digits=2)
beta<- round(runif(1,3,5),digits=2)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta,"\n")       
# generate y
y<-round(alpha+beta*((x-10)/5)^2+rnorm(95,0,3),digits=1)
# display sample correlation
cat("sample correlation = ", cor(x,y),"\n") 
# run regression y = alpha+beta*x
RegModel<-lm(y~x)
summary(RegModel)     # parameter estimates and p-value for test of no relationship
alphahat<-RegModel$coefficients[1] # estimate of intercept
betahat<-RegModel$coefficients[2]  # estimate of slope
cat("alphahat = ",alphahat,"   betahat = ",betahat,"\n")
se<-summary(RegModel)$sigma        # estimate of sigma
cat("estimate of sigma = ",se,"\n")
muhat<-RegModel$fitted.values      # fitted responses
r<- RegModel$residuals             # residuals
rstar <- r/se                      # standardized residuals
# Scatterplot of data with fitted line
plot(x,y,col="blue")
title(main="Scatterplot with Fitted Line")
abline(a=alphahat,b=betahat,col="red",lwd=2)
# Residual plots
plot(x,rstar,xlab="x",ylab="Standardized Residual")
title(main="Residual vs x")
abline(0,0,col="red",lwd=1.5)
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
title(main="Residual vs Muhat")
abline(0,0,col="red",lwd=1.5)
qqnorm(rstar,main="")
qqline(rstar,col="red",lwd=1.5)  # add line for comparison
title(main="Qqplot of Residuals")
###################################################################################
#
###################################################################################
# Problem 2 Modified
id<-20842360
set.seed(id+82630)
# generate x
x<-round(runif(95,1,20),digits=1)
# generate random values for alpha and beta 
alpha<-round(runif(1,0,5),digits=2)
beta<- round(runif(1,3,5),digits=2)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta,"\n")       
# generate y
y<-round(alpha+beta*((x-10)/5)^2+rnorm(95,0,3),digits=1)
# display sample correlation
cat("sample correlation = ", cor(x,y),"\n") 
# run regression y = alpha+beta*x
RegModel<-lm(y~x + I(x^2))
summary(RegModel)     # parameter estimates and p-value for test of no relationship
alphahat<-RegModel$coefficients[1] # estimate of intercept
betahat<-RegModel$coefficients[2]  # estimate of slope
gammahat<-RegModel$coefficients[3]  # estimate of gamma
cat("alphahat = ",alphahat,"   betahat = ",betahat,"\n" )
cat("gammahat = " ,gammahat)
se<-summary(RegModel)$sigma        # estimate of sigma
cat("estimate of sigma = ",se,"\n")
muhat<-RegModel$fitted.values      # fitted responses
r<- RegModel$residuals             # residuals
rstar <- r/se                      # standardized residuals
# Scatterplot of data with fitted line
plot(x,y,col="blue")
title(main="Scatterplot with Fitted Line")
curve(alphahat+betahat*x+gammahat*x^2,col="green",add=T,lwd=2)
# Residual plots
plot(x,rstar,xlab="x",ylab="Standardized Residual")
title(main="Residual vs x")
abline(0,0,col="red",lwd=1.5)
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
title(main="Residual vs Muhat")
abline(0,0,col="red",lwd=1.5)
qqnorm(rstar,main="")
qqline(rstar,col="red",lwd=1.5)  # add line for comparison
title(main="Qqplot of Residuals")
###################################################################################
#
###################################################################################
# Problem 3
id<-20842360
set.seed(id+29401)
# generate x
x<-round(runif(200,1,10),digits=1)
# generate random values for alpha and beta 
alpha<-round(runif(1,0,5),digits=2)
beta<- round(runif(1,5,10),digits=2)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta,"\n")       
# generate y
w<-sqrt(round(alpha+beta*x+x*rnorm(200,0,1),digits=1))
# display sample correlation
cat("sample correlation = ", cor(x,y),"\n") 
# run regression y = alpha+beta*x
RegModel<-lm(w~x)
summary(RegModel)     # parameter estimates and p-value for test of no relationship
alphahat<-RegModel$coefficients[1] # estimate of intercept
betahat<-RegModel$coefficients[2]  # estimate of slope
cat("alphahat = ",alphahat,"   betahat = ",betahat,"\n")
se<-summary(RegModel)$sigma        # estimate of sigma
cat("estimate of sigma = ",se,"\n")
muhat<-RegModel$fitted.values      # fitted responses
r<- RegModel$residuals             # residuals
rstar <- r/se                      # standardized residuals
# Scatterplot of data with fitted line
plot(x,w,col="blue")
title(main="Scatterplot with Fitted Line")
abline(a=alphahat,b=betahat,col="red",lwd=2)
# Residual plots
plot(x,rstar,xlab="x",ylab="Standardized Residual")
title(main="Residual vs x")
abline(0,0,col="red",lwd=1.5)
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
title(main="Residual vs Muhat")
abline(0,0,col="red",lwd=1.5)
qqnorm(rstar,main="")
qqline(rstar,col="red",lwd=1.5)  # add line for comparison
title(main="Qqplot of Residuals")
# 95% Prediction interval for response at x=10
predict(RegModel,data.frame("x"=10),interval="prediction",lev=0.95)
###################################################################################

###################################################################################
# Problem 3 Modified
id<-20842360
set.seed(id+29401)
# generate x
x<-round(runif(200,1,10),digits=1)
# generate random values for alpha and beta 
alpha<-round(runif(1,0,5),digits=2)
beta<- round(runif(1,5,10),digits=2)
# display values of alpha and beta
cat("alpha = ", alpha, ", beta = ", beta,"\n")       
# generate y
y<-round(alpha+beta*x+x*rnorm(200,0,1),digits=1)
# display sample correlation
cat("sample correlation = ", cor(x,y),"\n") 
# run regression y = alpha+beta*x
RegModel<-lm(y~x)
summary(RegModel)     # parameter estimates and p-value for test of no relationship
alphahat<-RegModel$coefficients[1] # estimate of intercept
betahat<-RegModel$coefficients[2]  # estimate of slope
cat("alphahat = ",alphahat,"   betahat = ",betahat,"\n")
se<-summary(RegModel)$sigma        # estimate of sigma
cat("estimate of sigma = ",se,"\n")
muhat<-RegModel$fitted.values      # fitted responses
r<- RegModel$residuals             # residuals
rstar <- r/se                      # standardized residuals
# Scatterplot of data with fitted line
plot(x,y,col="blue")
title(main="Scatterplot with Fitted Line")
abline(a=alphahat,b=betahat,col="red",lwd=2)
# Residual plots
plot(x,rstar,xlab="x",ylab="Standardized Residual")
title(main="Residual vs x")
abline(0,0,col="red",lwd=1.5)
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
title(main="Residual vs Muhat")
abline(0,0,col="red",lwd=1.5)
qqnorm(rstar,main="")
qqline(rstar,col="red",lwd=1.5)  # add line for comparison
title(main="Qqplot of Residuals")
# 95% Prediction interval for response at x=10
predict(RegModel,data.frame("x"=10),interval="prediction",lev=0.95)
###################################################################################
