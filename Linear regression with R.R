ls()
rm(list=ls())
library(MASS) 
data(Boston)
help(Boston)
summary(Boston)
Boston[1:3,]
dim(Boston)
n<-nrow(Boston) 
n
summary(Boston$medv)


##histogramofthedistribution 
##xlab=graphicaloptiontoassignalabeltothex-axis 
##main:title of the graph 
hist(Boston$medv,prob=TRUE,xlab='Medianvalue',main='Histogram')


##boxplot of the distribution 
boxplot(Boston$medv,xlab='Medianvalue',main='Boxplot')


## dispersionplot 
## pch=19typeofpoint; 
## cex=0.5 reducing the dimension of the points(defaulting to 1) 
## ylab:analogous to xlab but relative to y-axis 
plot(Boston$medv,Boston$lstat,main='Dispersionplot', xlab='%oflowerstatusofthepopulation', ylab='Medianvalue',pch=19,cex=0.5)


cor(Boston$medv,Boston$lstat)


## Simple linear regression model
beta1<-cov(Boston$medv,Boston$lstat)/var(Boston$lstat) 
beta1
beta0<-mean(Boston$medv)-beta1*mean(Boston$lstat) 
beta0


mean((Boston$lstat-mean(Boston$lstat))^2)/n
mean(Boston$lstat^2)-(mean(Boston$lstat)^2)
var(Boston$lstat)*(n-1)/n


model<-lm(medv~lstat,data=Boston)
model
summary(model)


names(model)
model$coefficients
est.values<-fitted(model)


plot(Boston$lstat,Boston$medv,pch=19,cex=0.5, xlab='% of lower status of the population',ylab='Median value')
## add on the estimated values 
points(Boston$lstat,est.values,pch='x',col='green') 
## add on the least squares regression line

abline(coef(model)[1],coef(model)[2],lty=2,col='red',lwd=3) 
## equalto 
## abline (beta0, beta1,lty=2, col='red') 
## lty = 2 specifies dashed line (defaulting to lty = 1 solid line) 
## lwd = 3 specifies line width (defaulting to lwd = 1)

## variance/covariance matrix associated to the parameter estimates
vcov(model)
## standard error 
se<-sqrt(diag(vcov(model))) 
se


## for beta1 
beta1-qt(0.975,df=n-2)*se[2] 
beta1+qt(0.975,df=n-2)*se[2] 
c(beta1-qt(0.975,df=n-2)*se[2],beta1+qt(0.975,df=n-2)*se[2])
c(beta1-qnorm(0.975)*se[2],beta1+qnorm(0.975)*se[2])
confint(model)
confint(model,level=0.90)


statistic.t<-(beta1-(-1))/se[2] 
statistic.t

qt(0.025,df=n-2)
## p-value of the test 
2*min(pt(statistic.t,n-2),1-pt(statistic.t,n-2))


predict(model,newdata=data.frame(list(lstat=c(5,10,25))))
## Predictions with prediction interval 
predict(model,newdata=data.frame(list(lstat=c(5,10,25))), interval='prediction')
res<-residuals(model)

# subdivide the window into 4parts , 2 rows and 2 columns 
par(mfrow=c(2,2)) 
hist(res,prob=TRUE) 
plot(res,pch=19,cex=0.5,ylab='Residuals') 
## add on the line parallel to the x-axis 
abline(h=0,lty=2) 
plot(est.values,res,pch=19,cex=0.5,xlab='Estimated values', ylab='Residuals') 
abline(h=0,lty=2) 
plot(Boston$lstat,res,ylab='Residuals', xlab='% of lower status of the population',pch=19,cex=0.5) 
abline(h=0,lty=2)
