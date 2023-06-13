summary(travel)
cor(travel)
dim(travel)
str(travel)
View(travel)
head(travel)
tail(travel)
names(travel)
plot(travel)
attach(travel)
class(Distance)
length(Distance)
unique(Prov)
View(sort(table(Prov), decreasing = TRUE))
barplot(sort(table(Prov), decreasing = TRUE))

travel[is.na(Prov) , ] #looks for a missing data


class(Distance)
length(Distance)
summary(Distance)
boxplot(Distance)
hist(Distance)

summary(travel)
head(travel)
View(travel)
names(travel)
summary(travel$Distance)
hist(travel$Distance, probability = TRUE, xlab = 'Distance of trip', main = 'Histogram of Dist')
boxplot(travel$Distance, xlab = 'Distance', main = 'Boxplot')
plot(travel$Distance, travel$Cost, main = 'Dispersion plot',
     xlab = 'Distance of the trip',
     ylab = 'Cost of the trip',
     pch = 19, cex = 0.5)
cor(travel$Distance, travel$Cost)
b1 <- cov(travel$Cost, travel$Distance)/var(travel$Distance)
b1

b2 <- mean(travel$Cost) - b1*mean(travel$Distance)
b2


mean((travel$Distance - mean(travel$Cost))^2)/n
mean(travel$Distance^2) - (mean(travel$Distance)^2)
var(travel$Distance)*(n-1)/n
model <- lm(Cost~Distance, data = travel)
model
summary(model)
names(model)
model$coefficients
estimated <- (fitted(model))
plot(travel$Distance, travel$Cost, pch = 19, cex = 0.5,
     xlab = 'Dist', ylab = 'Cost')
points(travel$Distance, estimated, pch = 'x', col = 'green')
abline(coef(model)[1], coef(model)[2], lty = 2, col = 'red', lwd = 3)
vcov(model)
se <- sqrt(diag(vcov(model)))
se
b1 - qt(0.975, df = n-2) * se[2]
b1 + qt(0.975, df = n-2) * se[2]
c(b1 - qnorm(0.975)*se[2], b1 + qnorm(0.975)*se[2])
confint(model, level = 0.90)
statistic <- (b1 - (-1))/se[2]
statistic
qt(0.025, df = n-2)
2*min(pt(statistic, n-2), 1-pt(statistic, n-2))
predict(model, newdata = data.frame(list(Distance = c(5, 10, 25))), interval = 'prediction')
residual <- residuals(model)
par(mfrow = c(2,2))
hist(residual, probability = TRUE)
plot(residual, pch = 19,cex = 0.5, ylab = 'Residuals')
abline(h=0, lty=2)
plot(estimated, residual, pch = 19, cex = 0.5, xlab = 'Estimated Values', ylab = 'Residuals')
abline(h=0, lty=2)
plot(travel$Distance, residual, ylab = 'Residuals', xlab = 'Cost of the trip', pch = 19, cex = 0.5)
abline(h=0, lty =2)

