library(MASS)
library(ISLR)
library(car)

fix(Boston)
names(Boston)

lm.fit <- lm(medv~lstat, data=Boston)
summary(lm.fit)
names(lm.fit)
coefficients(lm.fit)
confint(lm.fit, level=0.99)

x <- data.frame(lstat=c(5,10,15))
predict(lm.fit, x, interval = "confidence", level = 0.99)  #置信区间
predict(lm.fit, x, interval = "prediction")  #预测区间

plot(Boston$lstat, Boston$medv)
abline(lm.fit, col="red", lwd=3)
par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit <- lm(medv~lstat+age, data=Boston)
summary(lm.fit)
lm.fit <- lm(medv~., data=Boston)
summary(lm.fit)
vif(lm.fit)

lm.fit <- lm(medv~.-age, data=Boston)
summary(lm.fit)
lm.fit <- update(lm.fit, ~.-indus)

lm.fit <- lm(medv~lstat*age-age, data=Boston)
summary(lm.fit)
lm.fit1 <- lm(medv~lstat + I(lstat^2), data=Boston)
summary(lm.fit1)
lm.fit2 <- lm(medv~lstat, data=Boston)
anova(lm.fit1, lm.fit2)

plot(lm.fit1)

lm.fit3 <- lm(medv~poly(lstat, 5), data=Boston)
summary(lm.fit3)
summary(lm(medv~log(rm)+poly(lstat, 5), data=Boston))

#下面使用Carseats数据
fix(Carseats)
names(Carseats)

lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)
