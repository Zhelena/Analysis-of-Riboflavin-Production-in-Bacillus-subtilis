library(dplyr)
library(forecast)
library(MASS)
library(caret)
library(BSDA)
library(UsingR)
library(fmsb)
library(car)
library(lmtest)



riboflavin2_boxcox <- read.csv('riboflavin2_boxcox.csv')
y <- riboflavin2_boxcox[,1]
x <- riboflavin2_boxcox[,-1]

shapiro.test(riboflavin2_boxcox$y)



model <- lm(y ~ .-x.NADB_at.1, data = riboflavin2_boxcox)
summary(model)

# 残差线性
# 残差是常数 -> homoscedasticity
scatter.smooth(resid(model) ~ fitted(model), col = "grey", las = 1,
               ylab = "Standardized residuals", xlab = "Fitted values")
title('Residuals vs Fitted value')
abline(h = 0, col = "red", lty = 2)



# Breusch-Pagan 检验
bptest(model)



# Multicollinearity
vif(model)

model11 <- lm(y ~ .-x.NADB_at.1-x.YVFO_at, data = riboflavin2_boxcox)
model12 <- lm(y ~ .-x.NADB_at.1-x.FFH_at, data = riboflavin2_boxcox)
model13 <- lm(y ~ .-x.NADB_at.1-x.DPPD_at, data = riboflavin2_boxcox)
model14 <- lm(y ~ .-x.NADB_at.1-x.YVFK_at, data = riboflavin2_boxcox)



model21 <- lm(y ~ .-x.NADB_at.1-x.YVFO_at-x.FFH_at, data = riboflavin2_boxcox)
model22 <- lm(y ~ .-x.NADB_at.1-x.DPPD_at-x.YVFO_at, data = riboflavin2_boxcox)
model23 <- lm(y ~ .-x.NADB_at.1-x.FFH_at-x.DPPD_at, data = riboflavin2_boxcox)
model24 <- lm(y ~ .-x.NADB_at.1-x.YVFK_at-x.DPPD_at, data = riboflavin2_boxcox)
model25 <- lm(y ~ .-x.NADB_at.1-x.YVFK_at-x.FFH_at, data = riboflavin2_boxcox)
model26 <- lm(y ~ .-x.NADB_at.1-x.YVFK_at-x.YVFO_at, data = riboflavin2_boxcox)



model2 <- lm(y ~ .-x.NADB_at.1-x.YVFK_at-x.FFH_at, data = riboflavin2_boxcox)
vif(model2)

scatter.smooth(resid(model2) ~ fitted(model2), col = "grey", las = 1,
               ylab = "Standardized residuals", xlab = "Fitted values")
title('Residuals vs Fitted value')
abline(h = 0, col = "red", lty = 2)

# Normality
shapiro.test(riboflavin2_boxcox$y)
qqnorm(resid(model2))
qqline(resid(model2), col = 2)

# prediction
predicted <- predict(model2, x)
lm_mse <- mean((riboflavin2_boxcox[,1]-predicted)^2)
lm_mse
summary(model2)


