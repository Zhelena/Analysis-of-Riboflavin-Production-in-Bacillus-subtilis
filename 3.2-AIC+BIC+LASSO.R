riboflavin2_boxcox <- read.csv('riboflavin2_boxcox.csv')
y <- riboflavin2_boxcox[,1]
x <- riboflavin2_boxcox[,-1]



### AIC
library(MASS)
modelAIC <- stepAIC(model2, direction = 'both')
prediction_AIC <- predict(modelAIC, newx = x)
AIC_mse <- mean((prediction_AIC - riboflavin2_boxcox$y)^2)
AIC_mse

plot(y, type = "l", col = "red", lwd = 2, main = "Actual vs Predicted (AIC)",
     xlab = "Index", ylab = "y")
lines(y_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lwd = 2)

summary(modelAIC)


### BIC
modelBIC <- stepAIC(model2, direction = 'both', k = log(nrow(model.frame(model2))))
prediction_BIC <- predict(modelBIC, newdata = riboflavin2_boxcox)
BIC_mse <- mean((prediction_BIC - y)^2)
BIC_mse
summary(modelBIC)


### LASSO
library(glmnet)

lasso_model <- cv.glmnet(as.matrix(x), y, alpha = 1)
best_lambda <- lasso_model$lambda.min
final_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
lasso_coefficients <- coef(final_lasso_model)

# predict
y_pred <- predict(final_lasso_model, newx = as.matrix(x))
LASSO_mse <- mean((y - y_pred)^2)
LASSO_mse


ss_res <- sum((y - y_pred)^2)
ss_tot <- sum((y - mean(y))^2)
r_squared <- 1 - ss_res / ss_tot

n <- length(y)
lasso_coef <- coef(final_lasso_model, s = best_lambda)
p <- sum(lasso_coef != 0) - 1
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))

r_squared
adjusted_r_squared


plot(y, type = "l", col = "red", lwd = 2, main = "Actual vs Predicted (LASSO)",
     xlab = "Index", ylab = "y")
lines(y_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lwd = 2)


