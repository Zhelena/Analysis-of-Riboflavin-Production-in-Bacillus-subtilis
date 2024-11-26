library(glmnet)



riboflavin <- read.csv('riboflavin.csv')
x <- riboflavin[-1]
y <- riboflavin$y




model_lasso <- cv.glmnet(as.matrix(x), y, alpha = 1)
print(model_lasso$lambda.min)

coef(model_lasso, s = "lambda.min")
sum(coef(model_lasso, s = "lambda.min") != 0)



prediction_lasso <- predict(model_lasso, newx = as.matrix(riboflavin[, -1]))
print(prediction_lasso)
mse_lasso <- mean((prediction_lasso - riboflavin$y)^2)
mse_lasso

ss_res <- sum((y - prediction_lasso)^2)
ss_tot <- sum((y - mean(y))^2)
r_squared <- 1 - ss_res / ss_tot

n <- length(y)
p <- length(which(coef(model_lasso, s = "lambda.min") != 0)) - 1
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
r_squared
adjusted_r_squared




