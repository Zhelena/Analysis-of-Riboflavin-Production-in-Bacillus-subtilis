library(glmnet)


riboflavin <- read.csv('riboflavin2_boxcox.csv')
x <- riboflavin[-1]
y <- riboflavin$y



alpha_values <- seq(0, 1, by = 0.1)  # alpha
cv_results <- lapply(alpha_values, function(alpha) {
  cv.glmnet(as.matrix(x), y, alpha = alpha)
})

# best alpha and best lambda
cv_mse <- sapply(cv_results, function(cv) min(cv$cvm))
best_alpha_index <- which.min(cv_mse)
best_alpha <- alpha_values[best_alpha_index]
best_lambda <- cv_results[[best_alpha_index]]$lambda.min
model_elastic <- glmnet(x, y, alpha = best_alpha, lambda = best_lambda)
elastic_net_coefficients <- coef(model_elastic)
num_non_zero <- sum(elastic_net_coefficients != 0)

prediction_elastic <- predict(model_elastic, newx = as.matrix(riboflavin[, -1]))
print(prediction_elastic)
mse_elastic <- mean((prediction_elastic - riboflavin$y)^2)
mse_elastic

ss_res <- sum((y - prediction_elastic)^2)
ss_tot <- sum((y - mean(y))^2)
r_squared <- 1 - ss_res / ss_tot

n <- length(y)
p <- length(which(coef(model_elastic, s = "lambda.min") != 0)) - 1
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
r_squared
adjusted_r_squared




