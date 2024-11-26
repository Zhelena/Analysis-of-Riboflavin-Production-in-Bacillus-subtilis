library('Matrix')
library(hdi)
library(glmnet)
library(MASS)


riboflavin2 <- read.csv('riboflavin2_boxcox.csv')

X <- riboflavin2[-1]
y <- riboflavin2$y

num_repeats <- 1

variable_selection_frequency <- matrix(0, nrow = num_repeats, ncol = ncol(X))


alpha_values <- seq(0, 1, by = 0.1)

for (i in 1:num_repeats) {
  set.seed(i)
  train_indices <- sample(1:nrow(X), size = 0.7 * nrow(X))
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]
  
  cv_results <- lapply(alpha_values, function(alpha) {
    cv.glmnet(as.matrix(X_train), y_train, alpha = alpha)
  })
  
  cv_mse <- sapply(cv_results, function(cv) min(cv$cvm))
  best_alpha_index <- which.min(cv_mse)
  best_alpha <- alpha_values[best_alpha_index]
  best_lambda <- cv_results[[best_alpha_index]]$lambda.min
  
  final_model <- glmnet(X_train, y_train, alpha = best_alpha, lambda = best_lambda)
  
  coefs <- coef(final_model, s = best_lambda)
  selected_vars <- as.numeric(coefs[-1] != 0)
  variable_selection_frequency[i, ] <- selected_vars
}


selection_frequency <- colMeans(variable_selection_frequency)
threshold <- 0.5
selected_variables <- which(selection_frequency > threshold)


print(paste("Number of selected variables:", length(selected_variables)))
print("Selected variables indices:")
print(selected_variables)

print(paste("Number of selected variables:", length(selected_variables)))
print("Selected variables indices:")
print(selected_variables)


X_train_selected <- X_train[, selected_variables]
X_test_selected <- X_test[, selected_variables]


final_model <- glmnet(X_train_selected, y_train, alpha = best_alpha, lambda = best_lambda)
y_test_pred <- predict(final_model, newx = X_test_selected)




plot(1:length(y_test), y_test, type = "l", col = "red", lwd = 2, ylab = "Response", xlab = "Index", main = "Elastic Net Predictions vs True Values")
lines(1:length(y_test), y_test_pred, col = "blue", lwd = 2)
legend("topright", legend = c("True", "Predicted"), col = c("red", "blue"), lwd = 2, cex = 0.7)

mse <- mean((y_test - y_test_pred)^2)
mse


sst <- sum((y_test - mean(y_test))^2)
sse <- sum((y_test - y_test_pred)^2)
r_squared <- 1 - (sse / sst)

n <- length(y_train)
p <- length(selected_variables)
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
r_squared
adjusted_r_squared
