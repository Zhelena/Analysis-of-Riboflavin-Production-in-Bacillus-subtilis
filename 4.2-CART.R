library(tree)

riboflavin2_boxcox <- read.csv('riboflavin2_boxcox.csv')
y <- riboflavin2_boxcox[,1]
x <- riboflavin2_boxcox[,-1]

riboflavin2_boxcox <- as.data.frame(riboflavin2_boxcox)



tree_model <- tree(y ~ .,data = riboflavin2_boxcox)
plot(tree_model)
text(tree_model)


# prediction
predicted <- predict(tree_model, as.data.frame(x))
tree_mse  <- mean((riboflavin2_boxcox[,1]-predicted)^2)
tree_mse

plot(y, type = "l", col = "red", lwd = 2, main = "Actual vs Predicted (Regression Tree)")
lines(predicted, col = "blue", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lwd = 2)




ss_res <- sum((y - predicted)^2)
ss_tot <- sum((y - mean(y))^2)
r_squared <- 1 - ss_res / ss_tot

n <- length(y)
p <- length(unique(tree_model$frame$var[tree_model$frame$var != "<leaf>"]))
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))

r_squared
adjusted_r_squared

