library(mda)

riboflavin2_boxcox <- read.csv('riboflavin2_boxcox.csv')
y <- riboflavin2_boxcox[,1]
x <- riboflavin2_boxcox[,-1]


mars_model <-  mars(x, y, degree = 2)
summary(mars_model)
# prediction
predicted <- predict(mars_model, x)
mars_mse  <- mean((riboflavin2_boxcox[,1]-predicted)^2)

mars_mse



ss_res <- sum((y - predicted)^2)
ss_tot <- sum((y - mean(y))^2)
r_squared <- 1 - ss_res / ss_tot

n <- length(y)
p <- length(mars_model$selected.terms) - 1
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
r_squared
adjusted_r_squared


