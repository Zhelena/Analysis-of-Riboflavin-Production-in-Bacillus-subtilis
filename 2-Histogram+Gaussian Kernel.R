library(sm)
library(MASS)


riboflavin <- read.csv('riboflavin2.csv')
x <- riboflavin[-1]
y <- riboflavin[1]





hcv_result <- sm.density(y, hcv = TRUE)
optimal_bandwidth <- hcv_result$h

hist(riboflavin$y, breaks = "FD", freq = FALSE, main = "Histogram of y with Density Curve",
     xlab = "y", col = "#7FFFD4", border = "black")
density_estimate <- sm.density(y, h = optimal_bandwidth, display = "none")
lines(density_estimate$eval.points, density_estimate$estimate, col = "#FF69B4", lwd = 2)
legend("topleft", legend = c("Gaussian Kernel"), col = c("#FF69B4"), lwd = 2, cex = 0.7)


###################
##### Box-Cox #####
###################
y <- as.matrix(y)


min_y <- min(y)
if (min_y <= 0) {
  y <- y - min_y + 1
}

boxcox_result <- boxcox(y ~ 1, lambda = seq(-2, 2, by = 0.1))
best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
print(paste("Best lambda:", best_lambda))
if (best_lambda == 0) {
  y_boxcox <- log(y)
} else {
  y_boxcox <- (y^best_lambda - 1) / best_lambda
}

#############################
### New Kernel Estimation ###
#############################
riboflavin2_boxcox <- read.csv('riboflavin2_boxcox.csv')
x <- riboflavin2_boxcox[-1]
y <- riboflavin2_boxcox[1]



hcv_result <- sm.density(y, hcv = TRUE)
optimal_bandwidth <- hcv_result$h


hist(riboflavin2_boxcox$y, breaks = "FD", freq = FALSE, main = "Histogram of y with Density Curve",
     xlab = "y", col = "#7FFFD4", border = "black")
density_estimate <- sm.density(y, h = optimal_bandwidth, display = "none")
lines(density_estimate$eval.points, density_estimate$estimate, col = "#FF69B4", lwd = 2)
legend("topleft", legend = c("Gaussian Kernel"), col = c("#FF69B4"), lwd = 2, cex = 0.7)


