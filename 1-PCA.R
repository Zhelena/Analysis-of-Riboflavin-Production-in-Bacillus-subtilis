library('hdi')
riboflavin <- read.csv('riboflavin.csv')
X <- riboflavin[-1]
y <- riboflavin[1]


pca_result <- prcomp(X, scale. = TRUE)
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)
num_components <- 41
cumulative_variance[num_components]
plot(cumulative_variance, type = "b", pch = 19, col = "blue", xlab = "Number of Principal Components", ylab = "Cumulative Variance Explained", main = "Cumulative Variance Explained by Principal Components")
abline(h = 0.95, col = "red", lty = 2)
PC_variables <- list()

for (i in 1:41) {
  PC_index <- which.max(abs(pca_result$rotation[, i]))
  PC_variable <- rownames(pca_result$rotation)[PC_index]
  PC_variables[[paste0("PC", i)]] <- PC_variable
}


selected_columns <- riboflavin[, unlist(PC_variables)]

riboflavin2 <- cbind(y, selected_columns)
write.csv(riboflavin2, file = "riboflavin2.csv", row.names = FALSE)



