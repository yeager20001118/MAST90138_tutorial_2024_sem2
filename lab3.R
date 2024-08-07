sigma1 <- matrix(c(1,0,0,2), nrow = 2, byrow = T)
sigma2 <- matrix(c(5,1,1,2), nrow = 2, byrow = T)
sigma3 <- matrix(c(5,2,2,2), nrow = 2, byrow = T)
sigma4 <- matrix(c(5,2,2,1), nrow = 2, byrow = T)

mu <- c(0, 0)

n <- 200

library(MASS)
sample1 <- mvrnorm(n, mu, sigma1)
sample2 <- mvrnorm(n, mu, sigma2)
sample3 <- mvrnorm(n, mu, sigma3)
sample4 <- mvrnorm(n, mu, sigma4)

par(mfrow = c(2, 2))

plot(sample1, pch = "*", col = "red", xlab = "X1", ylab = "X2", asp = 1)
plot(sample2, pch = "*", col = "red", xlab = "X1", ylab = "X2", asp = 1)
plot(sample3, pch = "*", col = "red", xlab = "X1", ylab = "X2", asp = 1)
plot(sample4, pch = "*", col = "red", xlab = "X1", ylab = "X2", asp = 1)
