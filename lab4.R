data(iris)

colnames(iris)

plot(iris[,1:4])

plot(iris[,1:4], col=c(2,3,4)[iris$Species], main = "Iris data")
# title("Iris data")

X <- as.matrix(iris[,1:4])
n <- nrow(X)
X_centered <- X - matrix(rep(1, n), nrow = n) %*% colMeans(X)
X_centered

Sigma <- cov(X)
EE <- eigen(Sigma)

G <- EE$vectors
lambda <- EE$values

Y1 <- G[,1]
PC1 <- X %*% Y1
PC1

PCX <- X %*% G
PCX

fracvar <- lambda / sum(lambda)
fracvar

plot(PC1, rep(0, n), col=iris$Species, xlab = "PC1", yaxt="n")
legend("topright", legend = unique(iris$Species), fill = c(1,2,3))

plot(PC1, PCX[,2], col=iris$Species, xlab = "PC1", ylab = "PC2")
legend("topright", legend = unique(iris$Species), fill = c(1,2,3))
