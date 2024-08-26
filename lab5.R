data("iris")

PCX = prcomp(iris[,1:4])

S <- PCX$rotation
S

lambda <- PCX$sdev^2
lambda

Y <- PCX$x
Y

screeplot(PCX, type = "lines")

phis <- cumsum(lambda) / sum(lambda)
phis

PCX = prcomp(iris[,1:4], scale. = T)
X <- PCX$x
(vec <- PCX$rotation)

diag(cov(iris[,1:4]))

new_PCX = prcomp(iris[,1:4], scale. = T)
new_PCX$rotation
new_PCX$sdev^2 / sum(new_PCX$sdev^2)

plot(X[,1], X[,2], col = iris$Species, xlab = "PC1", ylab = "PC2")

