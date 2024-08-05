# Task1
A <- matrix(c(1,1,2,1,0,1,0,2,2), nrow = 3, byrow = T)
A

(B <- matrix(c(-1, 1, 3, 5, 1, 2), nrow = 3))

At <- t(A)
At

diagA <- diag(A)
diagA

AtA <- At %*% A
AtA

AtB <- At %*% B
AtB

(D <- A^3)
E <- A %*% A %*% A
E

dim(A)

traceA <- sum(diag(A))
traceA

det(A)

EE <- eigen(A)
EE

Evalues <- round(EE$values, digits = 4)
Evalues

lambda1 <- Evalues[1]
lambda2 <- Evalues[2]
lambda3 <- Evalues[3]

Evectors <- EE$vectors
Evectors

v1 <- Evectors[,1]
v1
v2 <- Evectors[,2]
v2
v3 <- Evectors[,3]
v3

library("Matrix")
rankMatrix(A)

# Task2
Data <- read.csv("google_review_ratings.txt")
X <- as.matrix(Data[,2:25])

colMeans(X)
cov(X)
cor(X)

pairs(X[,1:10], pch = ".")
