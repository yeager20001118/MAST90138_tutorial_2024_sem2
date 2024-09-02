X <- as.matrix(read.csv("Xtrainphoneme.txt", header = F))[,-257]
Xnew <- as.matrix(read.csv("Xtestphoneme.txt", header = F))[,1:256]
Z <- as.numeric(read.csv("Ztrainphoneme.txt", header = F))
Znew <- as.numeric(read.csv("Ztestphoneme.txt", header = F))

nnew <- 1417

Xcent <- scale(X, scale = F)
Zcent <- Z - mean(Z)
Xbar <- matrix(rep(colMeans(X), nnew), nrow = nnew, byrow = T)

# Predict Z by standard least square estimator
betahat <- solve(t(Xcent) %*% Xcent, t(Xcent) %*% Zcent)
betahat

Zls <- mean(Z) + (Xnew - Xbar) %*% betahat

# Predict Z by first q PCs, where q from 2 to 10
PCX <- prcomp(X)
Sigma <- PCX$rotation
Y <- PCX$x

Zpc <- c()
for (q in 2:10) {
  Yq <- Y[,1:q]
  betapc <- solve(t(Yq) %*% Yq) %*% t(Yq) %*% Zcent
  Zpc <- cbind(Zpc, mean(Z) + ((Xnew - Xbar) %*% Sigma)[,1:q] %*% betapc)
}

# Calculate errors and log squared errors for each prediction
log_sq_error_list = list()
error_list = list()
for(i in 1:9){
  log_sq_error_list[[i]] = log((Zpc[,i]-Znew)^2)
  error_list[[i]] = (Zpc[,i]-Znew)
}
log_sq_error_list[[10]] = log((Zls-Znew)^2)
error_list[[10]] = Zls-Znew

# Plot boxplot to see the mean, min, max and quantiles points for each predictions
par(mfrow=c(1,2))
boxplot(log_sq_error_list, ylim=c(-20,20),
        names = c(2:10,"LS"))
boxplot(error_list,ylim=c(-100,100),
        names = c(2:10,"LS"))

# Plot scatterplot to see the relationship between predictions and true output
plot(Znew,Zls)
abline(0,1)
plot(Znew,Zpc[,9])
abline(0,1)