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
for (q in 2:20) {
  Yq <- Y[,1:q]
  betapc <- solve(t(Yq) %*% Yq) %*% t(Yq) %*% Zcent
  Zpc <- cbind(Zpc, mean(Z) + ((Xnew - Xbar) %*% Sigma)[,1:q] %*% betapc)
}

# Calculate errors and log squared errors for each prediction
log_sq_error_list = list()
error_list = list()
for(i in 1:19){
  log_sq_error_list[[i]] = log((Zpc[,i]-Znew)^2)
  error_list[[i]] = (Zpc[,i]-Znew)
}
log_sq_error_list[[20]] = log((Zls-Znew)^2)
error_list[[20]] = Zls-Znew

# Plot boxplot to see the mean, min, max and quantiles points for each predictions
par(mfrow=c(1,2))
boxplot(log_sq_error_list, ylim=c(-20,20),
        names = c(2:20,"LS"))
boxplot(error_list,ylim=c(-100,100),
        names = c(2:20,"LS"))

# Plot scatterplot to see the relationship between predictions and true output
plot(Znew,Zls)
abline(0,1)

plot(Znew,Zpc[,9])
abline(0,1)
plot(Znew,Zpc[,13])
abline(0,1)

# Q4
# Do the cross-validation on cross-validation criterion
loss = rep(0, 20)
for (k in 2:20) {
  for (i in 1:300) { # choose ith row data as the validation set
    Ycv <- Y[-i,1:k] # Ycv contains all other 299 training data except the ith data 
    betapcCV <- solve(t(Ycv) %*% Ycv, t(Ycv) %*% Zcent[-i])
    # first compute the \hat{Z_[c, i]}, compute the difference between \hat{Z_[c, i]} and Z_[c, i], and accumulate the squared error into the total loss for each q value
    loss[k] <- loss[k] + (Zcent[i] - Y[i, 1:k] %*% betapcCV)^2 
  }
}
loss = loss[-1]
bestq <- which.min(loss) + 1
bestq

Zpc[, bestq]

Yq <- Y[,1:bestq]
betapcbest <- solve(t(Yq) %*% Yq) %*% t(Yq) %*% Zcent
Zpcbest <- mean(Z) + ((Xnew - Xbar) %*% Sigma)[,1:bestq] %*% betapcbest

# Work on the criterion of residual sum of square (RSS)
loss = rep(0, 40)
for (k in 2:40) {
  Ypc <- Y[,1:k] # Ycv contains all other 299 training data except the ith data 
  x <- solve(t(Ypc) %*% Ypc, t(Ypc) %*% Zcent)
  for (i in 1:300) { # choose ith row data as the validation set
    # first compute the \hat{Z_[c, i]}, compute the difference between \hat{Z_[c, i]} and Z_[c, i], and accumulate the squared error into the total loss for each q value
    loss[k] <- loss[k] + (Zcent[i] - Y[i, 1:k] %*% betapc)^2 
  }
}
loss = loss[-1]
bestq <- which.min(loss) + 1
bestq
