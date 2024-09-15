TrainData <- read.csv("XGtrainTecator.txt", sep = "")
TestData <- read.csv("XGtestTecator.txt", sep = "")

m1 <- lm(G ~ X1 + X2, data = TrainData)
beta0 <- m1$coefficients[1]
beta <- m1$coefficients[2:3]

# Do the prediction
pred <- beta0 + beta[1] * TestData$X1 + beta[2] * TestData$X2
pred[pred >= 0.5] = 1
pred[pred < 0.5] = 0 
pred

train_pred <- beta0 + beta[1] * TrainData$X1 + beta[2] * TrainData$X2
train_pred[train_pred >= 0.5] = 1
train_pred[train_pred < 0.5] = 0 
train_pred

# Draw the decision boundary on training data
plot(TrainData$X1, TrainData$X2, col = TrainData$G + 2, pch = train_pred)
x1 <- seq(min(TrainData$X1), max(TrainData$X1), (max(TrainData$X1) - min(TrainData$X1))/100)
lines(x1, (1/2 - beta0 - x1*beta[1]) / beta[2], type = "l")

# Draw the decision boundary on testing data
plot(TestData$X1, TestData$X2, col = TestData$G + 2, pch = pred)
x1 <- seq(min(TrainData$X1), max(TrainData$X1), (max(TrainData$X1) - min(TrainData$X1))/100)
lines(x1, (1/2 - beta0 - x1*beta[1]) / beta[2], type = "l")

sum(pred != TestData$G)
mean(pred != TestData$G)

# Q2 logistic regression model
m2 <- glm(G ~ X1 + X2, data = TrainData, family = binomial())
beta0 <- m2$coefficients[1]
beta <- m2$coefficients[2:3]

# Do the prediction
pred <- beta0 + beta[1] * TestData$X1 + beta[2] * TestData$X2
pred[pred >= 0] = 1
pred[pred < 0] = 0 
pred

train_pred <- beta0 + beta[1] * TrainData$X1 + beta[2] * TrainData$X2
train_pred[train_pred >= 0.5] = 1
train_pred[train_pred < 0.5] = 0 
train_pred

# Draw the decision boundary on training data
plot(TrainData$X1, TrainData$X2, col = TrainData$G + 2, pch = train_pred)
x1 <- seq(min(TrainData$X1), max(TrainData$X1), (max(TrainData$X1) - min(TrainData$X1))/100)
lines(x1, (1/2 - beta0 - x1*beta[1]) / beta[2], type = "l")

# Draw the decision boundary on testing data
plot(TestData$X1, TestData$X2, col = TestData$G + 2, pch = pred)
x1 <- seq(min(TrainData$X1), max(TrainData$X1), (max(TrainData$X1) - min(TrainData$X1))/100)
lines(x1, (1/2 - beta0 - x1*beta[1]) / beta[2], type = "l")

sum(pred != TestData$G)
mean(pred != TestData$G)
