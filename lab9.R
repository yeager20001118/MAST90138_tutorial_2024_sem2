TrainData <- read.csv("XGtrainTecator.txt", sep = "")
TestData <- read.csv("XGtestTecator.txt", sep = "")

# Use the training data to constrcut LDA and QDA
library(MASS)
m1 <- lda(G~X1+X2, data = TrainData)
m2 <- qda(G~X1+X2, data = TrainData)

# prediction
pred.lda <- predict(m1, TestData)$class
pred.qda <- predict(m2, TestData)$class

# Confusion matrix: 2x2 matrix depends class number
table(pred.lda, TestData$G)
table(pred.qda, TestData$G)

# Question 2
# Train Test split first before we fit the model
data(iris)
set.seed(90138)
trainIndex <- sample(1:150, 120)
testIndex <- setdiff(1:150, trainIndex)
trainData <- iris[trainIndex, ]
testData <- iris[testIndex,]

# Fit the training data on multiclass logistic regression model
m3 <- nnet::multinom(Species~., data = trainData)

predict(m3, testData)
predict(m3, testData, type = "prob")

outputs <- summary(m3)

betaK1 <- outputs$coefficients[1,]
betaK2 <- outputs$coefficients[2,]
betaK1

logitsK1 <- exp(betaK1[1] + betaK1[2] * testData[,1] + betaK1[3] * testData[,2] + betaK1[4] * testData[,3] + betaK1[5] * testData[,4])
logitsK2 <- exp(betaK2[1] + betaK2[2] * testData[,1] + betaK2[3] * testData[,2] + betaK2[4] * testData[,3] + betaK2[5] * testData[,4])

pK1 <- logitsK1 / (1 + logitsK1 + logitsK2)
pK2 <- logitsK2 / (1 + logitsK1 + logitsK2)
pK3 <- 1 - pK1 - pK2

predict(m3, testData, type="probs")
