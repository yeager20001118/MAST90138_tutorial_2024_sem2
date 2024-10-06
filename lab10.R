# Train Test split first before we fit the model
data(iris)
set.seed(90138)
trainIndex <- sample(1:150, 120)
testIndex <- setdiff(1:150, trainIndex)
trainData <- iris[trainIndex, ]
testData <- iris[testIndex,]

# load libraries
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Fit the model
tree <- rpart(Species~., data = trainData, method = "class")
rpart.plot(tree, type=1)
rpart.plot(tree, type=2)
rpart.plot(tree, type=3)
rpart.plot(tree, type=4, clip.right.labs = F)

sum(predict(tree, testData, type = "class") != testData[, 5])

# Fit the random forest
# install.packages("randomForest")
library(randomForest)
forest <- randomForest(Species~., data = trainData, ntree = 1000, importance = T)
importance(forest)

varImpPlot(forest)

predict(forest)

pred <- predict(forest, testData)
sum(pred != testData[, 5])

# Question 2, implement decision tree on the regression problem
X <- read.csv("Xtrainphoneme.txt", header = F)[,-257]
Xnew <- read.csv("Xtestphoneme.txt", header = F)[,1:256]
Z <- as.numeric(read.csv("Ztrainphoneme.txt", header = F))
Znew <- as.numeric(read.csv("Ztestphoneme.txt", header = F))

XZtrain <- cbind(X, Z)

library(pls)
PLS <- plsr(Z~., data = XZtrain)
PLS$scores
# represent the original data projected on the PLS function space
as.matrix(scale(X, center = T, scale = F)) %*% PLS$projection

PLS$scores - as.matrix(scale(X, center = T, scale = F)) %*% PLS$projection

XZtest <- cbind(Xnew, Znew)
names(XZtest)[257] <- "Z"
XZtest

rgTree <- rpart(Z~., data = XZtrain)
rpart.plot(rgTree)

predict(rgTree, XZtest)
plot(predict(rgTree, XZtest), Znew)
# line(Znew, Znew)

length(unique(predict(rgTree, XZtest)))
