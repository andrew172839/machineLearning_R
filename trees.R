require(rpart)

train = read.csv('train.csv')
train = train[, 1: 41]
Y = train[, 1]
n = length(Y)
sum(Y) / n
X = as.matrix(log(1 + train[, 2: 41]))
X = scale(X) / sqrt(n - 1)
dat = data.frame(Y, X)

# medium cp
ans = rpart(Y ~ ., data = dat, method = 'class', xval = 5, cp = 0.01)
plot(ans, margin = 0.05)
text(ans, use.n = TRUE)
summary(ans)
plotcp(ans)

# low cp
ans = rpart(Y ~ ., data = dat, method = 'class', xval = 5, cp = 0.005)
plot(ans, margin = 0.05)
text(ans, use.n = TRUE, cex = 0.75)
summary(ans)
plotcp(ans)

# split data
index = sample(1: nrow(X), floor(nrow(X) * 0.3))
xtest = X[index,]
xtrain = X[-index,]
ytest = Y[index]
ytrain = Y[-index]
trainFrameData = data.frame(as.factor(ytrain), xtrain)
names(trainFrameData)[1] = 'ytrain'
trainData = data.frame(ytrain, xtrain)
testData = data.frame(ytest, xtest)

# trees
fit1 = rpart(ytrain ~ ., data = trainData, method = 'class', xval = 5, cp = 0.01)
trainPredict = apply(predict(fit1), 1, which.max) - 1
tree.trerr = sum(abs(ytrain - trainPredict)) / length(ytrain)
testPredict = apply(predict(fit1, newdata = data.frame(xtest)), 1, which.max) - 1
tree.tserr = sum(abs(ytest - testPredict)) / length(ytest)
tree.err = c(tree.trerr, tree.tserr)
tree.err

# logistic regression
fit2 = glm(ytrain ~ ., data = trainData, family = 'binomial')
trainPredict = (sign(predict(fit2)) + 1) / 2
logit.trerr = sum(abs(ytrain - trainPredict)) / length(ytrain)
testPredict = (sign(predict(fit2, newdata = data.frame(xtest))) + 1) / 2
logit.tserr = sum(abs(ytest - testPredict)) / length(ytest)
logit.err = c(logit.trerr, logit.tserr)
logit.err

# adaboost
require(ada)
fit3 = ada(x = xtrain, y = ytrain, loss = 'exponential', type = 'discrete', iter = 150)
ab.trerr = (fit3$confusion[2, 1] + fit3$confusion[1, 2]) / length(ytrain)
plot(fit3)
testPredict = round(predict(fit3, newdata = data.frame(xtest), type = 'probs'))
ab.tserr = sum(abs(ytest - (apply(testPredict, 1, which.max) - 1))) / length(ytest)
ab.err = c(ab.trerr, ab.tserr)
ab.err

# logitboost
fit4 = ada(x = xtrain, y = ytrain, loss = 'logistic', type = 'real', iter = 150)
lb.trerr = (fit4$confusion[2, 1] + fit4$confusion[1, 2]) / length(ytrain)
plot(fit4)
testPredict = round(predict(fit4, newdata = data.frame(xtest), type = 'probs'))
lb.tserr = sum(abs(ytest - (apply(testPredict, 1, which.max) - 1))) / length(ytest)
lb.err = c(lb.trerr, lb.tserr)
lb.err

# gradient boosting
require(xgboost)
fit5 = xgboost(data = xtrain, label = ytrain, max.depth = 2, eta = 1, objective = 'binary:logistic', nround = 100)
trainPredict = round(predict(fit5, newdata = xtrain))
gb.trerr = sum(abs(ytrain - trainPredict)) / length(ytrain)
testPredict = round(predict(fit5, newdata = xtest))
gb.tserr = sum(abs(ytest - testPredict)) / length(ytest)
gb.err = c(gb.trerr, gb.tserr)
gb.err
