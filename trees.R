require(rpart)

train = read.csv("train.csv")
train = train[, 1: 41]
Y = train[, 1]
n = length(Y)
sum(Y) / n
X = as.matrix(log(1 + train[, 2: 41]))
X = scale(X) / sqrt(n - 1)
dat = data.frame(Y, X)

#medium cp
ans = rpart(Y ~ ., data = dat, method = "class", xval = 5, cp = .01)
plot(ans, margin = .05)
text(ans, use.n = TRUE)
summary(ans)
plotcp(ans)

#low cp
ans = rpart(Y ~ ., data = dat, method = "class", xval = 5, cp = .005)
plot(ans, margin = .05)
text(ans, use.n = TRUE, cex = .75)
summary(ans)
plotcp(ans)

#splitting data into training and testing
ind = sample(1: nrow(X), floor(nrow(X) * .3))
xts = X[ind,]
xtr = X[-ind,]
yts = Y[ind]
ytr = Y[-ind]
trfdat = data.frame(as.factor(ytr), xtr)
names(trfdat)[1] = "ytr"
trdat = data.frame(ytr, xtr)
tsdat = data.frame(yts, xts)

#trees
fit1 = rpart(ytr ~ ., data = trdat, method = "class", xval = 5, cp = .01)
trpred = apply(predict(fit1), 1, which.max) - 1
tree.trerr = sum(abs(ytr - trpred)) / length(ytr)
tspred = apply(predict(fit1, newdata = data.frame(xts)), 1, which.max) - 1
tree.tserr = sum(abs(yts - tspred)) / length(yts)
tree.err = c(tree.trerr, tree.tserr)
tree.err

#logistic regression
fit2 = glm(ytr ~ ., data = trdat, family = "binomial")
trpred = (sign(predict(fit2)) + 1) / 2
logit.trerr = sum(abs(ytr - trpred)) / length(ytr)
tspred = (sign(predict(fit2, newdata = data.frame(xts))) + 1) / 2
logit.tserr = sum(abs(yts - tspred)) / length(yts)
logit.err = c(logit.trerr, logit.tserr)
logit.err

#adaboost
require(ada)
fit3 = ada(x = xtr, y = ytr, loss = "exponential", type = "discrete", iter = 150)
ab.trerr = (fit3$confusion[2, 1] + fit3$confusion[1, 2]) / length(ytr)
plot(fit3)
tspred = round(predict(fit3, newdata = data.frame(xts), type = "probs"))
ab.tserr = sum(abs(yts - (apply(tspred, 1, which.max) - 1))) / length(yts)
ab.err = c(ab.trerr, ab.tserr)
ab.err

#logitboost
fit4 = ada(x = xtr, y = ytr, loss = "logistic", type = "real", iter = 150)
lb.trerr = (fit4$confusion[2, 1] + fit4$confusion[1, 2]) / length(ytr)
plot(fit4)
tspred = round(predict(fit4, newdata = data.frame(xts), type = "probs"))
lb.tserr = sum(abs(yts - (apply(tspred, 1, which.max) - 1))) / length(yts)
lb.err = c(lb.trerr, lb.tserr)
lb.err

#gradient boosting
require(xgboost)
fit5 = xgboost(data = xtr, label = ytr, max.depth = 2, eta = 1, objective = "binary:logistic", nround = 100)
trpred = round(predict(fit5, newdata = xtr))
gb.trerr = sum(abs(ytr - trpred)) / length(ytr)
tspred = round(predict(fit5, newdata = xts))
gb.tserr = sum(abs(yts - tspred)) / length(yts)
gb.err = c(gb.trerr, gb.tserr)
gb.err
