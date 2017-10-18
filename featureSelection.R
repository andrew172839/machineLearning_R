library(MASS)
library(leaps)
library(glmnet)

train = read.csv("train.csv")
train = train[, 1: 41]

#feature selection in linear models
#best subsets selection
fitbsub = regsubsets(x = train[, 2: 41], y = train[, 1])
summary(fitbsub)

#backwards step-wise - via bic
fit = lm(data = train)
fitb = stepAIC(fit, direction = "backward", data = train, k = log(nrow(train)))
summary(fitb)

#l1 regularization
Y = as.numeric(train[, 1]);
Y = Y - mean(Y)
X = as.matrix(train[, -1]);
X = scale(X, center = T, scale = T)

fit0 = lm(Y ~ X - 1)
lam = 1
fitl = glmnet(x = X, y = Y, family = "gaussian", lambda = lam, alpha = 1)
cbind(fit0$coef, as.matrix(fitl$beta))

#lasso paths
fitl = glmnet(x = X, y = Y, family = "gaussian", alpha = 1)
plot(fitl, col = 1: 40)
legend(0, 4, legend = names(train)[2: 41], col = 1: 41, lty = rep(1, 41), cex = .41)
