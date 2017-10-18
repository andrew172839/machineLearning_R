library(glmnet)
library(ncvreg)

train = read.csv("train.csv")
train = train[, 1: 41]

#l1 regularization
Y = as.numeric(train[, 1])
Y = Y - mean(Y)
X = as.matrix(train[, -1])
X = scale(X, center = T, scale = T)

fit0 = lm(Y ~ X - 1)
lam = 1
fitl = glmnet(x = X, y = Y, family = "gaussian", lambda = lam, alpha = 1)
cbind(fit0$coef, as.matrix(fitl$beta))

#lasso paths
fitl = glmnet(x = X, y = Y, family = "gaussian", alpha = 1)
plot(fitl, col = 1: 40)
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40), cex = .40)

#least squares, lasso, adaptive lasso, scad, ridge, elastic net, mc+
lam = 1
betals = solve(t(X)%*%X)%*%t(X)%*%Y
fitl = glmnet(x = X, y = Y, family = "gaussian", lambda = lam, alpha = 1)
fital = glmnet(x = X, y = Y, family = "gaussian", lambda = lam, alpha = 1, penalty.factor = 1 / abs(betals))
fitel = glmnet(x = X, y = Y, family = "gaussian", lambda = lam, alpha = .5)
fitscad = ncvreg(X, Y, family = "gaussian", penalty = "SCAD", lambda = lam)
fitmcp = ncvreg(X, Y, family = "gaussian", penalty = "MCP", lambda = lam)

#compare ridge, lasso, elastic net & scad regualrization paths
par(mfrow = c(2, 3))
par(mar = c(5, 4, 3, 2))
betals = solve(t(X)%*%X)%*%t(X)%*%Y
lambdas = exp(seq(log(.01), log(100*nrow(train)), l = 50))
plot(c(1, length(lambdas)), range(betals), type = "n", ylab = "Coefficients", xlab = "Lambda Index", main = "Ridge")
legend(0, 4, legend = names(train)[2: 41], col = 1: 41, lty = rep(1, 41), cex = .75)

fitl = glmnet(x = X, y = Y, family = "gaussian", alpha = 1)
plot(fitl, col = 1: 40, main = "Lasso")
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40), cex = .75)

fitel = glmnet(x = X, y = Y, family = "gaussian", alpha = .5)
plot(fitel, col = 1: 8, main = "EL alpha = .5")
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40), cex = .75)

fitel = glmnet(x = X, y = Y, family = "gaussian", alpha = .25)
plot(fitel, col = 1: 8, main = "EL alpha = .25")
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40), cex = .75)

fitscad = ncvreg(X, Y, family = "gaussian", penalty = "SCAD")
plot(fitscad, col = 1: 8, main = "SCAD", shade = F)
legend(6, 6, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40), cex = .75)

fitmcp = ncvreg(X, Y, family = "gaussian", penalty = "MCP")
plot(fitmcp, col = 1: 8, main = "MC+", shade = F)
legend(6, 6, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40), cex = .75)
