library(glmnet)
library(ncvreg)

train = read.csv('train.csv')
train = train[, 1: 41]

# l1 regularization
Y = as.numeric(train[, 1])
Y = Y - mean(Y)
X = as.matrix(train[, -1])
X = scale(X, center = T, scale = T)

fit0 = lm(Y ~ X - 1)
lam = 1
fitl = glmnet(x = X, y = Y, family = 'gaussian', lambda = lam, alpha = 1)
cbind(fit0$coef, as.matrix(fitl$beta))

# lasso paths
fitl = glmnet(x = X, y = Y, family = 'gaussian', alpha = 1)
plot(fitl, col = 1: 40)
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40), cex = .40)

# ridge, lasso, adaptive lasso, elastic net, scad, mcp
lam = 1
betals = solve(t(X) %*% X) %*% t(X) %*% Y
fitl = glmnet(x = X, y = Y, family = 'gaussian', lambda = lam, alpha = 1)
fital = glmnet(x = X, y = Y, family = 'gaussian', lambda = lam, alpha = 1, penalty.factor = 1 / abs(betals))
fiten = glmnet(x = X, y = Y, family = 'gaussian', lambda = lam, alpha = 0.5)
fitscad = ncvreg(X, Y, family = 'gaussian', penalty = 'SCAD', lambda = lam)
fitmcp = ncvreg(X, Y, family = 'gaussian', penalty = 'MCP', lambda = lam)

# regualrization paths
par(mfrow = c(2, 3))
par(mar = c(5, 4, 3, 2))
betals = solve(t(X) %*% X) %*% t(X) %*% Y
lambdas = exp(seq(log(0.01), log(100 * nrow(train)), l = 50))
plot(c(1, length(lambdas)), range(betals), type = 'n', ylab = 'coefs', xlab = 'lambda index', main = 'ridge')
legend(0, 4, legend = names(train)[2: 41], col = 1: 41, lty = rep(1, 41))

fitl = glmnet(x = X, y = Y, family = 'gaussian', alpha = 1)
plot(fitl, col = 1: 40, main = 'lasso')
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40))

fiten = glmnet(x = X, y = Y, family = 'gaussian', alpha = 0.5)
plot(fiten, col = 1: 8, main = 'en alpha = 0.5')
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40))

fiten = glmnet(x = X, y = Y, family = 'gaussian', alpha = 0.25)
plot(fiten, col = 1: 8, main = 'en alpha = 0.25')
legend(0, 4, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40))

fitscad = ncvreg(X, Y, family = 'gaussian', penalty = 'SCAD')
plot(fitscad, col = 1: 8, main = 'scad')
legend(6, 6, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40))

fitmcp = ncvreg(X, Y, family = 'gaussian', penalty = 'MCP')
plot(fitmcp, col = 1: 8, main = 'mcp')
legend(6, 6, legend = names(train)[2: 41], col = 1: 40, lty = rep(1, 40))
