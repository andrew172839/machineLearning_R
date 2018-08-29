train = read.csv("train.csv")
train = train[, 1: 41]

# least squares
Y = as.numeric(train[, 1])
X = cbind(rep(1, nrow(train)), as.matrix(train[, -1]))

# coefficients
betahat = solve(t(X) %*% X) %*% t(X) %*% Y

# fitted data
Yhat = X %*% betahat

# hat matrix
H = X %*% solve(t(X) %*% X) %*% t(X)
evH = eigen(H)
evH$values[1: 10]

# linear model
fit = lm(data = train)
summary(fit)

cbind(betahat, t(t(fit$coefficients)))

layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(fit)
dev.off()
