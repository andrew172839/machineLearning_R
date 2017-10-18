train = read.csv("train.csv")
train = train[, 1: 41]

#directly fit least squares
Y = as.numeric(train[, 1])
aX = cbind(rep(1, nrow(train)), as.matrix(train[, -1]))

#estimated coefficients
betahat = solve(t(aX) %*% aX) %*% t(aX) %*% Y

#fitted data
Yhat = aX %*% betahat

#hat matrix
H = aX %*% solve(t(aX) %*% aX) %*% t(aX)
evH = eigen(H)
evH$values[1: 10]

#fit linear model
fit = lm(data = train)
summary(fit)

cbind(betahat, t(t(fit$coefficients)))

#diagnostic plots
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(fit)
dev.off()
