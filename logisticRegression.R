#univariate logistic regression - simulated example
#change coefficients to understand logistic function
n = 250
p = 1
x = matrix(rnorm(n * p), n, p)
beta0 = 0
beta = 1
eps = matrix(rnorm(n), n, 1)
probs = exp(beta0 + x * beta + eps) / (1 + exp(beta0 + x * beta + eps))
Y = as.numeric(probs > .5)

#fit logistic model
fit = glm(Y ~ x, family = "binomial")
summary(fit)
plot(x, Y)
xs = seq(min(x), max(x), l = 1000)
pihat = exp(fit$coefficients[1] + xs * fit$coefficients[2]) / (1 + exp(fit$coefficients[1] + xs * fit$coefficients[2]))
lines(xs, pihat)
lines(c(min(x), max(x)), c(.5, .5), lty = 2)

train = read.csv("train.csv", header = FALSE)
train = train[, 1: 41]
Y = train[, 1]
n = length(Y)
X = as.matrix(log(1 + train[, 2: 41]))
X = scale(X) / sqrt(n - 1)
dat = data.frame(Y, X)
sdat = data.frame(Y, dat$V2, dat$V3, dat$V4, dat$V5, dat$V6, dat$V7, dat$V8, dat$V9, dat$V10)

fits = glm(Y~., data = sdat, family = "binomial", maxit = 50)
summary(fits)

fit = glm(Y~dat.V2, data = sdat, family = "binomial")
summary(fit)
plot(sdat$dat.V2, sdat$Y)
pihat = exp(fit$coefficients[1] + xs * fit$coefficients[2]) / (1 + exp(fit$coefficients[1] + xs * fit$coefficients[2]))
lines(xs, pihat)
lines(c(min(xs), max(xs)), c(.5, .5), lty = 2)

fit = glm(Y~dat.V10, data = sdat, family = "binomial")
summary(fit)
plot(sdat$dat.V10, sdat$Y)
pihat = exp(fit$coefficients[1] + xs * fit$coefficients[2]) / (1 + exp(fit$coefficients[1] + xs * fit$coefficients[2]))
lines(xs, pihat)
lines(c(min(xs), max(xs)), c(.5, .5), lty = 2)
