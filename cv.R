require(glmnet)
require(Hmisc)

#model complexity
set.seed(9001)
n = 100
p = 50
Btrue = matrix(0,  p,  1)
Btrue[1:  10] = rnorm(10) * 1; 
Xtr = scale(matrix(rnorm(n * p),  n,  p))
Ytr = Xtr %*% Btrue + matrix(rnorm(n),  n,  1)
Xts = scale(matrix(rnorm(n * p),  n,  p))
Yts = Xts %*% Btrue + matrix(rnorm(n),  n,  1)

fit = glmnet(x = Xtr, y = Ytr, family = "gaussian", standardize = FALSE, nlambda = 50, lambda.min.ratio = .001)
Yhtr = predict(fit, newx = Xtr)
MSEtr = apply((Yhtr - Ytr %*% matrix(1, 1, length(fit$lambda))) ^ 2, 2, mean)
Yhts = predict(fit, newx = Xts)
MSEts = apply((Yhts - Yts %*% matrix(1, 1, length(fit$lambda))) ^ 2, 2, mean)

plot(1: length(fit$lambda), MSEtr, type = "l", col = 4, xlab = "sparsity", ylab = "error")
lines(1: length(fit$lambda), MSEtr, col = 4)
lines(1: length(fit$lambda), MSEts, col = 2)
legend(30, 10, legend = c("training error", "test error"), col = c(4, 2), lty = c(1, 1), cex = .75)

t(t(fit$beta[, 15]))

#cv
fold = 5
sam = sample(1: n, n)
CVerrs = NULL
for(i in 1: fold)
{
	ind = sam[((i - 1) * n / fold + 1): (i * n / fold)]
	Xin = Xtr[-ind, ]
	Yin = Ytr[-ind]
	Xout = Xtr[ind, ]
	Yout = Ytr[ind]
	fit = glmnet(x = Xin, y = Yin, family = "gaussian", standardize = FALSE, nlambda = 50, lambda.min.ratio = .001)
	Yh = predict(fit, newx = Xout)
	CVerrs = cbind(CVerrs, apply((Yh - Yout %*% matrix(1, 1, length(fit$lambda))) ^ 2, 2, mean))
}
CVerr = apply(CVerrs, 1, mean)

#minimum cv error rule
lines(1: length(fit$lambda), CVerr, col = 1)
optlam = fit$lambda[which.min(CVerr)]

#one se rule
SE = sqrt(apply(CVerrs, 1, var) / fold)
errbar(1: length(fit$lambda), CVerr, CVerr+SE, CVerr-SE, add = TRUE)
minSE = CVerr[which.min(CVerr)] + SE[which.min(CVerr)]
minSEx = which(CVerr < minSE)[1]
optlam = fit$lambda[minSEx]

#refit to training data at optimal lambda
fit = glmnet(x = Xtr, y = Ytr, family = "gaussian", standardize = FALSE, lambda = optlam)
Yhtr = predict(fit, newx = Xtr)
TRerr = mean( (Yhtr - Ytr) ^ 2)
Yhts = predict(fit, newx = Xts)
TSerr = mean( (Yhts - Yts) ^ 2)
optlam
sum(fit$beta != 0)
TRerr
TSerr
