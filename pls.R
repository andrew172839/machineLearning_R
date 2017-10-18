train = read.csv("train.csv")
train = train[, 1: 41]
Y = as.numeric(train[, 1])
Y = Y - mean(Y)
X = as.matrix(train[, -1])
X = scale(X, center = T, scale = T)
lam = nrow(train)

#ridge regression coefficient paths
lambdas = exp(seq(log(.01), log(100 * nrow(train)), l = 100))
betasr = matrix(0, length(lambdas), 40)
plot(c(1, length(lambdas)), range(betasr), type = "n", ylab = "Coefficients", xlab = "Lambda Index")
for (j in 1: 40)
{
	lines(betasr[length(lambdas): 1, j], col = j)
}
legend(0, 4, legend = names(train)[2: 41], col = 1: 41, lty = rep(1, 41))

#looking at principal components
svdx = svd(X)

#scatterplots of samples pcs
par(mar = c(1, 1, 1, 1))
layout(matrix(1: 25, 5, 5))
mycols = rainbow(length(Y))
orY = order(Y)
for (i in 1: 5)
{
	for (j in 1: 5)
	{
		plot(svdx$u[, i], svdx$u[, j], type = "p", pch = 16, col = mycols[orY])
	}
}

#amount of variance explained
varex = 0
cumvarex = 0
for (i in 1: 40)
{
	varex[i] = svdx$d[i] / sum(svdx$d)
	cumvarex[i] = sum(varex)
}
par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2))
barplot(varex, ylab = "Amount of Var Explained", xlab = "PCs")
barplot(cumvarex, ylab = "Cummulative Var Explained", xlab = "PCs")

#pc direction weights
par(mfrow = c(3, 2))
par(mar = c(5, 4, 3, 2))
for (i in 1: 6)
{
	barplot(svdx$v[, i], names.arg = names(train)[2: 41])
}

#ridge paths again
dev.off()
plot(c(1, length(lambdas)), range(betasr), type = "n", ylab = "Coefficients", xlab = "Lambda Index")
for (j in 1: 40)
{
	lines(betasr[length(lambdas): 1, j], col = j)
}
legend(0, 4, legend = names(train)[2: 41], col = 1: 41, lty = rep(1, 41))

#least squares on derived inputs
#pc regression
betapcr = diag(svdx$d) %*% t(svdx$u) %*% Y / nrow(train)

#pls regression
plsfunc = function(x, y)
{
	p = ncol(x)
	n = nrow(x)
	M = t(x) %*% y
	Z = NULL
	V = NULL
	P = NULL
	for (k in 1: p)
	{
		svdm = svd(M)
		z = x %*% svdm$u
		z = z * as.numeric(1 / sqrt(t(z) %*% z))
		V = cbind(V, svdm$u)
		p = t(x) %*% z / as.numeric(t(z) %*% z)
		P = cbind(P, p)
		Z = cbind(Z, z)
		M = M - P %*% solve(t(P) %*% P) %*% t(P) %*% M
	}
	return(list(Z = Z, V = V))
}
plsx = plsfunc(X, Y)

#scatterplots of pls components
par(mar = c(1, 1, 1, 1))
layout(matrix(1: 25, 5, 5))
mycols = rainbow(length(Y))
orY = order(Y)
for (i in 1: 5)
{
	for (j in 1: 5)
	{
		plot(plsx$Z[, i], plsx$Z[, j], type = "p", pch = 16, col = mycols[orY])
	}
}
betapls = t(plsx$Z) %*% Y / nrow(train)
cbind(betapcr, betapls)
