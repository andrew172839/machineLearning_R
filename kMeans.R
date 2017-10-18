#k-means
n = 300
mu1 = c(3, 3)
mu2 = c(7, 4)
mu3 = c(6, 5.5) 
Sig = matrix(c(1, .5, .5, 1), 2, 2)
x1 = t(matrix(mu1, 2, n / 3)) + matrix(rnorm(n), n / 3, 2)
xx = matrix(rnorm(n * 2 / 3), n / 3, 2)
x2 = t(matrix(mu2, 2, n / 3)) + xx %*% chol(Sig)
xx = matrix(rnorm(n * 2 / 3), n / 3, 2)
x3 = t(matrix(mu3, 2, n / 3)) + xx %*% chol(Sig)
x = rbind(x1, x2, x3)
Y = c(rep(1, n / 3), rep(2, n / 3), rep(3, n / 3))
y = factor(Y)

plot(x[, 1], x[, 2], col = as.numeric(y), pch = 16)

#try changing k
k = 3
km = kmeans(x, centers = k)
plot(x[, 1], x[, 2], col = km$cluster, pch = 16)
cens = km$centers
points(cens[, 1], cens[, 2], col = 1: k, pch = 16, cex = 3)

#code to understand k-means algorithm
require("animation")
mv.kmeans = function(x, k, cens = NULL)
{
	n = nrow(x)
	if (is.null(cens))
	{
		cens = x[sample(1: n, k),]
	}
	plot(x[, 1], x[, 2], pch = 16)
	points(cens[, 1], cens[, 2], col = 1: k, pch = 16, cex = 3)
	thr = 1e-6
	ind = 1
	iter = 1
	while (ind > thr)
	{
		oldcen = cens
		km = kmeans(x, centers = cens, iter.max = 1, nstart = 1, algorithm = "MacQueen")
		plot(x[, 1], x[, 2], col = km$cluster, pch = 16)
		points(cens[, 1], cens[, 2], col = 1: k, pch = 16, cex = 3)
		cens = km$centers
		print(cens)
		plot(x[, 1], x[, 2], col = km$cluster, pch = 16)
		points(cens[, 1], cens[, 2], col = 1: k, pch = 16, cex = 3)
		ind = sum(diag((oldcen - cens) %*% t(oldcen - cens)))
		print(ind)
	} 
}

train = read.csv("train.csv")
train = train[, 1: 41]
X = as.matrix(train[, -1]);

#apply k-means
K = 9
km = kmeans(t(X), centers = K)

#pca - take svd to get solution
sv = svd(X)
U = sv$u
V = sv$v
D = sv$d
Z = X %*% V

plot(Z[, 1], Z[, 2], col = km$cluster, type = "n")
text(Z[, 1], Z[, 2], colnames(X), cex = .75, col = km$cluster)
cens = km$centers

#re-run and see if solution changes
K = 9
km = kmeans(t(X), centers = K)
plot(Z[, 1], Z[, 2], col = km$cluster, type = "n")
text(Z[, 1], Z[, 2], colnames(X), cex = .75, col = km$cluster)
cens = km$centers

#try different k
K = 5
km = kmeans(t(X), centers = K)
plot(Z[, 1], Z[, 2], col = km$cluster, type = "n")
text(Z[, 1], Z[, 2], colnames(X), cex = .75, col = km$cluster)
cens = km$centers
