# k means
n = 300
mu1 = c(3, 3)
mu2 = c(7, 4)
mu3 = c(6, 5.5) 
sigma = matrix(c(1, .5, .5, 1), 2, 2)
x1 = t(matrix(mu1, 2, n / 3)) + matrix(rnorm(n), n / 3, 2)
xx = matrix(rnorm(n * 2 / 3), n / 3, 2)
x2 = t(matrix(mu2, 2, n / 3)) + xx %*% chol(sigma)
xx = matrix(rnorm(n * 2 / 3), n / 3, 2)
x3 = t(matrix(mu3, 2, n / 3)) + xx %*% chol(sigma)
x = rbind(x1, x2, x3)
Y = c(rep(1, n / 3), rep(2, n / 3), rep(3, n / 3))
y = factor(Y)
plot(x[, 1], x[, 2], col = as.numeric(y))

k = 3
km = kmeans(x, centers = k)
plot(x[, 1], x[, 2], col = km$cluster)
centers = km$centers
points(centers[, 1], centers[, 2], col = 1: k, cex = 3)

require("animation")
kmeans = function(x, k, centers = NULL)
{
	n = nrow(x)
	if (is.null(centers))
	{
		centers = x[sample(1: n, k),]
	}
	plot(x[, 1], x[, 2])
	points(centers[, 1], centers[, 2], col = 1: k, cex = 3)
	thr = 1e-6
	index = 1
	iter = 1
	while (index > thr)
	{
		oldcenter = centers
		km = kmeans(x, centers = centers, iter.max = 1, nstart = 1, algorithm = 'macqueen')
		plot(x[, 1], x[, 2], col = km$cluster)
		points(centers[, 1], centers[, 2], col = 1: k, cex = 3)
		centers = km$centers
		print(centers)
		plot(x[, 1], x[, 2], col = km$cluster)
		points(centers[, 1], centers[, 2], col = 1: k, cex = 3)
		index = sum(diag((oldcenter - centers) %*% t(oldcenter - centers)))
		print(index)
	} 
}

train = read.csv("train.csv")
train = train[, 1: 41]
X = as.matrix(train[, -1]);

k = 9
km = kmeans(t(X), centers = k)

# pca
sv = svd(X)
U = sv$u
V = sv$v
D = sv$d
Z = X %*% V

plot(Z[, 1], Z[, 2], col = km$cluster)
text(Z[, 1], Z[, 2], colnames(X), cex = 0.75, col = km$cluster)
centers = km$centers

# re-run
k = 9
km = kmeans(t(X), centers = k)
plot(Z[, 1], Z[, 2], col = km$cluster)
text(Z[, 1], Z[, 2], colnames(X), cex = 0.75, col = km$cluster)
centers = km$centers

k = 5
km = kmeans(t(X), centers = k)
plot(Z[, 1], Z[, 2], col = km$cluster)
text(Z[, 1], Z[, 2], colnames(X), cex = 0.75, col = km$cluster)
centers = km$centers
