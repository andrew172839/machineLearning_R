train = read.csv('train.csv')
train = train[, 1: 41]
X = as.matrix(train[, -1])

# pca
svd = svd(X)
U = svd$u
V = svd$v
D = svd$d
Z = X %*% V

# pcs
par(mfrow = c(1, 1))
plot(Z[, 2], Z[, 3])
pX = X %*% V[, 1: 25]

# ica
require('fastICA')
k = 15
icafit = fastICA(t(X), n.comp = k)
