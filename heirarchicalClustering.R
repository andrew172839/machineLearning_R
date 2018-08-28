# hierarchical clustering
train = read.csv("train.csv")
train = train[, 1: 41]
X = as.matrix(train[, -1]);
dim(X)
unique(colnames(X))

# complete linakge, euclidean distance
cols = as.numeric(as.factor(colnames(X)))
dmat = dist(t(X))
complete.hclust = hclust(dmat, method = "complete")
plot(complete.hclust, main = "complete linkage")

# single linakge
single.hclust = hclust(dmat, method = 'single')
plot(single.hclust, main = 'single linkage')

# average linakge
average.hclust = hclust(dmat, method = "average")
plot(average.hclust, main = "average linkage")

# ward's linakge
ward.hclust = hclust(dmat, method = "ward.D")
plot(ward.hclust, main = "ward's linkage")

# complete linkage with different distances
dmat = dist(t(X), method = "manhattan")
complete.hclust = hclust(dmat, method = "complete")
plot(complete.hclust, main = "complete linkage - l1")
