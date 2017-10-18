#hierarchical clustering
train = read.csv("train.csv")
train = train[, 1: 41]
X = as.matrix(train[, -1]);
dim(X)
unique(colnames(X))

#complete linakge - euclidean distance
cols = as.numeric(as.factor(colnames(X)))
Dmat = dist(t(X))
com.hclust = hclust(Dmat, method = "complete")
plot(com.hclust, cex = .7, main = "Complete Linkage")

#single linakge
dev.new()
sing.hclust = hclust(Dmat, method = "single")
plot(sing.hclust, cex = .7, main = "Single Linkage")

#average linakge
dev.new()
ave.hclust = hclust(Dmat, method = "average")
plot(ave.hclust, cex = .7, main = "Average Linkage")

#ward's linakge
dev.new()
ward.hclust = hclust(Dmat, method = "ward.D")
plot(ward.hclust, cex = .7, main = "Ward's Linkage")

#complete linkage with different distances
dev.new()
Dmat = dist(t(X), method = "manhattan")
com.hclust = hclust(Dmat, method = "complete")
plot(com.hclust, cex = .7, main = "Complete Linkage - L1 Dist")
