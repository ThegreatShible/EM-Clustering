source("../src/em.R")

# simple dataset, only quantitative variables
n = 100
a = c(rep(1, 2*n), rep(2, 2*n))
b = c(rep(3, n), rep(4, n), rep(3, n), rep(4, n))
X = cbind(a, b)
X = as.data.frame(apply(X, 2, factor))
split_by_var_type(X)

K=2
clusty = clust(X=X, nbClust=K, models="VVV",  nbInit=20, initMethod="random", epsilon=0.01)
Z = clusty[[1]][[1]]$Z
Z = cbind(Z, apply(Z, 1, which.max))
Res <- as.matrix(X)
class(Res) <- "numeric"
plot(Res[,"a"], Res[,"b"], col=Z[,(max(K)+1)])
unique(Z[,K+1])