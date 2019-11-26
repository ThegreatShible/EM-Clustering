source("../src/em.R")

# simple dataset, only quantitative variables
n = 100
a = c(rep(1, 2*n), rep(2, 2*n))
b = c(rep(3, n), rep(4, n), rep(3, n), rep(4, n))
X = cbind(a, b)
X = as.data.frame(apply(X, 2, factor))
split_by_var_type(X)

clusty = clust(X=X, nbClust=4, models="VVV",  nbInit=1, initMethod="random", epsilon=0.1)
Z = clusty[[1]][[1]]$Z
Z = cbind(Z, apply(Z, 1, which.max))
plot(X[,1], X[,2], col=c("red", "green" ,"blue")[Z[,3]])
