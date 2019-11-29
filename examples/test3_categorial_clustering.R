source("./src/em.R")

# simple dataset, only quantitative variables
n = 100
a = c(rep(1, 2*n), rep(2, 2*n))
b = c(rep(3, n), rep(4, n), rep(3, n), rep(4, n))
X = cbind(a, b)
X = as.data.frame(apply(X, 2, factor))
split_by_var_type(X)
K=2:4
clusty = clust(X=X, nbClust=K,  nbInit=20, initMethod="random", epsilon=0.1)
plot_result(clusty)
plot(X, best_model(clusty, "icl", TRUE)$Z)
