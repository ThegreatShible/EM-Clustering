source("./src/em.R")

# simple dataset, only quantitative variables
n = 1000
a = c(rnorm(n, 0, 0.5), rnorm(n, 4, 1))
b = c(rnorm(n, 0, 2), rnorm(n, 4, 0.5))
X = as.data.frame(cbind(a, b))

plot(X)

K=4
clusty = clust(X=X, nbClust=c(2,4,6),  nbInit=10, initMethod="kmeans", epsilon=0.5)

Z = clusty[[1]][[1]]$Z
Z = cbind(Z, apply(Z, 1, which.max))
plot(X[,1], X[,2], col=Z[,K+1])

plot_result(clusty)
