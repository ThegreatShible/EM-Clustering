source("./src/em.R")

# simple dataset, only quantitative variables
n = 1000
a = c(rnorm(n, 0, 0.5), rnorm(n, 4, 1), rnorm(n, 1.5, 0.5))
b = c(rnorm(n, 0, 2), rnorm(n, 4, 0.5), rnorm(n, 0, 2))
X = as.data.frame(cbind(a, b))

plot(X)

K=2:4
clusty = clust(X=X, nbClust=K,  nbInit=1, initMethod="kmeans", epsilon=0.5)

plot_result(clusty)
plot(X[,1], X[,2], col=best_model(clusty, best_cluster = T)$Z)


