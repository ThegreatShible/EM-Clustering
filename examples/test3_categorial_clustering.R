library(mixtClust)
source("./src/em.R")

# simple dataset, only quantitative variables
n = 100
a = c(rep(1, n), rep(2, n), rep(3, n))
b = c(rnorm(n), rnorm(2*n, 4, 2))
X = data.frame(cbind(a, b))
X$a <- as.factor(X$a)
split_by_var_type(Byar)
K=2:8
clusty = clust(X=X, nbClust=K,  nbInit=20, initMethod="kmeans", epsilon=0.1)
plot_result(clusty)
X$a <- as.numeric(X$a)
plot(X)
plot(X, col=best_model(clusty, "icl", TRUE)$Z)

mm <-mixmodCluster(X,K)
plot(mm)


