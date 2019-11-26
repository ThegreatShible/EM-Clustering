source("../src/em.R")

# simple dataset, only quantitative variables
n = 1000
a = c(rnorm(n, 0, 0.5), rnorm(n, 4, 1))
b = c(rnorm(n, 0, 2), rnorm(n, 4, 0.5))
X = as.data.frame(cbind(a, b))

plot(X)

K=4
clusty = clust(X=X, nbClust=1:K, models="VVV",  nbInit=2, initMethod="random", epsilon=0.5)

for( c in clusty[[1]]) {
  print(c$icl)
}
Z = clusty[[1]][[1]]$Z
Z = cbind(Z, apply(Z, 1, which.max))
#plot(X[,1], X[,2], col=c("red", "green" ,"blue")[Z[,nbClust+1]])

plot_result(clusty)
