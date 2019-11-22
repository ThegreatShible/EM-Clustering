source("../src/em.R")

# simple dataset, only quantitative variables
n = 1000
a = c(rnorm(n, 0, 0.5), rnorm(n, 4, 1))
b = c(rnorm(n, 0, 2), rnorm(n, 4, 0.5))
X = cbind(a, b)

plot(X)

clust(X, 2, "VVV", 10, "kmeans", 0.1)
