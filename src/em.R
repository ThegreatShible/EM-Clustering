library(plot3D)

# This functions generates a dataset according to 2 gaussian laws
# and then gives conditionnal probabilities that elements belong
# to each class with classify function.
test_function_1 <- function() {
  
  n = 50
  
  theta = list(
    list(
      mean=c(0,0,0),
      sd=diag(c(1,1,1))
    ),
    list(
      mean=c(2,2,2),
      sd=diag(c(1,1,1))
    )
  )
  
  X = cbind(
    c(rnorm(n, theta[[1]]$mean, theta[[1]]$sd[1,1]), rnorm(n, theta[[2]]$mean, theta[[2]]$sd[1,1])),
    c(rnorm(n, theta[[1]]$mean, theta[[1]]$sd[2,2]), rnorm(n, theta[[2]]$mean, theta[[2]]$sd[2,2])),
    c(rnorm(n, theta[[1]]$mean, theta[[1]]$sd[3,3]), rnorm(n, theta[[2]]$mean, theta[[2]]$sd[3,3]))
  )
  y = c(rep(1,n), rep(2,n))
  
  points3D(X[,1], X[,2], X[,3], col=c("red", "blue")[y])
  
  classify(X, theta)
}

EM <- function(X, K, nb_init=10) {
  d = ncol(X)
  n = nrow(X)
  
  theta = init_theta(d, k)
  proba = classify(X, theta)
  
  # E-step
  # Store all the likelihoods in a N x k amtrix
  Q = 0
  for (k in 1:K) {
    # Refer to slide 68/90 for math detail
    tk = proba[,k]
    nk = sum(tk)
    pk = nk / n
    mean_k = apply(X * tk, 2, sum) / nk
    centered = apply(X, 1, function(i) i - mean_k)
    sd_k = sum(tk * apply(centered, 1, function(i) sum(i^2))) / nk
  }
}

# Returns a list of length k
# Each element contains a vector of mean and a matrix of variance
init_theta <- function(d, k) {
  m = rep(0,d)
  s = matrix(0, d, d)
  t = list(mean=m, sd=s)
  theta = rep(list(t), k)
  return(theta)
}

# Returns matrices of size n x k of probabilities
# of each element belonging to each class
classify <- function(X, theta) {
  n = nrow(X)
  d = ncol(X)
  k = length(theta)
  proba = apply(matrix(1:k, nrow=1), 2, function(i) mdnorm(X, theta[[i]]$mean, theta[[i]]$sd))
  proba = t(apply(proba, 1, function(i) i / sum(i)))
  return(proba)
}

# Equivalent of function dnorm but takes as inputs
# a vector of mean and a matrix of standard deviation
mdnorm <- function(X, mean, sd) {
  X = matrix(X, ncol=length(mean))
  p = length(mean)
  inv_sd = solve(sd)
  a = ((2 * pi) ^ (p / 2)) * (det(sd) ^ (1/2))
  b = apply(X, 1, function(x) - (1/2) * (t(x-mean) %*% inv_sd %*% (x-mean)))
  return((1 / a) * exp(b))
}

# Plots a 2D gaussian law representation in 3D
visualize_2d_gaussian_law <- function(n=5000) {
  X = cbind(runif(n,-3,3), runif(n,-3,3))
  y = mdnorm(X, c(0,0), diag(c(1,1)))
  points3D(X[,1], X[,2], y)
}