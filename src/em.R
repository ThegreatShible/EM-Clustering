library(plot3D)

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
    X_centered = apply(X, 1, function(i) i - mean_k)
    sd_k = sum(tk * apply(X_centered, 1, function(i) sum(i^2))) / nk
    inv_sd_k = solve(sd_k)
    det_sd_k = det(sd_k)
    
    Q = Q + sum(apply(X_centered, 1, function(i) log(pk) - p * log(2 * pi) / 2 - log(det_sd_k) / 2 - (t(i) %*% inv_sd_k %*% i)))
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