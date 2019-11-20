library(plot3D)

EM <- function(X, K, nb_init=10) {
  d = ncol(X)
  n = nrow(X)
  
  # TODO : split X into Xc and Xq
  
  theta = init_theta(d, k)
  proba = classify(theta, Xc, Xq)
  
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
classify <- function(thetas, Xc=NULL, Xq=NULL) {
  if (!is.null(Xc)) n = nrow(Xc)
  else {
    if (!is.null(Xq)) n = nrow(Xq)
    else stop("Dataset is empty")
  }
  proba = apply(matrix(seq_along(thetas), nrow=1), 2, function(i) multinomial(Xq, thetas[[i]]$alpha) * thetas[[i]]$p * mdnorm(Xc, thetas[[i]]$mean, thetas[[i]]$sd))
  
  # TODO : To check, but in theory the following line can be deleted
  # It was there to balance probabilities before I put the pk (thetas[[i]]$p) in the line above
  
  # proba = t(apply(proba, 1, function(i) i / sum(i)))
  
  return(proba)
}

# Equivalent of function dnorm but takes as inputs
# a vector of mean and a matrix of standard deviation
mdnorm <- function(X, mean, sd) {
  if (is.null(X)) return(1)
  X = matrix(X, ncol=length(mean))
  p = length(mean)
  inv_sd = solve(sd)
  a = ((2 * pi) ^ (p / 2)) * (det(sd) ^ (1/2))
  b = apply(X, 1, function(x) - (1/2) * (t(x-mean) %*% inv_sd %*% (x-mean)))
  return((1 / a) * exp(b))
}

multinomial <- function(X, alpha) {
  # alpha est un vecteur de taille la somme des nombres de modalités de toutes les variables
  if (is.null(X)) return(1)
  
  nb_modalities = sum(apply(X, 2, function(c) length(levels(factor(c)))))
  len_alpha = length(alpha)
  if (nb_modalities != len_alpha)
    stop(paste(c("Number of modalities (", nb_modalities, ") is different from the number of values in alpha (", len_alpha, ")"), collapse=""))
  
  X_hot = NULL
  p = ncol(X)
  for (j in 1:p) {
    X_hot = cbind(X_hot, one_hot(factor(X[,j])))
  }
  print(X_hot)
  return(apply(X_hot, 1, function(i) prod(alpha ^ i)))
}

one_hot <- function(x) {
  lvl = levels(x)
  len = length(lvl)
  t(apply(matrix(x, ncol=1), 1, function(i) {
    res = rep(0,len)
    res[which(lvl == i)] = 1
    res
  }))
}
