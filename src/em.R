library(plot3D)

EM <- function(X, K, nb_init=10) {
  d = ncol(X)
  n = nrow(X)
  
  # TODO : split X into Xc and Xq
  
  theta = create_theta(d, k)
  proba = classify(theta, Xc, Xq)
  
  # E-step
  # Store all the likelihoods in a N x k matrix

}

# Returns a list of length k
# Each element contains a vector of mean, a matrix of variance,
# a vector alphas of length the total number of modalities and
# a propobability pk
create_theta <- function(dq, dc, k) {
  # dq : Dimension of quantitative variables
  # dc : Number of modalities of all categorial variables
  m = rep(NA,dq)
  s = matrix(NA, dq, dq)
  p = NA
  a = rep(NA, dc)
  t = list(mean=m, sd=s, p=p, alpha=a)
  theta = rep(list(t), k)
  return(theta)
}

# Returns matrices of size n x k of probabilities
# of each element belonging to each class
E_Step <- function(thetas, Xc=NULL, Xq=NULL) {
  if (!is.null(Xc)) n = nrow(Xc)
  else {
    if (!is.null(Xq)) n = nrow(Xq)
    else stop("Dataset is empty")
  }
  proba = apply(matrix(seq_along(thetas), nrow=1), 2, function(i) multinomial(Xq, thetas[[i]]$alpha) * thetas[[i]]$p * mdnorm(Xc, thetas[[i]]$mean, thetas[[i]]$sd))
  proba = t(apply(proba, 1, function(i) i / sum(i)))
  
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

get_nb_modalities <- function(X) {
  if (is.null(X)) return(0)
  return(sum(apply(X, 2, function(c) length(levels(factor(c))))))
}

multinomial <- function(X, alpha) {
  # alpha est un vecteur de taille la somme des nombres de modalit?s de toutes les variables
  if (is.null(X)) return(1)
  
  nb_modalities = get_nb_modalities(X)
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

#TODO
M_Step <- function(Xc, Xq, Z, model){
  
  # Temporary : To be moved to VVV model
  
  for (k in seq_along(ncol(Z))) {
    tk = Z[,k]
    nk = sum(tk)
    pk = nk / n
    mean_k = apply(Xc * tk, 2, sum) / nk
    X_centered = apply(Xc, 1, function(i) i - mean_k)
    sd_k = sum(tk * apply(X_centered, 1, function(i) sum(i^2))) / nk
    theta[[k]]$p = pk
    theta[[k]]$mean = mean_k
    theta[[k]]$sd = sd_k
  }
    
  if(!is.null(Xc)) {
    alphas_sum = rowsum(Xc)
    alphas = alphas_sum/ r 
  }
  
}


clust <- function(X, nbClust, models,  nbInit, initMethod, epsilon){
  newX = splitByVarType(X)
  Xc = newX$cat
  Xq = newX$quant
  if(is.numeric(nbClust)) nbClusts = 1:nbClust
  else nbClusts = nbClust
  i  = 0
  for(model in models) {
    for (K in nbClusts){
      thetas_0 = init_thetas(Xc, Xq, initMethod, nbInit, K)
      best_likelihood = -Inf
      best_theta = NULL
      for(theta_0 in thetas_0){
        em = EM(Xc, Xq, theta_0, model, epsilon)
        if(em$likelihood > best_likelihood){
          best_likelihood = em$likelihood
          best_em = em
        }
      }
      bic = BIC(Xq, model, best_likelihood,K)
      icl = ICL(bic, best_em$Z)
      res_i = list(model=model, nbClusters=K, theta=best_em$theta, bic=bic, icl=icl, Z=best_em$Z)
      res[[i]] = res_i
      i = i+1
    }
  }
  return(res)
}

BIC <- function(Xq, model, likelihood, K){
  n <- nrow(Xq)
  nb_par = getNbParameters(Xq, model)
  return(-2*likelihood + nb_par* log(n))
}
getNbParameters <- function(Xq, model, K) {
  nbUs <- ncol(Xq)
  res = K + K*nbUs
  nbVar = 0
  if(model == "EII"){
    nbVar=1
  }else if (model == "VII") nbVar = K
  else if (model == "EEI") nbVar = nbUs
  else if (model == "VEI") nbVar = nbUs + K -1
  else if (model == "EVI") nbVar = 1 + K*(nbUs-1)
  else if (model == "VVI") nbVar =K*nbUs
  else if (model == "EEE") nbVar =nbUs * (nbUs+1)/2
  else if (model == "VEE" ) nbVar = K + (nbUs+2)*(nbUs-1)/2
  else if(model == "EVE") nbVar = 1 + (nbUs + 2*K)(nbUs-1)/2
  else if (model =="EEV") nbVar = 1 + (nbUs-1) + K*(nbUs*(nbUs-1)/2)
  else if (model == "VVE") nbVar = K + (nbUs+2*K)*(nbUs-1)/2
  else if (model =="EVV") nbVar = 1 + K*(nbUs + 2)*(nbUs-1)/2
  else if (model == "VEV") nbVar = K+ (nbUs-1)+ K*(nbUs*(nbUs-1)/2)
  else nbVar = K*(nbUs*(nbUs+1)/2)
  return (res+nbVar)
}
ICL <- function(bic, Z){
  log_z = log(Z)
  e_m <- sum((Z*log_z))
  return(bic + e_m )
}

splitByVarType <- function(X) {
  
}

init_thetas <- function(Xc, Xq, initMethod, nbInit, K){
  
}

EM <- function(Xc, Xq, theta_0, model, epsilon){
  last_likelihood = -Inf
  current_likelihood= -Inf
  theta = theta_0
  repeat{
    last_likelihood = current_likelihood
    Z <- E_Step(Xc, Xq, theta, model)
    new_theta = M_step(Xc, Xq, Z, model)
    current_likelihood = processLikelihood(Xc, Xq, Z, theta)
    theta = new_theta
    if(current_likelihood - last_likelihood < epsilon )
      break
  }
  res = list(likelihood= current_likelihood, Z=Z, theta= new_theta)
  return(res)
}

processlikelihood <- function(Xc, Xq, Z, theta){
  Q = 0
  for (k in 1:K) {
    # Refer to slide 68/90 for math detail
    
    # Refer to slide 59/90 for math detail
    # tk = Z[,k]
    # nk = sum(tk)
    # pk = nk / n
    mean_k = theta[[k]]$mean
    # mean_k = apply(Xc * tk, 2, sum) / nk
    # X_centered = apply(Xc, 1, function(i) i - mean_k)
    sd_k = theta[[k]]$sd
    # sd_k = sum(tk * apply(X_centered, 1, function(i) sum(i^2))) / nk
    # inv_sd_k = solve(sd_k)
    # det_sd_k = det(sd_k)
    alpha_k = theta[[k]]$alpha
    
    Q = Q + sum(apply(X_centered, 1, function(i) {
      #log(pk) - p * log(2 * pi) / 2 - log(det_sd_k) / 2 - (t(i) %*% inv_sd_k %*% i)
      fk_q = mdnorm(Xq, mean_k, sd_k)
      fk_c = multinomial(Xc, alpha_k)
      z[i,k] * (log(fk_q) + log(fk_c))
    }))
  }
  return(Q)
}

