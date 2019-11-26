library(plot3D)

GK = 0
GI= 0
EMtmp <- function(X, K, nb_init=10) {
  d = ncol(X)
  n = nrow(X)
  

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
  one_theta = list()
  if(!dq == 0){
    m = rep(NA,dq)
    s = matrix(NA, dq, dq)
    one_theta$mean = m
    one_theta$sd = s
  }
  if(!dc == 0){
    a = rep(NA, dc)
    one_theta$alpha=a
  }
  p = NA
  one_theta$p = p
  theta = rep(list(one_theta), k)
  return(theta)
}

#Process f(x|K) with K being the cluster and Xc and Xq matrices
#Returns fc : f(Xc), fq : f(Xq), f: f(Xc::Xq)
fK <- function(Xc, Xq, alphas, mean, sd) {
  fXc = 1
  fXq = 1
  if(!is.null(Xc) && !ncol(Xc) == 0){
    fXc = multinomial2(Xc, alphas)
  }else if(!is.null(Xq) && !ncol(Xq) == 0) {
    fXq = mdnorm2(Xq, mean = mean, sd = sd)
  }
  return(list(fc = fXc, fq=fXq, f=fXc*fXq))
}

#Process f(X|K)*Pk with K being the cluster, for each K
#Returns a matrix of N*K with N = number of observations and K number of clusters
all_fK <- function(Xc, Xq, thetas,res="f"){
  if(res=="fq"){
    return(sapply(thetas, function(theta) fK(Xc,Xq, theta$alpha, theta$mean, theta$sd)$fq))
  }else if(res=="fc") {
    return(sapply(thetas, function(theta) fK(Xc,Xq, theta$alpha, theta$mean, theta$sd)$fc))
  }else {
    return(sapply(thetas, function(theta) fK(Xc,Xq, theta$alpha, theta$mean, theta$sd)$f))
  }
}




# Returns matrices of size n x k of probabilities
# of each element belonging to each class
E_Step <- function(thetas, Xc=NULL, Xq=NULL, model="VVV") {
  if (!is.null(Xq)) n = nrow(Xq)
  else {
    if (!is.null(Xc)) n = nrow(Xc)
    else stop("Dataset is empty")
  }
  proba = apply(matrix(seq_along(thetas), nrow=1), 2, function(i) multinomial(Xc, thetas[[i]]$alpha) * thetas[[i]]$p * mdnorm(Xq, thetas[[i]]$mean, thetas[[i]]$sd))
  proba = t(apply(proba, 1, function(i) i / sum(i)))
  
  return(proba)
}
E_Step2 <- function(thetas, Xc=NULL, Xq=NULL, model="VVV") {
  Z_temp = all_fK(Xc=Xc, Xq=Xq, thetas=thetas)
  Ps = sapply(thetas, function(theta) theta$p)
  Z_temp = sweep(Z_temp, 2, Ps, "*")
  Z = Z_temp / rowSums(Z_temp)
  zeros = (Z == 0)
  Z = replace(Z, zeros, .Machine$double.xmin)
  Z
}

# Equivalent of function dnorm but takes as inputs
# a vector of mean and a matrix of standard deviation
#TODO verify
mdnorm <- function(X, mean, sd) {
  if (ncol(X) == 0) return(1)
  #X = matrix(X, ncol=length(mean))
  p = length(mean)
  inv_sd = solve(sd)
  a = ((2 * pi) ^ (p / 2)) * (det(sd) ^ (1/2))
  b = apply(X, 1, function(x) - (1/2) * (t(x-mean) %*% inv_sd %*% (x-mean)))
  return((1 / a) * exp(b))
}

mdnorm2 <- function(X, mean,sd) {
  if (ncol(X) == 0) return(1)
  #X = matrix(X, ncol=length(mean))
  p = length(mean)
  inv_sd = solve(sd)
  a = ((2 * pi) ^ (p / 2)) * (det(sd) ^ (1/2))
  reduced_X = as.matrix(sweep(X, 2, mean))
  b = -(1/2) * rowSums((reduced_X %*% inv_sd)* reduced_X)
  return((1 / a) * exp(b))
}


get_nb_modalities <- function(X) {
  if (is.null(X) || ncol(X) == 0) return(0)
  return(apply(X, 2, function(c) length(levels(factor(c)))))
}

#Xc is assumed to be onehot encoded (it is one observation)
multinomial2 <- function(Xc, alphas){
  n <- nrow(Xc)
  alphaMat <- matrix(rep(alphas,each=n), nrow=n)
  powMat <- alphaMat^Xc
  apply(powMat, 1, function(line) prod(line))
}
  
multinomial <- function(X, alpha) {
  # alpha est un vecteur de taille la somme des nombres de modalit?s de toutes les variables
  n = nrow(X)
  if (ncol(X) == 0) return(rep(1, n))
  
  nb_modalities = sum(get_nb_modalities(X))
  len_alpha = length(alpha)
  if (nb_modalities != len_alpha)
    stop(paste(c("Number of modalities (", nb_modalities, ") is different from the number of values in alpha (", len_alpha, ")"), collapse=""))
  
  X_hot = matrix(NA, nrow=n, ncol=0)
  p = ncol(X)
  for (j in seq(p)) {
    X_hot = cbind(X_hot, one_hot(factor(X[,j])))
  }
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

#TODO : check if quanti and quali are non null
M_step <- function(Xc, Xq, Z, model){
  
  # Temporary : To be moved to VVV model
  K = ncol(Z)
  n = nrow(Xc)
  nqCol = ncol(Xq)
  ncCol = ncol(Xc)
  theta = create_theta(ncol(Xq), ncol(Xc), K)
  for (k in seq(K)) {
    tk = Z[,k]
    nk = sum(tk)
    norm_tk = tk/nk
    pk = nk / n
    if(!nqCol == 0){
      mean_k = apply(Xq * norm_tk, 2, sum)
      X_centered = t(apply(Xq, 1, function(i) i - mean_k))
      sd_k = matrix(0, nrow=nqCol, ncol=nqCol)
      for (i in (1: n)) {
        mat = norm_tk[i] * X_centered[i,] %*% t(X_centered[i,])
        sd_k = sd_k + mat
      }
      theta[[k]]$mean = mean_k
      theta[[k]]$sd = sd_k
    }
    if(!ncol(Xc) == 0) {
      theta[[k]]$alpha = colSums(tk * Xc) / sum(tk)
    }
    
    theta[[k]]$p = pk
  }
  return(theta)
}


clust <- function(X, nbClust, models,  nbInit, initMethod, epsilon){
  if(is.null(X) || nrow(X)==0 || ncol(X) == 0)
    stop("Error : Empty dataset")
  newX = split_by_var_type(X)
  Xc = newX$Xc
  Xq = newX$Xq
  modalities = newX$modalities
  #if(is.numeric(nbClust)) nbClusts = 2:nbClust
  #else nbClusts = nbClust
  
  # res is a list for all models
  res = list()
  i  = 1
  for(model in models) {
    j = 1
    # A particular model is a sub list for all clusters
    res[[i]] = list()
    for (K in nbClust){
      GK <-  GK+1
      print(GK)
      thetas_0 = init_thetas(Xc, Xq, initMethod, nbInit, K, modalities)
      best_likelihood = -Inf
      best_theta = NULL
      tmp = 0
      for(theta_0 in thetas_0){
        tmp = tmp +1
        assign("GI", tmp, envir = .GlobalEnv)
        em = EM(Xc, Xq, theta_0, model, epsilon)
        if(em$likelihood > best_likelihood){
          best_likelihood = em$likelihood
          best_em = em
        }
      }
      bic = BIC(Xq,Xc, model, best_likelihood,K)
      icl = ICL(bic, best_em$Z)
      res_i = list(model=model, nbClusters=K, theta=best_em$theta, 
                   bic=bic, icl=icl, 
                   Z=best_em$Z)
      res[[i]][[j]] = res_i
      j = j+1
    }
    i = i+1
  }
  return(res)
}

BIC <- function(Xq, Xc, model, likelihood, K){
  if(!is.null(Xq) && ncol(Xq) != 0)
    n <- nrow(Xq)
  else 
    n <- nrow(Xc)
  nb_par = getNbParameters(Xq, Xc, model, K)
  return(2*likelihood - nb_par* log(n))
}
getNbParameters <- function(Xq, Xc, model="VVV", K) {
  #TODO : add the other parameters
  Qres = 0
  Cres = 0
  if(!is.null(Xq) && !ncol(Xq) == 0){
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
    
    Qres = nbVar + res
  }
  if (!is.null(Xc) && !ncol(Xc) == 0){
    Cres = ncol(Xc) * K
  }
  return (Qres + Cres)
}
ICL <- function(bic, Z){
  log_z = log(Z)
  e_m <- sum((Z*log_z))
  return(bic + e_m )
}

split_by_var_type <- function(X) {
  n = nrow(X)
  num = unlist(lapply(X, is.numeric))
  Xq = X[,num]
  Xc = X[,!num]
  X_hot = matrix(NA, nrow=n, ncol=0)
  p = ncol(Xc)
  modalities = 0
  if(p != 0){
    for (j in seq(p)) {
      X_hot = cbind(X_hot, one_hot(factor(Xc[,j])))
    }
  modalities = get_nb_modalities(Xc)
  }else X_hot= Xc
  return(list(Xc=X_hot, Xq=Xq, modalities = modalities))
}

init_thetas <- function(Xc, Xq, initMethod, nbInit, K,modalities){
  #modalities = get_nb_modalities(Xc)
  #dc = sum(modalities)
  #because we one hot the vector at the beggining
  dc = ncol(Xc)
  dq = ncol(Xq)
  inits = rep(list(create_theta(dq, dc, K)), nbInit)
  if (initMethod == "random") {
    if(!dq == 0){
      minX = apply(Xq, 2, min)
      maxX = apply(Xq, 2, max)
    }
    for (i in seq_along(inits)) {
      p = runif(K)
      p = p / sum(p)
      for (k in seq_along(inits[[i]])) {
        # generate Means and deviation between min and max of each dimension
        if(!dq == 0) {
          inits[[i]][[k]]$mean = unlist(lapply(1:dq, function(l) runif(1, minX[l], maxX[l])))
          # TODO : How should a random sd look ? diagonal ? triangular ? full (hmm no) ?
          inits[[i]][[k]]$sd = diag(unlist(lapply(1:dq, function(l) runif(1, 0, maxX[l] - minX[l]))))
        }
        if(!dc == 0)
          inits[[i]][[k]]$alpha = unlist(lapply(modalities, function(i) { p=runif(i,0,1); return(p/sum(p)) }))
        inits[[i]][[k]]$p = p[k]
      }
    }
  }
  return(inits)
}

EM <- function(Xc, Xq, theta_0, model, epsilon){
  last_likelihood = -Inf
  current_likelihood= -Inf
  theta = theta_0
  repeat{
    last_likelihood = current_likelihood
    Z <- E_Step2(theta, Xc, Xq, model)
    new_theta = M_step(Xc, Xq, Z, model)
    current_likelihood = process_likelihood2(Xc, Xq, Z, new_theta)
    theta = new_theta
    likelihood_diff = current_likelihood - last_likelihood
    tryCatch({
      if (likelihood_diff < 0){
        cat("likelihood_diff: ",likelihood_diff, " current : ", current_likelihood, " last: ",last_likelihood, "GI ", GI, "\n")
      }
    }, error = function(error_condition){
      cat("ERROR:  current_likelihood: ",current_likelihood, " last_likelihood: ", last_likelihood, "\n")
      current_likelihood = -Inf
      print(Xc)
      print(Z)
      print(new_theta)
      break
    })
      #stop(paste(c("New likelihood is inferior to previous one : Suspicious regression of ", likelihood_diff), collapse=""))
    if (abs(likelihood_diff) < epsilon)
      break
  }
  res = list(likelihood= current_likelihood, Z=Z, theta= new_theta)
  return(res)
}


process_likelihood2 <- function(Xc, Xq, Z, thetas) {
  fc <- all_fK(Xc, NULL, thetas, res="fc")
  fq <- all_fK(NULL, Xq, thetas, res="fq")
  
  zeros = (fc == 0)
  fc = replace(fc, zeros, .Machine$double.xmin)
  zeros = (fq == 0)
  fq = replace(fq, zeros, .Machine$double.xmin)
  
  lfc = log(fc)
  lfq = log(fq)
  ps= sapply(thetas, function(theta) theta$p)
  cat("p ", ps,"GK GI", GK," ", GI, "\n")
  lp = log(ps)
  logdens= sweep(lfq+lfc, 2, lp, "+")
  
  res <- Z * (logdens)
  return(sum(res))
  
}
process_likelihood <- function(Xc, Xq, Z, theta){
  Q = 0
  K = length(theta)
  for (k in 1:K) {
    # Refer to slide 68/90 for math detail
    
    # Refer to slide 59/90 for math detail
    # tk = Z[,k]
    # nk = sum(tk)
    # pk = nk / n
    mean_k = theta[[k]]$mean
    # mean_k = apply(Xc * tk, 2, sum) / nk
    X_centered = t(apply(Xq, 1, function(i) i - mean_k))
    sd_k = theta[[k]]$sd
    # sd_k = sum(tk * apply(X_centered, 1, function(i) sum(i^2))) / nk
    # inv_sd_k = solve(sd_k)
    # det_sd_k = det(sd_k)
    alpha_k = theta[[k]]$alpha
    
    # TODO : Here we have values so small they are 0
    # Temporary fix is to add minimal possible value so that logarithm is
    # not -Inf but not sure if that's the best way...
    
    fk_q = mdnorm(Xq, mean_k, sd_k) + .Machine$double.xmin
    fk_c = multinomial(Xc, alpha_k) + .Machine$double.xmin
    log_fk = log(fk_q) + log(fk_c)

    Q = Q + sum(Z[,k] * log_fk)
    
    # Q = Q + sum(apply(X_centered, 1, function(i) {
    #   #log(pk) - p * log(2 * pi) / 2 - log(det_sd_k) / 2 - (t(i) %*% inv_sd_k %*% i)
    #   Z[i,k] * log_fk
    # }))
  }
  return(Q)
}

plot_result <- function(result) {
  plot.new()
  xmax=-Inf
  ymax=-Inf
  xmin=Inf
  ymin=Inf
  to_plot = list()
  for (model in result) {
    p = matrix(unlist(lapply(model, function(m) c(m$nbClusters, m$bic, m$icl))), nrow=3)
    to_plot = c(to_plot, list(p))
    k = p[1,]

    xmax=max(xmax, max(k))
    xmin=min(xmin, min(k))
    ymax=max(ymax, max(p[-1,]))
    ymin=min(ymin, min(p[-1,]))
  }
  plot(0, type="n", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  i=0
  for (p in to_plot) {
    i = i + 1
    lines(p[1,], p[2,], col=i)
    i = i + 1
    lines(p[1,], p[3,], col=i)
  }
}
