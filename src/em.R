library(plot3D)


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
fK <- function(Xc, Xq, alphas, mean, sd,log) {
  if(log){
    fXc = 0
    fXq = 0
  }else {
    fXc = 1
    fXq = 1
  }
 
  if(!is.null(Xc) && !ncol(Xc) == 0){
    fXc = multinomial2(Xc, alphas,log=log)
  }
  if(!is.null(Xq) && !ncol(Xq) == 0) {
    fXq = mdnorm2(Xq, mean = mean, sd = sd, log=log)
  }
  
  if(log){
    f = fXc + fXq
  }else {
    f = fXc*fXq
  }
  return(list(fc = fXc, fq=fXq, f=f))
}

#Process f(X|K)*Pk with K being the cluster, for each K
#Returns a matrix of N*K with N = number of observations and K number of clusters
all_fK <- function(Xc, Xq, thetas,res="f", log=FALSE){
  if(res=="fq"){
    return(sapply(thetas, function(theta) fK(Xc,Xq, theta$alpha, theta$mean, theta$sd, log)$fq))
  }else if(res=="fc") {
    return(sapply(thetas, function(theta) fK(Xc,Xq, theta$alpha, theta$mean, theta$sd, log)$fc))
  }else {
    return(sapply(thetas, function(theta) fK(Xc,Xq, theta$alpha, theta$mean, theta$sd,log)$f))
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
  lZ_temp = all_fK(Xc=Xc, Xq=Xq, thetas=thetas, log=T)
  lPs = sapply(thetas, function(theta) log(theta$p))
  lZ_temp = sweep(lZ_temp, 2, lPs, "+")
  lZ = lZ_temp - apply(lZ_temp, 1, logsum)
  Z = exp(lZ)
  zeros = (Z == 0)
  Z = replace(Z, zeros, .Machine$double.xmin)
  Z
}
logsum <- function(x) {
  max(x) + log(sum(exp(x - max(x))))
}
# Equivalent of function dnorm but takes as inputs
# a vector of mean and a matrix of standard deviation

mdnorm2 <- function(X, mean,sd,log=F) {
  if (ncol(X) == 0) return(1)
  #X = matrix(X, ncol=length(mean))
  p = length(mean)
  inv_sd = solve(sd)
  a = ((2 * pi) ^ (p / 2)) * (det(sd) ^ (1/2))
  reduced_X = as.matrix(sweep(X, 2, mean))
  b = -(1/2) * rowSums((reduced_X %*% inv_sd)* reduced_X)
  if (log) -log(a) + b
  else (1 / a) * exp(b)
}

#mdnorm2 <- function(X,mean, sd, log=FALSE) {
#  require(mvtnorm)
#  dmvnorm(X, mean , sd,log = log)
#}


get_nb_modalities <- function(X) {
  if (is.null(X) || ncol(X) == 0) return(0)
  return(apply(X, 2, function(c) length(levels(factor(c)))))
}

multinomial2 <- function(Xc, alphas, log=FALSE){
  n <- nrow(Xc)
  alphaMat <- matrix(rep(alphas,each=n), nrow=n)
  if (!log) {
    powMat <- alphaMat^Xc
    return(apply(powMat, 1, function(line) prod(line)))
  } else {
    logAlpha <- log(alphaMat)
    mulMat <- logAlpha*Xc
    res <- rowSums(mulMat)
    return(res)
  }
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

      thetas_0 = init_thetas(Xc, Xq, initMethod, nbInit, K, modalities)
      best_likelihood = -Inf
      best_theta = NULL
      tmp = 0
      for(theta_0 in thetas_0){
        tmp = tmp +1
        tryCatch({
            em = EM(Xc, Xq, theta_0, model, epsilon)
            if(em$likelihood > best_likelihood){
              best_likelihood = em$likelihood
              best_em = em
            }
          }, 
          error = function(error_condition){
          }
        )
        
        
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
  print(likelihood)
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
  Xq = as.data.frame(X[,num])
  Xc = as.data.frame(X[,!num])
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

init_theta <- function(Xc, Xq, init_method,K, modalities ) {
  dc = ncol(Xc)
  dq = ncol(Xq)
  init = list(create_theta(dq, dc, K))
  if (initMethod == "random") {
    if(!dq == 0){
      minX = apply(Xq, 2, min)
      maxX = apply(Xq, 2, max)
      totaldiff =maxX - minX
    }
  
    p = runif(K)
    p = p / sum(p)
    for (k in seq_along(init)) {
      # generate Means and deviation between min and max of each dimension
      if(!dq == 0) {
        
        sd_mean = as.numeric((totaldiff)/(4*K))
        mean_mean = sapply(totaldiff, function(t) (k-1) * t + t/2)
        means = sapply(1:dq, function(i) rnorm(1, mean_mean[i], sd_mean[i]) )
        init[[k]]$mean = as.numeric(means)
        det = 0
        mean_sd=  as.numeric(totaldiff/K)
        while(det == 0) {
          sd = t(sapply(1:dq ,function(i) abs(rnorm(dq,mean = mean_sd[i],mean_sd[i]))))
          det = det(sd)
        }
        init[[k]]$sd = as.matrix(sd)
      }
      if(!dc == 0)
        init[[k]]$alpha = unlist(lapply(modalities, function(i) { p=runif(i,0,1); return(p/sum(p)) }))
      init[[k]]$p = p[k]
    }
    
  }
  return(init)
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
      totaldiff =maxX - minX
    }
    for (i in seq_along(inits)) {
      p = runif(K)
      p = p / sum(p)
      for (k in seq_along(inits[[i]])) {
        # generate Means and deviation between min and max of each dimension
        if(!dq == 0) {
          
          sd_mean = as.numeric((totaldiff)/(4*K))
          mean_mean = sapply(totaldiff, function(t) (k-1) * t + t/2)
          means = sapply(1:dq, function(i) rnorm(1, mean_mean[i], sd_mean[i]) )
          inits[[i]][[k]]$mean = as.numeric(means)

          # TODO : How should a random sd look ? diagonal ? triangular ? full (hmm no) ?
          det = 0
          while(det == 0) {
            mean_sd=  as.numeric(totaldiff/K)
            sd = t(sapply(1:dq ,function(i) abs(rnorm(dq,mean = mean_sd[i],mean_sd[i]))))
            det = det(sd)
          }
          inits[[i]][[k]]$sd = as.matrix(sd)
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
        #cat("likelihood_diff: ",likelihood_diff, " current : ", current_likelihood, " last: ",last_likelihood, "\n")
      }
    }, error = function(error_condition){
      #cat("ERROR:  current_likelihood: ",current_likelihood, " last_likelihood: ", last_likelihood, "\n")
      current_likelihood = -Inf
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
  fc <- all_fK(Xc, NULL, thetas, res="fc", log=TRUE)
  fq <- all_fK(NULL, Xq, thetas, res="fq", log=TRUE)
  
  #zeros = (fc == 0)
  #fc = replace(fc, zeros, .Machine$double.xmin)
  #zeros = (fq == 0)
  #fq = replace(fq, zeros, .Machine$double.xmin)
  
  lfc = fc
  lfq = fq
  ps= sapply(thetas, function(theta) theta$p)
  lp = log(ps)
  logdens= sweep(lfq+lfc, 2, lp, "+")
  
  res <- Z * (logdens)
  return(sum(res))
  
}

plot_result <- function(result) {
  xmax=-Inf
  ymax=-Inf
  xmin=Inf
  ymin=Inf
  to_plot = list()
  legend=c()
  i=0
  for (model in result) {
    for (m in model) {
      legend = c(legend, paste(c(m$model, "BIC"), collapse = " "), paste(c(m$model, "ICL"), collapse=" "))
      break
    }
    i = i + 1
    p = matrix(unlist(lapply(model, function(m) c(m$nbClusters, m$bic, m$icl))), nrow=3)
    to_plot[[i]] = p
    k = p[1,]

    xmax=max(xmax, max(k))
    xmin=min(xmin, min(k))
    ymax=max(ymax, max(p[-1,]))
    ymin=min(ymin, min(p[-1,]))
  }
  plot(0, type="n", xlim=c(xmin, xmax), ylim=c(ymin, ymax), xlab="Nb Clusters", ylab="BIC/ICL values")
  legend("bottomright", legend=legend, lty=1, col=(1:length(legend)), cex=0.8)
  i=0
  for (p in to_plot) {
    i = i + 1
    points(p[1,], p[2,], col=i)
    lines(p[1,], p[2,], col=i)
    i = i + 1
    points(p[1,], p[3,], col=i)
    lines(p[1,], p[3,], col=i)
  }
}
