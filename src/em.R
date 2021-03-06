library(plot3D)
library(mixtools)
library(sets)


# Returns a list of length k
# Each element contains a vector of mean, a matrix of variance,
# a vector alphas of length the total number of modalities and
# a propobability pk
.create_theta <- function(dq, dc, k) {
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
.fK <- function(Xc, Xq, alphas, mean, sd,log) {
  if(log){
    fXc = 0
    fXq = 0
  }else {
    fXc = 1
    fXq = 1
  }
  
  if(!is.null(Xc) && !ncol(Xc) == 0){
    fXc = .multinomial2(Xc, alphas,log=log)
  }
  if(!is.null(Xq) && !ncol(Xq) == 0) {
    fXq = .mdnorm2(Xq, mean, sd, log)
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
.all_fK <- function(Xc, Xq, thetas,res="f", log=FALSE){
  if(res=="fq"){
    return(sapply(thetas, function(theta) .fK(Xc,Xq, theta$alpha, theta$mean, theta$sd, log)$fq))
  }else if(res=="fc") {
    return(sapply(thetas, function(theta) .fK(Xc,Xq, theta$alpha, theta$mean, theta$sd, log)$fc))
  }else {
    return(sapply(thetas, function(theta) .fK(Xc,Xq, theta$alpha, theta$mean, theta$sd,log)$f))
  }
}




# Returns matrices of size n x k of probabilities
# of each element belonging to each class
.E_Step2 <- function(thetas, Xc=NULL, Xq=NULL, model="VVV") {
  lZ_temp = .all_fK(Xc=Xc, Xq=Xq, thetas=thetas, log=T)
  lPs = sapply(thetas, function(theta) log(theta$p))
  lZ_temp = sweep(lZ_temp, 2, lPs, "+")
  lZ = lZ_temp - apply(lZ_temp, 1, .logsum)
  Z = exp(lZ)
  #zeros = (Z == 0)
  #Z = replace(Z, zeros, .Machine$double.xmin)
  Z
}
.logsum <- function(x) {
  res = max(x) + log(sum(exp(x - max(x))))
  return (res)
}
# Equivalent of function dnorm but takes as inputs
# a vector of mean and a matrix of standard deviation

.mdnorm2 <- function(X,mean, sd, log=FALSE) {
  require(mvtnorm)
  noorm=dmvnorm(X, mean , sd,log = log)
  #noorm = replace(noorm, which(noorm==Inf), 10)
  #if(log){
  #  noorm = replace(noorm, which(noorm==-Inf), -5)
  #}
  noorm
}


.get_nb_modalities <- function(X) {
  if (is.null(X) || ncol(X) == 0) return(0)
  return(apply(X, 2, function(c) length(levels(factor(c)))))
}

.multinomial2 <- function(Xc, alphas, log=FALSE){
  n <- nrow(Xc)
  alphaMat <- matrix(rep(alphas,each=n), nrow=n)
  if (!log) {
    powMat <- alphaMat^Xc
    return(apply(powMat, 1, function(line) prod(line)))
  } else {
    logAlpha <- log(alphaMat)
    mulMat <- logAlpha*Xc
    mulMat[is.na(mulMat)] <- 0
    res <- rowSums(mulMat)
    return(res)
  }
}




.one_hot <- function(x) {
  lvl = levels(x)
  len = length(lvl)
  t(apply(matrix(x, ncol=1), 1, function(i) {
    res = rep(0,len)
    res[which(lvl == i)] = 1
    res
  }))
}

#TODO : check if quanti and quali are non null
.M_step <- function(Xc, Xq, Z, model){
  
  # Temporary : To be moved to VVV model
  tryCatch({
    
    
    
    
    
    K = ncol(Z)
    n = nrow(Xc)
    nqCol = ncol(Xq)
    ncCol = ncol(Xc)
    theta = .create_theta(ncol(Xq), ncol(Xc), K)
    for (k in seq(K)) {
      tk = Z[,k]
      nk = sum(tk)
      norm_tk = tk/nk
      pk = nk / n
      if(!nqCol == 0){
        mean_k = apply(Xq * norm_tk, 2, sum)
        
        if(nqCol == 1){
          X_centered = apply(Xq, 1, function(i) i - mean_k)
          sd_k = 0
          for (i in (1: n)) {
            mat = norm_tk[i] * (t(X_centered[i]) %*% X_centered[i])
            sd_k = sd_k + mat
          }
        }
        else{
          X_centered = t(apply(Xq, 1, function(i) i - mean_k))
          
          sd_k = matrix(0, nrow=nqCol, ncol=nqCol)
          for (i in (1: n)) {
            mat = norm_tk[i] * (X_centered[i,] %*% t(X_centered[i,]))
            sd_k = sd_k + mat
          }
          if(det(sd_k) < 1e-300){
            stop("Non invertible matrix in M_step")
            #sd_k = diag(rep(0.01,nqCol))
          }
        }
        
        #if (det(sd_k) < 1e-4) {
        #  sd_k = diag(rep(0.01,nqCol))
        #}
        
        
        theta[[k]]$mean = mean_k
        theta[[k]]$sd = sd_k
      }
      if(!ncol(Xc) == 0) {
        theta[[k]]$alpha = colSums(tk * Xc) / sum(tk)
      }
      
      theta[[k]]$p = pk
    }
    return(theta)
  }, error = function(error_condition){
    return (NULL)
  })
}


clust <- function(X, nbClust,  nbInit=5, initMethod="kmeans", epsilon= 0.1, nbIterations =30, verbose=T){
  models = c("VVV")
  if(initMethod != "kmeans" && initMethod != "random")
    stop("Wrong initMethod: must be either kmeans or random ")
  if(is.null(X) || nrow(X)==0 || ncol(X) == 0)
    stop("Error : Empty dataset")
  for (i in nbClust)
    if (i <= 0 || round(i) != i)
      stop("wrong nbClust : must be a non null natural number or vector of non null natural numbers")
  nbClust = as.set(nbClust)
  if (nbInit <= 0 || round(nbInit) != nbInit)
    stop("wrong nbInit : must be a non null natural number")
  newX = .split_by_var_type(X)
  Xc = newX$Xc
  Xq = newX$Xq
  modalities = newX$modalities
  res = list()
  i  = 1
  for(model in models) {
    j = 1
    # A particular model is a sub list for all clusters
    res[[i]] = list()
    for (K in nbClust){
      
      best_likelihood = -Inf
      best_theta = NULL
      succInit = 0
      iter = 0
      nbErrors = 0
      
      
      while((succInit < nbInit) && (iter < 2*nbInit || is.null(best_theta)) && nbErrors < 20){
        iter = iter + 1
        if (K == 1) {
          Z <- matrix(1, nrow=nrow(Xq), ncol=1)
          best_theta = .M_step(Xc,Xq, Z)
          if(!is.null(best_theta)){
            best_likelihood = .process_likelihood2(Xc, Xq, best_theta)
            best_em = list(likelihood= best_likelihood, Z=Z, theta= best_theta)
          }else {
            nbErrors = nbErrors+1
            next
          }
          
          
        }else {
          
          
          theta_0 = .init_theta(Xc,Xq,K=K,modalities = modalities,initMethod=initMethod)
          if(is.null(theta_0)){
            nbErrors = nbErrors + 1
            next
          }
          em = .EM(Xc, Xq, theta_0, model, epsilon, nbIterations)
          if(em$likelihood == -Inf){
            nbErrors = nbErrors + 1
            next
          }
          if(em$likelihood > best_likelihood){
            best_likelihood = em$likelihood
            best_em = em
          }
          
        }
        succInit = succInit+1
      }
      if(nbErrors == 20 && best_likelihood ==-Inf)
        stop("Can't continue execution : Non invertible matrix")
      bic = .BIC(Xq,Xc, model, best_likelihood,K, length(modalities))
      icl = .ICL(bic, best_em$Z)
      res_i = list(model=model, nbClusters=K, theta=best_em$theta,
                   bic=bic, icl=icl,
                   Z=best_em$Z)
      res[[i]][[j]] = res_i
      if(verbose)
        cat("model ", models[i] , " with ", K, " clusters finished with likelihood ", best_likelihood, "\n")
      j = j+1
    }
    i = i+1
  }
  return(res[[1]])
}

.BIC <- function(Xq, Xc, model, likelihood, K,nbQualVariables){
  if(!is.null(Xq) && ncol(Xq) != 0)
    n <- nrow(Xq)
  else
    n <- nrow(Xc)
  nb_par = .getNbParameters(Xq, Xc, model, K,nbQualVariables)
  return(2*likelihood - nb_par* log(n))
}
.getNbParameters <- function(Xq, Xc, model="VVV", K, nbQualVariables) {
  #TODO : add the other parameters
  Qres = 0
  Cres = 0
  if(!is.null(Xq) && !ncol(Xq) == 0){
    nbUs <- ncol(Xq)
    res = (K-1) + K*nbUs
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
    Cres = (ncol(Xc)- nbQualVariables) * K
  }
  return (Qres + Cres)
}
.ICL <- function(bic, Z){
  log_z = log(Z)
  z_log_z = (Z*log_z)
  z_log_z[is.nan(z_log_z) ] = 0
  e_m <- sum(z_log_z)
  return(bic + e_m )
}

.split_by_var_type <- function(X) {
  n = nrow(X)
  num = unlist(lapply(X, is.numeric))
  Xq = as.data.frame(X[,num])
  Xc = as.data.frame(X[,!num])
  X_hot = matrix(NA, nrow=n, ncol=0)
  p = ncol(Xc)
  modalities = 0
  if(p != 0){
    for (j in seq(p)) {
      X_hot = cbind(X_hot, .one_hot(factor(Xc[,j])))
    }
    modalities = .get_nb_modalities(Xc)
  }else X_hot= Xc
  return(list(Xc=X_hot, Xq=Xq, modalities = modalities))
}


.kmeans_init <- function(Xc, Xq,K, modalities, theta) {
  if(!is.null(Xq) && ncol(Xq)!= 0 ){
    km <- kmeans(Xq, centers = K, nstart = 5)
    Z = .one_hot(factor(km$cluster))
    km_theta = .M_step(Xc,Xq,Z)
    if(is.null(km_theta)){
      return (NULL)
    }else {
      for (i in seq_along(theta)) {
        theta[[i]]$mean = unlist(km_theta[[i]]$mean)
        theta[[i]]$sd = as.matrix(km_theta[[i]]$sd)
        theta[[i]]$p = unlist(km_theta[[i]]$p)
      }
      if(!is.null(Xc)&& ncol(Xc)!=0){
        for (k in 1:K){
          Z_k = Z[,k]
          Z_Xc = Xc* Z_k
          alpha_k = colSums(Z_Xc) / nrow(Z_Xc)
          theta[[k]]$alpha = unlist(alpha_k)
        }
      }
    }
    if(!is.null(Xc) && ncol(Xc)!= 0 ){
      for (k in 1:K){
        theta[[k]]$alpha = unlist(lapply(modalities, function(i) { p=runif(i,0,1); return(p/sum(p)) }))
      }
    }
    
    return(theta)
  }
}

.init_theta <- function(Xc, Xq, initMethod="kmeans",K, modalities ) {
  dc = ncol(Xc)
  dq = ncol(Xq)
  init = .create_theta(dq, dc, K)
  if (initMethod == "random") {
    if(!dq == 0){
      minX = as.numeric(apply(Xq, 2, min))
      maxX = as.numeric(apply(Xq, 2, max))
      totaldiff =maxX - minX
    }
    
    p = runif(K)
    p = p / sum(p)
    
    for (k in seq_along(init)) {
      # generate Means and deviation between min and max of each dimension
      if( dq != 0) {
        
        #sd_mean = as.numeric((totaldiff)/(4*K))
        #mean_mean = sapply(totaldiff, function(t) (k-1) * t + t/2)
        #means = sapply(1:dq, function(i) rnorm(1, mean_mean[i], abs(sd_mean[i]) ))
        init[[k]]$mean = unlist(Xq[sample(1:nrow(Xq), 1),])
        #init[[k]]$mean = as.numeric(means)
        det = 0
        mean_sd=  as.numeric(totaldiff/K)
        while(det == 0) {
          sd = t(sapply(1:dq ,function(i) abs(rnorm(dq,mean = mean_sd[i],abs(mean_sd[i])))))
          for (i in 1:(dq-1)) {
            for (j in (i+1):dq) {
              sd[j,i] = sd[i,j]
            }
          }
          det = det(sd)
        }
        init[[k]]$sd = as.matrix(sd)
      }
      if(!dc == 0)
        init[[k]]$alpha = unlist(lapply(modalities, function(i) { p=runif(i,0,1); return(p/sum(p)) }))
      init[[k]]$p = p[k]
    }
    
  }
  else if (initMethod == "kmeans") {
    init  =.kmeans_init(Xc, Xq,K, modalities, init)
  }
  else stop("Unknown initialization method")
  return(init)
}



.EM <- function(Xc, Xq, theta_0, model, epsilon,nbIterations){
  last_likelihood = -Inf
  current_likelihood= -Inf
  theta = theta_0
  iter = 0
  best_theta = NULL
  best_likelihood = -Inf
  it = 0
  repeat{
    last_likelihood = current_likelihood
    Z <- .E_Step2(theta, Xc, Xq, model)
    Z = t(apply(Z, 1, function(z) {
      if (sum(is.nan(z))>1) {
        zz=runif(length(z))
        return(zz/sum(zz))
      }
      else return(z)
    }))
    
    
    #plot(Xq)
    #for (t in theta) {
    #  ellipse(t$mean, t$sd)
    #}
    
    new_theta = .M_step(Xc, Xq, Z, model)
    if(!is.null(new_theta)){
      current_likelihood = .process_likelihood2(Xc, Xq,  new_theta)
      if(current_likelihood > best_likelihood){
        best_likelihood = current_likelihood
        best_theta  = new_theta
        theta = new_theta
        likelihood_diff = current_likelihood - last_likelihood
        if (abs(likelihood_diff) < epsilon)
          break
      }
    }else{
      current_likelihood = -Inf
    }
    
    iter  = iter +1
    it = it+1
    
    
    if(iter >= nbIterations){
      break
    }
  }
  res = list(likelihood= best_likelihood, Z=Z, theta= best_theta)
  return(res)
}


.process_likelihood2 <- function(Xc, Xq, thetas) {
  fc <- .all_fK(Xc, NULL, thetas, res="fc", log=TRUE)
  fq <- .all_fK(NULL, Xq, thetas, res="fq", log=TRUE)
  lfc = fc
  lfq = fq
  ps= sapply(thetas, function(theta) theta$p)
  lp = log(ps)
  logdens= sweep(lfq+lfc, 2, lp, "+")
  log_fxi = apply(logdens, 1, .logsum)
  log_like = sum(log_fxi)
  
  return(log_like)
  
}

plot_result <- function(result) {
  xmax=-Inf
  ymax=-Inf
  xmin=Inf
  ymin=Inf
  to_plot = list()
  legend=c()
  i=0
  for (model in list(result)) {
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

best_model <- function(result, criterion="icl", best_cluster=T){
  if(criterion == "bic"){
    best = NULL
    for(model in result){
      if(is.null(best))
        best = model
      else if(model$bic > best$bic) {
        best = model
      }
    }
  }else if(criterion == "icl"){
    best = NULL
    for(model in result){
      if(is.null(best))
        best = model
      else if(model$icl > best$icl) {
        best = model
      }
    }
  }else {
    stop("Unknown criterion")
  }
  if(best_cluster){
    newZ = apply(best$Z, 1, which.max)
    best$Z = newZ
  }
  best
  
}
