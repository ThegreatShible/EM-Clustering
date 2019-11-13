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

# Plots a 2D gaussian law representation in 3D
visualize_2d_gaussian_law <- function(n=5000) {
  X = cbind(runif(n,-3,3), runif(n,-3,3))
  y = mdnorm(X, c(0,0), diag(c(1,1)))
  points3D(X[,1], X[,2], y)
}