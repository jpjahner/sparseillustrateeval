#' Create covariance matrix using spherical parameterization
#'
#' @param p Number of predictors.
#' @param scale Strength of correlations between covariates using spherical
#'   parameterization of correlation matrix (higher values mean weaker
#'   correlations; must be positive).
#'
#' @return A p x p covariance matrix.
#' @export
#'
#' @examples
get_Sigma_spherical <- function(p, scale) {
  
  L <- matrix(0, nrow = p, ncol = p)
  l <- L
  l[upper.tri(l, diag = T)] <- runif(sum(upper.tri(L, diag = T)), -scale, scale)
  
  for(i in 1:p) {
    l_i <- l[,i]
    for(j in 2:(i-1)) {
      L[j,i] <- l_i[1]*prod(ifelse((j%%2) == 0, sin(l_i[j]), cos(l_i[j])))
    }
    L[i,i] <- l_i[1]*prod(sin(l_i[2:i]))
    L[1,i] <- l_i[1]*cos(l_i[2])
  }
  
  return(t(L) %*% L)
}

#' Create correlation matrix with constant correlation
#'
#' @param p Number of predictors.
#' @param rho Correlation coefficient.
#'
#' @return A p x p correlation matrix.
#' @export
#'
#' @examples
get_Sigma_constant <- function(p, rho) {
  Sigma <- matrix(rho, nrow = p, ncol = p)
  diag(Sigma) <- 1
  return(Sigma)
}


#' Simulate Data
#'
#' @param n Number of observations.
#' @param p Number of covariates.
#' @param cluster_size Number of covariates per cluster (must be a factor of
#'   `p`).
#' @param n_causal Number of covariates with non-zero effect.
#' @param pct_causal Proportion of predictors with non-zero effect.
#' @param beta_mean Mean value of non-zero betas.
#' @param beta_sd Standard deviation of non-zero betas.
#' @param scale Strength of correlations between covariates using spherical
#'   parameterization of correlation matrix (higher values mean weaker
#'   correlations; must be positive).
#' @param test_p Proportion of observations to flag as being in test set.
#' @param set_seed Logical, whether to specify seed.
#' @param corpar Method for generating covariate correlation matrix ("spherical"
#'   or "constant").
#' @param rho Correlation between covariates using constant parameterization of
#'   correlation matrix.
#'
#' @return A named list
#'  * `beta`: coefficient values (vector of length `n`)
#'  * `X`: matrix of covariate values (`n` by `p`)
#'  * `y`: simulated values of the dependent variable (vector of length `n`)
#'  * `y_mu`: expected values of the dependent variable (vector of length `n`)
#'  * `test_ind`: logical vector indicating whether observations belong to the test set
#' @export
#'
#' @examples
sparse_sim <- function(n = 1e3, p = 100, cluster_size = 100,
                       n_causal = 10, pct_causal = NULL, beta_mean = 0.1, 
                       beta_sd = 0, corpar = "spherical", scale = 1, rho = NULL, 
                       test_p = 0, set_seed = F) {
  

  require(tidyverse)
  require(MASS)
  
  
  if(isTRUE(set_seed)) set.seed(42)
  if(!is.null(pct_causal)) n_causal = round(p*pct_causal)
  
  n_clusters <- p/cluster_size
  
    
  if(n_clusters == p) {
    
    Sigma <- get_Sigma_constant(p = p, rho = rho)
      
    X <- mvrnorm(n, rep(0, p), Sigma) %>% apply(2, function(i) (i-mean(i))/sd(i))
    
  } else if (n_clusters == 1) {
    
    if(corpar == "spherical") 
      Sigma <- get_Sigma_spherical(p, scale)
    
    if(corpar == "constant") {
      Sigma <- get_Sigma_constant(p, rho)
    }
    
    X <- mvrnorm(n, rep(0, p), Sigma) %>% apply(2, function(i) (i-mean(i))/sd(i))
    
  } else {
    
    if(corpar == "spherical") 
      Sigma <- replicate(n_clusters, get_Sigma_spherical(cluster_size, scale), simplify = F)
    
    if(corpar == "constant")
      Sigma <- replicate(n_clusters, get_Sigma_constant(cluster_size, rho), simplify = F)
    
    X <- lapply(Sigma, function(i) mvrnorm(n, mu = rep(0, cluster_size), Sigma = i)) %>% 
      do.call(cbind, .) %>% 
      apply(2, function(i) (i-mean(i))/sd(i))
    
  } 

  beta <- rep(0, p)
  beta[sample(1:p, n_causal)] <- rnorm(n_causal, beta_mean, beta_sd)
  
  y_mu <- X %*% beta
  y <- rnorm(n, y_mu)
  
  test_ind <- sample(1:n, round(n*test_p))
  
  return(list(beta = beta, X = X, y = y, y_mu = y_mu, test_ind = test_ind))
  
  if(isTRUE(set_seed)) rm(.Random.seed, envir=.GlobalEnv)
  
}



#' Simulate Data for Covariate Correlation Experiment
sparse_sim_corr <- function(n = 1e3, p = 100, n_clusters = 5, 
                            cluster_size = 12, n_causal = 10, rho = 0.2, 
                            beta = 0.3, set_seed = F) {
  
  require(tidyverse)
  require(MASS)
  
  if(isTRUE(set_seed)) set.seed(42)
  
  cc <- round(n_causal/n_clusters)       # number of causal variables per cluster
  nc <- cluster_size - cc                # number of non-causal variables per cluster
  n_ind <-  p - n_clusters*cluster_size  # number of non-cluster variables
  
  Sigma <- get_Sigma_constant(cluster_size, rho)
  
  # simulate predictors and beta's for variables in clusters
  X_clust <- list()
  beta_clust <- list()
  for(i in 1:n_clusters) {
    X_clust[[i]] <- mvrnorm(n = n, mu = rep(0, cluster_size), Sigma = Sigma)
    beta_clust[[i]] <- c(rep(beta, cc), rep(0, nc))
  }
  
  X_clust <- do.call('cbind', X_clust)
  beta_clust <- do.call('c', beta_clust)
  
  # add data for out-of-cluster variables
  X_ind <- sapply(1:n_ind, function(i) rnorm(n))
  X <- cbind(X_clust, X_ind)
  beta <- c(beta_clust, rep(0, n_ind))
  
  # simulate response
  y_mu <- X %*% beta
  y <- rnorm(n, y_mu)
  

  return(list(beta = beta, X = X, y = y, y_mu = y_mu))
  
  if(isTRUE(set_seed)) rm(.Random.seed, envir=.GlobalEnv)
  
}
