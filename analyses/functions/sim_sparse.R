#' Generate correlations between predictors
#' @param p number of predictors
#' @param scale strength of correlations among predictors
get_Sigma <- function(p, scale) {

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

  Sigma <- t(L) %*% L
}


#' Simulate datasets with different degrees of sparsity
#' @description Simulates datasets with different numbers of predictors and
#' observations to evaluate sparse modeling approaches
#' @param n integer; the number of observations
#' @param p integer; the number of predictor variables
#' @param cluster_size integer; number of predictors per cluster (must be a factor of p)
#' @param n_causal integer; number of predictors estimated to have a non-zero effect
#' @param pct_causal numeric between 0-1; proportion of predictors estimated to have a non-zero effect
#' @param beta_mean numeric; mean value of non-zero effects
#' @param beta_sd numeric; standard deviation of non-zero effects
#' @param scale positive numeric; strength of correlations between predictors where higher values mean weaker correlations
sim_sparse <- function(n = 1e3, p = 100, cluster_size = 100,
                       n_causal = 10, pct_causal = NULL, beta_mean = 0.1,
                       beta_sd = 0, scale = 1, set_seed = F) {

  requireNamespace("dplyr", quietly=T)

  if(isTRUE(set_seed)) set.seed(42)
  if(!is.null(pct_causal)) n_causal = round(p*pct_causal)

  n_clusters <- p/cluster_size
  Sigma <- replicate(n_clusters, get_Sigma(cluster_size, scale), simplify = F)


  if(n_clusters > 1) {
    X <- lapply(Sigma, function(i) MASS::mvrnorm(n, mu = rep(0, cluster_size), Sigma = i)) %>% do.call(cbind, .) %>% apply(2, function(i) (i-mean(i))/sd(i))
  } else {
    X <- MASS::mvrnorm(n, rep(0, p), Sigma[[1]]) %>% apply(2, function(i) (i-mean(i))/sd(i))
  }

  beta <- rep(0, p)
  beta[sample(1:p, n_causal)] <- rnorm(n_causal, beta_mean, beta_sd)

  y_mu <- X %*% beta
  y <- rnorm(n, y_mu)

  return(list(beta = beta, X = X, y = y, y_mu = y_mu))

  if(isTRUE(set_seed)) rm(.Random.seed, envir=.GlobalEnv)

}
