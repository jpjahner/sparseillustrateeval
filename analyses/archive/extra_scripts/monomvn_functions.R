#' Run Bayesian LASSO on training and test data
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a logical vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training set
#' @param revjump logical; if TRUE, uses reversible jump MCMC
#' @return a list containing point estimates for beta coefficients, simulated
#' observations (y.sim) based on coefficients for the training data (i.e. for
#' a posterior predictive check), and predicted observations for test data
run_blasso <- function(X, y, indices,
                       revjump=TRUE, iter=2000, prior="hs",
                       beta_matrix=F){

  # run the model using only the training data

  mod <- monomvn::blasso(X[indices,], y[indices], RJ=revjump,
                         T=iter, case=prior)

  # for each iteration, we simulate an observed value based on the coefficient
  # estimates, akin to a posterior predictive check; we do this for each
  # iteration rather than a mean or median so that we propagate the uncertainty
  y.sim <- apply(apply(mod$beta, 1, function(x){
    X[indices,] %*% x
  }), 1, mean)

  # we also want to make out-of-sample predictive checks, i.e. our test data.
  # if we wanted to, we could fold this into some cross-validation, but that
  # seems like overkill and is also on the backburner for the sparse project
  if(!all(indices)){
    y.pred <- apply(apply(mod$beta, 1, function(x){
      X[!indices,] %*% x
    }), 1, mean)
  }else{y.pred <- NULL}

  # mean beta estimates across all iterations
  if(beta_matrix){
    est.betas <- mod$beta
  }else{
    est.betas <- apply(mod$beta, 2, mean)
  }
  return(list(betas=est.betas, y.sim=y.sim, y.pred=y.pred))
}

#' Run LASSO on training and test data
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a logical vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training set
run_lasso <- function(X, y, indices){
  mod <- monomvn::regress(X[indices,], y[indices], method="lasso")
  est.betas <- mod$b[-1]
  y.sim <- X[indices,] %*% est.betas
  if(!all(indices)){
    y.pred <- X[!indices,] %*% est.betas
  }else{y.pred <- NULL}
  return(list(betas=est.betas,  y.sim=y.sim, y.pred=y.pred))
}

#' Run ridge regression on training and test data
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a logical vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training set
#' @return a list containing point estimates for beta coefficients, simulated
#' observations (y.sim) based on coefficients for the training data (i.e. for
#' a posterior predictive check), and predicted observations for test data
run_ridge <- function(X, y, indices){
  mod <- monomvn::regress(X[indices,], y[indices], method="ridge")
  est.betas <- mod$b[-1]
  y.sim <- X[indices,] %*% est.betas
  if(!all(indices)){
    y.pred <- X[!indices,] %*% est.betas
  }else{y.pred <- NULL}
  return(list(betas=est.betas,  y.sim=y.sim, y.pred=y.pred))
}

