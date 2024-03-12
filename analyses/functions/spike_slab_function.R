
# This function calls the spikeslab package which is much, much faster than BoomSpikeSlab
#' Run spike and slab 
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a logical vector of length equal to length of y
run_spike_slab2 <- function(X, y, indices){
if(ncol(X)>length(y)){
  npswitch <- T
}else{
  npswitch <- F
}

  mod1 <- spikeslab::spikeslab(x=X[indices,], y=y[indices], bigp.smalln = npswitch)
  
  y.pred <- spikeslab::predict.spikeslab(mod1, newdata = X[!indices,])$yhat.bma
  y.sim <- spikeslab::predict.spikeslab(mod1)$yhat.bma
  est.betas <- mod1$bma
  
  return(list(betas=est.betas, y.sim=y.sim, y.pred=y.pred))
  
}


#' Run spike-slab regression
#'
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a logical vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training set
#' @param exp_r2 expected R2 of the model, used to create spike and slab prior
#' @param exp_size expected number of non-zero predictors, used to create the spike portion of the prior
#' @param ... additional arguments passed to BoomSpikeSlab functions
#' @return a list containing the fitted model, coefficient estimates, and
#' predictions for the training (y.sim) and test sets (y.pred)
run_spike_slab <- function(X, y, indices, exp_r2=0.5, exp_size=1, ...){

  # prepare data
  X_train <- X[indices,]
  X_test <- X[!indices,]
  y_train <- y[indices]

  # specify prior
  prior <- BoomSpikeSlab::SpikeSlabPrior(cbind(1,X_train), y_train,
                          expected.r2 = exp_r2,
                          expected.model.size = exp_size,
                          ...)

  # spike and slab regression
  mod <- BoomSpikeSlab::lm.spike(y_train ~ X_train,
                  niter = 1000,
                  error.distribution = "gaussian",
                  prior = prior,
                  ...)

  # coefficient estimates
  beta <- t(mod$beta[,-1])

  # get predictions for training set
  pred_train <- predict(mod, X_train)

  # get predictions for test set
  pred_test <- predict(mod, X_test)

  return(list(mod = mod, betas = beta, y.sim = pred_train, y.pred = pred_test))
}


