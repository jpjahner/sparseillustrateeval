#' Run spike-slab regression
#' 
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a logical vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training set
#' @param exp_r2 expected R2 of the model, used to create spike and slab prior
#' @param exp_size expected number of non-zero predictors, used to create the spike portion of the prior
#' @param ... additional arguments passed to BoomSpikeSlab functions
#'
#' @return a list containing the fitted model, coefficient estimates, and 
#' predictions for the training (pred_train) and test sets (pred_test)


run_spike_slab <- function(X, y, train_ind, exp_r2=0.5, exp_size=1, ...){
  require(BoomSpikeSlab)
  
  # prepare data
  X_train <- X[train_ind,]
  X_test <- X[-train_ind,]
  y_train <- y[train_ind]

  
  # specify prior
  prior <- SpikeSlabPrior(cbind(1,X_train), y_train,
                          expected.r2 = exp_r2,
                          expected.model.size = exp_size,
                          ...)
  
  # spike and slab regression
  mod <- lm.spike(y_train ~ X_train, 
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
  
  return(list(model = mod, beta = beta, pred_train = pred_train, pred_test = pred_test))
}