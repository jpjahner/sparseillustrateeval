#' Run random forest
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a logical vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training set
#' @return a list containing the fitted model, feature importance, and
#' predictions for the training (y.sim) and test sets (y.pred)
run_forest <- function(X, y, indices, tune = F, ...){
  # fit random forest
  if(isTRUE(tune)) {
    mod <- randomForest::tuneRF(X[indices,], y[indices], trace = F, plot = F, doBest = T)
  } else {
    mod <- randomForest::randomForest(X[indices,], y[indices], ...)
  }

  # get feature importance
  importance <- randomForest::importance(mod, type = 2)

  ## get predictions for training set
## No longer provide newdata=X[indices,] trick that allowed us to short-circuit cross-validation
    ## pred_train <- predict(mod, newdata=X[indices,])
    pred_train <- predict(mod)
    

  # get predictions for test set
  if(!all(indices)){
    pred_test <- predict(mod, X[!indices,])
  }else{
    pred_test <- NULL
  }

  return(list(mod = mod, importance = importance, y.sim = pred_train, y.pred = pred_test))
}
