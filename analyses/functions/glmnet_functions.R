# needs documentation
#' Fit penalized regression using glmnet
#'
#' @param X Model matrix (without a column of ones for the intercept)
#' @param y Response vector
#' @param indices Logical vector indicating which observations to use as training data
#' @param alpha Numeric value on the unit interval determining the proportion of lasso penalty to use.
#' \code{alpha = 1} is lasso regression, \code{alpha = 0} is ridge regression, and \code{alpha} \eqn{\in (0, 1)}
#' is elastic net.
#'
#' @return
#'
#' @examples
#' 
run_glmnet <- function(X, y, indices, alpha = 1){

  # fit the model
  mod <- glmnet::glmnet(X[indices,], y[indices], alpha=alpha, family="gaussian")
  
  # model selection using cross validation
  lambda <- glmnet::cv.glmnet(X[indices, ], y[indices])$lambda.1se
  
  # now use a predict call to get the coefficient estimates
  betas <- as.double(predict(
    mod, s = lambda, type = "coefficients",
    exact = T, x = X[indices, ], y = y[indices]
  ))[-1]
  
  # linear predictor for the observed and held-out data
  y.sim <- as.double(predict(mod, s = lambda, newx = X[indices,], type = "response"))
  y.pred <- as.double(predict(mod, s = lambda, newx = X[!indices,], type = "response"))

  out <- list(betas=betas, y.pred=y.pred, y.sim=y.sim)
  return(out)
}

