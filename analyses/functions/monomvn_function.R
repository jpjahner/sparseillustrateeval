# This script includes functions and a quick example of running sparse models
# through the monomvn package. It includes a wrapper function to run three
# methods: BLASSO, LASSO, and ridge. The function exports the estimated betas
# along with simulated observations (i.e. for the training data as a posterior
# predictive check) and predicted observations for withheld testing data.

# Define functions -------------------------------------------------------------
#' Wrapper for monomvn functions
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param method a string indicating if 'blasso', 'lasso' or 'ridge' should be run
#' @param training_perc numeric from 0 to 1; the proportion of data that should
#' be used to train the model; remaining data will be withheld for validation
#' @param training_indices a vector indicating which observations belong to the
#' @param revjump logical; if TRUE, uses reversible jump MCMC (only used when method = 'blasso')
#' @param iter numeric; indicate the number of iterations to run
#' @param beta_matrix logical; if TRUE, returns a matrix of coefficients instead of point estimates
#' @param prior a string indicating the prior to use (only when method = "blasso");
#' options are default, ridge, hs (horseshoe), and ng (Normal-Gamma)
#' training dataset (optional); if supplied, overrides training_perc
#' @param pip_thresh If NULL, then posterior inclusion probabilities (PIPs) for parameters estimated 
#' using blasso and horseshoe are ignored. Otherwise, a threshold probability for inclusion can be 
#' provided and any parameter that has PIP less than the threshold will be set to zero.It only really
#' makes sense to include when \code{revjump = TRUE}.
#'
#' 
fun_par <- function(x){do.call(run_blasso, x)}

monomvn_function<-function(X, y, method, training_perc=1, training_indices=NULL,
                           revjump=T, iter=2000, beta_matrix=F, nchains=1, pip_thresh = NULL){
  
  # create an index to subset a portion of the data for training and testing
  if(is.null(training_indices)){
    train <- as.logical(rbinom(length(y), 1, training_perc))
  }else{
    train <- training_indices
  }
  
  # there are more elegant ways to function calls, but for now, we use a series
  # of if statements to call different functions so we can add more later
  if(method=="blasso"){
    dat <- list(X=X, y=y, indices=train, revjump = revjump, iter=iter, 
                beta_matrix=beta_matrix, pip_thresh = pip_thresh)
    in.dat <- list(); length(in.dat) <- nchains
    for(i in 1:length(in.dat)){
      in.dat[[i]] <- dat
    }
    # note that we could use these chains to then calculate convergence, however,
    # at that point it is unclear why we would use monomvn and not just custom
    # scripts to run the models in e.g. JAGS because the amount of post-processing
    # that we would have to do to calculate convergence, update models until we
    # reach convergence, and average across model runs would add a lot of time
    # to the runs which is part of the advantage of using monomvn

    ##    if(nchains>1){
    ##      out <- parallel::mclapply(X=in.dat, FUN=fun_par)
    ##    }else{
    ## out <- fun_par(dat)
    ##    }

    out <- fun_par(dat)
    
    # need to add in function for convergence diagnostics
    # and averaging across chains and iterations
  }
  
  if(method == "horseshoe"){
    out <- run_horseshoe(
      X = X, y = y, indices = train, revjump = revjump, 
      iter = iter, beta_matrix = beta_matrix, pip_thresh = pip_thresh
    )
  }
  
  if(method=="lasso"){
    out <- run_lasso(X, y, train)
  }
  
  if(method=="ridge"){
    out <- run_ridge(X, y, train)
  }
  output <- out
  output[[4]] <- train
  names(output)[4] <- "train_indices"
  return(output)
}

#' Run Bayesian LASSO on training and test data
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training or test data
#' @param revjump logical; if TRUE, uses reversible jump MCMC
#' 
#' @return a list containing point estimates for beta coefficients, simulated
#' observations (y.sim) based on coefficients for the training data (i.e. for
#' a posterior predictive check), and predicted observations for test data
#' 
run_blasso <- function(X, y, indices, 
                       revjump=TRUE, iter=2000, prior="default",
                       beta_matrix=FALSE){
  # run the model using only the training data
  
  mod <- monomvn::blasso(X[indices,], y[indices], RJ=revjump, 
                         T=iter, case=prior)
  
  # beta estimates and predictions across all iterations
  beta.post.ma <- mod$beta
  
  est.betas.ma <- apply(beta.post.ma, 2, mean)
  est.betas.nz <- apply(beta.post.ma, 2, function(x){
    if(sum(x != 0) == 0){
      return(0)
    } else{
      return(mean(x[x != 0]))
    }
  })
  
  
  # within- and out-of-sample predictions
  # the old script mentions predicting for each draw of beta
  # but since we just take the mean after that, the following 
  # approach is identical (for means) and easier to implement for the
  # non-model-averaged estimates
  y.sim.ma <- as.double(X[indices, ] %*% est.betas.ma)
  y.sim.nz <- as.double(X[indices, ] %*% est.betas.nz)
  
  # we also want to make out-of-sample predictive checks, i.e. our test data.
  if(!all(indices)){
    y.pred.ma <- as.double(X[!indices, ] %*% est.betas.ma)
    y.pred.nz <- as.double(X[!indices, ] %*% est.betas.nz)
  }else{y.pred.ma <- NULL; y.pred.nz <- NULL}
  
  # can return matrix of betas if desired
  if(beta_matrix){
    betas.ma <- beta.post.ma
  } else{
    betas.ma <- est.betas.ma
  }
  
  return(list(
    betas.ma=betas.ma, betas.nz = est.betas.nz, 
    y.sim.ma=y.sim.ma, y.sim.nz=y.sim.nz,
    y.pred.ma=y.pred.ma, y.pred.nz=y.pred.nz
  ))
}



#' Run Bayesian regression with Horseshoe shrinkage priors on training and test data
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training or test data
#' @param revjump logical; if TRUE, uses reversible jump MCMC
#' 
#' @return a list containing point estimates for beta coefficients, simulated
#' observations (y.sim) based on coefficients for the training data (i.e. for
#' a posterior predictive check), and predicted observations for test data
run_horseshoe <- function(X, y, indices, 
                       revjump=TRUE, iter=2000, prior="hs",
                       beta_matrix=FALSE){
  # run the model using only the training data
  
  mod <- monomvn::blasso(X[indices,], y[indices], RJ=revjump, 
                         T=iter, case=prior)
  
  # beta estimates and predictions across all iterations
  beta.post.ma <- mod$beta
  
  est.betas.ma <- apply(beta.post.ma, 2, mean)
  est.betas.nz <- apply(beta.post.ma, 2, function(x){
    if(sum(x != 0) == 0){
      return(0)
    } else{
      return(mean(x[x != 0]))
    }
  })
  
  # within- and out-of-sample predictions
  # the old script mentions predicting for each draw of beta
  # but since we just take the mean after that, the following 
  # approach is identical (for means) and easier to implement for the
  # non-model-averaged estimates
  y.sim.ma <- as.double(X[indices, ] %*% est.betas.ma)
  y.sim.nz <- as.double(X[indices, ] %*% est.betas.nz)
  
  # we also want to make out-of-sample predictive checks, i.e. our test data.
  if(!all(indices)){
    y.pred.ma <- as.double(X[!indices, ] %*% est.betas.ma)
    y.pred.nz <- as.double(X[!indices, ] %*% est.betas.nz)
  }else{y.pred.ma <- NULL; y.pred.nz <- NULL}
  
  # can return matrix of betas if desired
  if(beta_matrix){
    betas.ma <- beta.post.ma
  } else{
    betas.ma <- est.betas.ma
  }
  
  return(list(
    betas.ma=betas.ma, betas.nz = est.betas.nz, 
    y.sim.ma=y.sim.ma, y.sim.nz=y.sim.nz,
    y.pred.ma=y.pred.ma, y.pred.nz=y.pred.nz
  ))
}


#' Run LASSO on training and test data
#' @param X a matrix of covariates with nrow equal to length of y
#' @param y a vector of observed values
#' @param indices a vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training or test data
run_lasso <- function(X, y, indices){
  mod <- monomvn::regress(X[indices,], y[indices], method="lasso", validation = "Cp")
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
#' @param indices a vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training or test data
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

