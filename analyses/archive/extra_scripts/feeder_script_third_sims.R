#!/usr/bin/env R
args<-commandArgs(TRUE)

## feeder script iterates over different analysis methods for a single simulation

#### load packages####
.libPaths("/project/modelscape/share/Rlibs/r4.0/")

require('susieR')
require('tidyverse')
require('caret')
require('Metrics') 
require('coda')
require('monomvn') 
require('MASS') 
require('ggplot2') 
require('stringr') 
require('tidyr')
require('randomForest')

cat("feeder_script_third_sims.R running on", R.utils::System$getHostname(), fill=TRUE)

###load our internal functions 
source("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/gemma_function.R")
source("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/monomvn_function.R")
source("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/random_forest_function.R")
source("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/performance_metrics.R")

sim<-readRDS(args[1])

##  previously was always 42
set.seed(as.numeric(Sys.time())%%100 + Sys.getpid())

methods_times <- numeric(6)
###start with Monomvn
training_perc <- 0.8

### what are all of the different ways you can do monomvn - go through Eliza's function!
###
cat("monomvn lasso ------", fill=TRUE)
methods_times[1] <- system.time(out_list_monomvn_lasso <- monomvn_function(sim$X, sim$y, method='lasso', revjump = T, training_perc = 0.8))[3]
parameter_metrics_monomvn_lasso <- parameter_metrics(sim$beta, out_list_monomvn_lasso$betas)
prediction_metrics_monomvn_lasso <- prediction_metrics(sim$y[out_list_monomvn_lasso$train_indices==FALSE],
                                                     out_list_monomvn_lasso$y.pred)

cat("monomvn ridge ------", fill=TRUE)
methods_times[2] <- system.time(out_list_monomvn_ridge <- monomvn_function(sim$X, sim$y, method='ridge', revjump = T, training_perc = 0.8))[3]
parameter_metrics_monomvn_ridge <- parameter_metrics(sim$beta, out_list_monomvn_ridge$betas)
prediction_metrics_monomvn_ridge <- prediction_metrics(sim$y[out_list_monomvn_ridge$train_indices==FALSE],
                                                       out_list_monomvn_ridge$y.pred)

cat("monomvn blasso ------", fill=TRUE)
methods_times[3] <- system.time(out_list_monomvn_blasso <- monomvn_function(sim$X, sim$y, method='blasso', revjump = T, training_perc = 0.8))[3]
parameter_metrics_monomvn_blasso <- parameter_metrics(sim$beta, out_list_monomvn_blasso$betas)
prediction_metrics_monomvn_blasso <- prediction_metrics(sim$y[out_list_monomvn_blasso$train_indices==FALSE],
                                                        out_list_monomvn_blasso$y.pred)

####Susie####
cat("Susie ------", fill=TRUE)
train_indices <-sample(seq(1:length(sim$y)), size = training_perc * length(sim$y))
test_indices <- setdiff(seq(1:length(sim$y)), train_indices)
methods_times[4] <- system.time(out_list_susie <-susie(sim$X[train_indices, ],sim$y[train_indices], L = 10,max_iter = 1000))[3]
parameter_metrics_susie <-parameter_metrics(sim$beta, coef(out_list_susie)[-1])
prediction_metrics_susie <-prediction_metrics(sim$y[test_indices],out_list_susie$fitted[test_indices])

###Gemma#####
cat("Gemma ------ ", fill=TRUE)
methods_times[5]<-system.time(gemma_out<-gemma_function(dataset = sim, test_indices))[3]
parameter_metrics_gemma<-parameter_metrics(sim$beta,data.frame(gemma_out[4]))
prediction_metrics_gemma<-prediction_metrics(sim$y[test_indices],
                                             data.frame(gemma_out[5])[!is.na(data.frame(gemma_out[5]))])
  
###Random forest###
cat("Random Forest ------", fill=TRUE)
methods_times[6]<-system.time(forest_out<-run_forest(sim$X,sim$y, test_indices))[3]
parameter_metrics_randomforest<-parameter_metrics(sim$beta,forest_out$importance)
prediction_metrics_randomforest<-prediction_metrics(sim$y[test_indices], forest_out$pred_train)

rm(sim)
save.image(file=args[2])

