# Workspace setup -------------------------------------------------------------

args <- commandArgs(TRUE)

.libPaths("/project/modelscape/share/Rlibs/")

cat("script running on", R.utils::System$getHostname(), fill=TRUE)

function_list <- list.files("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/functions", full.names = T)
lapply(function_list, source)

methods <- c("susie", 'gridge', 'glasso', 'elastic', "gemma", "blasso", "horseshoe", "lasso", "ridge", "forest", "spike_slab")

runtime <- numeric(length=length(methods))
names(runtime) <- methods

filebasename<-basename(args[1])
filefullpath<-args[1]
## prepend method before basename to construct outfile name 
# Sim load -----------------------------------------------------------------
sim<-readRDS(filefullpath)

set.seed(as.numeric(Sys.time())%%100 + Sys.getpid())
indices <- logical(length(sim$y))
indices[1:(length(sim$y)-500)] <- TRUE

# SUSIE ---------------------------------------------------------------------

runtime['susie'] <- system.time(
  mod_susie <- susieR::susie(sim$X[indices,],sim$y[indices], L = 10,max_iter = 1000))[3]

par_susie <- parameter_metrics(sim$beta, coef(mod_susie)[-1])
pred_susie <- prediction_metrics(sim$y[!indices], predict(mod_susie, sim$X[!indices,], type="response"))
pred_susieWS <- prediction_metrics(sim$y[indices], predict(mod_susie, sim$X[indices,], type="response"))
rm(mod_susie)

cat("Susie complete", fill=T)

# glmnet lasso, ridge, and elasticnet --------------------------------------------
alpha <- 0
runtime['gridge'] <- system.time(mod_gridge <- run_glmnet(sim$X, sim$y, indices, alpha = alpha))[3]
par_gridge <- parameter_metrics(sim$beta, mod_gridge$betas, 2)
pred_gridge <- prediction_metrics(sim$y[!indices], mod_gridge$y.pred)
pred_gridgeWS <- prediction_metrics(sim$y[indices], mod_gridge$y.sim)
rm(mod_gridge)
cat("gridge complete", fill=T)
### ----
alpha <- 1
runtime['glasso'] <- system.time(mod_glasso <- run_glmnet(sim$X, sim$y, indices, alpha = alpha))[3]
par_glasso <- parameter_metrics(sim$beta, mod_glasso$betas, 2)
pred_glasso <- prediction_metrics(sim$y[!indices], mod_glasso$y.pred)
pred_glassoWS <- prediction_metrics(sim$y[indices], mod_glasso$y.sim)
rm(mod_glasso)
cat("glasso complete", fill=T)
### ----
alpha <- 0.5
runtime['elastic'] <- system.time(mod_elastic <- run_glmnet(sim$X, sim$y, indices, alpha = alpha))[3]
par_elastic <- parameter_metrics(sim$beta, mod_elastic$betas, 2)
pred_elastic <- prediction_metrics(sim$y[!indices], mod_elastic$y.pred)
pred_elasticWS <- prediction_metrics(sim$y[indices], mod_elastic$y.sim)
rm(mod_elastic, alpha)
cat("elastic complete", fill=T)
# GEMMA ---------------------------------------------------------------

runtime['gemma'] <- system.time(
    ## mod_gemma <- run_gemma(sim$X, sim$y, indices=rep(T, length(sim$y))))[3]
    mod_gemma <- run_gemma(sim$X, sim$y, indices=indices))[3] #we changed this here, because we didn't understand the line above this. gemma runs through like this, but tpr breaks.

par_gemma <- parameter_metrics(sim$beta, mod_gemma$betas$eff)
total_effect<-mod_gemma$betas$eff+mod_gemma$alphas$alpha ### need both the alpha and beta effects to predict phenotype
gemma.sim <- sim$X[indices,] %*% total_effect
pred_gemma <- prediction_metrics(sim$y[!indices], mod_gemma$y.pred[!indices,])
pred_gemmaWS <- prediction_metrics(sim$y[indices], gemma.sim)

rm(mod_gemma)
cat("Gemma complete", fill=T)

# BLASSO ----------------------------------------------------------------

if (grepl("genomic", filebasename)==FALSE & grepl("bigN", filebasename)==FALSE){  ## blasso and horseshoe not run for genomic and bigN scenarios
runtime['blasso'] <- system.time(
  mod_blasso <- run_blasso(sim$X, sim$y, indices, beta_matrix = F))[3]

par_blasso_ma <- parameter_metrics(sim$beta, mod_blasso$betas.ma)
par_blasso_nz <- parameter_metrics(sim$beta, mod_blasso$betas.nz)
pred_blasso_ma <- prediction_metrics(sim$y[!indices], mod_blasso$y.pred.ma)
pred_blasso_nz <- prediction_metrics(sim$y[!indices], mod_blasso$y.pred.nz)
pred_blasso_maWS <- prediction_metrics(sim$y[indices], mod_blasso$y.sim.ma)
pred_blasso_nzWS <- prediction_metrics(sim$y[indices], mod_blasso$y.sim.nz)
rm(mod_blasso)
cat("blasso complete", fill=T)

# HORSESHOE -------------------------------------------------------------

runtime['horseshoe'] <- system.time(
  mod_horseshoe <- run_horseshoe(sim$X, sim$y, indices, beta_matrix = F))[3]

par_horseshoe_ma <- parameter_metrics(sim$beta, mod_horseshoe$betas.ma)
par_horseshoe_nz <- parameter_metrics(sim$beta, mod_horseshoe$betas.nz)
pred_horseshoe_ma <- prediction_metrics(sim$y[!indices], mod_horseshoe$y.pred.ma)
pred_horseshoe_nz <- prediction_metrics(sim$y[!indices], mod_horseshoe$y.pred.nz)
pred_horseshoe_maWS <- prediction_metrics(sim$y[indices], mod_horseshoe$y.sim.ma)
pred_horseshoe_nzWS <- prediction_metrics(sim$y[indices], mod_horseshoe$y.sim.nz)
rm(mod_horseshoe)
cat("horseshoe complete", fill=T)
} else { cat("blasso and horseshoe not performed for genomic scenarios", fill=T) }

# LASSO -----------------------------------------------------------------

runtime['lasso'] <- system.time(
  mod_lasso <- run_lasso(sim$X, sim$y, indices))[3]

par_lasso <- parameter_metrics(sim$beta, mod_lasso$betas)
pred_lasso <- prediction_metrics(sim$y[!indices], mod_lasso$y.pred)
pred_lassoWS <- prediction_metrics(sim$y[indices], mod_lasso$y.sim)
rm(mod_lasso)
cat("lasso complete", fill=T)

## Ridge -----------------------------------------------------------------

runtime['ridge'] <- system.time(
  mod_ridge <- run_ridge(sim$X, sim$y, indices))[3]

par_ridge <- parameter_metrics(sim$beta, mod_ridge$betas)
pred_ridge <- prediction_metrics(sim$y[!indices], mod_ridge$y.pred)
pred_ridgeWS <- prediction_metrics(sim$y[indices], mod_ridge$y.sim)
rm(mod_ridge)
cat("ridge complete", fill=T)

# Random forest ---------------------------------------------------------

runtime['forest'] <- system.time(
  mod_forest <- run_forest(sim$X, sim$y, indices))[3]

par_forest <- parameter_metrics(sim$beta, mod_forest$importance)
pred_forest <- prediction_metrics(sim$y[!indices], mod_forest$y.pred)
pred_forestWS <- prediction_metrics(sim$y[indices], mod_forest$y.sim)
rm(mod_forest)
cat("Random forest complete", fill=T)

# Spike slab ---------------------------------------------------------

runtime['spike_slab'] <- system.time(
  mod_spike_slab <- run_spike_slab2(sim$X, sim$y, indices))[3]

 par_spike_slab <- parameter_metrics(sim$beta, mod_spike_slab$betas)
 pred_spike_slab <- prediction_metrics(sim$y[!indices], mod_spike_slab$y.pred)
pred_spike_slabWS <- prediction_metrics(sim$y[indices], mod_spike_slab$y.sim)

 rm(mod_spike_slab)

cat("Spike and slab complete", fill=T)

### save measures after removing simulation and functions
rm(sim)
rm(list=ls()[sapply(mget(ls(), .GlobalEnv), class)=="function"])
save.image(file=paste0("out_", filebasename))
