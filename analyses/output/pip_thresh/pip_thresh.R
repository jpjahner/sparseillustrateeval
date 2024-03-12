#############################
## pip_thresh.R
#############################

## JPJ 26 xii 23

## before running script: module load arcc/1.0 gcc/12.2.0 r/4.2.2 gsl/2.7.1 openblas/0.3.21
## example usage: Rscript pip_thresh.R /project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario1_rep1.rds

args <- commandArgs(TRUE)
.libPaths("/project/modelscape/share/Rlibs/")
function_list <- list.files("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/functions", full.names=T)
lapply(function_list, source)

filefullpath <- args[1]
filebasename <- basename(args[1])
scenario_count <- sub("^([[:graph:]]*)_rep.*", "\\1", filebasename)
rep_count <- sub(".*(rep[[:digit:]]+).*", "\\1", filebasename)

sim <- readRDS(filefullpath)
set.seed(as.numeric(Sys.time())%%100 + Sys.getpid())
indices <- logical(length(sim$y))
indices[1:(length(sim$y)-500)] <- TRUE

out_mat <- matrix(NA, length(sim$beta), 5)
out_mat[,1] <- sim$beta

## PIP locations in output
	## susie: mod_susie$pip
	## gemma: the gamma column in the *param.txt file found in /output
	## blasso: colMeans(mod_blasso$betas.ma != 0)
	## horseshoe: colMeans(mod_horseshoe$betas.ma != 0)

## Note: to calculate PIPs for blasso and horseshoe, beta_matrix must be true
	## this is different than how we run things in sparsity.R




# SUSIE ---------------------------------------------------------------------
mod_susie <- susieR::susie(sim$X[indices,],sim$y[indices], L = 10,max_iter = 1000)
out_mat[,2] <- mod_susie$pip
rm(mod_susie)
cat("Susie complete", fill=T)


# GEMMA ---------------------------------------------------------------
mod_gemma <- run_gemma(sim$X, sim$y, indices=indices)
gemma_pip_file <- read.delim(paste0("output/", dir("output/", pattern="*param.txt")[1]), header=T)
out_mat[,3] <- gemma_pip_file[,7]
rm(mod_gemma)
cat("Gemma complete", fill=T)


# BLASSO ----------------------------------------------------------------
mod_blasso <- run_blasso(sim$X, sim$y, indices, beta_matrix = T)
out_mat[,4] <- colMeans(mod_blasso$betas.ma != 0)
rm(mod_blasso)
cat("blasso complete", fill=T)


# HORSESHOE -------------------------------------------------------------
mod_horseshoe <- run_horseshoe(sim$X, sim$y, indices, beta_matrix = T)
out_mat[,5] <- colMeans(mod_horseshoe$betas.ma != 0)
rm(mod_horseshoe)
cat("horseshoe complete", fill=T)


write.table(out_mat, file=paste0("pips_", scenario_count, "_", rep_count, ".txt"), quote=F, row.names=F, col.names=F)



