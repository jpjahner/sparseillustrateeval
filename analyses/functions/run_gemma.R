#' Run GEMMA method
#' @param X a matrix of predictor variables with nrow equal to the length of y
#' @param y a vector of the response variable with length equal to nrow of X
#' @param indices a logical vector of length equal to length of y indicating if an
#' observation and associated covariates are part of the training set
#' @return a list of the number of predictors with a non-zero effect, estimated effects, and predicted responses
run_gemma <-function(X, y, indices){
  y[!indices] <- NA
tmpy <- tempfile(tmpdir = ".", fileext = ".txt")
tmpx <- tempfile(tmpdir = ".", fileext = ".txt")
tmp_rel <- tempfile(tmpdir = "", fileext = "")
tmp_sim<-tempfile(tmpdir = "", fileext = "")
write.table(y, row.names = FALSE, col.names = FALSE, file=tmpy)

  x.t <-t(X)
  X_sims <- cbind(1:length(rowMeans(x.t)), rowMeans(x.t), rowMeans(x.t), x.t)
  write.table(X_sims, row.names = FALSE, col.names = FALSE, file=tmpx)

sysctl1 <- paste0("/project/modelscape/bin/gemma -g ", tmpx," -notsnp -p ", tmpy, " -gk 1 -o .", tmp_rel)

sysctl2 <- paste0("/project/modelscape/bin/gemma -g ", tmpx, " -notsnp -p ", tmpy," -k output", tmp_rel, ".cXX.txt -bslmm 1 -s 5000 -w 5000 -o .", tmp_sim)

sysctl3 <- paste0("/project/modelscape/bin/gemma -g ", tmpx, " -notsnp -p ", tmpy, " -epm output", tmp_sim, ".param.txt -emu output", tmp_sim, ".log.txt -ebv output", tmp_sim, ".bv.txt -k output", tmp_rel, ".cXX.txt -predict 1 -o .", tmp_sim, ".pheno")

system(sysctl1, wait=T)
system(sysctl2, wait=T)
system(sysctl3, wait=T)

  hyp.params <- read.table(paste0("output", tmp_sim, ".hyp.txt"), header=TRUE)

  n.gamma <- c("n.gamma",mean(hyp.params$n_gamma),quantile(hyp.params$n_gamma, probs=c(0.5,0.025,0.975)))

  params <- read.table(paste0("output", tmp_sim, ".param.txt"),header=T,sep="\t")
  params["eff"]<-params$beta*params$gamma

 y.pred <-  read.table(paste0("output", tmp_sim, ".pheno.prdt.txt"), header=FALSE)

  return(list(n.betas=n.gamma, betas=params["eff"], alphas=params["alpha"], y.pred=y.pred))
}
