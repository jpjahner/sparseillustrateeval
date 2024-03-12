
gemma_function<-function(dataset, test_indices){
  dataset$y[test_indices]<-NA
  write.table(dataset$y, row.names = FALSE, col.names = FALSE, file="ysims.txt")
  phenotypes<-t(dataset$X)
  X_sims <- cbind(1:length(rowMeans(phenotypes)), rowMeans(phenotypes), rowMeans(phenotypes), phenotypes)
  write.table(X_sims, row.names = FALSE, col.names = FALSE, file="Xsims.txt")

  system("/project/evolgen/bin/gemma -g Xsims.txt -notsnp -p ysims.txt -gk 1 -o relmatrix", wait=TRUE)
  system("/project/evolgen/bin/gemma -g Xsims.txt -notsnp -p ysims.txt -k output/relmatrix.cXX.txt -bslmm 1 -s 5000 -w 5000 -o sims", wait=TRUE)
  system("/project/evolgen/bin/gemma -g Xsims.txt -notsnp -p ysims.txt -epm output/sims.param.txt -emu output/sims.log.txt -ebv output/sims.bv.txt -k output/relmatrix.cXX.txt -predict 1 -o sims.pheno", wait=TRUE)

  hyp.params <- read.table("output/sims.hyp.txt", header=TRUE)
  converged <- ifelse(heidel.diag(hyp.params$pve)[1,4]==1, "converged", "not_converged")
  pve<-c("PVE", mean(hyp.params$pve),quantile(hyp.params$pve, probs=c(0.5,0.025,0.975)))
  ## pge -> proportion of genetic variance explained by major effect loci
  pge <- c("PGE",mean(hyp.params$pge),quantile(hyp.params$pge, probs=c(0.5,0.025,0.975)))
  ## n.gamma -> number of variants with major effect
  n.gamma <- c("n.gamma",mean(hyp.params$n_gamma),quantile(hyp.params$n_gamma, probs=c(0.5,0.025,0.975)))

  params <- read.table("output/sims.param.txt",header=T,sep="\t")
  params["eff"]<-abs(params$beta*params$gamma)

  predicted_phenos <-  read.table("output/sims.pheno.prdt.txt", header=FALSE)
  
  return(list(pve, pge, n.gamma, params["eff"], predicted_phenos))
}

#out_list <- list()
#for(i in 1:length(initial_sims)){
#    out_list[[i]] <- gemma_function(dataset = initial_sims[[i]], 0.8)
#}
  
##where out_list[[1]][[4]]= beta estimates and out_list[[i]][[5]] = predicted phenotypes
