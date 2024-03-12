#############################
## example_fig.R
#############################

## JPJ 6 iv 23

## before running script: module load arcc/1.0 gcc/12.2.0 r/4.2.2 gsl/2.7.1 openblas/0.3.21
## example usage: Rscript example_fig.R /project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario24_rep1.rds

args <- commandArgs(TRUE)

library(MetBrewer)	## for plot colors
plot_colors <- c("white", met.brewer("Redon", 12, "discrete"))

.libPaths("/project/modelscape/share/Rlibs/")
library(vioplot)


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

plot_lim_low <- min(sim$y) * 1.1
plot_lim_high <- max(sim$y) * 1.1

pdf(file=paste0("example_fig_", scenario_count, "_", rep_count, ".pdf"), height=9, width=9)
par(mar=c(5,5,1,1), mfrow=c(3,3), oma=c(0,2,2,0))

## random forest
mod_forest <- run_forest(sim$X, sim$y, indices)
rsq_in <- rsq(sim$y[indices], mod_forest$y.sim)
rsq_out <- rsq(sim$y[!indices], mod_forest$y.pred)
rmse_in <- rmse2(sim$y[indices], mod_forest$y.sim)
rmse_out <- rmse2(sim$y[!indices], mod_forest$y.pred)
tpr_par <- tpr(sim$beta, mod_forest$importance)
tnr_par <- 1 - fpr(sim$beta, mod_forest$importance)
plot(0, type="n", xlim=c(0,1), xaxt="n", ylim=c(min(mod_forest$importance), max(mod_forest$importance)*1.1), xlab="Predictor type", ylab="Importance", cex.lab=2, cex.axis=1.5, las=1)
	axis(side=1, at=c(0.075, 0.6), labels=c("Causal", "Non-causal"), cex.axis=1.5)
	abline(v=0.15, lwd=1.5)
	points(jitter(rep(0.075, 10), 15), mod_forest$importance[sim$beta!=0], pch=21, bg=adjustcolor(plot_colors[1], alpha.f=0.7), cex=1.5)
	vioplot(mod_forest$importance[sim$beta==0], at=0.6, col=plot_colors[1], pchMed=23, colMed="black", colMed2="white", cex=0, add=TRUE)
	mtext(paste0("TNR = ", round(tnr_par, digits=3)), adj=0.36, line=-1.375)
	mtext(paste0("TPR = ", round(tpr_par, digits=3)), adj=0.28, line=-3)
	mtext("Estimation/Selection", line=0.5, cex=1.75)
	mtext("Random Forest", side=2, line=5, cex=1.75)
plot(sim$y[indices], mod_forest$y.sim, type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
	points(sim$y[indices], mod_forest$y.sim, pch=21, bg=adjustcolor(plot_colors[1], alpha.f=0.7), cex=1.5)
	abline(a=0, b=1, lty=1, lwd=2)
	abline(lm(mod_forest$y.sim ~ sim$y[indices]), lty=2, lwd=2)
	mtext(paste0("RMSE = ", round(rmse_in, digits=3)), adj=0.03, line=-1.375)
	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
	mtext(paste0(round(rsq_in, digits=3)), adj=0.25, line=-3)
	mtext("IS Prediction", line=0.5, cex=1.75)
plot(sim$y[!indices], mod_forest$y.pred, type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
	points(sim$y[!indices], mod_forest$y.pred, pch=21, bg=adjustcolor(plot_colors[1], alpha.f=0.7), cex=1.5)
	abline(a=0, b=1, lty=1, lwd=2)
	abline(lm(mod_forest$y.pred ~ sim$y[!indices]), lty=2, lwd=2)
	mtext(paste0("RMSE = ", round(rmse_out, digits=3)), adj=0.03, line=-1.375)
	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
	mtext(paste0(round(rsq_out, digits=3)), adj=0.25, line=-3)
	mtext("OOS Prediction", line=0.5, cex=1.75)
rm(mod_forest)
paste("Forest completed")




## Lasso
mod_lasso <- run_lasso(sim$X, sim$y, indices)
rsq_in <- rsq(sim$y[indices], mod_lasso$y.sim)
rsq_out <- rsq(sim$y[!indices], mod_lasso$y.pred)
rmse_in <- rmse2(sim$y[indices], mod_lasso$y.sim)
rmse_out <- rmse2(sim$y[!indices], mod_lasso$y.pred)
rmse_par <- rmse2(sim$beta, mod_lasso$betas)
tpr_par <- tpr(sim$beta, mod_lasso$betas)
tnr_par <- 1 - fpr(sim$beta, mod_lasso$betas)
plot(0, type="n", xlim=c(0,1), xaxt="n", ylim=c(-0.5,1.15), xlab="Predictor type", ylab="Estimate", cex.lab=2, cex.axis=1.5, las=1)
	axis(side=1, at=c(0.075, 0.6), labels=c("Causal", "Non-causal"), cex.axis=1.5)
	points(0.075, 0.8, pch=23, bg="black", cex=2.5)
	abline(v=0.15, lwd=1.5)
	points(jitter(rep(0.075, 10), 15), mod_lasso$betas[sim$beta!=0], pch=21, bg=adjustcolor(plot_colors[6], alpha.f=0.7), cex=1.5)
	vioplot(mod_lasso$betas[sim$beta==0], at=0.6, col=plot_colors[6], pchMed=23, colMed="black", colMed2="white", cex=0, add=TRUE)
	points(0.6, 0, pch=23, bg="black", cex=2.5)
	mtext(paste0("RMSE = ", round(rmse_par, digits=3)), adj=0.4, line=-1.375)
	mtext(paste0("TNR = ", round(tnr_par, digits=3)), adj=0.36, line=-3)
	mtext(paste0("TPR = ", round(tpr_par, digits=3)), adj=0.31, line=-4.625)
	mtext("LASSO (monomvn)", side=2, line=5, cex=1.75)
plot(sim$y[indices], mod_lasso$y.sim, type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
	points(sim$y[indices], mod_lasso$y.sim, pch=21, bg=adjustcolor(plot_colors[6], alpha.f=0.7), cex=1.5)
	abline(a=0, b=1, lty=1, lwd=2)
	abline(lm(mod_lasso$y.sim ~ sim$y[indices]), lty=2, lwd=2)
	mtext(paste0("RMSE = ", round(rmse_in, digits=3)), adj=0.03, line=-1.375)
	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
	mtext(paste0(round(rsq_in, digits=3)), adj=0.25, line=-3)
plot(sim$y[!indices], mod_lasso$y.pred, type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
	points(sim$y[!indices], mod_lasso$y.pred, pch=21, bg=adjustcolor(plot_colors[6], alpha.f=0.7), cex=1.5)
	abline(a=0, b=1, lty=1, lwd=2)
	abline(lm(mod_lasso$y.pred ~ sim$y[!indices]), lty=2, lwd=2)
	mtext(paste0("RMSE = ", round(rmse_out, digits=3)), adj=0.03, line=-1.375)
	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
	mtext(paste0(round(rsq_out, digits=3)), adj=0.25, line=-3)
rm(mod_lasso)
paste("Lasso completed")





## susie
#mod_susie <- susieR::susie(sim$X[indices,],sim$y[indices], L = 10,max_iter = 1000)
#rsq_in <- rsq(sim$y[indices], predict(mod_susie, sim$X[indices,], type="response"))
#rsq_out <- rsq(sim$y[!indices], predict(mod_susie, sim$X[!indices,], type="response"))
#rmse_in <- rmse2(sim$y[indices], predict(mod_susie, sim$X[indices,], type="response"))
#rmse_out <- rmse2(sim$y[!indices], predict(mod_susie, sim$X[!indices,], type="response"))
#rmse_par <- rmse2(sim$beta, coef(mod_susie)[-1])
#tpr_par <- tpr(sim$beta, coef(mod_susie)[-1])
#tnr_par <- 1 - fpr(sim$beta, coef(mod_susie)[-1])
#plot(0, type="n", xlim=c(0,1), xaxt="n", ylim=c(-0.5,1.15), xlab="Predictor type", ylab="Estimate", cex.lab=2, cex.axis=1.5, las=1)
#	axis(side=1, at=c(0.075, 0.6), labels=c("Causal", "Non-causal"), cex.axis=1.5)
#	points(0.075, 0.8, pch=23, bg="black", cex=2.5)
#	abline(v=0.15, lwd=1.5)
#	points(jitter(rep(0.075, 10), 15), round(coef(mod_susie)[-1], 2)[sim$beta!=0], pch=21, bg=adjustcolor(plot_colors[12], alpha.f=0.7), cex=1.5)
#	vioplot(round(coef(mod_susie)[-1], 2)[sim$beta==0], at=0.6, col=plot_colors[12], pchMed=23, colMed="black", colMed2="white", cex=0, add=TRUE)
#	points(0.6, 0, pch=23, bg="black", cex=2.5)
#	mtext(paste0("RMSE = ", round(rmse_par, digits=3)), adj=0.4, line=-1.375)
#	mtext(paste0("TNR = ", round(tnr_par, digits=3)), adj=0.36, line=-3)
#	mtext(paste0("TPR = ", round(tpr_par, digits=3)), adj=0.31, line=-4.625)
#	mtext("SuSiE", side=2, line=5, cex=1.75)
#plot(sim$y[indices], predict(mod_susie, sim$X[indices,], type="response"), type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
#	points(sim$y[indices], predict(mod_susie, sim$X[indices,], type="response"), pch=21, bg=adjustcolor(plot_colors[12], alpha.f=0.7), cex=1.5)
#	abline(a=0, b=1, lty=1, lwd=2)
#	abline(lm(predict(mod_susie, sim$X[indices,], type="response") ~ sim$y[indices]), lty=2, lwd=2)
#	mtext(paste0("RMSE = ", round(rmse_in, digits=3)), adj=0.03, line=-1.375)
#	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
#	mtext(paste0(round(rsq_in, digits=3)), adj=0.25, line=-3)
#plot(sim$y[!indices], predict(mod_susie, sim$X[!indices,], type="response"), type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
#	points(sim$y[!indices], predict(mod_susie, sim$X[!indices,], type="response"), pch=21, bg=adjustcolor(plot_colors[12], alpha.f=0.7), cex=1.5)
#	abline(a=0, b=1, lty=1, lwd=2)
#	abline(lm(predict(mod_susie, sim$X[!indices,], type="response") ~ sim$y[!indices]), lty=2, lwd=2)
#	mtext(paste0("RMSE = ", round(rmse_out, digits=3)), adj=0.03, line=-1.375)
#	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
#	mtext(paste0(round(rsq_out, digits=3)), adj=0.25, line=-3)
#rm(mod_susie)
#paste("SuSiE completed")





## bslmm (gemma)
mod_gemma <- run_gemma(sim$X, sim$y, indices=indices)
total_effect <- mod_gemma$betas$eff + mod_gemma$alphas$alpha
gemma.sim <- sim$X[indices,] %*% total_effect
rsq_in <- rsq(sim$y[indices], gemma.sim)
rsq_out <- rsq(sim$y[!indices], mod_gemma$y.pred[!indices,])
rmse_in <- rmse2(sim$y[indices], gemma.sim)
rmse_out <- rmse2(sim$y[!indices], mod_gemma$y.pred[!indices,])
rmse_par <- rmse2(sim$beta, mod_gemma$betas$eff)
tpr_par <- tpr(sim$beta, mod_gemma$betas$eff)
tnr_par <- 1 - fpr(sim$beta, mod_gemma$betas$eff)
plot(0, type="n", xlim=c(0,1), xaxt="n", ylim=c(-0.5,1.15), xlab="Predictor type", ylab="Estimate", cex.lab=2, cex.axis=1.5, las=1)
	axis(side=1, at=c(0.075, 0.6), labels=c("Causal", "Non-causal"), cex.axis=1.5)
	points(0.075, 0.8, pch=23, bg="black", cex=2.5)
	abline(v=0.15, lwd=1.5)
	points(jitter(rep(0.075, 10), 15), mod_gemma$betas$eff[sim$beta!=0], pch=21, bg=adjustcolor(plot_colors[13], alpha.f=0.7), cex=1.5)
	vioplot(mod_gemma$betas$eff[sim$beta==0], at=0.6, col=plot_colors[13], pchMed=23, colMed="black", colMed2="white", cex=0, add=TRUE)
	points(0.6, 0, pch=23, bg="black", cex=2.5)
	mtext(paste0("RMSE = ", round(rmse_par, digits=3)), adj=0.4, line=-1.375)
	mtext(paste0("TNR = ", round(tnr_par, digits=3)), adj=0.36, line=-3)
	mtext(paste0("TPR = ", round(tpr_par, digits=3)), adj=0.31, line=-4.625)
	mtext("BSLMM", side=2, line=5, cex=1.75)
plot(sim$y[indices], gemma.sim, type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
	points(sim$y[indices], gemma.sim, pch=21, bg=adjustcolor(plot_colors[13], alpha.f=0.7), cex=1.5)
	abline(a=0, b=1, lty=1, lwd=2)
	abline(lm(gemma.sim ~ sim$y[indices]), lty=2, lwd=2)
	mtext(paste0("RMSE = ", round(rmse_in, digits=3)), adj=0.03, line=-1.375)
	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
	mtext(paste0(round(rsq_in, digits=3)), adj=0.25, line=-3)
plot(sim$y[!indices], mod_gemma$y.pred[!indices,], type="n", xlim=c(plot_lim_low, plot_lim_high), ylim=c(plot_lim_low, plot_lim_high), xlab="Actual", ylab="Predicted", cex.lab=2, cex.axis=1.5, las=1); box(lwd=1.5)
	points(sim$y[!indices], mod_gemma$y.pred[!indices,], pch=21, bg=adjustcolor(plot_colors[13], alpha.f=0.7), cex=1.5)
	abline(a=0, b=1, lty=1, lwd=2)
	abline(lm(mod_gemma$y.pred[!indices,] ~ sim$y[!indices]), lty=2, lwd=2)
	mtext(paste0("RMSE = ", round(rmse_out, digits=3)), adj=0.03, line=-1.375)
	mtext(expression(paste("R"^2, " = ")), adj=0.02, line=-3)
	mtext(paste0(round(rsq_out, digits=3)), adj=0.25, line=-3)
#print(mod_gemma$betas$eff[sim$beta!=0])
rm(mod_gemma)
paste("Bslmm completed")






dev.off()










