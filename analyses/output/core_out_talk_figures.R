##################################################
## core_out.R
##################################################

## CAB 20 Nov 2023
## Purpose: make plots to build up illustration of core output based on scenario difficulty


##################################################
## load data and packages
##################################################

library(dplyr)		## for mutate function
library(MetBrewer)	## for plot colors

input <- read.delim("metrics_out_10mar23_NEW_gemma.txt", header=TRUE, sep=" ")
#input <- read.delim("metrics_out_10mar23_NEW_gemma.txt", header=TRUE, sep=" ")

scenarios <- read.csv("sim_grid_round5.csv", header=TRUE)
#scenarios <- read.csv("sim_grid_round5.csv", header=TRUE)


##################################################
## calculate f1 scores (andrew's code)
##################################################

input_f1 <- left_join(input, scenarios) %>%
	mutate(	par_fnr = 1 - par_tpr,		## false negative rate
			par_tnr = 1 - par_fpr,		## true negative rate
			tp = par_tpr*10,			## true positives
			fp = par_fpr*(p-10),		## false positives
			fn = par_fnr*10,			## false negatives
			f1_score = (2*tp)/(2*tp + fp + fn))

##################################################
## organizing plotting data
##################################################

scen_betas <- unique(scenarios[,6])
scen_ns <- unique(scenarios[,2])
scen_ps <- unique(scenarios[,3])
uniq_scenarios <- vector()
scen_ctr <- 0
for (i in 1:length(scen_betas))
	{
	for (j in 1:length(scen_ns))
		{
		for (k in 1:length(scen_ps))
			{
			scen_ctr <- scen_ctr + 1
			uniq_scenarios[scen_ctr] <- scenarios[scenarios[,6]==scen_betas[i] & scenarios[,2]==scen_ns[j] & scenarios[,3]==scen_ps[k], 1]
			}
		}
	}

uniq_analyses <- c("forest", "gridge", "ridge", "elastic", "glasso", "lasso", "blasso_ma", "blasso_nz", "horseshoe_ma", "horseshoe_nz", "spike_slab", "susie", "gemma")
uniq_metrics <- c("Parameter RMSE", "IS R2", "F1", "OOS Prediction R2", "Runtime ln(s)")
uniq_metric_cols <- c(11,7,27,5,15)
	
plot_mat <- matrix(NA, length(uniq_analyses)*length(uniq_scenarios)*length(uniq_metric_cols), 5)
	dim(plot_mat)
ctr <- 0
for (i in 1:length(uniq_scenarios))
	{
	for (j in 1:length(uniq_analyses))
		{
		for (k in 1:length(uniq_metrics))
			{
			ctr <- ctr + 1
			plot_mat[ctr,1] <- uniq_scenarios[i]
			plot_mat[ctr,2] <- uniq_analyses[j]
			plot_mat[ctr,3] <- uniq_metrics[k]
			if (uniq_metrics[k]=="Runtime ln(s)") { plot_mat[ctr,4] <- log(median(input_f1[input_f1[,1]==uniq_scenarios[i] & input_f1[,3]==uniq_analyses[j], uniq_metric_cols[k]])) }
			else { plot_mat[ctr,4] <- median(input_f1[input_f1[,1]==uniq_scenarios[i] & input_f1[,3]==uniq_analyses[j], uniq_metric_cols[k]]) }
			plot_mat[ctr,5] <- paste(uniq_analyses[j], uniq_metrics[k])
			}
		}
	}
plot_mat <- subset(plot_mat, plot_mat[,5]!="blasso_nz Runtime ln(s)" & plot_mat[,5]!="horseshoe_nz Runtime ln(s)")
	## no time for blasso_nz and horseshoe_nz
plot_mat <- subset(plot_mat, plot_mat[,5]!="forest Parameter RMSE")
	## no parameter RMSE for random forest
plot_mat <- subset(plot_mat, plot_mat[,5]!="gridge F1")
	## no parameter selection for ridge
plot_mat <- subset(plot_mat, plot_mat[,5]!="ridge F1")
	## no parameter selection for ridge
plot_mat <- subset(plot_mat, plot_mat[,5]!="blasso_ma F1")
	## no parameter selection for blasso
plot_mat <- subset(plot_mat, plot_mat[,5]!="blasso_nz F1")
	## no parameter selection for blasso
plot_mat <- subset(plot_mat, plot_mat[,5]!="horseshoe_ma F1")
	## no parameter selection for horseshoe
plot_mat <- subset(plot_mat, plot_mat[,5]!="horseshoe_nz F1")
	## no parameter selection for horseshoe




##################################################
## plotting
##################################################

plot_colors <- c("white", met.brewer("Redon", 12, "discrete"))
diff_colors <- rev(met.brewer("Hokusai1", 36, "continuous"))
runtime_analyses <- c("forest", "gridge", "ridge", "elastic", "glasso", "lasso", "blasso_ma", "horseshoe_ma", "spike_slab", "susie", "gemma")
runtime_plot_colors <- plot_colors[-c(8,10)]
par_rmse_analyses <- c("gridge", "ridge", "elastic", "glasso", "lasso", "blasso_ma", "blasso_nz", "horseshoe_ma", "horseshoe_nz", "spike_slab", "susie", "gemma")
par_rmse_plot_colors <- plot_colors[-1]
f1_analyses <-c("forest", "elastic", "glasso", "lasso", "spike_slab", "susie", "gemma")
f1_plot_colors <- plot_colors[-c(2,3,7,8,9,10)]
legend_names <- c("Random Forest", "Ridge (glmnet)", "Ridge (monomvn)", "Elastic Net", "LASSO (glmnet)", "LASSO (monomvn)", "BLASSO (ma)", "BLASSO (nz)", "Horseshoe (ma)", "Horseshoe (nz)", "Spike-and-slab", "SuSiE", "BSLMM")
xaxis_labels <- rep(c(expression(italic("P")~"= 100"), expression(italic("P")~"= 1000"), expression(italic("P")~"= 10000"), expression(italic("P")~"= 100000")), 9)


## process signal boxes for IS and OOS prediction
	## beta = 0.1: 0.09732011
	## beta = 0.3: 0.4715198
	## beta = 0.8: 0.858247



#quartz(height=6, width=10)
pdf(file="core_out_empty.pdf", height=6, width=10)
par(mar=c(6,5,3,0.5))
plot(0, type="n", xlim=c(0,36), ylim=c(0,1), xaxt="n", xlab="", 
     ylab=bquote("In-sample R"^2), cex.lab=1.5, cex.axis=1.25, las=1)
i<-2 ## 2 is in-sample R^2, 3 is out-of-sample
plot.setup()
text(rep(33.5,length(legend_names)), seq(0.6, 0, length.out=length(legend_names)), legend_names, cex=0.6, adj=0)
points(rep(33, length(legend_names)), seq(0.6, 0, length.out=length(legend_names)), pch=21, bg=plot_colors, cex=1.5)
dev.off()

#quartz(height=6, width=10)
pdf(file="core_out_insample.pdf", height=6, width=10)
par(mar=c(6,5,3,0.5))
plot(0, type="n", xlim=c(0,36), ylim=c(0,1), xaxt="n", xlab="", 
     ylab=bquote("In-sample R"^2), cex.lab=1.5, cex.axis=1.25, las=1)
i<-2 ## 2 is in-sample R^2, 3 is out-of-sample
plot.setup()
for (j in 1:length(uniq_analyses)){
  analyses_sub <- subset(plot_mat, plot_mat[,3]==uniq_metrics[i] & plot_mat[,2]==uniq_analyses[j])
  temp_vect <- numeric(length(uniq_scenarios))
  for (k in 1:length(uniq_scenarios)) { 
    temp_vect[k] <- analyses_sub[analyses_sub[,1]==uniq_scenarios[k],4] 
  }
  lines(c(1:36)-0.5, temp_vect, type="o", pch=21, bg=plot_colors[j],cex=1.5)
}
text(rep(33.5,length(legend_names)), seq(0.6, 0, length.out=length(legend_names)), legend_names, cex=0.6, adj=0)
points(rep(33, length(legend_names)), seq(0.6, 0, length.out=length(legend_names)), pch=21, bg=plot_colors, cex=1.5)
dev.off()

#quartz(height=6, width=10)
pdf(file="core_out_oos.pdf", height=6, width=10)
par(mar=c(6,5,3,0.5))
plot(0, type="n", xlim=c(0,36), ylim=c(0,1), xaxt="n", xlab="", 
     ylab=bquote("Out-of-sample R"^2), cex.lab=1.5, cex.axis=1.25, las=1)
i<-4 ## 2 is in-sample R^2, 3 is out-of-sample
plot.setup()
for (j in 1:length(uniq_analyses)){
  analyses_sub <- subset(plot_mat, plot_mat[,3]==uniq_metrics[i] & plot_mat[,2]==uniq_analyses[j])
  temp_vect <- numeric(length(uniq_scenarios))
  for (k in 1:length(uniq_scenarios)) { 
    temp_vect[k] <- analyses_sub[analyses_sub[,1]==uniq_scenarios[k],4] 
  }
  lines(c(1:36)-0.5, temp_vect, type="o", pch=21, bg=plot_colors[j],cex=1.5)
}
text(rep(1.5,length(legend_names)), seq(1, 0.4, length.out=length(legend_names)), legend_names, cex=0.6, adj=0)
points(rep(1, length(legend_names)), seq(1, 0.4, length.out=length(legend_names)), pch=21, bg=plot_colors, cex=1.5)
dev.off()

plot.setup<-function(){
  axis(1, at=c(1:36)-0.5, labels=xaxis_labels, las=2)
  rect(par('usr')[1], par('usr')[3], 12, 0.09732011, col="light gray")
  rect(12, par('usr')[3], 24, 0.4715198, col="light gray")
  rect(24, par('usr')[3], par('usr')[2], 0.858247, col="light gray")
  abline(v=0, lty=2)
  abline(v=4, lty=2)
  abline(v=8, lty=2)
  abline(v=12, lwd=1.5)
  abline(v=16, lty=2)
  abline(v=20, lty=2)
  abline(v=24, lwd=1.5)
  abline(v=28, lty=2)
  abline(v=32, lty=2)
  abline(v=36, lty=2)
  mtext(expression(italic("N")~"= 50"), side=3, line=0.25, cex=0.65, adj=0.05)
  mtext(expression(italic("N")~"= 150"), side=3, line=0.25, cex=0.65, adj=0.16)
  mtext(expression(italic("N")~"= 500"), side=3, line=0.25, cex=0.65, adj=0.28)
  mtext(expression(italic("N")~"= 50"), side=3, line=0.25, cex=0.65, adj=0.39)
  mtext(expression(italic("N")~"= 150"), side=3, line=0.25, cex=0.65, adj=0.5)
  mtext(expression(italic("N")~"= 500"), side=3, line=0.25, cex=0.65, adj=0.61)
  mtext(expression(italic("N")~"= 50"), side=3, line=0.25, cex=0.65, adj=0.72)
  mtext(expression(italic("N")~"= 150"), side=3, line=0.25, cex=0.65, adj=0.84)
  mtext(expression(italic("N")~"= 500"), side=3, line=0.25, cex=0.65, adj=0.95)
  mtext(bquote(beta[causal]==0.1), side=3, line=1.25, cex=1, adj=0.12)
  mtext(bquote(beta[causal]==0.3), side=3, line=1.25, cex=1, adj=0.5)
  mtext(bquote(beta[causal]==0.8), side=3, line=1.25, cex=1, adj=0.88)
  mtext(expression(bold(symbol('\255'))), line=-2.5, adj=0, cex=1.75)
  box(lwd=1.25)
}



