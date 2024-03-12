##################################################
## variable_selection_plot.R
##################################################

## JPJ 2 x 23
## Purpose: to plot variable selection results


##################################################
## load data and packages
##################################################

library(dplyr)		## for mutate function
library(MetBrewer)	## for plot colors

input <- read.delim("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/output/metrics_out_12dec23.txt", header=TRUE, sep=" ")
#input <- read.delim("metrics_out_12dec23.txt", header=TRUE, sep=" ")

scenarios <- read.csv("/project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/sim_grid_round5.csv", header=TRUE)
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

uniq_scenarios <- c(1,4,7,2,5,8,3,6,9,10,13,16,11,14,17,12,15,18,19,22,25,20,23,26,21,24,27,28,31,34,29,32,35,30,33,36)
uniq_analyses <- c("forest", "elastic", "glasso", "lasso", "spike_slab", "gemma") ## not including ridges, blassos, horseshoes, susie


plot_mat <- matrix(NA, length(uniq_analyses)*length(uniq_scenarios), 5)
ctr <- 0
for (i in 1:length(uniq_scenarios))
	{
	for (j in 1:length(uniq_analyses))
		{
		ctr <- ctr + 1
		input_f1_sub <- subset(input_f1, input_f1[,1]==uniq_scenarios[i] & input_f1[,3]==uniq_analyses[j])
		plot_mat[ctr,1] <- uniq_scenarios[i]
		plot_mat[ctr,2] <- uniq_analyses[j]
		plot_mat[ctr,3] <- scenarios[scenarios[,1]==i, 3]	## p
		plot_mat[ctr,4] <- median(input_f1_sub[,8])	## par_tpr
		plot_mat[ctr,5] <- median(input_f1_sub[,9])	## par_fpr
		}
	}


##################################################
## f1 backgrounds
##################################################

## f1<-function(p, tp, tn){ 
##   (2*tp) / (2*tp + p-10-tn + 10-tp)
##   }
## 
## tp_list <- seq(0, 10, 1)
## tn_list_100 <- seq(0,1,0.1)*90
## tn_list_1000 <- seq(0,1,0.1)*990
## tn_list_10000 <- seq(0,1,0.1)*9990
## n_list_100000 <- seq(0,1,0.1)*99990
## 
## 
## f1_100 <- matrix(NA, 11, 11)
## f1_1000 <- matrix(NA, 11, 11)
## f1_10000 <- matrix(NA, 11, 11)
## f1_100000 <- matrix(NA, 11, 11)
## for (i in 1:11)
## 	{
## 	for (j in 1:11)
## 		{
## 			f1_100[i,j] <- f1(100, tp_list[i], tn_list_100[j])
## 			f1_1000[i,j] <- f1(1000, tp_list[i], tn_list_1000[j])
## 			f1_10000[i,j] <- f1(10000, tp_list[i], tn_list_10000[j])
## 			f1_100000[i,j] <- f1(100000, tp_list[i], tn_list_100000[j])
## 		}
## 	}




##################################################
## plotting
##################################################

plot_colors <- c("white", met.brewer("Redon", 12, "discrete"))
f1_plot_colors <- plot_colors[c(1,4,5,6,11,13)]

pdf(height=8, width=18, file="variable_selection_plot.pdf")
#quartz(height=8, width=18)
par(mar=c(0.5,0.5,0.5,0.5), mfrow=c(4,9), oma=c(5,5,5,5))
for (i in 1:length(uniq_scenarios))
	{
	plot(0, type="n", xlim=c(-0.02,1.02), ylim=c(-0.02,1.02), axes=FALSE)
	#if (i<10)		{ image(x=seq(0,1,0.1), y=seq(0,1,0.1), z=f1_100, xlab="", ylab="", axes=FALSE, col=hcl.colors(10, "Light Grays", rev=TRUE)) }
	#else if (i<19)	{ image(x=seq(0,1,0.1), y=seq(0,1,0.1), z=f1_1000, xlab="", ylab="", axes=FALSE, col=hcl.colors(10, "Light Grays", rev=TRUE)) }
	#else if (i<28)	{ image(x=seq(0,1,0.1), y=seq(0,1,0.1), z=f1_10000, xlab="", ylab="", axes=FALSE, col=hcl.colors(10, "Light Grays", rev=TRUE)) }
	#else			{ image(x=seq(0,1,0.1), y=seq(0,1,0.1), z=f1_100000, xlab="", ylab="", axes=FALSE, col=hcl.colors(10, "Light Grays", rev=TRUE)); axis(1, labels=c(0.0,0.2,0.4,0.6,0.8,1.0), at=c(0.0,0.2,0.4,0.6,0.8,1.0), cex.axis=1.25) }
	if (i%%9==1) { axis(2, labels=c(0.0,0.2,0.4,0.6,0.8,1.0), at=c(0.0,0.2,0.4,0.6,0.8,1.0), cex.axis=1.25, las=1) }
	if (i>27) { axis(1, labels=c(0.0,0.2,0.4,0.6,0.8,1.0), at=c(0.0,0.2,0.4,0.6,0.8,1.0), cex.axis=1.25) }
	box(lwd=1)

	if (i==2) { mtext(bquote(beta[causal]==0.1), side=3, line=2.5, cex=1.5) }
	if (i==5) { mtext(bquote(beta[causal]==0.3), side=3, line=2.5, cex=1.5) }
	if (i==8) { mtext(bquote(beta[causal]==0.8), side=3, line=2.5, cex=1.5) }

	if      (i==1) { mtext(expression(italic("N =")~"50"), side=3, line=0.5, cex=1.25) }
	if      (i==2) { mtext(expression(italic("N =")~"150"), side=3, line=0.5, cex=1.25) }
	if      (i==3) { mtext(expression(italic("N =")~"500"), side=3, line=0.5, cex=1.25) }
	if      (i==4) { mtext(expression(italic("N =")~"50"), side=3, line=0.5, cex=1.25) }
	if      (i==5) { mtext(expression(italic("N =")~"150"), side=3, line=0.5, cex=1.25) }
	if      (i==6) { mtext(expression(italic("N =")~"500"), side=3, line=0.5, cex=1.25) }
	if      (i==7) { mtext(expression(italic("N =")~"50"), side=3, line=0.5, cex=1.25) }
	if      (i==8) { mtext(expression(italic("N =")~"150"), side=3, line=0.5, cex=1.25) }
	if      (i==9) { mtext(expression(italic("N =")~"500"), side=3, line=0.5, cex=1.25) }

	if (i==9) { mtext(expression(italic("P")~"= 100"), side=4, line=1.5, cex=1.25) }
	else if (i==18) { mtext(expression(italic("P")~"= 1000"), side=4, line=1.5, cex=1.25) }
	else if (i==27) { mtext(expression(italic("P")~"= 10000"), side=4, line=1.5, cex=1.25) }
	else if (i==36) { mtext(expression(italic("P")~"= 100000"), side=4, line=1.5, cex=1.25) }
	
	if (i==32) { mtext("True Negative Rate (Specificity)", side=1, line=3.5, cex=1.5) }
	if (i==10) { mtext("True Positive Rate (Sensitivity)", side=2, line=3.5, cex=1.5, adj=0.95) }

	scen_sub <- subset(plot_mat, plot_mat[,1]==uniq_scenarios[i])
	for (j in 1:dim(scen_sub)[1])
		{
		points(jitter(1-as.numeric(scen_sub[j,5])), jitter(as.numeric(scen_sub[j,4])), pch=21, bg=f1_plot_colors[j], cex=2.5)
		}
	}
dev.off()









