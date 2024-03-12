##################################################
## f1_oos_plot.R
##################################################

## JPJ 23 x 23
## Purpose: to plot f1 vs out-of-sample prediction


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

uniq_analyses <- c("forest", "elastic", "glasso", "lasso", "spike_slab", "gemma")
	
plot_mat <- matrix(NA, length(uniq_analyses)*length(uniq_scenarios), 4)
	dim(plot_mat)
ctr <- 0
for (i in 1:length(uniq_scenarios))
	{
	for (j in 1:length(uniq_analyses))
		{
		ctr <- ctr + 1
		plot_mat[ctr,1] <- uniq_scenarios[i]
		plot_mat[ctr,2] <- uniq_analyses[j]
		plot_mat[ctr,3] <- median(input_f1[input_f1[,1]==uniq_scenarios[i] & input_f1[,3]==uniq_analyses[j], 27])	## f1
		plot_mat[ctr,4] <- median(input_f1[input_f1[,1]==uniq_scenarios[i] & input_f1[,3]==uniq_analyses[j], 5])	## oos r2
		}
	}



##################################################
## plotting
##################################################

plot_colors <- c("white", met.brewer("Redon", 12, "discrete"))
f1_plot_colors <- plot_colors[c(1,4,5,6,11,13)]



pdf("f1_oos_plot.pdf", height=8, width=18)
#quartz(height=8, width=18)
layout(matrix(c(1,4,7,2,5,8,3,6,9,
			  10,13,16,11,14,17,12,15,18,
			  19,22,25,20,23,26,21,24,27,
			  28,31,34,29,32,35,30,33,36), nrow=4, ncol=9,byrow=TRUE))
par(mar=c(0.5,0.5,0.5,0.5), oma=c(5,5,5,5))

for (i in 1:dim(scenarios)[1])
	{
	plot(0, type="n", xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE)
	if (i%%9==1)	{ axis(2, labels=c(0.0,0.2,0.4,0.6,0.8,1.0), at=c(0.0,0.2,0.4,0.6,0.8,1.0), cex.axis=1.25, las=1) }
	if (i>27)		{ axis(1, labels=c(0.0,0.2,0.4,0.6,0.8,1.0), at=c(0.0,0.2,0.4,0.6,0.8,1.0), cex.axis=1.25) }

	if (scenarios[i,6]==0.1)
		{
		rect(par('usr')[1], par('usr')[3], par('usr')[2], 0.09732011, col="light gray", border=NA)
		abline(h=0.09732011, lty=2)
		}
	else if (scenarios[i,6]==0.3)
		{
		rect(par('usr')[1], par('usr')[3], par('usr')[2], 0.4715198, col="light gray", border=NA)
		abline(h=0.4715198, lty=2)
		}
	else if (scenarios[i,6]==0.8)
		{
		rect(par('usr')[1], par('usr')[3], par('usr')[2], 0.858247, col="light gray", border=NA)
		abline(h=0.858247, lty=2)
		}

	box(lwd=1.25)
	abline(0,1)

	if (i==4) { mtext(bquote(beta[causal]==0.1), side=3, line=2.5, cex=1.5) }
	if (i==5) { mtext(bquote(beta[causal]==0.3), side=3, line=2.5, cex=1.5) }
	if (i==6) { mtext(bquote(beta[causal]==0.8), side=3, line=2.5, cex=1.5) }
	
	if      (i<4) { mtext(expression(italic("N =")~"50"), side=3, line=0.5, cex=1.25) }
	else if (i<7) { mtext(expression(italic("N =")~"150"), side=3, line=0.5, cex=1.25) }
	else if (i<10) { mtext(expression(italic("N =")~"500"), side=3, line=0.5, cex=1.25) }

	if (i==9) { mtext(expression(italic("P")~"= 100"), side=4, line=1.5, cex=1.25) }
	else if (i==18) { mtext(expression(italic("P")~"= 1000"), side=4, line=1.5, cex=1.25) }
	else if (i==27) { mtext(expression(italic("P")~"= 10000"), side=4, line=1.5, cex=1.25) }
	else if (i==36) { mtext(expression(italic("P")~"= 100000"), side=4, line=1.5, cex=1.25) }

	if (i==32) { mtext(bquote("F"[1]), side=1, line=4, cex=1.5) }
	if (i==10) { mtext(bquote("Out-of-sample R"^2), side=2, line=3, cex=1.5, adj=2.6) }

	for (j in 1:length(uniq_analyses))
		{
		points(plot_mat[plot_mat[,1]==i & plot_mat[,2]==uniq_analyses[j],3], plot_mat[plot_mat[,1]==i & plot_mat[,2]==uniq_analyses[j],4], pch=21, bg= f1_plot_colors[j], cex=2.5)
		}
	}
dev.off()

















