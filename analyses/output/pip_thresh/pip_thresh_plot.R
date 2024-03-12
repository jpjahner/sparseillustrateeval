#############################
## pip_thresh_plot.R
#############################

## JPJ 27 xii 23

.libPaths("/project/modelscape/share/Rlibs/")
library(vioplot)
library(MetBrewer)	## for plot colors
plot_colors <- c("white", met.brewer("Redon", 12, "discrete"))
thresh_colors <- plot_colors[c(7,9,12,13)]

thresh <- read.delim("pips_scenario24_rep1.txt", header=FALSE, sep=" ")
	dim(thresh)

zeros <- subset(thresh, thresh[,1]==0)
	dim(zeros)
nonzeros <- subset(thresh, thresh[,1]!=0)
	dim(nonzeros)
	
column_order <- c(4, 5, 2, 3)	## blasso, horseshoe, susie, gemma (first column is truth)


## calculate true positive rates, true negative rates, and f1

f1<-function(p, tp, tn){ 
  (2*tp) / (2*tp + p-10-tn + 10-tp)
  }

cutoffs <- seq(0, 0.5, 0.05)
tpr_mat <- matrix(NA, length(cutoffs), dim(thresh)[2]-1)
tnr_mat <- matrix(NA, length(cutoffs), dim(thresh)[2]-1)
f1_mat <- matrix(NA, length(cutoffs), dim(thresh)[2]-1)

for (i in 1:length(cutoffs))
	{
	tpr_mat[i,] <- colMeans(nonzeros[,column_order]>=cutoffs[i])
	tnr_mat[i,] <- colMeans(zeros[,column_order]<cutoffs[i])
	for (j in 1:(dim(thresh)[2]-1))
		{
		f1_mat[i,j] <- f1(10000, colSums(nonzeros[,column_order]>=cutoffs[i])[j], colSums(zeros[,column_order]<cutoffs[i])[j])
		}
	}




## make figures

pdf(file="pip_thresh_plot.pdf", height=9, width=6)
#quartz(height=9, width=6)
layout(matrix(c(1,2,3,4,5,5), nrow=3, ncol=2, byrow=TRUE))
par(mar=c(3,3,1,1), oma=c(2,2,0,0))
for (i in 1:length(column_order))
	{
	plot(0, type="n", xlim=c(0,1), ylim=c(0,1), xaxt="n", xlab="", ylab="", cex.axis=1.5, las=1)
		axis(side=1, at=c(0.075, 0.6), labels=c("Causal", "Non-causal"), cex.axis=1.5)
		abline(v=0.15, lwd=1.5)
		points(jitter(rep(0.075, 10), 15), nonzeros[,column_order[i]], pch=21, bg=adjustcolor(thresh_colors[i], alpha.f=0.7), cex=2)
		vioplot(zeros[,column_order[i]], at=0.6, col=thresh_colors[i], pchMed=23, colMed="black", colMed2="white", cex=0, add=TRUE)
		if (i==1 | i==3) {	mtext("PIP", side=2, line=3, cex=1.5) }
		if (i==3 | i==4) {	mtext("Predictor type", side=1, line=3, cex=1.5) }
		if (i==1) {	mtext("BLASSO", side=3, line=-1.75, adj=0.97, cex=1.25); mtext("A", side=3, adj=-0.275, line=-1, cex=1.5) }
		if (i==2) {	mtext("Horseshoe", side=3, line=-1.75, adj=0.97, cex=1.25) }
		if (i==3) {	mtext("SuSiE", side=3, line=-1.75, adj=0.97, cex=1.25) }
		if (i==4) {	mtext("BSLMM", side=3, line=-1.75, adj=0.97, cex=1.2) }
		box(lwd=1)
	}
par(mar=c(3,3,3,1))
plot(0, type="n", xlim=c(0,0.5), ylim=c(0,1), xlab="", ylab="", las=1, cex.axis=1.5)
	for (i in 1:dim(f1_mat)[2]) { lines(type="o", cutoffs, f1_mat[,i], pch=21, bg=thresh_colors[i], cex=2) }
	box(lwd=1)
	mtext(bquote("F"[1]), side=2, line=3, cex=1.5)
	mtext("PIP threshold", side=1, line=3, cex=1.5)
	mtext("B", side=3, adj=-0.12, line=-1, cex=1.5)
dev.off()



