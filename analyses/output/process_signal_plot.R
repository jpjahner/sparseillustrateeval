#########################################
## scenario_difficulty_plot.R
#########################################

## JPJ 7 vi 23
## Purpose: to make a plot showing the distribution of scenario difficulty

diff <- read.delim("sparse_R2vstheworld_out.txt", header=TRUE, sep=" ")
	dim(diff)
	head(diff)

sim_grid <- read.csv("/project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/sim_grid_round5.csv", header=TRUE)
	dim(sim_grid)
	head(sim_grid)

uniq_scens <- sort(unique(diff[,1]))
scen_list <- c(1,10,19,28,4,13,22,31,7,16,25,34,
				2,11,20,29,5,14,23,32,8,17,26,35,
				3,12,21,30,6,15,24,33,9,18,27,36)

diff_out <- matrix(NA, length(uniq_scens), 6)
	## columns: scenario, median R2, sd R2, N, P, beta

for (i in 1:length(uniq_scens))
	{
	diff_sub <- subset(diff, diff[,1]==scen_list[i])
	diff_out[i,1] <- scen_list[i]
	diff_out[i,2] <- median(diff_sub[,3])
	diff_out[i,3] <- sd(diff_sub[,3])
	diff_out[i,4] <- sim_grid[sim_grid[,1]==scen_list[i], 2] - 500
	diff_out[i,5] <- sim_grid[sim_grid[,1]==scen_list[i], 3]
	diff_out[i,6] <- sim_grid[sim_grid[,1]==scen_list[i], 6]
	}


xaxis_labels <- rep(c(expression(italic("P")~"= 100"), expression(italic("P")~"= 1000"), expression(italic("P")~"= 10000"), expression(italic("P")~"= 100000")), 9)

pdf(file="process_signal.pdf", width=10, height=6)
par(oma=c(5,0,3,0), mar=c(3,5,1,1))
plot(0, type="n", xlim=c(0,36), ylim=c(0,1), xlab="", ylab=expression("Reducible error (R"^2~")"), xaxt="n", cex.lab=1.75, cex.axis=1.5, las=1)
axis(1, at=c(1:36)-0.5, labels=xaxis_labels, las=2, cex.axis=1.5)
for(i in 1:length(uniq_scens))
	{
	segments(uniq_scens[i]-0.5, diff_out[i,2]-diff_out[i,3], uniq_scens[i]-0.5, diff_out[i,2]+diff_out[i,3], col="gray", lwd=5)
	points(uniq_scens[i]-0.5, diff_out[i,2], pch=21, bg="gray", cex=2)
	}
abline(v=0, lty=2, lwd=2)
abline(v=4, lty=2, lwd=2)
abline(v=8, lty=2, lwd=2)
abline(v=12, lwd=2)
abline(v=16, lty=2, lwd=2)
abline(v=20, lty=2, lwd=2)
abline(v=24, lwd=2)
abline(v=28, lty=2, lwd=2)
abline(v=32, lty=2, lwd=2)
abline(v=36, lty=2, lwd=2)
mtext(expression(italic("N")~"= 50"), side=3, line=0.25, cex=1.25, adj=0.06)
mtext(expression(italic("N")~"= 150"), side=3, line=0.25, cex=1.25, adj=0.165)
mtext(expression(italic("N")~"= 500"), side=3, line=0.25, cex=1.25, adj=0.28)
mtext(expression(italic("N")~"= 50"), side=3, line=0.25, cex=1.25, adj=0.39)
mtext(expression(italic("N")~"= 150"), side=3, line=0.25, cex=1.25, adj=0.5)
mtext(expression(italic("N")~"= 500"), side=3, line=0.25, cex=1.25, adj=0.61)
mtext(expression(italic("N")~"= 50"), side=3, line=0.25, cex=1.25, adj=0.72)
mtext(expression(italic("N")~"= 150"), side=3, line=0.25, cex=1.25, adj=0.835)
mtext(expression(italic("N")~"= 500"), side=3, line=0.25, cex=1.25, adj=0.94)
mtext(bquote(beta[causal]==0.1), side=3, line=1.5, cex=1.5, adj=0.14)
mtext(bquote(beta[causal]==0.3), side=3, line=1.5, cex=1.5, adj=0.5)
mtext(bquote(beta[causal]==0.8), side=3, line=1.5, cex=1.5, adj=0.86)

dev.off()









