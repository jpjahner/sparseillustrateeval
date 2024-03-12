###################################
## sparse_out_figs.R
###################################

## JPJ 17 vi 22
## PURPOSE: to make boxplot figures from metrics_out.txt

metrics <- read.delim("metrics_out.txt", header=TRUE, sep=" ")
	dim(metrics)
	names(metrics)
uniq_scenarios <- unique(metrics[,1])


## time plot (column 15)

quartz(height=12, width=12)
par(mar=c(5,5,1,1), mfrow=c(4,4), oma=c(0,0,4,2))
for (i in 1:length(uniq_scenarios))
	{
	scenario_sub <- subset(metrics, metrics[,1]==i)
	boxplot(scenario_sub[,15] ~ scenario_sub[,3], xlab="Analysis", ylab="Time (s)", ylim=c(0, max(metrics[,15], na.rm=T)), cex.lab=2, xaxt="n", cex.axis=1.25)
	axis(1, at=c(1:6), labels=c("B", "G", "L", "RF", "R", "S"), cex.axis=1.25)
	if (i==1)
		{
		mtext("N = 50", side=3, cex=1.5, line=2.5)
		mtext("N causal = 0", side=3, cex=1.5, line=0.5)
		}
	if (i==2)
		{
		mtext("N = 50", side=3, cex=1.5, line=2.5)
		mtext("N causal = 10", side=3, cex=1.5, line=0.5)
		}
	if (i==3)
		{
		mtext("N = 500", side=3, cex=1.5, line=2.5)
		mtext("N causal = 0", side=3, cex=1.5, line=0.5)
		}
	if (i==4)
		{
		mtext("N = 500", side=3, cex=1.5, line=2.5)
		mtext("N causal = 10", side=3, cex=1.5, line=0.5)
		mtext("P = 100", side=4, cex=1.5, line=1.5)
		}
	if (i==8)
		{
		mtext("P = 1000", side=4, cex=1.5, line=1.5)
		}
	if (i==12)
		{
		mtext("P = 10000", side=4, cex=1.5, line=1.5)
		}
	if (i==16)
		{
		mtext("P = 100000", side=4, cex=1.5, line=1.5)
		}
	box(lwd=1.5)
	}



## pred_rmse plot (column 12)

quartz(height=12, width=12)
par(mar=c(5,5,1,1), mfrow=c(4,4), oma=c(0,0,4,2))
for (i in 1:length(uniq_scenarios))
	{
	scenario_sub <- subset(metrics, metrics[,1]==i)
	boxplot(scenario_sub[,12] ~ scenario_sub[,3], xlab="Analysis", ylab="Parameter RMSE", ylim=c(0, max(metrics[,12], na.rm=T)), cex.lab=2, xaxt="n", cex.axis=1.25)
	axis(1, at=c(1:6), labels=c("B", "G", "L", "RF", "R", "S"), cex.axis=1.25)
	if (i==1)
		{
		mtext("N = 50", side=3, cex=1.5, line=2.5)
		mtext("N causal = 0", side=3, cex=1.5, line=0.5)
		}
	if (i==2)
		{
		mtext("N = 50", side=3, cex=1.5, line=2.5)
		mtext("N causal = 10", side=3, cex=1.5, line=0.5)
		}
	if (i==3)
		{
		mtext("N = 500", side=3, cex=1.5, line=2.5)
		mtext("N causal = 0", side=3, cex=1.5, line=0.5)
		}
	if (i==4)
		{
		mtext("N = 500", side=3, cex=1.5, line=2.5)
		mtext("N causal = 10", side=3, cex=1.5, line=0.5)
		mtext("P = 100", side=4, cex=1.5, line=1.5)
		}
	if (i==8)
		{
		mtext("P = 1000", side=4, cex=1.5, line=1.5)
		}
	if (i==12)
		{
		mtext("P = 10000", side=4, cex=1.5, line=1.5)
		}
	if (i==16)
		{
		mtext("P = 100000", side=4, cex=1.5, line=1.5)
		}
	box(lwd=1.5)
	}



## pred_r2 plot (column 6)

quartz(height=12, width=12)
par(mar=c(5,5,1,1), mfrow=c(4,4), oma=c(0,0,4,2))
for (i in 1:length(uniq_scenarios))
	{
	scenario_sub <- subset(metrics, metrics[,1]==i)
	boxplot(scenario_sub[,6] ~ scenario_sub[,3], xlab="Analysis", ylab=expression("Prediction R"^2), ylim=c(0, max(metrics[,6], na.rm=T)), cex.lab=2, xaxt="n", cex.axis=1.25)
	axis(1, at=c(1:6), labels=c("B", "G", "L", "RF", "R", "S"), cex.axis=1.25)
	if (i==1)
		{
		mtext("N = 50", side=3, cex=1.5, line=2.5)
		mtext("N causal = 0", side=3, cex=1.5, line=0.5)
		}
	if (i==2)
		{
		mtext("N = 50", side=3, cex=1.5, line=2.5)
		mtext("N causal = 10", side=3, cex=1.5, line=0.5)
		}
	if (i==3)
		{
		mtext("N = 500", side=3, cex=1.5, line=2.5)
		mtext("N causal = 0", side=3, cex=1.5, line=0.5)
		}
	if (i==4)
		{
		mtext("N = 500", side=3, cex=1.5, line=2.5)
		mtext("N causal = 10", side=3, cex=1.5, line=0.5)
		mtext("P = 100", side=4, cex=1.5, line=1.5)
		}
	if (i==8)
		{
		mtext("P = 1000", side=4, cex=1.5, line=1.5)
		}
	if (i==12)
		{
		mtext("P = 10000", side=4, cex=1.5, line=1.5)
		}
	if (i==16)
		{
		mtext("P = 100000", side=4, cex=1.5, line=1.5)
		}
	box(lwd=1.5)
	}





