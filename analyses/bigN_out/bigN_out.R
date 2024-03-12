##################################################
## bigN_out.R
##################################################

## JPJ 31 vii 23
## Purpose: to plot bigN output


##################################################
## load data and packages
##################################################

library(MetBrewer)	## for plot colors

input_core <- read.delim("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/output/metrics_out_12dec23.txt", header=TRUE, sep=" ")
#input_core <- read.delim("metrics_out_12dec23.txt", header=TRUE, sep=" ")

## only want to keep core scenarios 11, 14, and 17; and want to remove blassos and horseshoes
input_core_scens <- subset(input_core, input_core[,1]==11 | input_core[,1]==14 | input_core[,1]==17)
input_core_scens_noBlHo <- subset(input_core_scens, input_core_scens[,3]!="blasso_ma" & input_core_scens[,3]!="blasso_nz" & input_core_scens[,3]!="horseshoe_ma" & input_core_scens[,3]!="horseshoe_nz")

input_bigN <- read.delim("bigN_metrics_out_17dec23.txt", header=TRUE, sep=" ")


input_merged <- rbind(input_core_scens_noBlHo, input_bigN)


##################################################
## calculate f1 scores (andrew's code)
##################################################

##	input_f1 <- left_join(input, scenarios) %>%
##		mutate(	par_fnr = 1 - par_tpr,		## false negative rate
##				par_tnr = 1 - par_fpr,		## true negative rate
##				tp = par_tpr*10,			## true positives
##				fp = par_fpr*(p-10),		## false positives
##				fn = par_fnr*10,			## false negatives
##				f1_score = (2*tp)/(2*tp + fp + fn))

tp <- input_merged[,8]*10		## true positives (tp)
fp <- input_merged[,9]*990		## false positives (fp)
fn <- (1-input_merged[,8])*10	## false negatives (fn)
f1 <- (2*tp)/(2*tp + fp + fn)	## f1 score


input_merged_f1 <- cbind(input_merged, f1)

##################################################
## organizing plotting data
##################################################

uniq_scens <- c("11", "14", "17", "bigN_scenario1", "bigN_scenario2")
nlist <- c(50, 150, 500, 1000, 10000)
uniq_analyses <- c("forest", "gridge", "ridge", "elastic", "glasso", "lasso", "spike_slab", "susie", "gemma")
legend_names <- c("Random Forest", "Ridge (glmnet)", "Ridge (monomvn)", "Elastic Net", "LASSO (glmnet)", "LASSO (monomvn)", "Spike-and-slab", "SuSiE", "BSLMM")
plot_colors <- c("white", met.brewer("Redon", 12, "discrete"))
plot_colors_noBlHo <- plot_colors[-c(7:10)]
f1_analyses <-c("forest", "elastic", "glasso", "lasso", "spike_slab", "gemma")
f1_plot_colors <- plot_colors[c(1,4,5,6,11,13)]


plot_data <- matrix(NA, 45, 5)
ctr <- 0
for (i in 1:length(uniq_scens))
	{
	for (j in 1:length(uniq_analyses))
		{
		ctr <- ctr + 1
		input_sub <- subset(input_merged_f1, input_merged_f1[,1]==uniq_scens[i] & input_merged_f1[,3]==uniq_analyses[j])
		plot_data[ctr,1] <- nlist[i]				## N
		plot_data[ctr,2] <- uniq_analyses[j]		## analysis
		plot_data[ctr,3] <- median(input_sub[,16])	## f1
		plot_data[ctr,4] <- median(input_sub[,7])		## IS r2
		plot_data[ctr,5] <- median(input_sub[,5])		## OOS r2
		}
	}


## Note: plotting a horizontal grey line for the average process signal of the 5 scenarios
	## core scenario 11: 0.48539203
	## core scenario 14: 0.47551047
	## core scenario 17: 0.47334861
	## bigN scenario 1: 0.4704741
	## bigN scenario 2: 0.4677309
	## mean: 0.4744912


pdf(file="bigN_out.pdf", height=6, width=6)
par(mar=c(5,5,0,0), mfrow=c(2,2), oma=c(1,1,1,1))
## IS R2
plot(0, type="n", xlim=c(0,5), ylim=c(0,1), xlab="", ylab=bquote("In-sample R"^2), xaxt="n", cex.lab=1.5, cex.axis=1.25, las=1)
axis(side=1, at=c(1:5)-0.5, labels=c(50,150,500,1000,10000), cex.axis=1.25, las=2)
rect(par('usr')[1], par('usr')[3], par('usr')[2], 0.4744912, col="light gray")
for(k in 1:length(uniq_analyses))
	{
	analysis_subset <- subset(plot_data, plot_data[,2]==uniq_analyses[k])
	lines(c(1:5)-0.5, analysis_subset[,4], type="o", pch=21, bg=plot_colors_noBlHo[k], cex=1.5)
	}
mtext("A", side=3, cex=1.75, adj=-0.425, line=-0.5)
## legend
par(mar=c(2.5,2.5,0,0))
plot(0, type="n", axes=FALSE)
legend("center", legend=legend_names, pch=22, pt.bg=plot_colors_noBlHo, pt.cex=3, cex=1.5, bty="n")
## OOS R2
par(mar=c(5,5,0,0))
plot(0, type="n", xlim=c(0,5), ylim=c(0,1), xlab="", ylab=bquote("Out-of-sample R"^2), xaxt="n", cex.lab=1.5, cex.axis=1.25, las=1)
axis(side=1, at=c(1:5)-0.5, labels=c(50,150,500,1000,10000), cex.axis=1.25, las=2)
rect(par('usr')[1], par('usr')[3], par('usr')[2], 0.4744912, col="light gray")
for(k in 1:length(uniq_analyses))
	{
	analysis_subset <- subset(plot_data, plot_data[,2]==uniq_analyses[k])
	lines(c(1:5)-0.5, analysis_subset[,5], type="o", pch=21, bg=plot_colors_noBlHo[k], cex=1.5)
	}
mtext(bquote("Observations ("~italic("N")~")"), side=1, line=4.5, cex=1.25)
mtext("B", side=3, cex=1.75, adj=-0.425, line=-0.5)
## f1
plot(0, type="n", xlim=c(0,5), ylim=c(0,1), xlab="", ylab=bquote("F"[1]), xaxt="n", cex.lab=1.5, cex.axis=1.25, las=1)
axis(side=1, at=c(1:5)-0.5, labels=c(50,150,500,1000,10000), cex.axis=1.25, las=2)
for(k in 1:length(f1_analyses))
	{
	analysis_subset <- subset(plot_data, plot_data[,2]==f1_analyses[k])
	lines(c(1:5)-0.5, analysis_subset[,3], type="o", pch=21, bg=f1_plot_colors[k], cex=1.5)
	}
mtext(bquote("Observations ("~italic("N")~")"), side=1, line=4.5, cex=1.25)
mtext("C", side=3, cex=1.75, adj=-0.425, line=-0.5)
dev.off()



