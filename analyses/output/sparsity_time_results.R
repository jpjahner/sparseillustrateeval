##################################################
## sparsity_time_results.R
##################################################

core <- read.delim("metrics_out_12dec23.txt", header=TRUE, sep=" ")
	dim(core)
	head(core)

core_no_nz <- subset(core, core[,3]!="blasso_nz" & core[,3]!="horseshoe_nz")
	dim(core_no_nz)
	head(core_no_nz)

## how many years did all analyses take?
sum(core_no_nz[,15]) / 60 / 60 / 24 / 365
	## 2.488226

## fraction of run-time due to blasso and horseshoe
sum(core[core[,3]=="blasso_ma",15]) / 60 / 60 / 24 / 365
	## 1.15964
1.15964 / 2.488226 * 100
	## 46.60509


sum(core[core[,3]=="horseshoe_ma",15]) / 60 / 60 / 24 / 365
	## 1.162523
1.162523 / 2.488226 * 100
	## 46.72096





