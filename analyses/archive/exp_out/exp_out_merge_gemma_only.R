###################################
## sparse_out_merge.R
###################################

## JPJ 19 iv 23
## PURPOSE: to merge the prediction and parameter metrics from all experimental output files into one matrix
## RUN THIS FROM: /project/modelscape/analyses/sparseillustrate_bitbucket/analyses/exp_out/
## USAGE: Rscript exp_out_merge_gemma_only.R


## IMPORTANT: must change the date below to match to the corresponding trial you are merging

date <- "8may23_gemma"
trial_path <- paste0("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/trials_out_exp/", date, "/")


## set up the output matrix with the first 3 columns filled (scenario, rep, analysis)

scenarios <- c("correlation_scenario1", "correlation_scenario2", "correlation_scenario3", "correlation_scenario4", "correlation_scenario5", "correlation_scenario6", "correlation_scenario7", "correlation_scenario8", "correlation_scenario9", "ncausal_scenario1", "ncausal_scenario2", "ncausal_scenario3", "ncausal_scenario4", "ncausal_scenario5", "ncausal_scenario6", "ncausal_scenario7", "ncausal_scenario8", "ncausal_scenario9", "behav", "hydrology", "genomics", "popbio")
reps <- c(1:100)
analyses <- c("gemma")

metrics_out <- matrix(NA, length(scenarios)*length(reps)*length(analyses), 15)
colnames(metrics_out) <- c("scenario","rep","analysis","pred_rmse","pred_r2","pred_ws_rmse","pred_ws_r2","par_tpr","par_fpr","par_fnr","par_rmse","par_cov","par_kappa","par_alpha","time")

ctr <- 0
for (i in 1:length(scenarios))
	{
	for (j in 1:length(reps))
		{
		for (k in 1:length(analyses))
			{
			ctr <- ctr + 1
			metrics_out[ctr,1] <- scenarios[i]
			metrics_out[ctr,2] <- reps[j]
			metrics_out[ctr,3] <- analyses[k]
			}
		}
	}


## loop through the input files, adding metric data to the appropriate row of metrics_out
	## NOTE: methods_times order from feeder_script_third_sims.R = lasso, ridge, blasso, susie, gemma, rf

files <- list.files(path=trial_path, pattern="out_", recursive=TRUE)
print(paste0("Number of files: ", length(files)))

for (i in 1:length(files))
	{
	load(paste0(trial_path, files[i]))
	filebasename <- basename(files[i])
	scenario_count <- sub("out_([[:graph:]]*)_rep.*", "\\1", filebasename)
	rep_count <- sub(".*rep([[:digit:]]+).*", "\\1", files[i])	
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="gemma", c(4:15)] <- c(pred_gemma[c(1,3)], pred_gemmaWS[c(1,3)], par_gemma[c(1,3,5,7,9,10,11)], runtime[1])
	if (i %% 100 == 0) { print(i) }
	}

write.table(metrics_out, file=paste0("exp_metrics_out_", date, ".txt"), row.names=FALSE, quote=FALSE)



