###################################
## sparse_out_merge.R
###################################

## JPJ 31 v 22
## PURPOSE: to merge the prediction and parameter metrics from all output files into one matrix
## RUN THIS FROM: /project/modelscape/analyses/sparse/data/analysis_output/


## set up the output matrix with the first 3 columns filled (scenario, rep, analysis)

scenarios <- c(1:16)
reps <- c(1:100)
analyses <- c("blasso", "gemma", "lasso", "rf", "ridge", "susie")

metrics_out <- matrix(NA, length(scenarios)*length(reps)*length(analyses), 15)
colnames(metrics_out) <- c("scenario", "rep", "analysis", "pred_rmse", "pred_rmse_sd", "pred_r2", "pred_r2_sd", "param_tpr", "param_tpr_sd", "param_fpr", "param_fpr_sd", "param_rmse", "param_rmse_sd", "param_cov", "time")

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

files <- list.files(pattern="out_", recursive=TRUE)
print(paste0("Number of files: ", length(files)))

for (i in 1:length(files))
	{
	load(files[i])
	
	scenario_count <- sub(".*scenario([[:digit:]]+).*", "\\1", files[i])
	rep_count <- sub(".*rep([[:digit:]]+).*", "\\1", files[i])
	
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="blasso", c(4:15)] <- c(prediction_metrics_monomvn_blasso, parameter_metrics_monomvn_blasso, methods_times[3])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="gemma", c(4:15)] <- c(prediction_metrics_gemma, parameter_metrics_gemma, methods_times[5])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="lasso", c(4:15)] <- c(prediction_metrics_monomvn_lasso, parameter_metrics_monomvn_lasso, methods_times[1])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="rf", c(4:15)] <- c(prediction_metrics_randomforest, parameter_metrics_randomforest, methods_times[6])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="ridge", c(4:15)] <- c(prediction_metrics_monomvn_ridge, parameter_metrics_monomvn_ridge, methods_times[2])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="susie", c(4:15)] <- c(prediction_metrics_susie, parameter_metrics_susie, methods_times[4])
	if (i %% 100 == 0) { print(i) }
	}

write.table(metrics_out, file="metrics_out.txt", row.names=FALSE, quote=FALSE)



