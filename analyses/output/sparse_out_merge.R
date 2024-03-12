###################################
## sparse_out_merge.R
###################################

## JPJ 31 v 22
## PURPOSE: to merge the prediction and parameter metrics from all output files into one matrix
## RUN THIS FROM: /project/modelscape/analyses/sparseillustrate_bitbucket/analyses/output/
## USAGE: Rscript sparse_out_merge.R


## IMPORTANT: must change the date below to match to the corresponding trial you are merging

date <- "12dec23"
trial_path <- paste0("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/trials_out/", date, "/")


## set up the output matrix with the first 3 columns filled (scenario, rep, analysis)

scenarios <- c(1:36)
reps <- c(1:100)
analyses <- c("lasso","glasso","blasso_ma","blasso_nz","horseshoe_ma", "horseshoe_nz","ridge","gridge","elastic","gemma","susie","spike_slab","forest")

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
	scenario_count <- sub(".*scenario([[:digit:]]+[a-z]*).*", "\\1", files[i])
	rep_count <- sub(".*rep([[:digit:]]+).*", "\\1", files[i])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="susie", c(4:15)] <- c(pred_susie[c(1,3)], pred_susieWS[c(1,3)], par_susie[c(1,3,5,7,9,10,11)], runtime[1])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="gridge", c(4:15)] <- c(pred_gridge[c(1,3)], pred_gridgeWS[c(1,3)], par_gridge[c(1,3,5,7,9,10,11)], runtime[2])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="glasso", c(4:15)] <- c(pred_glasso[c(1,3)], pred_glassoWS[c(1,3)], par_glasso[c(1,3,5,7,9,10,11)], runtime[3])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="elastic", c(4:15)] <- c(pred_elastic[c(1,3)], pred_elasticWS[c(1,3)], par_elastic[c(1,3,5,7,9,10,11)], runtime[4])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="gemma", c(4:15)] <- c(pred_gemma[c(1,3)], pred_gemmaWS[c(1,3)], par_gemma[c(1,3,5,7,9,10,11)], runtime[5])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="blasso_ma", c(4:15)] <- c(pred_blasso_ma[c(1,3)], pred_blasso_maWS[c(1,3)], par_blasso_ma[c(1,3,5,7,9,10,11)], runtime[6])
        metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="blasso_nz", c(4:15)] <- c(pred_blasso_nz[c(1,3)], pred_blasso_nzWS[c(1,3)], par_blasso_nz[c(1,3,5,7,9,10,11)], runtime[6])
        metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="horseshoe_ma", c(4:15)] <- c(pred_horseshoe_ma[c(1,3)], pred_horseshoe_maWS[c(1,3)], par_horseshoe_ma[c(1,3,5,7,9,10,11)], runtime[7]) 
        metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="horseshoe_nz", c(4:15)] <- c(pred_horseshoe_nz[c(1,3)], pred_horseshoe_nzWS[c(1,3)], par_horseshoe_nz[c(1,3,5,7,9,10,11)], runtime[7])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="lasso", c(4:15)] <- c(pred_lasso[c(1,3)], pred_lassoWS[c(1,3)], par_lasso[c(1,3,5,7,9,10,11)], runtime[8])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="ridge", c(4:15)] <- c(pred_ridge[c(1,3)], pred_ridgeWS[c(1,3)], par_ridge[c(1,3,5,7,9,10,11)], runtime[9])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="forest", c(4:15)] <- c(pred_forest[c(1,3)], pred_forestWS[c(1,3)], par_forest[c(1,3,5,7,9,10,11)], runtime[10])
	metrics_out[metrics_out[,1]==scenario_count & metrics_out[,2]==rep_count & metrics_out[,3]=="spike_slab", c(4:15)] <- c(pred_spike_slab[c(1,3)], pred_spike_slabWS[c(1,3)], par_spike_slab[c(1,3,5,7,9,10,11)], runtime[11])
	if (i %% 100 == 0) { print(i) }
	}

write.table(metrics_out, file=paste0("metrics_out_", date, ".txt"), row.names=FALSE, quote=FALSE)



