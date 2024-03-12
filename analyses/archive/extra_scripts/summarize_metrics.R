method_names <- c("lasso", "ridge", "blasso", "susie", "gemma", "randomforest")
nmethods <- length(method_names)
nreplicates <- 100
nscenarios <- 16
nsims <- nscenarios * nreplicates
nsummaries <- nsims * nmethods

parameter_metrics <- data.frame(scenario = rep(1:nscenarios, each= nmethods * nreplicates),
                                replicate = rep(rep(1:100, each=nmethods), nscenarios),
                                method = rep(method_names, nsims),
                                tpr = numeric(nsummaries),
                                tpr_sd = numeric(nsummaries),
                                fpr = numeric(nsummaries),
                                fpr_sd = numeric(nsummaries),
                                rmse = numeric(nsummaries),
                                rmse_sd = numeric(nsummaries),
                                coverage = numeric(nsummaries))

prediction_metrics_all<-data.frame(scenario = rep(1:nscenarios, each= nmethods * nreplicates),
                                replicate = rep(rep(1:100, each=nmethods), nscenarios),
                                method = rep(method_names, nsims),
                                rmse  = numeric(nsummaries),
                                rmse_sd = numeric(nsummaries),
                                r2   = numeric(nsummaries),
                                r2_sd = numeric(nsummaries))
#### ,,,,, keeep working from here


### pulls together all of the different parameter and prediction metrics
for(i in 1:100){
parameter_metrics_all[[i]]<-data.frame(cbind(parameter_metrics_monomvn_lasso[[i]],
                                             parameter_metrics_monomvn_ridge[[i]],
                                             parameter_metrics_monomvn_blasso[[i]],
                                             parameter_metrics_susie[[i]],
                                             parameter_metrics_gemma[[i]],
                                             parameter_metrics_randomforest[[i]]))

names(parameter_metrics_all[[i]])<-c("Lasso", "Ridge", "Blasso", "Susie", "Gemma", "Random_Forest")
prediction_metrics_all[[i]]<-data.frame(cbind(prediction_metrics_monomvn_lasso[[i]],
                                              prediction_metrics_monomvn_ridge[[i]],
                                              prediction_metrics_monomvn_blasso[[i]],
                                              prediction_metrics_susie[[i]],
                                              prediction_metrics_gemma[[i]],
                                              prediction_metrics_randomforest[[i]]))
names(prediction_metrics_all[[i]])<-c("Lasso", "Ridge", "Blasso", "Susie", "Gemma", "Random_Forest")
}
#### want to make some comparative plots, that will hold when there are multiple replicates
methods_times_df<-data.frame(methods_times)
names(methods_times_df)<-c("Lasso", "Ridge", "Blasso", "Susie", "Gemma", "Random_Forest")

do.call("rbind", parameter_metrics_all)->parameter_metrics_all_df
rownames(parameter_metrics_all_df)->parameter_metrics_all_df$metric
parameter_metrics_all_df$metric<- str_match(parameter_metrics_all_df$metric, "(^.+)\\D+")[,1] 
parameter_metrics_all_df$metric[1:7]<-c("tpr", "tpr_sd", "fpr", "fpr_sd", "rmse", "rmse_sd", "coverage")
row.names(parameter_metrics_all_df)<-NULL
parameter_metrics_df<-parameter_metrics_all_df[,c(7,1:6)]
###pretty sure I can output a list of lists from here###
parameter_metrics_long<-gather(parameter_metrics_df, method, measurement, Lasso:Random_Forest)

do.call("rbind", prediction_metrics_all)->prediction_metrics_all_df
rownames(prediction_metrics_all_df)->prediction_metrics_all_df$metric
prediction_metrics_all_df$metric<-str_match(prediction_metrics_all_df$metric, "(^.+)\\D+")[,1]
prediction_metrics_all_df[is.na(prediction_metrics_all_df$metric)==TRUE,]$metric<-'r2'
#prediction_metrics_all_df$metric[1:4]<-c("rmse", "rmse_sd", "r2", "r2_sd") ### this is so hacky, but I've been killing myself with this
row.names(prediction_metrics_all_df)<-NULL
prediction_metrics_df<-prediction_metrics_all_df[,c(7,1:6)]
###pretty sure I can output a list of lists from here###
prediction_metrics_long<-gather(prediction_metrics_df, method, measurement, Lasso:Gemma)
#setwd("/gscratch/emcfarl2/software/sparseillustrateeval")
#out_file<-as.character(paste0(str_extract(args[1], "[^.]+"),"_", "out", ".RData"))
rm(list=ls()[! ls() %in% c("parameter_metrics_long", "prediction_metrics_long", "methods_times")])
save.image(file=args[2])
