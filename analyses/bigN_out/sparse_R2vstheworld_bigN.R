##### Script to 1) look at how well causal parameters explain response variables in sparse model simulations and
############### 2) make a plot to compare how each sparse modelling method does across the simulations that vary in this r2 metric
## originally made by Eryn; modified by JPJ
library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)

#### simulations read in ####

setwd("/project/modelscape/analyses/sparse/data/simulations/bigN/")

datafiles<-list.files("/project/modelscape/analyses/sparse/data/simulations/bigN/", pattern="scenario*", recursive=FALSE, include.dirs=TRUE)

### there are 2 possible simulations
### each with 100 replicates
scenario_rep<-data.frame(str_extract_all(datafiles, "\\d+", simplify = TRUE))

list<-list()
for(i in 1:200){
data<-readRDS(datafiles[i])
list[[i]]<-data.frame(summary(lm(data$y[1:(length(data$y)-500)]~data$y_mu[1:(length(data$y_mu)-500)]))[8])
}

R2<-do.call(rbind.data.frame, list)
R2_data<-cbind.data.frame(scenario_rep[1:200,], R2)
names(R2_data)<-c("scenario", "rep", "R2")
write.table(R2_data, file="/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/bigN_out/sparse_R2vstheworld_bigN_out.txt", row.names=F, quote=F)


#R2_data$scenario_rep<-paste(R2_data$scenario, R2_data$rep)
  
#metrics<-read.table("/project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/output/metrics_out_10mar23_NEW_gemma.txt", header=T)
#metrics$scenario_rep<-paste(metrics$scenario, metrics$rep)

#alldata<-merge(metrics, R2_data, by="scenario_rep")





## JPJ: turned off plotting because I just want to output R2_data and make plots in a different script
###cbind.data.frame between the metrics andd the r2.
### might need to do a vlookup if they're not in the same order.
#pdf(file="/gscratch/emcfarl2/sparse_pred_rmse.pdf", width=8, height=8)
### josh will hate this, change it to baseR
#ggplot(alldata, aes(R2, pred_rmse, colour=analysis))+geom_smooth(method=lm, se=FALSE)+theme_bw()
#dev.off()
#pdf(file="/gscratch/emcfarl2/sparse_pred_R2.pdf", width=8, height=8)
### josh will hate this, change it to baseR
#ggplot(alldata, aes(R2, pred_r2, colour=analysis))+geom_smooth(method=lm, se=FALSE)+theme_bw()
#dev.off()
#pdf(file="/gscratch/emcfarl2/sparse_par_rmse.pdf", width=8, height=8)
### josh will hate this, change it to baseR
#ggplot(alldata, aes(R2, par_rmse, colour=analysis))+geom_smooth(method=lm, se=FALSE)+theme_bw()
#dev.off()
#pdf(file="/gscratch/emcfarl2/sparse_pred_ws_rmse.pdf", width=8, height=8)
### josh will hate this, change it to baseR
#ggplot(alldata, aes(R2, pred_ws_rmse, colour=analysis))+geom_smooth(method=lm, se=FALSE)+theme_bw()
#dev.off()
#pdf(file="/gscratch/emcfarl2/sparse_pred_ws_r2.pdf", width=8, height=8)
### josh will hate this, change it to baseR
#ggplot(alldata, aes(R2, pred_ws_r2, colour=analysis))+geom_smooth(method=lm, se=FALSE)+theme_bw()
#dev.off()
#pdf(file="/gscratch/emcfarl2/sparse_time.pdf", width=8, height=8)
### josh will hate this, change it to baseR
#ggplot(alldata, aes(R2, time, colour=analysis))+geom_smooth(method=lm, se=FALSE)+theme_bw()
#dev.off()
