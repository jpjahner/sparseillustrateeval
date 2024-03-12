source("/pfs/tc1/project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/sparse_sim.R")

## genomics 
for(i in 1:100){
    genomics<-sparse_sim(n=1000+500, p=100000, n_causal=15, beta_mean = 0.3)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/experiments/', 'genomics_rep', i, ".rds")
    saveRDS(genomics, file=filename)
    print(i)
}


## hydrology
for(i in 1:100){
    hydrology<-sparse_sim(n=1000+500, p=100, n_causal=10, beta_mean = 0.3)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/experiments/','hydrology_rep', i, ".rds")
    saveRDS(hydrology, file=filename)
    print(i)
}


## population biology
    ## change beta_sd to have wildly variable beta sizes

for(i in 1:100){
    popbio<-sparse_sim(n=50+500, p=65, n_causal=15, cluster_size=65, beta_mean = 0, beta_sd=0.5)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/experiments/','popbio_rep', i, ".rds")
    saveRDS(popbio, file=filename)
    print(i)
}


## behavioral ecology
for(i in 1:100){
    behav<-sparse_sim(n=50+500, p=30, n_causal=3, cluster_size=30, beta_mean = 0.3)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/experiments/','behav_rep', i, ".rds")
    saveRDS(behav, file=filename)
    print(i)
}
