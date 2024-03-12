source("/project/modelscape/analyses/sparse/code/simulations/sparse_sim.R")

## genomics 
for(i in 1:100){
    genomics<-sparse_sim(n=1000, p=100000, n_causal=15)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/sim_bespoke/', 'scenario17_rep', i, ".rds")
    saveRDS(genomics, file=filename)
    print(i)
}


## hydrology
for(i in 1:100){
    hydrology<-sparse_sim(n=10000, p=100, n_causal=10)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/sim_bespoke/','scenario18_rep', i, ".rds")
    saveRDS(hydrology, file=filename)
    print(i)
}


## population biology
    ## change beta_sd to have wildly variable beta sizes
    ## lower cluster_size to size of p (is this reasonable???)

for(i in 1:100){
    popbio<-sparse_sim(n=50, p=65, n_causal=15, cluster_size=65, beta_sd=1)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/sim_bespoke/','scenario19_rep', i, ".rds")
    saveRDS(popbio, file=filename)
    print(i)
}


## behavioral ecology
    ## lower cluster_size to size of p (is this reasonable???)
for(i in 1:100){
    behav<-sparse_sim(n=50, p=30, n_causal=3, cluster_size=30)
    filename<-paste0('/project/modelscape/analyses/sparse/data/simulations/sim_bespoke/','scenario20_rep', i, ".rds")
    saveRDS(behav, file=filename)
    print(i)
}
