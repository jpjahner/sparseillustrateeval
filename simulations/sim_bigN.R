library(tidyverse)


source("/pfs/tc1/project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/sparse_sim.R")

sim_grid <- read_csv("/pfs/tc1/project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/sim_grid_bigN.csv")

dir <- "/pfs/tc1/project/modelscape/analyses/sparse/data/simulations/bigN/"

for(i in 1:nrow(sim_grid)) {
  for(j in 1:100) {
    sim <- plyr::splat(sparse_sim)(sim_grid[i,-1]) 
    path <- paste0(dir, "bigN_scenario", i, "_rep", j, ".rds")
    saveRDS(sim, path)
  }
}
