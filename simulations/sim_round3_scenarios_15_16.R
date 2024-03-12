library(tidyverse)


source("/pfs/tc1/project/modelscape/analyses/sparse/code/simulations/sparse_sim.R")

sim_grid <- read_csv("/pfs/tc1/project/modelscape/analyses/sparse/data/simulations/sim_round3/sim_grid.csv")

dir <- "/pfs/tc1/project/modelscape/analyses/sparse/data/simulations/sim_round3/"

for(i in 15:16) {
  for(j in 1:100) {
    sim <- plyr::splat(sparse_sim)(sim_grid[i,-1]) 
    path <- paste0(dir, "scenario", i, "_rep", j, ".rds")
    saveRDS(sim, path)
  }
}


