library(tidyverse)


source("/pfs/tc1/project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/sparse_sim.R")

sim_grid <- read_csv("/pfs/tc1/project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/sim_grid_round4.csv") %>%
  filter(beta_mean == 0.4) %>%
  mutate(beta_sd = 0, 
         scenario = str_replace(scenario, "a", "c"))

dir <- "/pfs/tc1/project/modelscape/analyses/sparse/data/simulations/sim_round4/"

for(i in 1:4) {
  for(j in 1:100) {
    sim <- plyr::splat(sparse_sim)(sim_grid[i,-1]) 
    path <- paste0(dir, "scenario", sim_grid$scenario[i], "_rep", j, ".rds")
    saveRDS(sim, path)
  }
}
