# Create initial set of simulations for evaluation of sparse models

source("simulations/sparse_sim.R")


# create grid of simulation scenarios
sim_grid <- expand.grid(n = c(50, 500), 
                        p = c(100, 1000, 10000, 10000), 
                        cluster_size = 10,
                        n_causal = c(0, 10), 
                        beta_mean = 0.2, 
                        beta_sd = 0.05, 
                        alpha = 1)

# simulate data for each scenario
sims <- apply(sim_grid, 1, function(i) plyr::splat(sparse_sim)(i))

# create descriptive names of simulations
names(sims) <- split(sim_grid, seq(nrow(sim_grid))) %>%
  map_chr(~sapply(1:7, function(i) paste(names(.x)[i], .x[i], sep = " = ")) %>% paste(collapse = "; "))

# export simulations

saveRDS(sims, "simulations/sim_round2.rds")
saveRDS(sim_grid, "simulations/sim_grid_round2.rds")
