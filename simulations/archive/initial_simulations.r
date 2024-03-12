# Create initial set of simulations for evaluation of sparse models

source("simulations/sparse_sim.R")


# create grid of simulation scenarios
sim_grid <- expand.grid(sparsity = c(0, 0.9), 
                        alpha = c(1, 1e3), 
                        beta_scale = c(0.1, 0.2))

# simulate data for each scenario
sims <- apply(sim_grid, 1, function(i) plyr::splat(sparse_sim)(i))

# create descriptive names of simulations
names(sims) <- split(sim_grid, seq(nrow(sim_grid))) %>%
  map_chr(~sapply(1:3, function(i) paste(names(.x)[i], .x[i], sep = " = ")) %>% paste(collapse = "; "))

# export simulations

saveRDS(sims, "simulations/initial_sims.rds")
saveRDS(sim_grid, "simulations/initial_sim_grid.rds")
