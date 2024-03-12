library(tidyverse)


setwd("/pfs/tc1/project/modelscape/analyses/sparse/data/simulations/sim_round4")

paths <- list.files(full.names = T) %>% 
  str_subset("scenario") %>%
  str_subset("rds")
  
scenario <- str_extract(paths, "(?<=scenario)[0-9]+[a-d]")


r2 <- rep(NA, length(paths))
for(i in 1:length(paths)) {
  sim = readRDS(paths[i])
  r2[i] = cor(sim$y, sim$y_mu)^2
}

out <- tibble(scenario = scenario,
              rep = str_extract(paths, "(?<=rep)[0-9]+"), 
              r2 = r2)

write_csv(out, "sim_r2_round4.csv")