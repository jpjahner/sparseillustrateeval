library(tidyverse)


setwd("/pfs/tc1/project/modelscape/analyses/sparse/data/simulations/sim_round3")

paths <- list.files(full.names = T) %>% 
  str_subset("scenario") %>%
  str_subset("rds")
  
scenario <- str_extract(paths, "(?<=scenario)[0-9]+")
good_scenario <- c(2,4,6,8,10,12,14)
keep <- scenario %in% good_scenario

paths <- paths[keep]

r2 <- rep(NA, length(paths))
for(i in 1:length(paths)) {
  sim = readRDS(paths[i])
  r2[i] = cor(sim$y, sim$y_mu)^2
}

out <- tibble(scenario = scenario[keep],
              rep = str_extract(paths, "(?<=rep)[0-9]+"), 
              r2 = r2)

write_csv(out, "sim_r2.csv")