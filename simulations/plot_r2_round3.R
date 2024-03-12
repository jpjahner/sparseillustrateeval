library(tidyverse)


r2 <- read_csv("simulations/sim_r2.csv") %>% mutate(scenario = factor(scenario))

ggplot(r2, aes(x = scenario, y = r2)) + geom_boxplot()

ggsave("simulations/sim_r2.pdf")
