library(tidyverse)


stats <- read_csv(here::here("docs", "tabs", "stats_ce_rzv.csv"))


stats %>% 
  select(Scenario:M) %>% 
  pivot_wider(names_from = Index, values_from = M)

