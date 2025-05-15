library(tidyverse)



dir(here::here("data", "raw_ce"))


coef_cost_hosp_n <- coef_cost_hosp <- read_csv(here::here("data", "raw_ce", "Cost HZ hospitalisation sim_coef_agelm_rr_100000.csv")) %>% 
  mutate(Key = 1:n()) %>% 
  select(Key, b0 = Intercept, ba = age)

save(coef_cost_hosp, file = here::here("data", "processed_ce", "coef_cost_hosp_nic.rdata"))


coef_cost_hosp_o <- coef_cost_hosp <- read_csv(here::here("data", "raw_ce", "Cost HZ hospitalisation IC sim_coef_agelm_rr_100000.csv")) %>% 
  mutate(Key = 1:n()) %>% 
  select(Key, b0 = Intercept, ba = age)

save(coef_cost_hosp, file = here::here("data", "processed_ce", "coef_cost_hosp_ic.rdata"))



crossing(Key = 1:200, Age = 50:100) %>% 
  left_join(coef_cost_hosp) %>% 
  mutate(
    Cost = b0 + ba * Age
  ) %>% 
  ggplot() +
  stat_interval(aes(x = Age, y = Cost)) +
  scale_colour_brewer() +
  scale_y_continuous("Cost per hospitalised HZ episode, £") +
  expand_limits(y = 0)

