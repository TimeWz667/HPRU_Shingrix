library(tidyverse)

source("scripts_analyses/fn_arrange_inputs.R")

seed <- 11667

## VE
pars_ve_zvl <- local({
  load(here::here("pars", "pars_ve_zvl_rw_zlg.rdata"))
  ve_zvl_age <- read_csv(here::here("pars", "pars_ve_zvl_agp.csv"))
  
  n_sims <- max(pars_ve_zvl$Key)
  
  crossing(Age = 50:100, Key = 1:n_sims, Yr = 1:50) %>% 
    filter(Age - Yr >= 50) %>% 
    left_join(pars_ve_zvl, by = c("Key", "Yr")) %>% 
    left_join(ve_zvl_age, by = "Age") %>% 
    rename(VE0 = VE) %>% 
    mutate(
      oddVE = log(VE0 / (1 - VE0)) + or70spline,
      VE = 1 / (1 + exp(-oddVE)),
      Vaccine = "ZVL"
    ) %>% 
    select(Key, Age, Yr, Vaccine, VE, IC)
  
}) 


save(pars_ve_zvl, file = here::here("pars", "pars_ve_zvl_rwa.rdata"))



file_inputs <- "parset_nic_c35q35y24n1k_trial.rdata"

set.seed(seed)
pars_set <- load_inputs_nic(discount_costs = 0.035, discount_effects = 0.035, 
                            year = 2024, n_sims = 1e3, realworld = F)
save(pars_set, file = here::here("pars", file_inputs))


file_inputs <- "parset_nic_c35q35y24n1k_realworld.rdata"

set.seed(seed)
pars_set <- load_inputs_nic(discount_costs = 0.035, discount_effects = 0.035, 
                            year = 2024, n_sims = 1e3, realworld = T)
save(pars_set, file = here::here("pars", file_inputs))


file_inputs <- "parset_nic_c35q35y24n1k_zle_realworld.rdata"

set.seed(seed)
pars_set <- load_inputs_nic(discount_costs = 0.035, discount_effects = 0.035, 
                            year = 2024, n_sims = 1e3, realworld = T, 
                            ve_rzv = "pars_ve_rzv_rw_zle.rdata")
save(pars_set, file = here::here("pars", file_inputs))


## Immuno-compromised population
file_inputs <- "parset_ic_c35q35y24n1k_trial.rdata"

set.seed(seed)
pars_set <- load_inputs_ic(discount_costs = 0.035, discount_effects = 0.035, 
                            year = 2024, n_sims = 1e3, realworld = F)
save(pars_set, file = here::here("pars", file_inputs))


file_inputs <- "parset_ic_c35q35y24n1k_realworld.rdata"

set.seed(seed)
pars_set <- load_inputs_ic(discount_costs = 0.035, discount_effects = 0.035, 
                           year = 2024, n_sims = 1e3, realworld = T)
save(pars_set, file = here::here("pars", file_inputs))






