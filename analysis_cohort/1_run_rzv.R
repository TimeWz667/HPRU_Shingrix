library(tidyverse)



source(here::here("models", "m_cohort_hz.R"))
source(here::here("models", "misc.R"))

# Load inputs
file_inputs <- "pars_nic.rdata"
if (!(file_inputs %in% dir(here::here("analysis_cohort", "inputs")))) {
  source(here::here("analysis_cohort", "fn_arrange_inputs.R"))
  pars_set <- load_inputs_nic(discount_costs = 0.035, discount_effects = 0.035, year = 2024, n_sims = 1e3)
  save(pars_set, file = here::here("analysis_cohort", "inputs", file_inputs))
} else {
  load(file = here::here("analysis_cohort", "inputs", file_inputs))
}


## Simulation -----
keys <- 1:pars_set$N_Sims
keys <- keys[1:100]

yss <- list()

pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  pars <- get_pars(pars_set, k)
  
  for (age0 in 60:95) {
    yss[[length(yss) + 1]] <- 
      sim_cohort_vac(pars, age0 = age0, vaccine0 = "Shingrix", agg = T) %>% 
      mutate(Key = k, Scenario = glue::as_glue("Vac_") + age0, Age0 = age0)
  }
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Arm, Type, Key)

save(yss, file = here::here("analysis_cohort", "temp", "yss_rzv.rdata"))

