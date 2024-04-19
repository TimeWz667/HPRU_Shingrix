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
keys <- keys[1:200]

yss <- list()

pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  pars <- get_pars(pars_set, k)
  
  for (age0 in seq(60, 95, 1)) {
    for (price in c(25, 50, 75, 100)) {
      pars$CostVac <- pars$CostVac %>% 
        mutate(
          cost_vac_per_dose = ifelse(Vaccine == "Shingrix", price, cost_vac_per_dose),
          cost_vac_pp = (cost_vac_per_dose + cost_admin_per_dose) * number_courses
        )
      
      yss[[length(yss) + 1]] <- 
        sim_cohort_vac(pars, age0 = age0, vaccine0 = "Shingrix", agg = T) %>% 
        mutate(Key = k, Scenario = glue::as_glue("Vac_") + age0, Age0 = age0, Price = price)
    }
    

  }
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Arm, Type, Key)

save(yss, file = here::here("analysis_cohort", "temp", "yss_rzv_sens_price.rdata"))


stats <- yss %>% 
  mutate(
    CostVac = (Price + 10) * 2,
    Price = factor(Price, sort(unique(Price))),
    CostVac = factor(CostVac, sort(unique(CostVac))),
    Age0 = factor(Age0, sort(unique(Age0)))  
  ) %>% 
  select(-Key) %>% 
  group_by(Scenario, Type, Arm, Age0, Price, CostVac) %>% 
  summarise_all(mean)
  
write_csv(stats, here::here("analysis_cohort", "tabs", "tab_rzv_sens_price.csv"))



