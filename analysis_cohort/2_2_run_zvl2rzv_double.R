library(tidyverse)



source(here::here("models", "m_cohort_hz.R"))
source(here::here("models", "misc.R"))

# Load inputs
file_inputs <- "pars_nic.rdata"
if (!(file_inputs %in% dir(here::here("analysis_cohort", "inputs")))) {
  source(here::here("analysis_cohort", "fn_arrange_inputs.R"))
  pars_set <- load_inputs_nic(discount_costs = 0.035, discount_effects = 0.035, year = 2024, n_sims = 1e3)
  if (!dir.exists(here::here("analysis_cohort", "inputs"))) {
    dir.create(here::here("analysis_cohort", "inputs"))
  }
  save(pars_set, file = here::here("analysis_cohort", "inputs", file_inputs))
} else {
  load(file = here::here("analysis_cohort", "inputs", file_inputs))
}


## Simulation -----
keys <- 1:pars_set$N_Sims
keys <- keys[1:300]

yss <- list()

pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  pars <- get_pars(pars_set, k)
  
  for (age0 in c(70, 75)) {
    for (age1 in (age0 + 1): 90) {
      yss[[length(yss) + 1]] <- 
        sim_cohort_vac(pars, age0 = age0, age1 = age1, vaccine0 = "Zostavax", vaccine1 = "Shingrix", agg = T) %>% 
        mutate(Key = k, Scenario = glue::as_glue("ReVac_") + age0 + ":" + age1, Age0 = age0, Age1 = age1)
    }
  }
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Age1, Arm, Type, Key)

save(yss, file = here::here("analysis_cohort", "temp", "yss_zvl2rzv.rdata"))


tab <- yss %>% 
  filter(Arm == "ReVac") %>% 
  group_by(Age0, Age1, Type) %>% 
  summarise(
    across(c(dC_All_d, dQ_All_d, ICER, NMB), list(
      M = median,
      L = function(x) quantile(x, 0.025),
      U = function(x) quantile(x, 0.975)
    ))
  )


g <- tab %>% 
  filter(Type == "Second") %>% 
  filter(Age0 %in% c(70, 75)) %>%
  filter(Age1 <= 95) %>% 
  mutate(
    a0 = paste0("Age of ZVL vaccination: ", Age0)
  ) %>% 
  ggplot(aes(x = Age1)) +
  geom_ribbon(aes(ymin = ICER_L, ymax = ICER_U), alpha = 0.2) + 
  geom_line(aes(y = ICER_M)) + 
  geom_hline(yintercept = 2e4, linetype = 2) +
  geom_hline(yintercept = 3e4, linetype = 2) +
  scale_x_continuous("Age of re-vaccination", breaks = c(70, 75, seq(80, 90, 2), 95)) +
  scale_y_continuous("Incremental cost-effectiveness ratio, \ncost per QALY gained, GBP", 
                     breaks = 0:6 * 1e4, labels = scales::label_dollar(prefix = "")) +
  expand_limits(y = c(0, 5e4), x = 70) +
  facet_grid(.~a0)

g
