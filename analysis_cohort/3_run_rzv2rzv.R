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
  
  for (age0 in c(60, 65, 70, 75, 80)) {
    for (age1 in (age0 + 1): 90) {
      yss[[length(yss) + 1]] <- 
        sim_cohort_vac(pars, age0 = age0, age1 = age1, vaccine0 = "Shingrix", vaccine1 = "Shingrix", agg = T) %>% 
        mutate(Key = k, Scenario = glue::as_glue("ReVac_") + age0 + ":" + age1, Age0 = age0, Age1 = age1)
    }
  }
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Age1, Arm, Type, Key)

save(yss, file = here::here("analysis_cohort", "temp", "yss_rzv2rzv.rdata"))




diffs <- local({
  temp <- yss %>% 
    select(Scenario, Arm, Age0, Age1, Key, starts_with("N_"), ends_with("_d")) %>% 
    pivot_longer(-c(Scenario, Arm, Age0, Age1, Key), names_to = "Index")
  
  temp %>% 
    left_join(temp %>% filter(Arm == "SOC") %>% 
                select(Scenario, Index, Key, value0 = value)) %>% 
    mutate(Diff = value - value0) %>% 
    select(-value, -value0)  
})


stats_mean <- yss %>% 
  group_by(Scenario, Arm, Age0, Age1) %>% 
  select(-Key) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  left_join(
    diffs %>% 
      mutate(Index = paste0("d", Index)) %>% 
      pivot_wider(names_from = Index, values_from = Diff) %>% 
      group_by(Scenario, Arm, Age0, Age1) %>% 
      select(-Key) %>% 
      summarise_all(mean) %>% 
      ungroup()
  )


yss_ce <- diffs %>% 
  filter(Index %in% c("C_All_d", "Q_All_d")) %>% 
  mutate(Index = paste0("d", Index)) %>% 
  pivot_wider(names_from = Index, values_from = Diff) %>% 
  filter(Arm != "SOC") %>% 
  mutate(ICER = dC_All_d / dQ_All_d)


stats_ce <- yss_ce %>% 
  pivot_longer(c(dC_All_d, dQ_All_d, ICER), names_to = "Index") %>% 
  group_by(Scenario, Arm, Age0, Age1, Index) %>% 
  summarise(
    Avg = mean(value),
    M = median(value),
    L = quantile(value, 0.025),
    U = quantile(value, 0.975)
  )


save(stats_mean, stats_ce, file = here::here("analysis_cohort", "tabs", "stats_rzv2rzv.rdata"))



