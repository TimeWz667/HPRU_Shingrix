library(tidyverse)



source(here::here("models", "sim_hz.R"))
source(here::here("models", "misc.R"))


a_run <- function(pars, age0) {
  with(model, {
    df0 <- populate(age0, pars) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      summarise(pars) %>% 
      mutate(Arm = "SOC")
    
    df1 <- populate(age0, pars) %>% 
      vaccinate(age0, "RZV", pars) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      summarise(pars) %>% 
      mutate(Arm = "Vac")
    
    bind_rows(df0, df1) %>% 
      mutate(Scenario = glue::as_glue("Vac_") + age0, Age0 = age0)
  })
}


# Load inputs
load(file = here::here("pars", "parset_nic_c35q35y24n1k.rdata"))



## Simulation -----
keys <- 1:pars_set$N_Sims
keys <- keys[1:10]

yss <- list()

pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  pars <- get_pars(pars_set, k)
  
  for (age0 in 60:99) {
    yss[[length(yss) + 1]] <- a_run(pars, age0 = age0) %>% mutate(Key = k)
  }
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Arm, Key)

save(yss, file = here::here("out", "yss_rzv.rdata"))


## Output statistics

stats_ys <- yss %>% 
  group_by(Scenario, Age0, Arm) %>% 
  select(-Key) %>% 
  summarise_all(list(
    M = median,
    L = function(x) quantile(x, 0.025, na.rm = T),
    U = function(x) quantile(x, 0.975, na.rm = T)
  )) %>% 
  pivot_longer(-c(Scenario, Age0, Arm), names_to = c("Index", "name"), names_pattern = "(\\S+)_(M|L|U)") %>% 
  pivot_wider()


write_csv(stats_ys, file = here::here("docs", "tabs", "stats_rzv.csv"))


stats_ce <- local({
  s0 <- yss %>% 
    filter(Arm == "SOC") %>% 
    select(Scenario, Age0, Key, Risk_HZ0 = Risk_HZ, Risk_Death0 = Risk_Death, 
           Q_HZ_d0 = Q_HZ_d, Q_Life_d0 = Q_Life_d, Q_All_d0 = Q_All_d,
           N_VacRZV_d0 = N_VacRZV_d,
           C_Vac_d0 = C_Vac_d, C_VacRZV_d0 = C_VacRZV_d, C_Med_d0 =  C_Med_d, C_All_d0 = C_All_d
    )
  
  yss %>% 
    filter(Arm != "SOC") %>% 
    select(Scenario, Age0, Arm, Key, Risk_HZ, Risk_Death, N_VacRZV_d,
           Q_HZ_d, Q_Life_d, Q_All_d, C_Vac_d, C_VacRZV_d, C_Med_d, C_All_d) %>% 
    left_join(s0, by = c("Scenario", "Age0", "Key")) %>% 
    mutate(
      AvtHZ = (Risk_HZ0 - Risk_HZ) / Risk_HZ0,
      AvtDeath = (Risk_Death0 - Risk_Death) / Risk_Death0,
      dQ_HZ_d = Q_HZ_d - Q_HZ_d0, 
      dQ_Life_d = Q_Life_d - Q_Life_d0, 
      dQ_All_d = Q_All_d - Q_All_d0, 
      dC_Vac_d = C_Vac_d - C_Vac_d0, 
      dC_VacRZV_d = C_VacRZV_d - C_VacRZV_d0, 
      dC_Med_d = C_Med_d - C_Med_d0, 
      dC_All_d = C_All_d - C_All_d0,
      dN_VacRZV_d = N_VacRZV_d - N_VacRZV_d0,
      ICER = dC_All_d / dQ_All_d,
      price = dC_VacRZV_d / dN_VacRZV_d,
      Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
      Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d,
    ) %>% 
    select(Scenario, Age0, Arm, starts_with(c("Avt", "dQ", "dC", "Thres")), ICER) %>% 
    group_by(Scenario, Age0, Arm) %>% 
    summarise_all(list(
      M = median,
      L = function(x) quantile(x, 0.025, na.rm = T),
      U = function(x) quantile(x, 0.975, na.rm = T)
    )) %>% 
    pivot_longer(-c(Scenario, Age0, Arm), names_to = c("Index", "name"), names_pattern = "(\\S+)_(M|L|U)") %>% 
    pivot_wider()
})


