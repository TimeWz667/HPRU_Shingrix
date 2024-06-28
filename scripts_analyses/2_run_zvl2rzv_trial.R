library(tidyverse)

options(dplyr.summarise.inform = FALSE)


source(here::here("models", "sim_hz.R"))
source(here::here("models", "misc.R"))


a_run <- function(pars, age0) {
  with(model, {
    dfs <- bind_rows(
      populate(age0, pars) %>% mutate(Arm = "SOC", Age1 = NA),
      populate(age0, pars) %>% 
        vaccinate(age0, "ZVL", pars) %>% 
        mutate(Arm = "Vac", Age1 = NA),
    ) %>%
      bind_rows(lapply((age0 + 1): 99, function(age1) {
        populate(age0, pars) %>% 
          vaccinate(age0, "ZVL", pars) %>% 
          vaccinate(age1, "ReRZV1", pars) %>% 
          mutate(Arm = "ReVac_RZV1", Age1 = age1)
      })) %>% 
      bind_rows(lapply((age0 + 1): 99, function(age1) {
        populate(age0, pars) %>% 
          vaccinate(age0, "ZVL", pars) %>% 
          vaccinate(age1, "ReRZV", pars) %>% 
          mutate(Arm = "ReVac_RZV2", Age1 = age1)
      })) %>% 
      mutate(Age0 = age0) %>% 
      group_by(Arm, Age0, Age1) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      mutate(Scenario = "Overall")
    
    
    dfs <- dfs %>% 
      bind_rows(bind_rows(lapply((age0 + 1): 99, function(age1) {
        dfs %>% 
          filter(Arm == "Vac" | Age1 == age1) %>% 
          filter(Age >= age1) %>% 
          mutate(Scenario = "Second_" + glue::as_glue(age1))
      }))) %>% 
      group_by(Scenario, Arm, Age0, Age1) %>% 
      summarise(pars) %>% 
      ungroup()

    dfs
  })
}


# Load inputs
load(file = here::here("pars", "parset_nic_c35q35y24n1k.rdata"))


## Simulation -----
keys <- 1:pars_set$N_Sims
keys <- keys[1:200]

yss <- list()

pb <- txtProgressBar(min = 1, max = max(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  pars <- get_pars(pars_set, k)
  
  for (age0 in 70:79) {
    yss[[length(yss) + 1]] <- a_run(pars, age0 = age0) %>% mutate(Key = k)
  }
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss) %>% 
  relocate(Scenario, Age0, Age1, Arm, Key)

save(yss, file = here::here("out", "yss_zvl2rzv.rdata"))


## Output statistics

stats_ys <- yss %>% 
  group_by(Scenario, Arm, Age0, Age1) %>% 
  select(-Key) %>% 
  summarise_all(list(
    M = median,
    L = function(x) quantile(x, 0.025, na.rm = T),
    U = function(x) quantile(x, 0.975, na.rm = T)
  )) %>% 
  pivot_longer(-c(Scenario, Age0, Age1, Arm), names_to = c("Index", "name"), names_pattern = "(\\S+)_(M|L|U)") %>% 
  pivot_wider()


write_csv(stats_ys, file = here::here("docs", "tabs", "stats_ys_zvl2rzv.csv"))



stats_ce <- local({
  s0 <- yss %>% 
    filter(Scenario == "Overall") %>% 
    filter(Arm == "SOC") %>% 
    select(Scenario, Age0, Key, Risk_HZ0 = Risk_HZ, Risk_Death0 = Risk_Death, 
           Q_HZ_d0 = Q_HZ_d, Q_Life_d0 = Q_Life_d, Q_All_d0 = Q_All_d,
           N_VacRZV_d0 = N_VacRZV_d,
           C_Vac_d0 = C_Vac_d, C_VacRZV_d0 = C_VacRZV_d, C_Med_d0 =  C_Med_d, C_All_d0 = C_All_d
    )
  
  dy0 <- yss %>% 
    filter(Scenario == "Overall") %>% 
    filter(Arm != "SOC") %>% 
    select(Scenario, Age0, Age1, Arm, Key, N0 = N0, Risk_HZ, Risk_Death, N_VacRZV_d,
           Q_HZ_d, Q_Life_d, Q_All_d, C_Vac_d, C_VacRZV_d, C_Med_d, C_All_d) %>% 
    left_join(s0, by = c("Scenario", "Age0", "Key")) %>% 
    mutate(Type = "Overall")
  
  
  s0 <- yss %>% 
    filter(Scenario != "Overall") %>% 
    filter(Arm == "Vac") %>% 
    select(Scenario, Age0, Key, Risk_HZ0 = Risk_HZ, Risk_Death0 = Risk_Death, 
           Q_HZ_d0 = Q_HZ_d, Q_Life_d0 = Q_Life_d, Q_All_d0 = Q_All_d,
           N_VacRZV_d0 = N_VacRZV_d,
           C_Vac_d0 = C_Vac_d, C_VacRZV_d0 = C_VacRZV_d, C_Med_d0 =  C_Med_d, C_All_d0 = C_All_d
    )
  
  dy1 <- yss %>% 
    filter(Scenario != "Overall") %>% 
    filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
    select(Scenario, Age0, Arm, Age1, Arm, Key, N0 = N0, Risk_HZ, Risk_Death, N_VacRZV_d,
           Q_HZ_d, Q_Life_d, Q_All_d, C_Vac_d, C_VacRZV_d, C_Med_d, C_All_d) %>% 
    left_join(s0, by = c("Scenario", "Age0", "Key")) %>% 
    mutate(Type = "Second")
  
  bind_rows(dy0, dy1) %>% 
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
    select(Scenario, Age0, Age1, Arm, Type, N0, starts_with(c("Avt", "dQ", "dC", "dN", "Thres")), ICER) %>% 
    group_by(Scenario, Age0, Age1, Arm, Type) %>% 
    summarise_all(list(
      M = median,
      L = function(x) quantile(x, 0.025, na.rm = T),
      U = function(x) quantile(x, 0.975, na.rm = T)
    )) %>% 
    pivot_longer(-c(Scenario, Age0, Age1, Arm, Type), names_to = c("Index", "name"), names_pattern = "(\\S+)_(M|L|U)") %>% 
    pivot_wider()
})


write_csv(stats_ce, file = here::here("docs", "tabs", "stats_ce_zvl2rzv.csv"))



stats_ce %>% 
  filter(Type == "Second") %>% 
  filter(Index == "Thres30") %>%
  filter(Age1 %in% seq(80, 100, 5)) %>% 
  ggplot() +
  geom_pointrange(aes(x = Age1, y = M, ymin = L, ymax = U, colour = Arm), position = position_dodge(1)) +
  facet_grid(.~Age0) +
  expand_limits(y = 0)


stats_ce %>% 
  filter(Type == "Second") %>% 
  filter(Index == "ICER") %>% 
  ggplot() +
  geom_line(aes(x = Age1, y = M, colour = Arm)) +
  geom_hline(yintercept = c(2e4, 3e4), linetype = 2) +
  facet_grid(.~Age0)




