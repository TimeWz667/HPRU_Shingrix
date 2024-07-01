library(tidyverse)

options(dplyr.summarise.inform = FALSE)


source(here::here("models", "sim_hz.R"))
source(here::here("models", "misc.R"))


amlu <- list(
  A = mean,
  M = median,
  L = function(x) quantile(x, 0.025, na.rm = T),
  U = function(x) quantile(x, 0.975, na.rm = T)
)


a_run <- function(pars, age0) {
  with(model, {
    dfs <- bind_rows(
      populate(age0, pars) %>% mutate(Arm = "SOC", Age1 = NA),
      populate(age0, pars) %>% 
        vaccinate(age0, "ZVL", pars) %>% 
        mutate(Arm = "Vac", Age1 = NA),
    ) %>%
      bind_rows(lapply(80:99, function(age1) {
        populate(age0, pars) %>% 
          vaccinate(age0, "ZVL", pars) %>% 
          vaccinate(age1, "ReRZV1", pars) %>% 
          mutate(Arm = "ReVac_RZV1", Age1 = age1)
      })) %>% 
      bind_rows(lapply(80:99, function(age1) {
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


for (ve_type in c("trial", "realworld")) {
  ve_type <- glue::as_glue(ve_type)
  
  load(file = here::here("pars", "parset_nic_c35q35y24n1k_" + ve_type + ".rdata"))

  
  ## Simulation -----
  keys <- 1:pars_set$N_Sims
  # keys <- keys[1:20]
  
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
  
  save(yss, file = here::here("out", "yss_zvl2rzv_" + ve_type + ".rdata"))
  
  
  ## Output statistics
  
  stats_ys <- yss %>% 
    group_by(Scenario, Arm, Age0, Age1) %>% 
    select(-Key) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(-c(Scenario, Age0, Age1, Arm), names_to = c("Index", "name"), names_pattern = "(\\S+)_(M|L|U)") %>% 
    pivot_wider()
  
  
  write_csv(stats_ys, file = here::here("docs", "tabs", "stats_ys_zvl2rzv_" + ve_type + ".csv"))
  
  
  
  stats_ce <- local({
    temp <- yss %>% 
      pivot_longer(-c(Scenario, Age0, Age1, Arm, Key, N0, Year0), names_to = "Index")
    
    dy0 <- temp %>% 
      filter(Scenario == "Overall") %>%
      filter(Arm != "SOC") %>% 
      left_join(
        temp %>% 
          filter(Arm == "SOC") %>% 
          filter(Scenario == "Overall") %>%
          select(Scenario, Age0, Key, Index, value0 = value),
        relationship = "many-to-many"
      )
    
    dy1 <- temp %>% 
      filter(Scenario != "Overall") %>%
      filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
      left_join(
        temp %>% 
          filter(Arm == "Vac") %>% 
          filter(Scenario != "Overall") %>%
          select(Scenario, Age0, Key, Index, value0 = value),
        relationship = "many-to-many"
      )
    
    dy <- bind_rows(dy0, dy1) %>% 
      mutate(
        Index = paste0("d", Index),
        Diff = value - value0
      ) %>% 
      select(-value, -value0) %>% 
      pivot_wider(names_from = Index, values_from = "Diff") %>% 
      mutate(
        across(starts_with("d"), \(x) x/N0),
        ICER = dC_All_d / dQ_All_d,
        Price0 = dC_VacRZV_d / dN_VacRZV_d,
        Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
        Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d,
      ) %>% 
      select(-N0) %>% 
      group_by(Scenario, Age0, Age1, Arm, Year0) %>% 
      select(-Key) %>% 
      summarise(
        across(everything(), amlu),
        Thres20_50 = median(Thres20),
        Thres30_90 = quantile(Thres30, 0.9)
      ) %>% 
      mutate(
        Thres = pmin(Thres20_50, Thres30_90)
      )
  })
  
  write_csv(stats_ce, file = here::here("docs", "tabs", "stats_ce_zvl2rzv_" + ve_type + ".csv"))

}

