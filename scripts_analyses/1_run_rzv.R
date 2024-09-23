library(tidyverse)

## RZV analysis with trial-based VE

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
    
    df2 <- populate(age0, pars) %>% 
      vaccinate(age0, "ReRZV1", pars) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      summarise(pars) %>% 
      mutate(Arm = "Vac1")
    
    bind_rows(df0, df1, df2) %>% 
      mutate(Scenario = glue::as_glue("Vac_") + as.character(age0), Age0 = age0)
  })
}



for (ve_type in c("trial", "realworld")) {
  ve_type <- glue::as_glue(ve_type)
  
  # Load inputs
  load(file = here::here("pars", "parset_nic_c35q35y24n1k_" + ve_type + ".rdata"))
  
  ## Simulation -----
  keys <- 1:pars_set$N_Sims
  # keys <- keys[1:10]
  
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
  
  save(yss, file = here::here("out", "yss_rzv_" + ve_type + ".rdata"))
  
}


## Output statistics
for (ve_type in c("trial", "realworld")) {
  ve_type <- glue::as_glue(ve_type)
  
  load(file = here::here("out", "yss_rzv_" + ve_type + ".rdata"))
  
  stats_ys <- yss %>% 
    group_by(Scenario, Age0, Arm) %>% 
    select(-Key) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(-c(Scenario, Age0, Arm), names_to = c("Index", "name"), names_pattern = "(\\S+)_(A|M|L|U)") %>% 
    pivot_wider()
  
  
  write_csv(stats_ys, file = here::here("docs", "tabs", "stats_ys_rzv_" + ve_type + ".csv"))
  
  yss_diff <- local({
    temp <- yss %>% 
      pivot_longer(-c(Scenario, Age0, Arm, Key, N0, Year0), names_to = "Index")
    
    temp %>% 
      filter(Arm != "SOC") %>% 
      left_join(
        temp %>% 
          filter(Arm == "SOC") %>% 
          select(Scenario, Age0, Key, Index, value0 = value),
        relationship = "many-to-many"
      ) %>% 
      mutate(
        Index = paste0("d", Index),
        Diff = value - value0
      ) %>% 
      select(-value, -value0) %>% 
      pivot_wider(names_from = Index, values_from = "Diff") %>% 
      mutate(
        ICER = dC_All_d / dQ_All_d,
        Price0 = dC_VacRZV_d / dN_VacRZV_d,
        Thres20 = (dQ_All_d * 2e4 - dC_Med_d) / dN_VacRZV_d,
        Thres30 = (dQ_All_d * 3e4 - dC_Med_d) / dN_VacRZV_d,
      )
    
  })
  
  
  stats_ce <- yss_diff %>% 
    group_by(Scenario, Age0, Arm, Year0, N0) %>% 
    select(-Key) %>% 
    summarise(
      across(everything(), amlu),
      Thres20_50 = median(Thres20),
      Thres30_90 = quantile(Thres30, 0.1)
    ) %>% 
    mutate(
      Thres = pmin(Thres20_50, Thres30_90)
    )
  
  write_csv(stats_ce, file = here::here("docs", "tabs", "stats_ce_rzv_" + ve_type + ".csv"))
  
  
  stats_icer <- yss_diff %>% 
    mutate(
      ICER25 = (dC_Med_d + dN_VacRZV_d * 25) / dQ_All_d,
      ICER50 = (dC_Med_d + dN_VacRZV_d * 50) / dQ_All_d,
      ICER75 = (dC_Med_d + dN_VacRZV_d * 75) / dQ_All_d,
      ICER100 = (dC_Med_d + dN_VacRZV_d * 100) / dQ_All_d,
      ICER_Thres = (dC_Med_d + dN_VacRZV_d * Thres20) / dQ_All_d # For validation
    ) %>% 
    select(Scenario, Age0, Arm, Year0, starts_with("ICER")) %>% 
    group_by(Scenario, Age0, Arm, Year0) %>% 
    summarise_all(amlu)
  
  
  write_csv(stats_icer, file = here::here("docs", "tabs", "stats_icer_rzv_" + ve_type + ".csv"))
}



