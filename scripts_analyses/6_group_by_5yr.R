library(tidyverse)


amlu <- list(
  A = mean,
  M = median,
  L = function(x) quantile(x, 0.025, na.rm = T),
  U = function(x) quantile(x, 0.975, na.rm = T)
)


## Load population
pop <- local({
  load(here::here("data", "processed_demography", "Population_ONS.rdata"))
  demo_ons %>% 
    filter(Location == "England" & Year == 2023) %>% 
    select(Age, N)
  
})



## Yss to 5 groups -----
## RZV -----
for (ve_type in c("realworld", "trial")) {
  ve_type <- glue::as_glue(ve_type)
  
  load(here::here("out", "yss_rzv_" + ve_type + ".rdata"))
  
  
  yss <- yss %>% 
    left_join(pop %>% select(Age0 = Age, N)) %>% 
    mutate(
      Agp0 = cut(Age0, seq(60, 100, 5), right = F)
    ) %>% 
    group_by(Agp0, Arm, Key, Year0) %>% 
    summarise(
      N0 = weighted.mean(N0, w = N),
      across(starts_with(c("Year_", "Risk")), \(x) weighted.mean(x, w = N)),
      across(starts_with(c("Q_", "C_", "N_")), \(x) sum(x * N))
    ) %>% 
    mutate(
      Scenario = paste0("Vac", Agp0)
    )
  
  save(yss, file = here::here("out", "yss_5yr_rzv_" + ve_type + ".rdata"))
  
  
  stats_ys <- yss %>% 
    group_by(Scenario, Agp0, Arm, Year0, N0) %>% 
    select(-Key) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(-c(Scenario, Agp0, Arm, Year0, N0), names_to = c("Index", "name"), names_pattern = "(\\S+)_(A|M|L|U)") %>% 
    pivot_wider()
  
  
  write_csv(stats_ys, file = here::here("docs", "tabs", "stats_ys_5yr_rzv_" + ve_type + ".csv"))
  

  stats_ce <- local({
    temp <- yss %>% 
      pivot_longer(-c(Scenario, Agp0, Arm, Key, N0, Year0), names_to = "Index") %>% 
      ungroup()
    
    temp %>% 
      filter(Arm != "SOC") %>% 
      left_join(
        temp %>% 
          filter(Arm == "SOC") %>% 
          select(Scenario, Agp0, Key, Index, value0 = value),
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
      ) %>% 
      group_by(Scenario, Agp0, Arm, Year0, N0) %>% 
      select(-Key) %>% 
      summarise(
        across(everything(), amlu),
        Thres20_50 = median(Thres20, na.rm = T),
        Thres30_90 = quantile(Thres30, 0.9, na.rm = T)
      ) %>% 
      mutate(
        Thres = pmin(Thres20_50, Thres30_90)
      )
    
  })
  
  write_csv(stats_ce, file = here::here("docs", "tabs", "stats_ce_5yr_rzv_" + ve_type + ".csv"))
}


## ZVL2RZV -----
for (ve_type in c("realworld", "trial")) {
  ve_type <- glue::as_glue(ve_type)
  
  load(here::here("out", "yss_zvl2rzv_" + ve_type + ".rdata"))

  yss <- yss %>% 
    filter(Scenario != "Overall") %>% 
    mutate(
      sa = as.numeric(gsub("Second_", "", Scenario)),
      Agp = cut(sa, seq(60, 100, 5), right = F),
      across(starts_with(c("Q_", "C_", "N_")), \(x) x / N0),
      N0 = 1,
      Scenario = paste0("Second_", Agp),
      Age1 = cut(Age1, seq(60, 100, 5), right = F)
    ) %>% 
    left_join(pop %>% select(sa = Age, N), by = "sa")%>% 
    group_by(Scenario, Arm, Age0, Age1, Key, Year0) %>% 
    summarise(
      N0 = weighted.mean(N0, w = N),
      across(starts_with(c("Year_", "Risk")), \(x) weighted.mean(x, w = N)),
      across(starts_with(c("Q_", "C_", "N_")), \(x) sum(x * N))
    )
  
  save(yss, file = here::here("out", "yss_5yr_zvl2rzv_" + ve_type + ".rdata"))
  
  
  stats_ys <- yss %>% 
    group_by(Scenario, Arm, Year0, Age0, Age1) %>% 
    select(-Key) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(-c(Scenario, Arm, Year0, Age0, Age1), names_to = c("Index", "name"), names_pattern = "(\\S+)_(A|M|L|U)") %>% 
    pivot_wider()
  
  stats_ys %>% filter(Index == "Q_All_d") %>% filter(Age0 == 75)
  
  write_csv(stats_ys, file = here::here("docs", "tabs", "stats_ys_5yr_zvl2rzv_" + ve_type + ".csv"))
  
  
  stats_ce <- local({
    temp <- yss %>% 
      pivot_longer(-c(Scenario, Age0, Age1, Arm, Key, Year0), names_to = "Index") %>% 
      ungroup()

    
    dy <- temp %>% 
      filter(Arm %in% c("ReVac_RZV1", "ReVac_RZV2")) %>% 
      left_join(
        temp %>% 
          filter(Arm == "Vac") %>% 
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
      ) %>% 
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
  
  write_csv(stats_ce, file = here::here("docs", "tabs", "stats_ce_5yr_zvl2rzv_" + ve_type + ".csv"))
}



