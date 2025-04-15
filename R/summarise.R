

summarise_cohort <- function(yss) {
  require(tidyverse)

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
  
  ss <- list()
  
  ss$stats_uv_ys <- yss %>% 
    group_by(Scenario, Age0, Arm) %>% 
    select(-Key) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(ends_with(c("_A", "_M", "_L", "_U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()
  
  
  
  ss$stats_uv_ce <- yss_diff %>% 
    group_by(Scenario, Age0, Arm, Year0, N0) %>% 
    select(-Key) %>% 
    summarise(
      across(everything(), amlu),
      Thres20_50_M = median(Thres20),
      Thres30_90_M = quantile(Thres30, 0.1)
    ) %>% 
    mutate(
      Thres_M = pmin(Thres20_50_M, Thres30_90_M)
    ) %>% 
    pivot_longer(ends_with(c("_A", "_M", "_L", "_U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()
  
  return(ss)
}


summarise_cohort_re <- function(yss) {
  require(tidyverse)
  
  
  ### Comparison -----
  yss_diff <- local({
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
      filter(Arm %in% c("ReRZV_1d", "ReRZV_2d")) %>% 
      left_join(
        temp %>% 
          filter(Arm == "Vac") %>% 
          filter(Scenario != "Overall") %>%
          select(Scenario, Age0, Key, Index, value0 = value),
        relationship = "many-to-many"
      )
    
    bind_rows(dy0, dy1) %>% 
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
      )
  })

  
  ss <- list()
  
  ss$stats_re_ys <- yss %>% 
    group_by(Scenario, Arm, Age0, Age1) %>% 
    select(-Key) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(ends_with(c("_A", "_M", "_L", "_U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()

  
  ss$stats_re_ce <- yss_diff %>% 
    #select(-N0) %>% 
    group_by(Scenario, Age0, Age1, Arm, Year0) %>% 
    select(-Key) %>% 
    summarise(
      N0 = mean(N0),
      across(everything(), amlu),
      Thres20_50_M = median(Thres20, na.rm = T),
      Thres30_90_M = quantile(Thres30, 0.1, na.rm = T)
    ) %>% 
    mutate(
      Thres_M = pmin(Thres20_50_M, Thres30_90_M)
    ) %>% 
    pivot_longer(ends_with(c("_A", "_M", "_L", "_U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()

  return(ss)
}


summarise_proj <- function(yss) {
  require(tidyverse)
  
  ss <- list()
  
  ss$stats_proj_all <- yss$Yss_All %>% 
    select(-Key) %>% 
    group_by(Scenario, Year, Agp) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(ends_with(c("A", "M", "L", "U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()
  
  ss$stats_proj_agp <- yss$Yss_Agp %>% 
    select(-Key) %>% 
    group_by(Scenario, Year, Agp) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(ends_with(c("A", "M", "L", "U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()
  
  ss$stats_proj_68 <- yss$Yss_68 %>% 
    select(-Key) %>% 
    group_by(Scenario, Year, Agp) %>% 
    summarise_all(amlu) %>% 
    pivot_longer(ends_with(c("A", "M", "L", "U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()
  
  # yss <- tar_read(yss_proj, 1)
  # yss_all <- yss$yss_proj_3ce36ec664bd14c0_Yss_All
  
  yss_all <- yss$Yss_All
  
  ss$diff_proj_zvl <- yss_all %>% 
    select(Scenario, Agp, Key, Year, N_HZ, N_HZ_Death, IncR_HZ, MorR_HZ) %>% 
    filter(!Scenario %in% c("Null")) %>% 
    left_join(yss_all %>% 
                select(Baseline = Scenario, Agp, Key, Year, N_HZ0 = N_HZ, N_HZ_Death0 = N_HZ_Death, IncR_HZ0 = IncR_HZ, MorR_HZ0 = MorR_HZ) %>% 
                filter(Baseline == "Stay"), by = c("Agp", "Year", "Key")) %>% 
    group_by(Scenario, Agp, Key) %>%
    arrange(Year) %>% 
    mutate(
      dN_HZ = N_HZ - N_HZ0,
      dN_HZ_Death = N_HZ_Death  - N_HZ_Death0,
      PAF_Inc = - dN_HZ / N_HZ0,
      PAF_Mor = - dN_HZ_Death / N_HZ_Death0,
      Avt_Inc = - cumsum(dN_HZ) / cumsum(N_HZ0),
      Avt_Mor = - cumsum(dN_HZ_Death) / cumsum(N_HZ_Death0)
    )  %>% 
    ungroup() %>% 
    select(-Key) %>% 
    group_by(Scenario, Baseline, Year, Agp) %>% 
    summarise(across(starts_with(c("dN", "PAF", "Avt")), amlu)) %>% 
    pivot_longer(ends_with(c("A", "M", "L", "U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()
  
  
  ss$diff_proj_ch <- yss_all %>% 
    select(Scenario, Agp, Key, Year, N_HZ, N_HZ_Death, IncR_HZ, MorR_HZ) %>% 
    filter(Scenario %in% c("Sch", "Sch1d85", "Sch1d95", "Sch2d85", "Sch2d95")) %>% 
    left_join(yss_all %>% 
                select(Baseline = Scenario, Agp, Key, Year, N_HZ0 = N_HZ, N_HZ_Death0 = N_HZ_Death, IncR_HZ0 = IncR_HZ, MorR_HZ0 = MorR_HZ) %>% 
                filter(Baseline == "Sch"), by = c("Agp", "Year", "Key")) %>% 
    group_by(Scenario, Agp, Key) %>%
    arrange(Year) %>% 
    mutate(
      dN_HZ = N_HZ - N_HZ0,
      dN_HZ_Death = N_HZ_Death  - N_HZ_Death0,
      PAF_Inc = - dN_HZ / N_HZ0,
      PAF_Mor = - dN_HZ_Death / N_HZ_Death0,
      Avt_Inc = - cumsum(dN_HZ) / cumsum(N_HZ0),
      Avt_Mor = - cumsum(dN_HZ_Death) / cumsum(N_HZ_Death0)
    )  %>% 
    ungroup() %>% 
    select(-Key) %>% 
    group_by(Scenario, Baseline, Year, Agp) %>% 
    summarise(across(starts_with(c("dN", "PAF", "Avt")), amlu)) %>% 
    pivot_longer(ends_with(c("A", "M", "L", "U")), names_pattern = "(\\S+)_(A|M|L|U)", names_to = c("Index", "name")) %>% 
    pivot_wider() %>% 
    ungroup()

  return(ss)
}


save_tabs <- function(ss, prefix = "", folder = NA) {
  require(tidyverse)
  
  if (!is.na(folder)) {
    root <- here::here("docs", "tabs", folder)
    dir.create(root, showWarnings = F)
  } else {
    root <- here::here("docs", "tabs")
  }
  
  if (prefix != "") {
    prefix <- paste0(prefix, "_")
  }
  
  for (key in names(ss)) {
     write_csv(ss[[key]], here::here(root, paste0(prefix, key, ".csv")))
  }
}
