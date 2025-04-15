

sim_profile <- function(pars_proj) {
  pop <- pars_proj$Demography$N
  uptake <- pars_proj$Uptake
  
  
  sim_cov <- local({
    df <- tibble(
      Year = 2013, Age = 50:100, N = 1
    ) %>% 
      mutate(
        p0 = case_when(
          Age < 70 ~ 0,
          Age == 70 ~ uptake$p_initial,
          Age < 80 ~ uptake$p_catchup,
          T ~ 0
        ),
        N_Vac = N * p0,
        N = N - N_Vac
      )
    
    
    vac <- df %>% select(YearVac = Year, Age, N = N_Vac) %>% 
      filter(N > 0) %>% 
      mutate(
        Year = 2023,
        TimeVac = Year - YearVac,
        Age = TimeVac + Age
      )
    
    
    while (TRUE) {
      df <- df %>% 
        mutate(
          Year = Year + 1,
          Age = Age + 1,
          p0 = case_when(
            Age < 70 ~ 0,
            Age == 70 ~ uptake$p_initial,
            Age < 80 ~ uptake$p_catchup,
            T ~ 0
          ),
          N_Vac = N * p0,
          N = N - N_Vac
        )
      
      
      vac <- vac %>% 
        bind_rows(
          df %>% 
            select(YearVac = Year, Age, N = N_Vac) %>% 
            filter(N > 0) %>% 
            mutate(
              Year = 2023,
              TimeVac = Year - YearVac,
              Age = TimeVac + Age
            )
        )
      
      
      if (df$Year[1] >= 2023) break
    }
    
    
    bind_rows(df %>% select(Year, Age, N), vac) %>% 
      filter(Age <= 100)
  })
  
  ve_zvl <- pars_proj$VE_ZVL %>% 
    group_by(Age, TimeVac) %>% 
    summarise(Protection = mean(Protection)) %>% 
    ungroup() %>% 
    mutate(TimeVac = TimeVac - 1)
  
  
  
  pop %>% 
    filter(Year == 2023) %>% 
    filter(Age >= 60) %>% 
    select(Age, Pop = N) %>% 
    left_join(sim_cov %>% rename(Pr = N), by = "Age") %>% 
    mutate(N = Pr * Pop) %>% 
    select(- Pop) %>% 
    left_join(ve_zvl, by = c("Age", "TimeVac")) %>% 
    mutate(
      Protection = ifelse(is.na(Protection), 0, Protection),
      Agp = cut(Age, seq(60, 105, 5), right = F),
      TimeVac = ifelse(is.na(TimeVac), -1, TimeVac)
    ) %>% 
    filter(Age < 100)
  
}


make_profile <- function(df, n_all) {
  df %>% 
    group_by(Eligibility, Agp) %>% 
    summarise(
      across(starts_with("d"), \(x) sum(x * N_Uptake)),
      N_Doses = sum(N_Uptake * ifelse(endsWith(Arm, "1d"), 1, 2)),
      N = sum(N),
      N_Uptake = sum(N_Uptake),
      Arm = paste(unique(Arm), collapse = "")
    ) %>% 
    ungroup() %>% 
    mutate(
      across(c(N, N_Uptake, N_Doses), \(x) cumsum(x), .names = "cum_{.col}"),
      across(starts_with("d"), \(x) cumsum(x), .names = "cum_{.col}"),
      Thres = (cum_dQ_All_d * 2e4 - cum_dC_Med_d) / cum_dN_VacRZV_d,
      cum_dC_All_d = cum_dC_Med_d + cum_dN_VacRZV_d * 60,
      dC_All_d = dC_Med_d + dN_VacRZV_d * 60,
      CumICER = cum_dC_All_d / cum_dQ_All_d,
      Prop_Uptake = cum_N_Uptake / cum_N_Uptake[3],
      Prop_Doses = cum_N_Doses / cum_N_Doses[3],
      Prop_Case = cum_dRisk_HZ / cum_dRisk_HZ[3],
      Prop_dQ = cum_dQ_All_d / cum_dQ_All_d[3],
      Prop_dC_Med = cum_dC_Med_d / cum_dC_Med_d[3],
      ICER = dC_All_d / dQ_All_d
    ) %>% 
    left_join(n_all) %>% 
    select(Eligibility, Agp, Arm, N_All, N, N_Uptake, N_Doses, 
           dRisk_HZ, dC_Med_d, dQ_All_d, Thres, starts_with("Prop"))
  
}


exec_programme <- function(pars_proj, profile, stats_uv, stats_re) {
  require(tidyverse)
  
  stats_ce <- stats_uv$stats_uv_ce
  stats_cer <- stats_re$stats_re_ce
  
  ss <- list()
  
  ss$baseline_profile <- profile %>% 
    group_by(Agp) %>% 
    summarise(
      N_All = sum(N, na.rm = T),
      N_ZVL = sum(N * (TimeVac > 0), na.rm = T),
      N_ZVL2 = sum(N * (TimeVac >= 2), na.rm = T),
      N_ZVL5 = sum(N * (TimeVac >= 5), na.rm = T),
      N_Protected = sum(Protection * N, na.rm = T),
      P_ZVL = N_ZVL / N_All,
      P_ZVL2 = N_ZVL2 / N_All,
      P_ZVL5 = N_ZVL5 / N_All,
      P_Protected = N_Protected / N_All
    )
  
  sel_cols <- c("dRisk_HZ", "dQ_All_d", "dC_All_d", "dC_Med_d", "dC_VacRZV_d", "dN_VacRZV_d")
  
  
  n_all <- profile %>% 
    group_by(Agp) %>% 
    summarise(N_All = sum(N))
  
  
  vaccine <- bind_rows(
    stats_ce %>% 
      filter(Index %in% sel_cols) %>% 
      select(Age = Age0, Arm, N0, name = Index, value = M) %>% 
      pivot_wider() %>% 
      mutate(TimeVac = -1),
    stats_cer %>% 
      filter(Scenario != "Overall") %>% 
      mutate(TimeVac = Age1 - Age0) %>% 
      filter(Index %in% sel_cols) %>% 
      select(Age = Age1, Arm, TimeVac, name = Index, value = M) %>% 
      pivot_wider()
  ) %>% 
    mutate(
      nd = ifelse(endsWith(Arm, "1d"), 1, 2),
      Price = dC_VacRZV_d / dN_VacRZV_d,
      fac = nd / dN_VacRZV_d,
      across(starts_with("d"), \(x) x * fac)
    ) %>% 
    select(-N0, -nd, -Price, fac)
  
  
  ce0 <- profile %>% 
    mutate(
      #TimeVac = ifelse(Age >= 85, -1, TimeVac),
      Eligibility = case_when(
        Age < 65 & TimeVac == -1 ~ "",
        Age == 65 & TimeVac == -1 ~ "New2023",
        Age < 70 & TimeVac == -1 ~ "",
        Age < 80 & TimeVac == -1 ~ "SOC",
        Age < 80 ~ "",
        Age < 85 & TimeVac == -1 ~ "UV_85",
        Age < 85 ~ "ZVL_85",
        Age < 95 & TimeVac == -1 ~ "UV_95",
        Age < 95 ~ "ZVL_90",
        T ~ "UV_100"
      ),
      Arm = case_when(
        Eligibility %in% c("New2023", "SOC") ~ "RZV_2d",
        Eligibility %in% c("ZVL_85", "ZVL_90") ~ "ReRZV_1d",
        Eligibility %in% c("UV_85", "UV_95", "UV_100") ~ "RZV_1d",
        T ~ NA
      ) 
    ) %>% 
    filter(!is.na(Arm)) %>% 
    mutate(
      Eligibility = factor(Eligibility, c("New2023", "SOC", "ZVL_85", "UV_85", "ZVL_90", "UV_95", "UV_100"))
    ) %>% 
    arrange(Eligibility)
  
  
  
  uptake <- pars_proj$Uptake
  
  ss$prog_cont_1d <- ce0 %>% 
    mutate(
      N_Uptake = N * ifelse(Age %in% c(65, 70), uptake$p_initial, uptake$p_catchup)
    ) %>% 
    left_join(vaccine) %>% 
    make_profile(n_all=n_all)
  
  
  ss$prog_cont_2d <- ce0 %>% 
    mutate(
      N_Uptake = N * ifelse(Age %in% c(65, 70), uptake$p_initial, uptake$p_catchup),
      Arm = case_when(
        Arm == "RZV_1d" ~ "RZV_2d",
        Arm == "ReRZV_1d" ~ "ReRZV_2d",
        T ~ Arm
      ) 
    ) %>% 
    left_join(vaccine) %>% 
    make_profile(n_all=n_all)
  
  
  ss$prog_boost_1d <- ce0 %>% 
    mutate(
      N_Uptake = N * ifelse(Age %in% c(65, 70, 80), uptake$p_initial, uptake$p_catchup)
    ) %>% 
    left_join(vaccine) %>% 
    make_profile(n_all=n_all)
  
  
  ss$prog_boost_2d <- ce0 %>% 
    mutate(
      N_Uptake = N * ifelse(Age %in% c(65, 70, 80), uptake$p_initial, uptake$p_catchup),
      Arm = case_when(
        Arm == "RZV_1d" ~ "RZV_2d",
        Arm == "ReRZV_1d" ~ "ReRZV_2d",
        T ~ Arm
      ) 
    ) %>% 
    left_join(vaccine) %>% 
    make_profile(n_all=n_all)
  
  ss$prog_all <- bind_rows(
    ss$prog_cont_1d %>% mutate(Scenario = "Cont_1d"),
    ss$prog_cont_2d %>% mutate(Scenario = "Cont_2d"),
    ss$prog_boost_1d %>% mutate(Scenario = "Boost_1d"),
    ss$prog_boost_2d %>% mutate(Scenario = "Boost_2d")
  )
 
  return(ss) 
}


