
sim_cohort_vac <- function(pars, age0 = 70, age1 = NA, vaccine0 = "Shingrix", vaccine1 = NA, agg = T, coverage = 0.483) {
  fn <- function(df) {
    df %>% mutate(
      Protection = ifelse(is.na(Protection), 0, Protection),
      r_hz = r_hz * (1 - Protection),
      r_mor_hz = r_mor_hz * (1 - Protection),
      
      r_mor = r_mor_hz + r_mor_bg,
      p_mor_hz = r_mor_hz * (1 - exp(- r_mor)) / r_mor,
      p_survival = c(1, cumprod(1 - r_mor)[-n()]),
      
      N_Alive = p_survival * cohort_size,
      N_Start = c(cohort_size, N_Alive[-length(N_Alive)]),
      N_NewUptake = N_Start * NewUptake,
      
      N_HZ = r_hz * N_Alive,
      N_HZ_GP = p_gp * N_HZ,
      N_HZ_Hosp = N_HZ - N_HZ_GP,
      N_HZ_PHN = p_phn * N_HZ,
      N_HZ_PHN_GP = p_gp * N_HZ_PHN,
      N_HZ_Death = p_mor_hz * N_HZ,
    )
  }
  
  
  vaccination <- function(df, pars, age, vac) {
    df0 <- df %>% filter(Age < age)
    df1 <- df %>% filter(Age >= age) %>% 
      mutate(
        Vaccine = vac,
        AgeVac = age,
        NewUptake = ifelse(Age == age, 1, 0)
      )
    
    if ("Protection" %in% names(df1)) {
      df1 <- df1 %>% select(-Protection)
    }
    
    if (vac == "Shingrix") {
      pv <- pars$VE_RZV %>% mutate(Age = age + TimeVac - 1) %>% select(Age, Protection)
      df1_ve <- df1 %>% left_join(pv, by = "Age")
    } else if (vac == "Shingrix_Single") {
      pv <- pars$VE_Re1RZV %>% mutate(Age = age + TimeVac - 1) %>% select(Age, Protection)
      df1_ve <- df1 %>% left_join(pv, by = "Age")
    } else {
      pv <- pars$VE_ZVL %>% mutate(AgeVac = Age - TimeVac + 1) %>% 
        filter(AgeVac == age) %>% 
        select(Age, Protection)
      df1_ve <- df1 %>% left_join(pv, by = "Age")
    }
    bind_rows(df0, df1_ve)
  }
  
  ys <- list()
  
  cohort_size <- pars$Demography %>% filter(Age == age0) %>% pull(N) * coverage
  
  year0 <- pars$Year0
  
  pop0 <- tibble(Age = age0:100) %>% 
    mutate(
      Year = year0 - age0 + Age,
      N = cohort_size
    ) %>% 
    left_join(pars$Demography %>% select(Age, r_mor_bg = r_death), by = "Age") %>% 
    left_join(pars$Epidemiology, by = "Age")
  
  
  pop_soc <- pop0 %>% mutate(
    Vaccine = "None",
    AgeVac = NA, 
    NewUptake = 0,
    Protection = 0
  ) %>% 
    fn() %>% 
    mutate(Arm = "SOC")


  pop_vac <- pop0 %>%
    vaccination(pars, age0, vaccine0) %>% 
    fn() %>% 
    mutate(Arm = "Vac")
  
  if (is.na(vaccine1)) {
    res <- bind_rows(pop_soc, pop_vac) %>% select(-starts_with(c("p_", "r_")))
  } else {
    pop_revac <- pop0 %>%
      vaccination(pars, age0, vaccine0) %>% 
      vaccination(pars, age1, vaccine1) %>% 
      fn() %>% 
      mutate(Arm = "ReVac")
    
    res <- bind_rows(pop_soc, pop_vac, pop_revac) %>% select(-starts_with(c("p_", "r_")))
  }
  
  ## Append cost effectiveness information
  pars_cv <- tibble(Age = c(age0, age1), Vaccine = c(vaccine0, vaccine1)) %>% 
    left_join(pars$CostVac, by = "Vaccine") %>% 
    select(Age, cost_vac_pp) %>% 
    filter(!is.na(Age))
  
  res_ce <- res %>% 
    left_join(pars_cv, by = "Age") %>% 
    left_join(pars$CostEff, by = "Age") %>% 
    mutate(
      cost_vac_pp = ifelse(is.na(cost_vac_pp), 0, cost_vac_pp),
      dis_ql = 1 / ((1 + pars$discount_effects) ^ (Year - year0)),
      dis_cost = 1 / ((1 + pars$discount_costs) ^ (Year - year0)),
      QL_y2_d = QL_y2 / (1 + pars$discount_effects),
      QL_HZ = QL_y1 + QL_y2,
      QL_HZ_d = QL_y1 + QL_y2_d,
      Q_Life = N_Start * QOL,
      Q_HZ = - N_HZ * QL_HZ,
      Q_All = Q_Life + Q_HZ,
      Q_Life_d = Q_Life * dis_ql,
      Q_HZ_d = - N_HZ * QL_HZ_d * dis_ql,
      Q_All_d = Q_Life_d + Q_HZ_d,
      C_Hosp = N_HZ_Hosp * cost_hosp_pp_inf,
      C_GP_NonPHN = (N_HZ_GP - N_HZ_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf,
      C_GP_PHN = N_HZ_PHN_GP * cost_GP_pp_PHN_inf,
      C_GP = C_GP_NonPHN + C_GP_PHN,
      C_Vac = cost_vac_pp * N_NewUptake,
      C_Vac_d = C_Vac * dis_ql,
      C_All = C_Hosp + C_GP + C_Vac,
      C_Hosp_d = C_Hosp * dis_cost,
      C_GP_NonPHN_d = C_GP_NonPHN * dis_cost,
      C_GP_PHN_d = C_GP_PHN * dis_cost,
      C_GP_d = C_GP * dis_cost,
      C_Med_d = C_Hosp_d + C_GP_d,
      C_All_d = C_Med_d + C_Vac_d,
      across(starts_with(c("C_", "Q_")), function(x) ifelse(is.na(x), 0, x))
    )
  
  
  calc_cea <- function(df, sc0 = "SOC", wtp = 3e5) {
    df %>% 
      group_by(Arm) %>% 
      summarise(across(starts_with(c("N_", "C_", "Q_")), sum)) %>% 
      select(Arm, N_HZ, N_NewUptake, Q_Life_d, Q_HZ_d, Q_All_d, C_Med_d, C_Vac_d, C_All_d) %>% 
      mutate(
        N_Uptake0 = N_NewUptake[Arm == sc0],
        N_HZ0 = N_HZ[Arm == sc0],
        Q_Life_d0 = Q_Life_d[Arm == sc0],
        Q_HZ_d0 = Q_HZ_d[Arm == sc0],
        Q_All_d0 = Q_All_d[Arm == sc0],
        C_Med_d0 = C_Med_d[Arm == sc0],
        C_Vac_d0 = C_Vac_d[Arm == sc0],
        C_All_d0 = C_All_d[Arm == sc0],
        dN_Uptake = N_NewUptake - N_Uptake0,
        dN_HZ = N_HZ - N_HZ0,
        dQ_Life_d = Q_Life_d0 - Q_Life_d0,
        dQ_HZ_d = Q_HZ_d - Q_HZ_d0,
        dQ_All_d = Q_All_d - Q_All_d0,
        dC_Med_d = C_Med_d - C_Med_d0,
        dC_Vac_d = C_Vac_d - C_Vac_d0,
        dC_All_d = C_All_d - C_All_d0,
        ICER = dC_All_d / dQ_All_d, ICER = ifelse(is.na(ICER), 0, ICER),
        NMB = dQ_All_d * wtp - dC_All_d
      )
  }
  
  
  if (agg) {
    res_ce_all <- res_ce %>% 
      filter(Age < 100) %>% 
      calc_cea() %>% 
      mutate(Type = "Overall")
    
    if (!is.na(vaccine1)) {
      res_ce <- bind_rows(
        res_ce_all, 
        res_ce %>% 
          filter(Age < 100) %>% 
          filter(Arm != "SOC") %>% 
          filter(Age >= age1) %>% 
          calc_cea("Vac") %>%
          mutate(Type = "Second_d"),
        res_ce %>% 
          filter(Age < 100) %>% 
          filter(Arm != "SOC") %>% 
          filter(Age >= age1) %>% 
          mutate(
            across(starts_with("C_"), function(x) x / dis_cost[1]),
            across(starts_with("Q_"), function(x) x / dis_ql[1])
          ) %>% 
          calc_cea("Vac") %>%
          mutate(Type = "Second")
        )
    } else (
      res_ce <- res_ce_all
    )
  }

  return(res_ce)
}




  
