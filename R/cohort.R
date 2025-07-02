

m_cohort <- list()

m_cohort$populate <- function(keys, pars, age0) {
  year0 <- pars$Year0
  
  pop0 <- crossing(Key = keys, Age = age0:100) %>% 
    mutate(
      Year = year0 - age0 + Age,
      N = 1,
      Vaccine = "None",
      AgeVac = NA,
      Protection = 0,
      n_uptake = 0,
    ) %>% 
    left_join(pars$Demography %>% select(Age, r_mor_bg = r_death, norm), by = "Age") %>% 
    left_join(pars$Epidemiology, by = c("Key", "Age"))
  
  return(pop0)
}


m_cohort$vaccinate <- function(df, vac_at, vac, pars) {
  ve <- pars[[glue::as_glue("VE_") + vac]]
  
  df0 <- df %>% filter(Age < vac_at)
  df1 <- df %>% filter(Age >= vac_at) %>% 
    mutate(
      Vaccine = vac,
      AgeVac = vac_at,
      n_uptake = ifelse(Age == vac_at, 1, 0)
    )
  
  if (!("Age" %in% names(ve))) {
    df1_ve <- df1 %>% 
      left_join(ve %>% 
                  mutate(Age = vac_at + TimeVac - 1) %>% 
                  select(Key, Age, Protection1 = Protection), by = c("Key", "Age"))
  } else {
    df1_ve <- df1 %>% 
      left_join(ve %>% 
                  mutate(AgeVac = Age - TimeVac + 1) %>% 
                  filter(AgeVac == vac_at) %>% 
                  select(Key, Age, Protection1 = Protection), by = c("Key", "Age"))
  }
  
  bind_rows(df0, df1_ve %>% 
              mutate(Protection = pmax(Protection, Protection1)) %>% 
              select(-Protection1))
}


m_cohort$run_to_end <- function(df, pars) {
  cohort_size <- df$N[1]
  
  df %>% 
    left_join(pars$CostVac, by = "Vaccine") %>% 
    mutate(
      Protection = ifelse(is.na(Protection), 0, Protection),
      r_hz = r_hz * (1 - Protection),
      r_mor_hz = r_mor_hz * (1 - Protection),
      
      r_mor = r_mor_hz + r_mor_bg,
      p_mor_hz = r_mor_hz * (1 - exp(- r_mor)) / r_mor,
      p_survival = c(1, cumprod(1 - r_mor)[-n()]),
      
      N_Alive = p_survival * cohort_size,
      N_Start = c(cohort_size, N_Alive[-length(N_Alive)]),
      N_NewUptake = N_Start * n_uptake,
      N_VacZVL = N_NewUptake * number_courses * (Vaccine == "ZVL"),
      N_VacRZV = N_NewUptake * number_courses * (Vaccine != "ZVL"),
      N_HZ = r_hz * N_Alive,
      N_HZ_GP = p_gp * N_HZ,
      N_HZ_Hosp = N_HZ - N_HZ_GP,
      N_HZ_PHN = p_phn * N_HZ,
      N_HZ_PHN_GP = p_gp * N_HZ_PHN,
      N_HZ_Death = r_mor_hz * N_Alive,
      QALY = norm * (N_Alive + N_Start) / 2
    ) %>% 
    select(-starts_with(c("r_", "p_", "n_"), ignore.case = F), - N)
}


m_cohort$append_ce <- function(df, pars) {
  df %>% 
    left_join(pars$CostEff, by = c("Key", "Age")) %>% 
    mutate(
      cost_vac_pp = ifelse(is.na(cost_vac_pp), 0, cost_vac_pp),
      Q_Life = QALY,
      Q_HZ = - N_HZ * QL_ph,
      Q_HZ_Norm = - N_HZ * QL_pn,
      Q_All = Q_Life + Q_HZ,
      C_Hosp = N_HZ_Hosp * cost_hosp_pp_inf,
      C_GP_NonPHN = (N_HZ_GP - N_HZ_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf,
      C_GP_PHN = N_HZ_PHN_GP * cost_GP_pp_PHN_inf,
      C_GP = C_GP_NonPHN + C_GP_PHN,
      C_Med = C_GP + C_Hosp,
      C_VacZVL = N_VacZVL * (cost_vac_per_dose + cost_admin_per_dose),
      C_VacRZV = N_VacRZV * (cost_vac_per_dose + cost_admin_per_dose),
      across(starts_with("C_Vac"), \(x) ifelse(is.na(x), 0, x)),
      C_Vac = C_VacZVL + C_VacRZV,
      C_All = C_Med + C_Vac,
      across(starts_with(c("C_", "Q_", "N_")), \(x) ifelse(is.na(x), 0, x))
    )
}


m_cohort$summarise_ce <- function(df, pars) {
  dis_cost <- pars$dis_c
  dis_eff <- pars$dis_e
  year0 <- pars$Year0
  
  df %>% 
    mutate(
      dis_e = 1 / ((1 + dis_eff) ^ (Year - Year[1])),
      dis_c = 1 / ((1 + dis_cost) ^ (Year - Year[1])),
      #QL_y2_d = QL_y2 / (1 + dis_eff),
      #QL_HZ_d = (QL_y1 + QL_y2_d) * dis_e,
      Q_HZ_d = Q_HZ * dis_e, 
      Q_HZ_Norm_d = Q_HZ_Norm * dis_e, 
      Q_Life_d = Q_Life * dis_e,
      Q_All_d = Q_Life_d + Q_HZ_d,
      N_VacZVL_d = N_VacZVL * dis_c,
      N_VacRZV_d = N_VacRZV * dis_c,
      across(starts_with("C_"), \(x) x * dis_c, .names = "{.col}_d")
    ) %>% 
    summarise(
      N0 = N_Start[1],
      Year0 = year0,
      Year_Life = sum(N_Alive),
      Year_Immunised = sum(N_Alive * Protection),
      Risk_HZ = sum(N_HZ) / N_Alive[1],
      Risk_Hosp = sum(N_HZ_Hosp) / N_Alive[1],
      Risk_PHN = sum(N_HZ_PHN) / N_Alive[1],
      Risk_Death = sum(N_HZ_Death) / N_Alive[1],
      across(starts_with(c("Q_", "C_", "N_Vac")), \(x) sum(x, na.rm = T))
    )
}


# single run for first-time vaccination
a_run_uv <- function(keys, pars, age0) {
  with(m_cohort, {
    df <- populate(keys, pars, age0)
    
    df <- bind_rows(
      df %>% mutate(Arm = "SOC"),
      df %>% 
        vaccinate(age0, "RZV_1d", pars) %>% 
        mutate(Arm = "RZV_1d"),
      df %>% 
        vaccinate(age0, "RZV_2d", pars) %>% 
        mutate(Arm = "RZV_2d"),
    )
    
    df <- df %>% 
      group_by(Key, Arm) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      summarise_ce(pars)  %>% 
      mutate(
        Scenario = glue::as_glue("Vac_") + as.character(age0), 
        Age0 = age0
      )
  })
}


# single run for revaccination
a_run_re <- function(keys, pars, age0, age1s) {
  with(m_cohort, {
    df <- populate(keys, pars, age0) %>% mutate(Arm = "SOC", Age1 = NA)
    
    df <- bind_rows(
      df,
      df %>% vaccinate(age0, "ZVL", pars) %>% mutate(Arm = "Vac"),
      age1s %>% lapply(\(age1) {
        df %>% 
          vaccinate(age0, "ZVL", pars) %>% 
          vaccinate(age1, "ReRZV_1d", pars) %>% 
          mutate(Arm = "ReRZV_1d", Age1 = age1)
      }) %>% bind_rows(),
      age1s %>% lapply(\(age1) {
        df %>% 
          vaccinate(age0, "ZVL", pars) %>% 
          vaccinate(age1, "ReRZV_2d", pars) %>% 
          mutate(Arm = "ReRZV_2d", Age1 = age1)
      }) %>% bind_rows(),
    ) %>% 
      group_by(Key, Arm, Age1) %>% 
      run_to_end(pars) %>% 
      append_ce(pars) %>% 
      mutate(Age0 = age0)
    
    bind_rows(
      df %>% mutate(Scenario = "Overall"),
      age1s %>% lapply(\(age1){
        df %>% 
          filter(Arm %in% c("SOC", "Vac") | Age1 == age1) %>% 
          filter(Age >= age1) %>% 
          mutate(Scenario = "Second_" + glue::as_glue(age1))
      })
    ) %>% 
      group_by(Scenario, Arm, Key, Age0, Age1) %>% 
      summarise_ce(pars) %>% 
      ungroup()
  })
}


exec_cohort_rzv <- function(pars, age0s) {
  keys <- 1:pars$N_Sims
  
  pb <- txtProgressBar(min = min(age0s), max = max(age0s), style = 3,  width = 50, char = "=") 
  
  yss <- age0s %>% 
    lapply(\(age0) {
      setTxtProgressBar(pb, age0)
      keys %>% a_run_uv(pars, age0 = age0)
    }) %>% 
    bind_rows() %>% 
    relocate(Key, Scenario, Age0, Arm)
 
  yss 
}


exec_cohort_rerzv <- function(pars, age0s, age1s) {
  keys <- 1:pars$N_Sims

  pb <- txtProgressBar(min = min(age0s), max = max(age0s), style = 3,  width = 50, char = "=") 
  
  yss <- age0s %>% 
    lapply(\(age0) {
      setTxtProgressBar(pb, age0)
      keys %>% a_run_re(pars, age0 = age0, age1s = age1s)
    }) %>% 
    bind_rows() %>% 
    relocate(Key, Scenario, Age0, Age1, Arm)

  yss
}







