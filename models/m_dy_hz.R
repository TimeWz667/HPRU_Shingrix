

sim_death_im <- function(df, p0) {
  df %>%
    left_join(p0, by = c("Year", "Age")) %>%
    mutate(
      Death = r_death * N,
      Im = r_immigration * N,
    ) %>% 
    select(-c(r_death, r_immigration))
}


sim_death <- function(df, p0) {
  df %>%
    left_join(p0, by = c("Year", "Age")) %>%
    mutate(
      Death = r_death * N,
    ) %>% 
    select(-c(r_death, r_immigration))
}


sim_bir_ageing <- function(df, p0, yr, hz = F) {
  # Ageing
  sim_1 <- df %>% 
    mutate(
      N = N - Death + Im - ifelse(hz, N_HZ_Death, 0),
      Year = yr + 1, 
      Age = Age + 1
    ) %>% 
    select(Year, Age, N, Vaccine, AgeVac, Protection) %>% 
    filter(Age <= 100)
  
  # Birth
  bind_rows(
    tibble(Year = yr + 1, Age = 0, 
           N = p0 %>% filter(Year == yr + 1) %>% pull(N_Birth)),
    sim_1
  )
}


sim_hz <- function(df, p0) {
  df %>% 
    left_join(p0, by = c("Age")) %>% 
    fill(r_hz, p_gp, p_phn, r_mor_hz, .direction = "up") %>% 
    mutate(
      N_HZ = r_hz * (1 - Protection) * N,
      N_HZ_GP = p_gp * N_HZ,
      N_HZ_Hosp = N_HZ - N_HZ_GP,
      N_HZ_PHN = p_phn * N_HZ,
      N_HZ_PHN_GP = p_gp * N_HZ_PHN,
      N_HZ_Death = r_mor_hz * (1 - Protection) * N_HZ
    ) %>% 
    select(-starts_with(c("r_", "p_")))
}


sim_uptake_zvl <- function(df) {
  eligible <- df %>% filter(uptake > 0)
  others <- df %>% filter(uptake <= 0)
  
  eligible %>% 
    mutate(
      N_None = N * (1- uptake),
      N_ZVL = N * uptake
    ) %>% 
    select(-c(Vaccine, uptake, N)) %>% 
    pivot_longer(starts_with("N_"), names_pattern = "N_(\\w+)", names_to = "Vaccine", values_to = "N") %>% 
    mutate(
      AgeVac  =  ifelse(Vaccine == "None", NA, Age),
      NewUptake = ifelse(Vaccine == "None", 0, N)
    ) %>% 
    bind_rows(others) %>% 
    arrange(Age, Vaccine)
}


sim_uptake_rzv <- function(df) {
  eligible <- df %>% filter(uptake > 0)
  others <- df %>% filter(uptake <= 0)
  
  eligible %>% 
    mutate(
      N_None = N * (1- uptake),
      N_RZV = N * uptake
    ) %>% 
    select(-c(Vaccine, uptake, N)) %>% 
    pivot_longer(starts_with("N_"), names_pattern = "N_(\\w+)", names_to = "Vaccine", values_to = "N") %>% 
    mutate(
      AgeVac  =  ifelse(Vaccine == "None", NA, Age),
      NewUptake = ifelse(Vaccine == "None", 0, N)
    ) %>% 
    bind_rows(others) %>% 
    arrange(Age, Vaccine)
}


sim_ve <- function(df, ve = NULL) {
  if (!is.null(ve)) {
    df <- df %>% select(-Protection) %>% 
      left_join(ve, by = c("Age", "AgeVac", "Vaccine")) %>% 
      mutate(Protection = ifelse(is.na(Protection), 0, Protection))
  }
  df
}


find_eligible_default <- function(df, p0, yr, cap = 80) {
  if (yr < 2023) {
    df <- df %>% mutate(uptake = case_when(
      Vaccine != "None" ~ 0,
      Age < 70 ~ 0,
      Age == 70 ~ p0$p_initial,
      Age <= 80 ~ p0$p_catchup,
      T ~ 0
    ))
  } else if (yr < 2028) {
    df <- df %>% mutate(uptake = case_when(
      Vaccine != "None" ~ 0,
      Age < 65 ~ 0,
      Age == 65 ~ p0$p_initial,
      Age == 70 ~ p0$p_initial,
      Age <= cap ~ p0$p_catchup,
      T ~ 0
    ))
  } else if (yr < 2033) {
    df <- df %>% mutate(uptake = case_when(
      Vaccine != "None" ~ 0,
      Age < 60 ~ 0,
      Age == 60 ~ p0$p_initial,
      Age == 65 ~ p0$p_initial,
      Age <= cap ~ p0$p_catchup,
      T ~ 0
    ))
  } else {
    df <- df %>% mutate(uptake = case_when(
      Vaccine != "None" ~ 0,
      Age < 60 ~ 0,
      Age == 60 ~ p0$p_initial,
      Age <= cap ~ p0$p_catchup,
      T ~ 0
    ))
  }
  return(df)
}


sim_dy_hz_vac <- function(pars, year0 = 2013, year1 = 2040, rule_eligible = find_eligible_default) {
  ys <- list()
  
  ve_zvl <- crossing(Age = 51:100, AgeVac = 51:100) %>% 
    mutate(TimeVac = Age - AgeVac + 1) %>% 
    filter(TimeVac > 0) %>% 
    left_join(pars$VE_ZVL, by = c("Age", "TimeVac"))

  ve_rzv <- crossing(Age = 51:100, AgeVac = 51:100) %>% 
    mutate(TimeVac = Age - AgeVac + 1) %>% 
    filter(TimeVac > 0) %>% 
    left_join(pars$VE_RZV, by = "TimeVac")
  
  pars_ve <- bind_rows(ve_zvl, ve_rzv) %>% select(-TimeVac)
  
  sim_0 <- pars$Demography$N %>% filter(Year == year0) %>% 
    mutate(Vaccine = "None", Protection = 0, AgeVac  = NA)
  
  for (yr in year0:year1) {
    if (yr < 2023) {
      fn_vac <- sim_uptake_zvl
    } else {
      fn_vac <- sim_uptake_rzv
    }

    sim_t <- sim_0 %>% 
      rule_eligible(pars$Uptake, yr = yr) %>% 
      fn_vac() %>%
      sim_ve(pars_ve) %>% 
      sim_hz(pars$Epidemiology) %>% 
      sim_death_im(pars$Demography$DeathIm)
    
    sim_0 <- sim_bir_ageing(sim_t, pars$Demography$Birth, yr, hz = (yr < 2023))
    
    ys[[length(ys) + 1]] <- sim_t
  }
  ys <- bind_rows(ys) %>% 
    mutate(
      NewUptake = ifelse(is.na(NewUptake), 0, NewUptake),
      Year = Year + 1  
    )
  
  ## Append CE information
  ys <- ys %>% 
    left_join(pars$CostVac %>% mutate(Vaccine = ifelse(Vaccine == "Shingrix", "RZV", "ZVL")), by = "Vaccine") %>% 
    left_join(pars$CostEff, by = "Age") %>% 
    mutate(
      cost_vac_pp = ifelse(is.na(cost_vac_pp), 0, cost_vac_pp),
      dis_ql = 1 / ((1 + pars$discount_effects) ^ (Year - year0)),
      dis_cost = 1 / ((1 + pars$discount_costs) ^ (Year - year0)),
      QL_y2_d = QL_y2 / (1 + pars$discount_effects),
      QL_HZ = QL_y1 + QL_y2,
      QL_HZ_d = QL_y1 + QL_y2_d,
      Q_Life = N * QOL,
      Q_HZ = - N_HZ * QL_HZ,
      Q_All = Q_Life + Q_HZ,
      Q_Life_d = Q_Life * dis_ql,
      Q_HZ_d = - N_HZ * QL_HZ_d * dis_ql,
      Q_All_d = Q_Life_d + Q_HZ_d,
      C_Hosp = N_HZ_Hosp * cost_hosp_pp_inf,
      C_GP_NonPHN = (N_HZ_GP - N_HZ_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf,
      C_GP_PHN = N_HZ_PHN_GP * cost_GP_pp_PHN_inf,
      C_GP = C_GP_NonPHN + C_GP_PHN,
      C_Vac = cost_vac_pp * NewUptake,
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
  
  
  return(ys)
}


summarise_dy_hz <- function(yss, soc = "SOC", year0 = 2023) {
  yss_collapse <- yss %>% 
    group_by(Year, Age, Scenario, Key) %>% 
    summarise(
      Coverage_RZV = sum(N[Vaccine == "RZV"]) / sum(N),
      Coverage_ZVL = sum(N[Vaccine == "ZVL"]) / sum(N),
      across(c(N, starts_with(c("N_", "C_", "Q_")), NewUptake), sum)
    ) %>% 
    ungroup()
  
  stats <- yss_collapse %>% 
    filter(Age >= age0) %>% 
    group_by(Year, Scenario, Key) %>% 
    summarise(
      across(c(Coverage_RZV, Coverage_ZVL), function(x) weighted.mean(x, w = N)),
      across(-c(Coverage_RZV, Coverage_ZVL), sum),
    ) %>% 
    group_by(Year, Scenario) %>% 
    select(-Key) %>% 
    summarise_all(mean) %>% 
    ungroup()
  
  avt <- stats %>% 
    group_by(Year, Scenario) %>% 
    summarise(
      Cases = sum(HZ),
      Q_All_d = sum(Q_All_d),
      C_All_d = sum(C_All_d)
    ) %>% 
    arrange(Year) %>%
    group_by(Scenario) %>% 
    mutate(
      CumCases = cumsum(Cases),
      CumQ = cumsum(Q_All_d),
      CumC = cumsum(C_All_d),
    ) %>% 
    ungroup()
  
  avt <- avt %>% 
    left_join(avt %>% filter(Scenario == soc) %>% 
                select(Year, CumCases0 = CumCases, CumQ0 = CumQ, CumC0 = CumC), by = "Year")
  
  
  list(
    Yss = yss %>% filter(Age >= age0),
    Stats = stats,
    Avt = avt
  ) 
}
