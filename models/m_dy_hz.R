

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
      N = N - Death + Im - ifelse(hz, HZ_Death, 0),
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
    fill(r_hz, p_gp, p_phn, p_mor_hz, .direction = "up") %>% 
    mutate(
      HZ = (1 - Protection) * r_hz * N,
      HZ_GP = p_gp * HZ,
      HZ_Hosp = HZ - HZ_GP,
      HZ_PHN = p_phn * HZ,
      HZ_PHN_GP = p_gp * HZ_PHN,
      HZ_Death = p_mor_hz * HZ
    ) %>% 
    select(-starts_with(c("r_", "p_")))
}


sim_uptake_zvl <- function(df) {
  eligible <- df %>% filter(uptake > 0)
  others <- df %>% filter(uptake <= 0)
  
  eligible %>% 
    mutate(
      N_None = N * (1- uptake),
      N_Zostavax = N * uptake
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
      N_Shingrix = N * uptake
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
  
  sim_0 <- pars$N %>% filter(Year == year0) %>% 
    mutate(Vaccine = "None", Protection = 0, AgeVac  = NA)
  
  for (yr in year0:year1) {
    fn_vac <- ifelse(yr < 2023, sim_uptake_zvl, sim_uptake_rzv)
    
    sim_t <- sim_0 %>% 
      rule_eligible(pars$Uptake, yr = yr) %>% 
      fn_vac() %>%
      sim_ve(pars$VE) %>% 
      sim_hz(pars$Epi) %>% 
      sim_death_im(pars$DeathIm)
    
    sim_0 <- sim_bir_ageing(sim_t, pars$Birth, yr, hz = (yr < 2023))
    
    ys[[length(ys) + 1]] <- sim_t
  }
  ys <- bind_rows(ys) %>% 
    mutate(
      NewUptake = ifelse(is.na(NewUptake), 0, NewUptake),
      Year = Year + 1  
    )
  ys
}


decorate_by_ce <- function(df, pars_ce, cost_vac, dis_effects, dis_costs, year0) {
  yss %>% 
    left_join(pars_ce, by = "Age") %>% 
    left_join(cost_vac, by = "Vaccine") %>% 
    mutate(
      dis_ql = 1 / ((1 + dis_effects) ^ (Year - year0)),
      dis_cost = 1 / ((1 + dis_costs) ^ (Year - year0)),
      QL_y2_d = QL_y2 / (1 + dis_effects),
      QL_HZ = QL_y1 + QL_y2,
      QL_HZ_d = QL_y1 + QL_y2_d,
      Q_Life = N * QOL,
      Q_HZ = - HZ * QL_HZ,
      Q_All = Q_Life + Q_HZ,
      Q_Life_d = Q_Life * dis_ql,
      Q_HZ_d = - HZ * QL_HZ_d * dis_ql,
      Q_All_d = Q_Life_d + Q_HZ_d,
      C_Hosp = HZ_Hosp * cost_Hospitalisation_pp_inf,
      C_GP_NonPHN = (HZ_GP - HZ_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf,
      C_GP_PHN = HZ_PHN_GP * cost_GP_pp_PHN_inf,
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
}


summarise_dy_hz <- function(yss, pars_ce, cost_vac, soc = "SOC", 
                            dis_effects = 0.035, dis_costs = 0.035, age0 = 50, year0 = 2023) {
  yss_collapse <- yss %>% 
    decorate_by_ce(pars_ce, cost_vac, dis_effects = dis_effects, dis_costs = dis_costs, year0 = year0) %>% 
    group_by(Year, Age, Scenario, Key) %>% 
    summarise(
      Coverage_RZV = sum(N[Vaccine == "Shingrix"]) / sum(N),
      Coverage_ZVL = sum(N[Vaccine == "Zostavax"]) / sum(N),
      across(c(N, starts_with(c("HZ", "C_", "Q_")), NewUptake), sum)
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
