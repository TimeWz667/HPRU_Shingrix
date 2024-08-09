m_dy <- list()
class(m_dy) = "dy_hz"


run <- function (x, ...) {
  UseMethod("run", x)
}


run.dy_hz <- function(model, pars, strategy, year0 = 2012, year1 = 2045, year_base = 2023, sim_0 = NULL) {
  
  # Initialise
  ve_rzv <- bind_rows(pars$VE_RZV, pars$VE_ReRZV, pars$VE_ReRZV1)
  ve_zvl <- pars$VE_ZVL
  
  if (is.null(sim_0)) {
    sim_0 <- model$populate(pars, year0)
  }
  
  
  # Rounds by year
  ys <- list() # Collector for runs
  
  for (yr in year0: year1) {
    sim_t <- sim_0 %>% 
      model$simulate_vaccination(pars, strategy, yr, ve_zvl, ve_rzv) %>% 
      model$simulate_hz(pars) %>% 
      model$simulate_death_im(pars)
    
    ys[[length(ys) + 1]] <- sim_t
    
    sim_0 <- model$simulate_ageing(sim_t, pars, yr)
  }
  
  # Bind results
  ys <- bind_rows(ys) %>% 
    model$append_ce(pars, year_base) %>% 
    model$summarise_hz()
  
  return(ys)
}




m_dy$populate <- function(pars, year) {
  pars$Demography$N %>% filter(Year == year) %>% 
    mutate(Vaccine = "None", Immunity = 0, TimeVac  = -1)
} 


m_dy$simulate_vaccination <- function(df, pars, strategy, yr, ve_zvl, ve_rzv) {
  d0 <- df %>% strategy(pars, yr)
  
  d_v <- d0 %>% filter(p_uptake > 0)
  
  if(nrow(d_v) <= 0) {
    return(df)
  }
  df <- bind_rows(
    d0 %>% filter(p_uptake <= 0),
    d_v %>% mutate(N = N * (1 - p_uptake)),
    d_v %>% mutate(
      N = N * p_uptake,
      Vaccine = Eli,
      TimeVac = 1
    )
  ) %>% select(- p_uptake, - Eli)
  
  df <- bind_rows(
    df %>% filter(TimeVac < 0),
    df %>% filter(TimeVac >= 0 & Vaccine == "ZVL") %>% 
      left_join(ve_zvl, by = c("Age", "Vaccine", "TimeVac")) %>% 
      mutate(
        Protection = ifelse(is.na(Protection), 0, Protection),
        Immunity = pmax(Immunity, Protection)
      ) %>% 
      select( - Protection),
    df %>% filter(TimeVac >= 0 & Vaccine != "ZVL") %>% 
      left_join(ve_rzv, by = c("Vaccine", "TimeVac")) %>% 
      mutate(
        Protection = ifelse(is.na(Protection), 0, Protection),
        Immunity = pmax(Immunity, Protection)
      ) %>% 
      select( - Protection)
  )
  
  return(df)
}




m_dy$simulate_death_im <- function(df, pars) {
  df %>%
    left_join(pars$Demography$DeathIm, by = c("Year", "Age")) %>%
    mutate(
      Death = r_death * N,
      Im = r_immigration * N,
    ) %>% 
    select(-c(r_death, r_immigration))
}


m_dy$simulate_hz <- function(df, pars) {
  df %>% 
    left_join(pars$Epidemiology, by = c("Age")) %>% 
    fill(r_hz, p_gp, p_phn, r_mor_hz, .direction = "up") %>% 
    mutate(
      N_HZ = r_hz * (1 - Immunity) * N,
      N_HZ_GP = p_gp * N_HZ,
      N_HZ_Hosp = N_HZ - N_HZ_GP,
      N_HZ_PHN = p_phn * N_HZ,
      N_HZ_PHN_GP = p_gp * N_HZ_PHN,
      N_HZ_Death = r_mor_hz * (1 - Immunity) * N_HZ
    ) %>% 
    select(-starts_with(c("r_", "p_")))
}


m_dy$simulate_ageing <- function(df, pars, year) {
  yr <- year + 1
  n_bir <- pars$Demography$Birth %>% filter(Year == yr) %>% pull(N_Birth)
  
  # Ageing
  sim_1 <- df %>% 
    mutate(
      N = N - Death + Im, # - ifelse(year >= 2023, N_HZ_Death, 0),
      Year = yr, 
      Age = Age + 1,
      TimeVac = ifelse(Vaccine == "None", TimeVac, TimeVac + 1)
    ) %>% 
    select(Year, Age, N, Vaccine, TimeVac, Immunity) %>% 
    filter(Age <= 100)
  
  # Birth
  bind_rows(
    tibble(Year = yr, Age = 0, N = n_bir, 
           Vaccine = "None", TimeVac = -1, Immunity = 0),
    sim_1
  )
}



m_dy$append_ce <- function(ys, pars, year0 = 2023) {
  dis_cost <- pars$discount_costs
  dis_eff <- pars$discount_effects
  
  ys %>% 
    left_join(pars$CostVac, by = "Vaccine") %>% 
    left_join(pars$CostEff, by = "Age") %>% 
    mutate(
      Q_Life = N * QOL,
      QL_HZ = QL_y1 + QL_y2,
      Q_HZ = - N_HZ * QL_HZ,
      Q_All = Q_Life + Q_HZ,
      C_Hosp = N_HZ_Hosp * cost_hosp_pp_inf,
      C_GP_NonPHN = (N_HZ_GP - N_HZ_PHN_GP) * cost_GP_pp_non_PHN_HZ_inf,
      C_GP_PHN = N_HZ_PHN_GP * cost_GP_pp_PHN_inf,
      C_GP = C_GP_NonPHN + C_GP_PHN,
      C_Med = C_GP + C_Hosp,
      C_Vac = cost_vac_pp * N * (TimeVac == 1),
      across(c(starts_with("C_"), number_courses), \(x) ifelse(is.na(x), 0, x)),
      C_All = C_Med + C_Vac,
      dis_e = 1 / ((1 + dis_eff) ^ (Year - year0)),
      dis_c = 1 / ((1 + dis_cost) ^ (Year - year0)),
      QL_y2_d = QL_y2 / (1 + dis_eff),
      QL_HZ_d = (QL_y1 + QL_y2_d) * dis_e,
      Q_HZ_d = - N_HZ * QL_HZ_d,    
      Q_Life_d = Q_Life * dis_e,
      Q_All_d = Q_Life_d + Q_HZ_d,
      across(starts_with("C_"), \(x) x * dis_c, .names = "{.col}_d")
    )
}


m_dy$summarise_hz <- function(ys) {
  ys %>% 
    group_by(Year, Age) %>% 
    summarise(
      C_VacZVL = sum(C_Vac * (Vaccine == "ZVL")),
      C_VacRZV = sum(C_Vac) - C_VacZVL,
      C_VacZVL_d = sum(C_Vac_d * (Vaccine == "ZVL")),
      C_VacRZV_d = sum(C_Vac_d) - C_VacZVL_d,
      across(starts_with(c("N_", "C_", "Q_")), sum),
      N_Immuned = sum(N * Immunity),
      N_Vac = sum(N * (Vaccine != "None")),
      N_VacZVL = sum(N * (Vaccine == "ZVL")),
      N_VacRZV = N_Vac - N_VacZVL,
      N_Uptake = sum(N * (Vaccine != "None") * (TimeVac == 1) * number_courses),
      N_UptakeZVL = sum(N * (Vaccine == "ZVL") * (TimeVac == 1) * number_courses),
      N_UptakeRZV = N_Uptake - N_UptakeZVL,
      N_Uptake_d = sum(N * (Vaccine != "None") * (TimeVac == 1) * number_courses * dis_c),
      N_UptakeZVL_d = sum(N * (Vaccine == "ZVL") * (TimeVac == 1) * number_courses * dis_c),
      N_UptakeRZV_d = N_Uptake_d - N_UptakeZVL_d,
      N = sum(N)
    )
  
}




