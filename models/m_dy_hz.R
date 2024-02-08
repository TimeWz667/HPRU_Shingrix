

sim_death_im <- function(df, p0) {
  df %>%
    left_join(p0, by = c("Year", "Age")) %>%
    mutate(
      Death = r_death * N,
      Im = r_immigration * N,
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


sim_dy_hz <- function(pars, year0 = 2013, year1 = 2040) {
  ys <- list()
  
  sim_0 <- pars$N %>% filter(Year == year0) %>% 
    mutate(Vaccine = "None", Protection = 0, VaccineYear = NA)
  
  for (yr in year0:year1) {
    sim_t <- sim_0 %>% 
      sim_death_im(pars$DeathIm) %>% 
      sim_hz(pars$Epi)
    
    sim_0 <- sim_bir_ageing(sim_t, pars$Birth, yr)
    
    ys[[length(ys) + 1]] <- sim_t
  }
  ys <- bind_rows(ys) 
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
      sim_death_im(pars$DeathIm) %>% 
      sim_hz(pars$Epi)
    
    sim_0 <- sim_bir_ageing(sim_t, pars$Birth, yr, hz = (yr < 2023))
    
    ys[[length(ys) + 1]] <- sim_t
  }
  ys <- bind_rows(ys) 
}




