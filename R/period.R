

model_proj <- list()
class(model_proj) <- "model_proj"


model_proj$uptaking <- function(df, yr, strategy, pars_uptake) {
  df <- df %>% strategy(pars_uptake, yr)
  if ((df %>% filter(!is.na(eli)) %>% nrow()) <= 0) {
    return(df)
  }
  df <- bind_rows(
    df %>% filter(is.na(eli)),
    df %>% filter(!is.na(eli)) %>% 
      mutate(
        Prop_n = Prop * (1 - p_uptake), 
        Prop_v = Prop * p_uptake
      ) %>% 
      pivot_longer(c(Prop_n, Prop_v)) %>% 
      mutate(
        Prop = value,
        TimeVac = case_when(
          name == "Prop_n" ~ TimeVac,
          Vaccine == eli ~ TimeVac,
          T ~ 1
        ),
        Vaccine = ifelse(name == "Prop_n", Vaccine, eli)
      ) %>% 
      select(-c(name, value))
  ) %>% 
    select(-c(eli, p_uptake)) %>% 
    arrange(Year, Age)
  
  return(df)
}


model_proj$ageing <- function(df, yr, age0, age1) {
  df <- df %>% 
    mutate(
      Age = Age + 1,
      TimeVac = ifelse(TimeVac > 0, TimeVac + 1, TimeVac),
      Year = yr
    ) %>% 
    filter(Age <= age1) %>%       
    bind_rows(
      tibble(Year = yr, Age = age0, Vaccine = "None", TimeVac = -1, Prop = 1)
    ) %>% 
    arrange(Age)
  
  return(df)
}


a_projection <- function(pars, strategy, year0 = 2013, year1 = 2050, age0 = 0, age1 = 100) {
  require(tidyverse)

  # pars <- tar_read(pars_proj)
  # names(pars) <- gsub("pars_proj_c334c586f7cb2b6d_", "", names(pars))
  # 
  p_uptake <- pars$Uptake
  p_demo <- pars$Demography
  
  population <- with(model_proj, {
    yr <- year0
    pop <- tibble(Year = yr, Age = age0:age1, Vaccine = "None", TimeVac = -1, Prop = 1) 
    pop <- pop %>% uptaking(yr = yr, strategy = strategy, pars_uptake = p_uptake)
    
    collector <- pop
    
    while (yr < year1) {
      yr <- yr + 1
      pop <- pop %>% 
        ageing(yr, age0, age1) %>% 
        uptaking(yr = yr, strategy = strategy, pars_uptake = p_uptake)
      collector <- collector %>% bind_rows(pop)
      # print(nrow(collector))
    }
  
    collector
  }) %>% 
  left_join(p_demo$N, by = c("Year", "Age")) %>%
  mutate(
    N = N * Prop
  ) %>% 
  select(-Prop)
    
  
  ves_rzv <- bind_rows(
    pars$VE_RZV_2d,
    pars$VE_RZV_1d,
    pars$VE_ReRZV_2d,
    pars$VE_ReRZV_1d,
  )
  
  ves_zvl <- pars$VE_ZVL
  
#  population <- population %>% crossing(Key = 1:100)
  population <- population %>% crossing(Key = 1:pars$N_Sims)
  
  vaccinated <- bind_rows(
    population %>% 
      filter(Vaccine == "ZVL") %>% 
      left_join(ves_zvl, by = c("Key", "Age", "Vaccine", "TimeVac")),
    population %>% 
      filter(Vaccine == "None") %>% 
      mutate(Protection = 0),
    population %>% 
      filter(!(Vaccine %in% c("ZVL", "None"))) %>% 
      left_join(ves_rzv, by = c("Key", "Vaccine", "TimeVac"))
  ) %>% 
    mutate(
      N_Uptake_ZVL = ifelse((TimeVac == 1) * (Vaccine == "ZVL"), N, 0),
      N_Uptake_RZV = ifelse((TimeVac == 1) * endsWith(Vaccine, "RZV_1d"), N, 0) +
                     ifelse((TimeVac == 1) * endsWith(Vaccine, "RZV_2d"), 2 * N, 0),
      N_Covered_ZVL = ifelse((TimeVac == 1) * (Vaccine == "ZVL"), N, 0),
      N_Covered_RZV = ifelse((TimeVac == 1) * !(Vaccine %in% c("None", "ZVL")), N, 0),
                            
    ) %>% 
    group_by(Key, Year, Age) %>% 
    summarise(
      Protection = weighted.mean(Protection, w = N),
      across(starts_with("N"), sum)
    ) %>% 
    ungroup()

  
  sims <- vaccinated %>% 
    left_join(p_demo$DeathIm %>% select(Year, Age, r_mor_bg = r_death), by = c("Year", "Age")) %>% 
    inner_join(pars$Epidemiology, by = c("Key", "Age")) %>% 
    mutate(
      r_mor = (1 - Protection) * r_mor_hz + r_mor_bg,
      p_mor_hz = (1 - Protection) * r_mor_hz * (1 - exp(- r_mor)) / r_mor,
      N_HZ = (1 - Protection) * r_hz * N,
      N_HZ_GP = p_gp * N_HZ,
      N_HZ_Hosp = N_HZ - N_HZ_GP,
      N_HZ_PHN = p_phn * N_HZ,
      N_HZ_PHN_GP = p_gp * N_HZ_PHN,
      N_HZ_Death = p_mor_hz * N_HZ
    ) %>% 
    select(-starts_with(c("r_", "p_")))
  
  
  yss_agp <- sims %>% 
    filter(Age >= 60 & Age < 100) %>% 
    mutate(
      Agp = cut(Age, seq(60, 100, 5), right = F)
    ) %>% 
    group_by(Key, Year, Agp) %>% 
    summarise(
      Protection = weighted.mean(Protection, N),
      across(starts_with("N"), sum)
    ) %>% 
    mutate(
      Coverage_ZVL = N_Covered_ZVL / N,
      Coverage_RZV = N_Covered_RZV / N,
      Coverage = Coverage_ZVL + Coverage_RZV,
      IncR_HZ = N_HZ / N,
      IncR_HZ_Hosp = N_HZ_Hosp  / N,
      IncR_HZ_PHN = N_HZ_PHN / N,
      MorR_HZ = N_HZ_Death / N
    ) %>% 
    ungroup()
  
  yss_68 <- sims %>% 
    filter(Age >= 60 & Age < 100) %>% 
    mutate(
      Agp = ifelse(Age < 80, "60_80", "80+")
    ) %>% 
    group_by(Key, Year, Agp) %>% 
    summarise(
      Protection = weighted.mean(Protection, N),
      across(starts_with("N"), sum)
    ) %>% 
    mutate(
      Coverage_ZVL = N_Covered_ZVL / N,
      Coverage_RZV = N_Covered_RZV / N,
      Coverage = Coverage_ZVL + Coverage_RZV,
      IncR_HZ = N_HZ / N,
      IncR_HZ_Hosp = N_HZ_Hosp  / N,
      IncR_HZ_PHN = N_HZ_PHN / N,
      MorR_HZ = N_HZ_Death / N
    ) %>% 
    ungroup()
  
  yss_all <- sims %>% 
    filter(Age >= 60 & Age < 100) %>% 
    group_by(Key, Year) %>% 
    summarise(
      Protection = weighted.mean(Protection, N),
      across(starts_with("N"), sum)
    ) %>% 
    mutate(
      Agp = "All",
      Coverage_ZVL = N_Covered_ZVL / N,
      Coverage_RZV = N_Covered_RZV / N,
      Coverage = Coverage_ZVL + Coverage_RZV,
      IncR_HZ = N_HZ / N,
      IncR_HZ_Hosp = N_HZ_Hosp  / N,
      IncR_HZ_PHN = N_HZ_PHN / N,
      MorR_HZ = N_HZ_Death / N
    ) %>% 
    ungroup()
  
  return(list(
    vtype = pars$vtype,
    Yss_Agp = yss_agp,
    Yss_68 = yss_68,
    Yss_All = yss_all
  ))
  
}


exec_projection <- function(pars, year1 = 2050) {
  res = list(
    "Null" = a_projection(pars, strategy_null, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "Stay" = a_projection(pars, strategy_zvl, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "ToRZV" = a_projection(pars, strategy_changeonly, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "Sch65" = a_projection(pars, strategy_scheduled65, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "Sch" = a_projection(pars, strategy_scheduled, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "Sch1d85" = a_projection(pars, strategy_scheduled_1d85, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "Sch1d95" = a_projection(pars, strategy_scheduled_1d95, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "Sch2d85" = a_projection(pars, strategy_scheduled_2d85, year0 = 2013, year1 = year1, age0 = 50, age1 = 100),
    "Sch2d95" = a_projection(pars, strategy_scheduled_2d95, year0 = 2013, year1 = year1, age0 = 50, age1 = 100)
  )
  
  
  yss_agp = lapply(names(res), \(k) res[[k]]$Yss_Agp %>% mutate(Scenario = k)) %>% bind_rows() 
  yss_68 = lapply(names(res), \(k) res[[k]]$Yss_68 %>% mutate(Scenario = k)) %>% bind_rows() 
  yss_all = lapply(names(res), \(k) res[[k]]$Yss_All %>% mutate(Scenario = k)) %>% bind_rows() 
  
  return(list(
    vtype = pars$vtype,
    Year1 = year1,
    Yss_Agp = yss_agp,
    Yss_68 = yss_68,
    Yss_All = yss_all
  ))
}



