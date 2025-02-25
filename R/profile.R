

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

