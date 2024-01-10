simulate_protection <- function(pars, year_birth = 1944, year0 = 2014, year1 = 2035, ic = FALSE) {
  require(tidyverse)
  year <- year0
  age <- year0 - year_birth
  
  y0 <- tibble(
    Year = year,
    Age = age,
    YearVac = NA,
    TypeVac = "None",
    State = "None",
    Pr = 1
  )
  
  sims <- list(y0)
  for (year in (year0+1):year1) {
    age <- year - year_birth
    uv <- y0 %>% filter(State == "None")
    n <- uv %>% pull(Pr)
    
    p_vac <- case_when(
      year < 2014 ~ 0,
      year < 2023 ~ case_when( # Previous programme with Zostavax
        age < 70 ~ 0,
        age == 70 ~ pars$p_initial,
        age < 80 ~ pars$p_catchup,
        T ~ 0
      ),
      ic ~ case_when( # Expanded programme with Shingrix for IC
        age < 50 ~ 0,
        age == 50 ~ pars$p_initial,
        age < 80 ~ pars$p_catchup,
        T ~ 0
      ),
      year < 2028 ~ case_when( # Stage 1 of expansion
        age < 65 ~ 0,
        age == 65 ~ pars$p_initial,
        age == 70 ~ pars$p_initial,
        age < 80 ~ pars$p_catchup,
        T ~ 0
      ),
      year < 2033 ~ case_when( # Stage 2 of expansion
        age < 60 ~ 0,
        age == 60 ~ pars$p_initial,
        age == 65 ~ pars$p_initial,
        age < 80 ~ pars$p_catchup,
        T ~ 0
      ),
      T ~ case_when( # Onward
        age < 60 ~ 0,
        age == 60 ~ pars$p_initial,
        age < 80 ~ pars$p_catchup,
        T ~ 0
      )
    )
    
    type_vac <- case_when(
      year < 2014 ~ "None",
      year < 2023 ~ "Zostavax",
      T ~ "Shingrix"
    )
    
    if (p_vac > 0) {
      y0 <- bind_rows(
        uv %>% mutate(Pr = n * p_vac, State = "Vac", YearVac = year, TypeVac = type_vac),
        uv %>% mutate(Pr = n * (1 - p_vac)),
        y0 %>% filter(State != "None")
      )
    }
    
    sims[[length(sims) + 1]] <- y0 %>% mutate(Age = age, Year = year)
  }
  
  sims <- bind_rows(sims) %>% 
    mutate(
      YearBirth = year_birth,
      AgeVac = YearVac - YearBirth,
      IC = ifelse(ic, "IC", "NonIC")
    ) %>% 
    arrange(Year)
  
  return(sims)
}
