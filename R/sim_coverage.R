simulate_protection <- function(p_initial, p_catchup, year_birth = 1944, 
                                year0 = 2014, year1 = 2040, ic = FALSE,
                                al_ic = 50, ah_ic = 80,
                                year_step1 = 2023, al_step1 = 65, ah_step1 = 80,
                                year_step2 = 2028, al_step2 = 60, ah_step2 = 80,
                                year_step3 = 2033) {
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
      year < year_step1 ~ case_when( # Previous programme with Zostavax
        age < 70 ~ 0,
        age == 70 ~ p_initial,
        age < 80 ~ p_catchup,
        T ~ 0
      ),
      ic ~ case_when( # Expanded programme with Shingrix for IC
        age < al_ic ~ 0,
        age == al_ic ~ p_initial,
        age < ah_ic ~ p_catchup,
        T ~ 0
      ),
      year < year_step2 ~ case_when( # Stage 1 of expansion
        age < al_step1 ~ 0,
        age == al_step1 ~ p_initial,
        age == 70 ~ p_initial,
        age < ah_step1 ~ p_catchup,
        T ~ 0
      ),
      year < year_step3 ~ case_when( # Stage 2 of expansion
        age < al_step2 ~ 0,
        age == al_step2 ~ p_initial,
        age == al_step1 ~ p_initial,
        age < ah_step2 ~ p_catchup,
        T ~ 0
      ),
      T ~ case_when( # Onward
        age < al_step2 ~ 0,
        age == al_step2 ~ p_initial,
        age < ah_step2 ~ p_catchup,
        T ~ 0
      )
    )
    
    type_vac <- case_when(
      year < 2014 ~ "None",
      year < year_step1 ~ "Zostavax",
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
