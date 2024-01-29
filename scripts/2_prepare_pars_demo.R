library(tidyverse)


load(here::here("data", "processed_demography", "Population_ONS.rdata"))


pars_demo <- lapply(c(England = "England", UK = "UK"), function(loc) {
  demo_fr <- demo_ons %>% 
    filter(Location == loc) %>% 
    select(Year0 = Year, Age0 = Age, dr = mortality, N0 = N)
  
  demo_to <- demo_ons %>% 
    filter(Location != loc) %>% 
    select(Year1 = Year, Age1 = Age, N1 = N) %>% 
    mutate(
      Year0 = Year1 - 1,
      Age0 = Age1 - 1
    )
  
  list(
    N = demo_fr %>% 
      select(Year = Year0, Age = Age0, N = N0),
    DeathIm = demo_fr %>% 
      left_join(demo_to) %>% 
      mutate(
        N1 = ifelse(is.na(N1), 0, N1),
        dN = N1 - N0,
        r_immigration = dN / N0 + dr
      ) %>% 
      select(Year = Year0, Age = Age0, r_death = dr, r_immigration),
    Birth = demo_ons %>% 
      filter(Location != loc) %>%
      filter(Age == 0) %>% 
      select(Year, N_Birth = N) %>% 
      mutate(
        Year = Year - 1
      )
  )
})



pars <- pars_demo$England



year0 <- 2015


# initialise

simulate_demo <- function(year0 = 2015, year1 = 2040, pars) {
  ys <- list()
  
  sim_0 <- pars$N %>% filter(Year == year0)
  
  for (yr in year0:year1) {
    sim_t <- sim_0 %>% 
      left_join(pars$DeathIm, by = c("Year", "Age")) %>%
      mutate(
        Death = r_death * N,
        Im = r_immigration * N,
        N = N - Death + Im
      ) %>% 
      select(Year, Age, N, Death, Im)
    
    sim_1 <- sim_t %>% 
      mutate(
        N = N - Death + Im,
        Year = yr + 1, 
        Age = Age + 1
      ) %>% 
      select(Year, Age, N) %>% 
      filter(Age <= 100)
    
    sim_0 <-  bind_rows(
      tibble(Year = yr + 1, Age = 0, 
             N = pars$Birth %>% filter(Year == yr) %>% pull(N_Birth)),
      sim_1
    )
    
    ys[[length(ys) + 1]] <- sim_t
  }
  ys <- bind_rows(ys)  
  
}



ys_eng <- simulate_demo(pars = pars)

ys_eng %>% 
  filter(Age > 98)


ys_eng %>% 
  mutate(
    AgeGrp <- case_wh
  )

  
  
  















