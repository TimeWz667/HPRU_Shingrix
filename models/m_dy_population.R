simulate_demo <- function(pars, year0 = 2015, year1 = 2040) {
  require(tidyverse)
  
  year_range <- range(pars$N$Year)
  
  if (year0 <= year_range[1]) stop("Unsupported year range")
  if (year1 >= year_range[2]) stop("Unsupported year range")
  
  ys <- list()
  
  sim_0 <- pars$N %>% filter(Year == year0)
  
  for (yr in year0:year1) {
    sim_t <- sim_0 %>% 
      left_join(pars$DeathIm, by = c("Year", "Age")) %>%
      mutate(
        Death = r_death * N,
        Im = r_immigration * N
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
             N = pars$Birth %>% filter(Year == yr + 1) %>% pull(N_Birth)),
      sim_1
    )
    
    ys[[length(ys) + 1]] <- sim_t
  }
  ys <- bind_rows(ys)  
  ys
}
