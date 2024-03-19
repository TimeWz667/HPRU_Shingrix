library(tidyverse)
library(tidybayes)



ve_zvl <- read_csv(here::here("pars", "pars_ve_zvl_zlgamma.csv"))
ve_zvl_agp <- read_csv(here::here("pars", "pars_ve_zvl_agp.csv"))
ve_rzv <- read_csv(here::here("pars", "pars_ve_rzv_zlgamma.csv"))




sim_ve_zvl <- crossing(Age = 50:100, Key = 1:100, Yr = 1:50) %>% 
  filter(Age - Yr >= 50) %>% 
  left_join(ve_zvl) %>% 
  left_join(ve_zvl_agp) %>% 
  mutate(
    VE = p0 * (1 - pgamma(Yr, alpha, beta)),
    oddVE = log(VE / (1 - VE)) + or70spline,
    VE2 = 1 / (1 + exp(-oddVE)),
    Vaccine = "ZVL"
  ) %>% 
  select(-oddVE)


sim_ve_zvl %>% 
  group_by(Age, Yr) %>% 
  summarise(across(c(VE, VE2), mean)) %>% 
  ggplot() +
  geom_point(aes(x = Age, y =VE2, colour = Yr))


sim_ve <- bind_rows(
  crossing(Yr = 1:20, Key = 1:100) %>% 
    left_join(ve_rzv) %>% 
    mutate(
      VE = p0 * (1 - pgamma(Yr, alpha, beta)) * 0.91,
      Vaccine = "RZV"
    ),
  crossing(Yr = 1:20, Key = 1:100) %>% 
    left_join(ve_zvl) %>% 
    mutate(
      VE = p0 * (1 - pgamma(Yr, alpha, beta)),
      Vaccine = "ZVL"
    )
)

sim_ve %>% 
  ggplot() +
  geom_line(aes(x = Yr, y = VE, colour = Vaccine, group = Key), alpha = 0.1) +
  facet_grid(.~Vaccine) +
  expand_limits(y = 0:1)








