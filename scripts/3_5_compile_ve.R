library(tidyverse)
library(tidybayes)



n_sims <- 1e3

## ZVL -----
ve_zvl <- read_csv(here::here("pars", "pars_ve_zvl_zlgamma.csv"))
ve_zvl_agp <- read_csv(here::here("pars", "pars_ve_zvl_agp.csv"))


pars_ve_rw_zvl <- crossing(Age = 50:100, Key = 1:n_sims, Yr = 1:50) %>% 
  filter(Age - Yr >= 50) %>% 
  left_join(ve_zvl) %>% 
  left_join(ve_zvl_agp) %>% 
  mutate(
    VE0 = p0 * (1 - pgamma(Yr, alpha, beta)),
    oddVE = log(VE0 / (1 - VE0)) + or70spline,
    VE = 1 / (1 + exp(-oddVE)),
    Vaccine = "ZVL",
    IC = F
  ) %>% 
  select(Key, Age, Yr, Vaccine, VE0, VE, IC)

save(pars_ve_rw_zvl, file = here::here("pars", "pars_ve_rw_zvl.rdata"))


pars_ve_rw_zvl %>% 
  group_by(Age, Yr) %>% 
  summarise(across(c(VE, VE0), mean)) %>% 
  ggplot() +
  geom_point(aes(x = Age, y = VE, colour = Yr)) +
  scale_y_continuous("Vaccine effectiveness, %", labels = scales::percent) +
  expand_limits(y = 0:1)



## RZV -----
ve_rzv <- read_csv(here::here("pars", "pars_ve_rzv_zlgamma.csv"))


pars_ve_rw_rzv <- crossing(Key = 1:n_sims, Yr = 1:50) %>% 
  left_join(ve_rzv) %>% 
  mutate(
    VE = p0 * (1 - pgamma(Yr, alpha, beta)) * 0.91,
    Vaccine = "RZV",
    IC = F
  ) %>% 
  select(Key, Vaccine, Yr, VE, IC)

save(pars_ve_rw_rzv, file = here::here("pars", "pars_ve_rw_rzv.rdata"))


pars_ve_rw_rzv %>% 
  group_by(Yr) %>% 
  summarise(VE = mean(VE)) %>% 
  ggplot() +
  geom_point(aes(x = Yr, y = VE, colour = Yr)) +
  scale_y_continuous("Vaccine effectiveness, %", labels = scales::percent) +
  expand_limits(y = 0:1)


## Plot for check
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
  scale_y_continuous("Vaccine effectiveness, %", labels = scales::percent) +
  expand_limits(y = 0:1)








