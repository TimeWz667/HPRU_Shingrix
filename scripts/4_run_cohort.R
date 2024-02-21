library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))


## Parameters: Demography -----
load(here::here("pars", "pars_demo.rdata"))



## Parameters: CE -----
load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
load(here::here("data", "processed_ce", "QOL_LE.rdata"))
cost_vac <- read_csv(here::here("data", "processed_ce", "Cost_Vac.csv"))

pars_ce <- QL %>% 
  select(- Key) %>%
  group_by(age) %>% 
  summarise(across(everything(), mean)) %>% 
  left_join(Cost_Hospitalisation_HZ %>% select(- Key)) %>% 
  bind_cols(Cost_GP %>% select(- Key) %>% summarise(across(everything(), mean))) %>% 
  rename(Age = age)

### discount rate costs
discount_rate_costs <- 0.035
### discount rate effects
discount_rate_effects <- 0.035


## Parameters: Epidemiology -----
pars_epi <- local({
  p_mor_hz <- local({
    load(here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD_bind.rdata"))
    Epi_HZ %>% 
      group_by(Age) %>% 
      summarise(p_mor_hz = mean(r_mor_hz) / mean(r_inc_hz))
  })
  load(here::here("pars", "pars_epi_gpr.rdata"))
  
  pars_epi %>% 
    filter(IC == 0) %>% 
    left_join(p_mor_hz) %>% 
    select(-IC) %>% 
    arrange(Key, Age)
})


## Parameters: Vaccination -----

pars_ve <- local({
  load(here::here("pars", "ves_ce.rdata"))
  
  ve_nic %>% filter(Type == "Real") %>% select(Age, AgeVac, Vaccine = TypeVac, Protection = VE)
})


## Simulation -----
keys <- pars_epi %>% pull(Key) %>% unique()


yss <- list()

pb <- txtProgressBar(min = 1, max = 500, style = 3,  width = 50, char = "=") 

for(k in keys[1:500]) {
  pars <- c(pars_demo$England, list(
    Epi = pars_epi %>% filter(Key == k) %>% select(-Key),
    VE = pars_ve
  ))
  
  for (age0 in 50:95) {
    yss[[length(yss) + 1]] <- sim_cohort_hz_vac(pars, age0 = age0, year = 2024) %>% 
      mutate(Key = k)
  }
  
  setTxtProgressBar(pb, k)
}

yss <- bind_rows(yss)
results <- summarise_cohort_hz(yss, pars_ce, cost_vac)


results$CE %>% 
  filter(Variable %in% c("Q_All_d", "C_All_d")) %>% 
  select(AgeVac, Key, Variable, Diff) %>% 
  pivot_wider(names_from = Variable, values_from = Diff) %>% 
  mutate(ICER = C_All_d / Q_All_d) %>% 
  ggplot() +
  geom_point(aes(x = AgeVac, y = ICER))
