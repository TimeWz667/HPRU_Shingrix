library(tidyverse)


# Demography
load(here::here("pars", "pars_demo.rdata"))


# Epidemiology
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


pars_uptake <- local({
  load(here::here("data", "fitted_coverage.rdata"))
  pred1$pars
})


pars_ves <- local({
  load(here::here("pars", "ves.rdata"))
  
  ves %>% 
    filter(!IC) %>% 
    select(Age, AgeVac, Vaccine = TypeVac, Protection = VE)
  
  load(here::here("pars", "ves_ce.rdata"))
  
  bind_rows(
    ves %>% 
      filter(!IC) %>% 
      filter(TypeVac != "Shingrix") %>% 
      select(Age, AgeVac, Vaccine = TypeVac, Protection = VE),
    ve_nic %>% filter(Type == "Real") %>% select(Age, AgeVac, Vaccine = TypeVac, Protection = VE)
  )
})



save(pars_demo, pars_epi, pars_uptake, pars_ves, file = here::here("analysis_programme", "inputs", "pars_base.rdata"))

