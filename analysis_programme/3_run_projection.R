library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))


## Eligibility functions -----
scenario_soc <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2022))

scenario_p65 <- function(df, p0, yr) find_eligible_default(df, p0, min(yr, 2027))

scenario_full <- find_eligible_default

scenario_2028_95 <- function(df, p0, yr) {
  if (yr < 2028) {
    df <- find_eligible_default(df, p0, yr)
  } else {
    df <- find_eligible_default(df, p0, yr, cap = 95)
  }
  return(df)
}

scenario_2033_95 <- function(df, p0, yr) {
  if (yr < 2033) {
    df <- find_eligible_default(df, p0, yr)
  } else {
    df <- find_eligible_default(df, p0, yr, cap = 95)
  }
  return(df)
}


## Loading parameters -----
load(here::here("pars", "pars_demo.rdata"))


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


## Simulation -----
yss_soc <- list()
yss_p1 <- list()
yss_p2 <- list()
yss_p1_95 <- list()
yss_p2_95 <- list()


keys <- pars_epi %>% pull(Key) %>% unique()
keys <- keys[1:100]

pb <- txtProgressBar(min = 1, max = length(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
  print(k)
  
  pars <- c(pars_demo$England, list(
    Epi = pars_epi %>% filter(Key == k) %>% select(-Key),
    Uptake = pars_uptake,
    VE = pars_ves
  ))
  
  yss_soc[[length(yss_soc) + 1]] <- sim_dy_hz_vac(pars, year1 = 2050, 
                                                  rule_eligible = scenario_soc) %>% mutate(Scenario = "SOC", Key = k)
  yss_p1[[length(yss_p1) + 1]] <- sim_dy_hz_vac(pars, year1 = 2050, 
                                                rule_eligible = scenario_p65) %>% mutate(Scenario = "Phase 1 only", Key = k)
  yss_p2[[length(yss_p2) + 1]] <- sim_dy_hz_vac(pars, year1 = 2050, 
                                                rule_eligible = scenario_full) %>% mutate(Scenario = "Scheduled", Key = k)
  yss_p1_95[[length(yss_p1_95) + 1]] <- sim_dy_hz_vac(pars, year1 = 2050, 
                                                      rule_eligible = scenario_2028_95) %>% mutate(Scenario = "To 95 yr at 2028", Key = k)
  yss_p2_95[[length(yss_p2_95) + 1]] <- sim_dy_hz_vac(pars, year1 = 2050, 
                                                      rule_eligible = scenario_2033_95) %>% mutate(Scenario = "To 95 yr at 2033", Key = k)
  
  setTxtProgressBar(pb, k)
}


save(yss_soc, file = here::here("outputs", "temp", "yss_soc.rdata"))
save(yss_p1, file = here::here("outputs", "temp", "yss_p1.rdata"))
save(yss_p2, file = here::here("outputs", "temp", "yss_p2.rdata"))
save(yss_p1_95, file = here::here("outputs", "temp", "yss_p1_95.rdata"))
save(yss_p2_95, file = here::here("outputs", "temp", "yss_p2_95.rdata"))

