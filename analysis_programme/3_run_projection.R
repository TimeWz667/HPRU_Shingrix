library(tidyverse)
library(tidybayes)

theme_set(theme_bw())

source(here::here("models", "m_dy_hz.R"))


## Loading parameters -----
root <- "analysis_programme"

load(here::here(root, "inputs", "pars_base.rdata"))



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



## Simulation -----
yss_soc <- list()
yss_p1 <- list()
yss_p2 <- list()
yss_p1_95 <- list()
yss_p2_95 <- list()


keys <- pars_epi %>% pull(Key) %>% unique()
keys <- keys[1:200]

pb <- txtProgressBar(min = 1, max = length(keys), style = 3,  width = 50, char = "=") 

for(k in keys) {
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
                                                      rule_eligible = scenario_2028_95) %>% mutate(Scenario = "To 95 yr since 2028", Key = k)
  yss_p2_95[[length(yss_p2_95) + 1]] <- sim_dy_hz_vac(pars, year1 = 2050, 
                                                      rule_eligible = scenario_2033_95) %>% mutate(Scenario = "To 95 yr since 2033", Key = k)
  
  setTxtProgressBar(pb, k)
}


## Outputs -----
save(yss_soc, file = here::here(root, "temp", "yss_soc.rdata"))
save(yss_p1, file = here::here(root, "temp", "yss_p1.rdata"))
save(yss_p2, file = here::here(root, "temp", "yss_p2.rdata"))
save(yss_p1_95, file = here::here(root, "temp", "yss_p1_95.rdata"))
save(yss_p2_95, file = here::here(root, "temp", "yss_p2_95.rdata"))

