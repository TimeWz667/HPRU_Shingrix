source(here::here("models", "misc.R"))



load_inputs_nic <- function(discount_costs = 0.035, discount_effects = 0.035, year = 2024, n_sims = 1e3,
                            ve_rzv = "pars_ve_rzv_rw_zlg.rdata", ve_zvl = "pars_ve_zvl_rwa_zlg.rdata") {
  require(tidyverse)
  
  pars <- list(
    Year0 = year,
    N_Sims = n_sims,
    discount_costs = discount_costs,
    discount_effects = discount_effects
  )
  
  ## Parameters: Demography -----
  load(here::here("pars", "pars_demo.rdata"))
  pars_demo <- pars_demo$England
  
  pars$Demography <- pars_demo$N %>% 
    filter(Year == year) %>% 
    left_join(pars_demo$DeathIm, by = c("Year", "Age")) %>% 
    select(Year, Age, N, r_death)

  ## Parameters: Epidemiology -----
  pars_epi <- local({
    r_mor_hz <- local({
      load(here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD_bind.rdata"))
      Epi_HZ %>% 
        select(Key, Age, r_mor_hz) %>% 
        sample_table(n_sims)
    })
    
    load(here::here("pars", "pars_epi_gpr.rdata"))
    
    pars_epi %>% 
      filter(IC == 0) %>% 
      left_join(r_mor_hz, by = c("Key", "Age")) %>% 
      select(-IC) %>% 
      arrange(Key, Age)
  })
  
  pars$Epidemiology <- pars_epi
  
  
  ## Parameters: CE -----
  load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
  load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
  load(here::here("data", "processed_ce", "QOL_LE.rdata"))
  cost_vac <- read_csv(here::here("data", "processed_ce", "Cost_Vac.csv"))
  
  pars$CostVac <- cost_vac
  
  pars_ce <- sample_table(QL, n_sims) %>% 
    left_join(Cost_Hospitalisation_HZ %>% select(Age, cost_hosp_pp_inf = cost_Hospitalisation_pp_inf), by = "Age") %>% 
    left_join(sample_table(Cost_GP, n_sims) %>% select(Key, ends_with("_inf")), by = "Key")
  
  pars$CostEff <- pars_ce

  
  ## Parameters: Vaccination -----
  load(here::here("pars", ve_zvl))
  pars$VE_ZVL <- sample_table(pars_ve_zvl %>% filter(!IC), n_sims) %>%
    select(Key, Vaccine, Age, TimeVac = Yr, Protection = VE)
  
  
  load(here::here("pars", ve_rzv))
  pars$VE_RZV <- sample_table(pars_ve_rzv %>% filter(!IC), n_sims) %>% 
    select(Key, Vaccine, TimeVac = Yr, Protection = VE)
  
  
  return(pars)
}


load_inputs_ic <- function(discount_costs = 0.035, discount_effects = 0.035, year = 2024, n_sims = 1e3,
                           ve_rzv = "pars_ve_rzv_rw_zlg.rdata", ve_zvl = "pars_ve_zvl_rwa_zlg.rdata") {
  pars <- list(
    Year0 = year,
    N_Sims = n_sims,
    discount_costs = discount_costs,
    discount_effects = discount_effects
  )
  
  ## Parameters: Demography ----- ## Todo: Use IC-specific rates
  load(here::here("pars", "pars_demo.rdata"))
  pars_demo <- pars_demo$England
  
  pars$Demography <- pars_demo$N %>% 
    filter(Year == year) %>% 
    left_join(pars_demo$DeathIm) %>% 
    select(Year, Age, N, r_death)
  
  ## Parameters: Epidemiology -----
  pars_epi <- local({
    r_mor_hz <- local({
      load(here::here("data", "processed_epi", "Epi_HZ_IC_CPRD_bind.rdata"))
      Epi_HZ %>% 
        select(Key, Age, r_mor_hz) %>% 
        sample_table(n_sims)
    })
    
    load(here::here("pars", "pars_epi_gpr.rdata"))
    
    pars_epi %>% 
      filter(IC == 1) %>% 
      left_join(r_mor_hz, by = c("Key", "Age")) %>% 
      select(-IC) %>% 
      arrange(Key, Age)
  })
  
  pars$Epidemiology <- pars_epi
  
  
  ## Parameters: CE -----
  load(here::here("data", "processed_ce", "Cost_Hospitalisation_IC.rdata"))
  load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
  load(here::here("data", "processed_ce", "QOL_LE.rdata"))
  cost_vac <- read_csv(here::here("data", "processed_ce", "Cost_Vac.csv"))
  
  pars$CostVac <- cost_vac
  
  pars_ce <- sample_table(QL, n_sims) %>% 
    left_join(Cost_Hospitalisation_HZ %>% select(Age, cost_hosp_pp_inf = cost_Hospitalisation_pp_inf)) %>% 
    left_join(sample_table(Cost_GP, n_sims) %>% select(Key, ends_with("_inf")))
  
  pars$CostEff <- pars_ce
  
  
  ## Parameters: Vaccination ----- ## Todo: Use IC-specific rates
  load(here::here("pars", ve_zvl))
  pars$VE_ZVL <- sample_table(pars_ve_zvl %>% filter(!IC), n_sims) %>%
    select(Key, Vaccine, Age, TimeVac = Yr, Protection = VE)
  
  
  load(here::here("pars", ve_rzv))
  pars$VE_RZV <- sample_table(pars_ve_rzv %>% filter(!IC), n_sims) %>% 
    select(Key, Vaccine, TimeVac = Yr, Protection = VE)
  
  
  return(pars)
}
