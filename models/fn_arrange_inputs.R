source(here::here("models", "misc.R"))


apply_lor <- function(p0, lor) 1 / (1 + exp(-log(p0 / (1 - p0)) - lor))



load_inputs_nic <- function(discount_costs = 0.035, discount_effects = 0.035, 
                            year = 2024, n_sims = 1e3, 
                            realworld = T,
                            ve_rzv = "pars_ve_rzv_rw_zlg.rdata", 
                            ve_zvl = "pars_ve_zvl_rwa.rdata", 
                            ve_lor = "pars_ve_lor.rdata") {
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
  load(here::here("pars", ve_lor))
  lor_re <- 0
  
  
  load(here::here("pars", ve_zvl))
  pars$VE_ZVL <- sample_table(pars_ve_zvl %>% filter(!IC), n_sims) %>%
    select(Key, Vaccine, Age, TimeVac = Yr, Protection = VE)
  
  
  load(here::here("pars", ve_rzv))
  pars$VE_RZV <- ve_rzv <- sample_table(pars_ve_rzv %>% filter(!IC), n_sims) %>% 
    select(Key, Vaccine, TimeVac = Yr, Protection = VE)
  
  pars$VE_ReRZV <- ve_rzv %>% 
    mutate(
      Vaccine = "ReRZV",
      Protection = apply_lor(Protection, lor_re) 
    )
  
  pars$VE_ReRZV1 <- ve_rzv %>% 
    mutate(
      Vaccine = "ReRZV1",
      Protection = apply_lor(Protection, lor_re + lor_single) 
    )
  
  if (!realworld) {
    pars$VE_RZV <- pars$VE_RZV %>% mutate(Protection = apply_lor(Protection, -lor_rw))
    pars$VE_ReRZV <- pars$VE_ReRZV %>% mutate(Protection = apply_lor(Protection, -lor_rw))
    pars$VE_ReRZV1 <- pars$VE_ReRZV1 %>% mutate(Protection = apply_lor(Protection, - lor_rw))
    pars$VE_ZVL <- pars$VE_ZVL %>% mutate(Protection = apply_lor(Protection, -lor_rw))
  }
  
  return(pars)
}


load_inputs_nic_t <- function(discount_costs = 0.035, discount_effects = 0.035, 
                              year = 2024, n_sims = 1e3, 
                              ve_rzv = "pars_ve_rzv_rw_zlg.rdata", 
                              ve_zvl = "pars_ve_zvl_rwa.rdata", 
                              ve_lor = "pars_ve_lor.rdata") {
  
  pars <- load_inputs_nic(discount_costs = discount_costs, discount_effects = discount_effects,
                          year = year, n_sims = n_sims, 
                          realworld = T,
                          ve_rzv = ve_rzv, 
                          ve_zvl = ve_zvl, 
                          ve_lor = ve_lor)
  
  load(here::here("pars", "pars_demo.rdata"))
  pars$Demography <- pars_demo$England
  
  return(pars)
}


load_inputs_ic <- function(discount_costs = 0.035, discount_effects = 0.035, 
                           year = 2024, n_sims = 1e3,
                           realworld = T,
                           ve_rzv = "pars_ve_rzv_rw_zlg.rdata", 
                           ve_zvl = "pars_ve_zvl_rwa.rdata", 
                           ve_lor = "pars_ve_lor.rdata") {
  require(tidyverse)
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
  
  pars$CostEff <- sample_table(QL, n_sims) %>% 
    left_join(sample_table(Cost_Hospitalisation_HZ, n_sims) %>% select(Key, Age, cost_hosp_pp_inf = cost_Hospitalisation_pp_inf), by = c("Key", "Age")) %>% 
    left_join(sample_table(Cost_GP, n_sims) %>% select(Key, ends_with("_inf")), by = "Key")
  

  ## Parameters: Vaccination -----
  load(here::here("pars", ve_lor))
  lor_re <- 0
  
  load(here::here("pars", ve_zvl))
  pars$VE_ZVL <- sample_table(pars_ve_zvl %>% filter(!IC), n_sims) %>%
    select(Key, Vaccine, Age, TimeVac = Yr, Protection = VE)
  
  
  load(here::here("pars", ve_rzv))
  pars$VE_RZV <- ve_rzv <- sample_table(pars_ve_rzv %>% filter(!IC), n_sims) %>% 
    select(Key, Vaccine, TimeVac = Yr, Protection = VE)
  
  pars$VE_ReRZV <- ve_rzv %>% 
    mutate(
      Vaccine = "ReRZV",
      Protection = apply_lor(Protection, lor_re) 
    )
  
  pars$VE_ReRZV1 <- ve_rzv %>% 
    mutate(
      Vaccine = "ReRZV1",
      Protection = apply_lor(Protection, lor_re + lor_single) 
    )
  
  pars$VE_RZV <- pars$VE_RZV %>% mutate(Protection = apply_lor(Protection, lor_ic))
  pars$VE_ReRZV <- pars$VE_ReRZV %>% mutate(Protection = apply_lor(Protection, lor_ic))
  pars$VE_ReRZV1 <- pars$VE_ReRZV1 %>% mutate(Protection = apply_lor(Protection, lor_ic))
  pars$VE_ZVL <- pars$VE_ZVL %>% mutate(Protection = apply_lor(Protection, lor_ic))
  
  
  if (!realworld) {
    pars$VE_RZV <- pars$VE_RZV %>% mutate(Protection = apply_lor(Protection, -lor_rw))
    pars$VE_ReRZV <- pars$VE_ReRZV %>% mutate(Protection = apply_lor(Protection, -lor_rw))
    pars$VE_ReRZV1 <- pars$VE_ReRZV1 %>% mutate(Protection = apply_lor(Protection, - lor_rw))
    pars$VE_ZVL <- pars$VE_ZVL %>% mutate(Protection = apply_lor(Protection, -lor_rw))
  }
  
  return(pars)
}
