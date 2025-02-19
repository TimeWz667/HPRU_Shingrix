

apply_lor <- function(p0, lor) 1 / (1 + exp(-log(p0 / (1 - p0)) - lor))


load_inputs_background <- function(year0 = 2024, n_sims = 1e3) {
  pars <- list(
    Year0 = year0,
    N_Sims = n_sims
  )
  
  ## Parameters: Demography -----
  load(here::here("pars", "pars_demo.rdata"))
  load(here::here("data", "sup_demo.rdata"))
  pars_demo <- pars_demo$England
  
  pars$Demography <- pars_demo$N %>% 
    filter(Year == year0) %>% 
    left_join(pars_demo$DeathIm, by = c("Year", "Age")) %>% 
    select(Year, Age, N, r_death) %>% 
    left_join(sup_demo %>% select(Year, Age, norm = norm_leoss))
  
  return(pars)
}


load_inputs_epi <- function(pars_bg) {
  pars <- pars_bg
  n_sims <- pars$N_Sims
  
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
  
  return(pars)
}


load_inputs_ce <- function(pars_epi, dis_e, dis_c) {
  pars <- pars_epi
  pars$dis_e <- dis_e
  pars$dis_c <- dis_c
  n_sims <- pars$N_Sims
  
  ## Parameters: CE -----
  load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
  load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
  # load(here::here("data", "processed_ce", "QOL_LE.rdata"))
  cost_vac <- read_csv(here::here("data", "processed_ce", "Cost_Vac.csv"))
  QL <- read_csv(here::here("data", "processed_ce", "QALY_loss_sims_uk_b.csv"))
  QL <- sample_table(QL, n_sims)
  
  if (dis_e >= 0.035) {
    QL <- QL %>% select(Key, Age, QL = QL35, QLH = QLH35)
  } else if (dis_e >= 0.015) {
    QL <- QL %>% select(Key, Age, QL = QL15, QLH = QLH15)
  } else {
    QL <- QL %>% select(Key, Age, QL = QL00, QLH = QLH00)
  }
  
  pars$CostVac <- cost_vac %>% 
    mutate(
      Vaccine = case_when(
        Vaccine == "RZV" ~ "RZV_2d",
        Vaccine == "RZV1" ~ "RZV_1d",
        Vaccine == "ReRZV" ~ "ReRZV_2d",
        Vaccine == "ReRZV1" ~ "ReRZV_1d",
        T ~ Vaccine
      )
    )
  
  ## CPI for healthcare (2015 = 100)
  cpi_healthcare_2012 <- 93.2
  cpi_healthcare_2023 <- 126.4
  
  ratio_inflation <- cpi_healthcare_2023 / cpi_healthcare_2012
  
  c_hosp <- Cost_Hospitalisation_HZ %>% 
    mutate(
      cost_hosp_pp_inf = Hospitalisation_costs_pp_HZ * ratio_inflation
    ) %>% 
    select(Age, cost_hosp_pp_inf)
  
  c_gp <- Cost_GP %>% 
    mutate(
      cost_GP_pp_non_PHN_HZ_inf = cost_gp_pp_non_PHN_HZ  * ratio_inflation,
      cost_GP_pp_PHN_inf = cost_GP_pp_PHN * ratio_inflation
    ) %>% 
    select(Key, cost_GP_pp_non_PHN_HZ_inf, cost_GP_pp_PHN_inf)
  
  
  pars$CostEff <- sample_table(QL, n_sims) %>% 
    left_join(c_hosp, by = "Age") %>% 
    left_join(sample_table(c_gp, n_sims), by = "Key")
  
  return(pars)
}


load_inputs <- function(pars_ce, vtype=c("rw", "tr"),
                        f_ve_zvl,
                        f_ve_rzv) {
  
  pars <- pars_ce
  pars$vtype <- vtype <- match.arg(vtype)
  n_sims <- pars$N_Sims
  
  load(f_ve_zvl)
  pars$VE_ZVL <- sample_table(pars_ve_zvl %>% filter(!IC), n_sims) %>%
    select(Key, Vaccine, Age, TimeVac = Yr, Protection = VE)
  
  load(here::here("pars", "pars_ve_lor.rdata"))
  load(f_ve_rzv)
  
  pars$VE_RZV_2d <- ve_rzv <- sample_table(pars_ve_rzv %>% filter(!IC), n_sims) %>% 
    mutate(Vaccine = "RZV_2d") %>% 
    select(Key, Vaccine, TimeVac = Yr, Protection = VE)
  
  pars$VE_RZV_1d <- ve_rzv %>% 
    mutate(
      Vaccine = "RZV_1d",
      Protection = apply_lor(Protection, lor_single) 
    )
  
  pars$VE_ReRZV_2d <- ve_rzv %>% 
    mutate(
      Vaccine = "ReRZV_2d",
      Protection = apply_lor(Protection, lor_re) 
    )
  
  pars$VE_ReRZV_1d <- ve_rzv %>% 
    mutate(
      Vaccine = "ReRZV_1d",
      Protection = apply_lor(Protection, lor_re + lor_single) 
    )
  
  if (vtype != "rw") {
    pars$VE_RZV_2d <- pars$VE_RZV_2d %>% mutate(Protection = apply_lor(Protection, -lor_rw))
    pars$VE_RZV_1d <- pars$VE_RZV_1d %>% mutate(Protection = apply_lor(Protection, -lor_rw))
    pars$VE_ReRZV_2d <- pars$VE_ReRZV_2d %>% mutate(Protection = apply_lor(Protection, -lor_rw))
    pars$VE_ReRZV_1d <- pars$VE_ReRZV_1d %>% mutate(Protection = apply_lor(Protection, - lor_rw))
    pars$VE_ZVL <- pars$VE_ZVL %>% mutate(Protection = apply_lor(Protection, -lor_rw))
  }
  
  return(pars)
}


save_pars <- function(pars, f) {
  save(pars, file = f)
  return(f)
}


load_inputs_proj <- function(pars) {
  
  load(here::here("pars", "pars_demo.rdata"))
  pars$Demography <- pars_demo$England
  
  load(here::here("data", "fitted_coverage.rdata"))
  pars$Uptake <- pred1$pars
  
  return(pars)
}



