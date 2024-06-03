library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_vaccine", x)


## VE sampler
sample_ve <- function(obj_ve, vaccination_age = 70, vid = NA, ...) {
  UseMethod("sample_ve")
}


### VE HZ NIC -----
sample_ve.ve_mcmc_nic <- function(obj_ve, vaccination_age = 70, years_projected = 100 - vaccination_age, vid = NA, ...) {
  if(is.na(vid)) {
    sel <- obj_ve$src %>% filter(VID == sample(VID, 1))
  } else {
    sel <- obj_ve$src %>% filter(VID == vid)  
  }
  
  
  tibble(age = vaccination_age:(vaccination_age+years_projected)) %>% 
    cross_join(sel) %>% 
    mutate(
      lambda = lambda_intercept + lambda_slope * age,
      VE = lambda * exp(- delta * seq(1, years_projected + 1) * 365)
    ) %>% 
    full_join(tibble(age = 0:100), by = "age") %>% 
    mutate(
      VE = ifelse(is.na(VE), 0, VE)
    ) %>% 
    select(age, VE) %>% 
    arrange(age)
}


sample_ve.ve_mcmc_phn <- function(obj_ve, vaccination_age = 70, years_projected = 100 - vaccination_age, vid = NA, ...) {
  if(is.na(vid)) {
    sel <- obj_ve$src %>% filter(VID == sample(unique(Key), 1))
  } else {
    sel <- obj_ve$src %>% filter(VID == vid)  
  }  
  
  sel %>% 
    mutate(
      age = vaccination_age + T_Covered
    ) %>% 
    select(age, VE_PHN = VE_PHN_w_HZ) %>% 
    full_join(tibble(age = 0:100)) %>% 
    arrange(age) %>% 
    mutate(VE_PHN = ifelse(is.na(VE_PHN), 0, VE_PHN))
}


#### Zostavax nonIC ----
VE <- list(
  src = read_csv(folder_raw("01-01-2019 MCMC_chain_zostavax_50000.csv"))[-1] %>% 
    rename(delta = X1, sigma_intercept = X2, sigma_slope = X3, lambda_intercept = X4, lambda_slope = X5, VID = iteration)
)
class(VE) <- "ve_mcmc_nic"


# Add VE_PHN_w_HZ
VE_PHN <- list(
  src = read_csv(folder_raw("03-05-2018 VE for PHN w HZ for each year post vac 100 000 draws.csv"))[-1] %>% 
    mutate(Key = 1:n()) %>% 
    pivot_longer(- Key, values_to = "VE_PHN_w_HZ", names_to = "T_Covered") %>% 
    mutate(T_Covered = as.numeric(gsub("age_", "", T_Covered)) - 70)
)
class(VE_PHN) <- "ve_mcmc_phn"


save(VE, VE_PHN, sample_ve, sample_ve.ve_mcmc_nic, sample_ve.ve_mcmc_phn, file = folder_data("VE_Zostavax_NIC.rdata"))


#### Shingrix nonIC ----
VE <- list(
  src = read_csv(folder_raw("31-07-2018 MCMC_chain_shingrix_50000.csv"))[-1] %>% 
    rename(delta = X1, sigma_intercept = X2, sigma_slope = X3, lambda_intercept = X4, lambda_slope = X5, VID = iteration)
)
class(VE) <- "ve_mcmc_nic"

save(VE, sample_ve, sample_ve.ve_mcmc_nic, file = folder_data("VE_Shingrix_NIC.rdata"))


### VE HZ IC -----
sample_ve.ve_mcmc_ic <- function(obj_ve, vaccination_age = 70, years_projected = 100 - vaccination_age, vid = NA, vid_ic = NA, ...) {
  if (is.na(vid)) {
    ve_nic <- obj_ve$src %>% filter(VID == sample(VID, 1))
  } else {
    ve_nic <- obj_ve$src %>% filter(VID == vid)
  }
  
  if (is.na(vid_ic)) {
    ve_ic <- sample(obj_ve$VE_IC$VE, 1)
  } else {
    ve_ic <- obj_ve$VE_IC$VE[vid_ic]
  }
  
  ratio54 <- with(as.list(ve_nic), {
    years_projected_ic <- 2.35
    lambda <- lambda_intercept + lambda_slope * 54
    ve54 <- lambda * exp(-delta * (round(365*years_projected_ic)))
    ve_ic / ve54
  })
  
  
  tibble(age = vaccination_age:(vaccination_age + years_projected)) %>% 
    cross_join(ve_nic) %>% 
    mutate(
      lambda = lambda_intercept + lambda_slope * age,
      VE = lambda * exp(- delta * seq(1, years_projected + 1) * 365),
      VE = VE * ratio54
    ) %>% 
    full_join(tibble(age = 0:100), by = "age") %>% 
    mutate(
      VE = ifelse(is.na(VE), 0, VE)
    ) %>% 
    select(age, VE) %>% 
    arrange(age)
}

# #### Zostavax IC ----
# mcmc_ve <- read_csv(folder_raw("01-01-2019 MCMC_chain_zostavax_50000.csv"))[-1] %>% 
#   rename(delta = X1, sigma_intercept = X2, sigma_slope = X3, lambda_intercept = X4, lambda_slope = X5, VID = iteration)
# 
# save(mcmc_ve, sample_ve, file = folder_data("VE_Zostavax_IC.rdata"))



#### Shingrix IC ----
VE <- list(
  src = read_csv(folder_raw("31-07-2018 MCMC_chain_shingrix_50000.csv"))[-1] %>% 
    rename(delta = X1, sigma_intercept = X2, sigma_slope = X3, lambda_intercept = X4, lambda_slope = X5, VID = iteration),
  VE_IC = read_csv(folder_raw("03-01-2019 VE Shingrix IC.csv"))[-1]
)
class(VE) <- "ve_mcmc_ic"


save(VE, sample_ve, sample_ve.ve_mcmc_ic, file = folder_data("VE_Shingrix_IC.rdata"))

