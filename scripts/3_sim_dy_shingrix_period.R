library(tidyverse)

theme_set(theme_bw())


folder_tab <- function(x) here::here("outputs", "tabs", x)
folder_temp <- function(x) here::here("outputs", "temp", x)

source(here::here("R", "sim_cases.R"))


rand_table <- function(df, n_iter) {
  tibble(ID = 1:n_iter, Key = sample(unique(df$Key), N_Iter, rep = T)) %>% 
    left_join(df, relationship = "many-to-many") %>% 
    select(- Key)
}


set.seed(11667)


IC_status<- "IC"
vaccine<-"shingrix"
N_Iter <- 500

### discount rate costs
discount_rate_costs <- 0.035
### discount rate effects
discount_rate_effects <- 0.035

### cost per vaccine dose
cost_vac_per_dose <- 75
### admin cost per vaccine dose (item of service fee for 2018/19)
cost_admin_per_dose <- 10
### number of doses
number_courses <- 2

### Vaccine cost per vaccination
cost_vac_pp <- (cost_vac_per_dose + cost_admin_per_dose) * number_courses


##########################
#### Data

load(here::here("data", "processed_demography", "Population_ONS.rdata"))
load(here::here("data", "processed_epi", "Epi_HZ_NIC_CPRD_bind.rdata"))

load(here::here("data", "processed_ce", "Cost_Hospitalisation_NIC.rdata"))
load(here::here("data", "processed_ce", "Cost_GP_Gauthier.rdata"))
load(here::here("data", "processed_ce", "QOL_LE.rdata"))

load(here::here("data", "processed_vaccine", "VE_Shingrix_NIC.rdata"))

load(here::here("data", "fitted_coverage.rdata"))



### Shared properties
sims0 <- crossing(ID = 1:N_Iter, age = 0:100) %>% 
  left_join(Pop) %>% 
  left_join(rand_table(Epi_HZ, N_Iter) %>% rename(age = Age)) %>% 
  left_join(rand_table(QL, N_Iter))  %>% 
  left_join(rand_table(Cost_Hospitalisation_HZ, N_Iter)) %>% 
  left_join(rand_table(Cost_GP, N_Iter)) %>%
  # left_join(QL_death0) %>% 
  rename(r_mor_bg = Background_mortality, Age = age) %>% 
  group_by(ID) %>% 
  arrange(ID, Age)

