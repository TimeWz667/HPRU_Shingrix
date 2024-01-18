## Update function with dynamic modelling
#########################################################################################################################
# Cost-effectiveness analysis of herpes zoster vaccination
#########################################################################################################################

library(tidyverse)
folder_data <- function(x) here::here("data", x)
folder_tab <- function(x) here::here("outputs", "tabs", x)
folder_temp <- function(x) here::here("outputs", "temp", x)

source(here::here("R", "sim_cases.R"))


rand_table <- function(df, n_iter) {
  tibble(ID = 1:n_iter, Key = sample(unique(df$Key), N_Iter, rep = T)) %>% 
    left_join(df, relationship = "many-to-many") %>% 
    select(- Key)
}


set.seed(11667)

IC_status<- "NIC"
vaccine<-"shingrix"
N_Iter <- 500

### discount rate costs
discount_rate_costs <- 0.035
### discount rate effects
discount_rate_effects <- 0.035

### vaccine coverage
vaccine_coverage <- 0.483 
### cost per vaccine dose
cost_vac_per_dose <- 75
### admin cost per vaccine dose (item of service fee for 2018/19, https://www.nhsemployers.org/-/media/Employers/Documents/Primary-care-contracts/V-and-I/201819-Vaccination-and-immunisation-guidance-and-audit-requirements.PDF?la=en&hash=B3DFFE1BE23C5826841A87704FF305964A481A42)
cost_admin_per_dose <- 10
### number of doses
number_courses <- 2

### Vaccine cost per vaccination
cost_vac_pp <- (cost_vac_per_dose + cost_admin_per_dose) * number_courses


##########################
#### Data
load(folder_data("Population_NIC_Ons_2015.rdata"))
load(folder_data("Epi_NIC.rdata"))
load(folder_data("QOL_LE.rdata"))
load(folder_data("Cost_GP_Gauthier.rdata"))
load(folder_data("Cost_Hospitalisation_NIC.rdata"))
load(folder_data("VE_Shingrix_NIC.rdata"))


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


### start loop

results <- list()

for (vaccination_age in 18:95){
  scenario <- sprintf("CEA_24_%s_%s_%s", vaccine, IC_status, vaccination_age)
  scenario <- glue::as_glue(scenario)
  print(scenario)
  
  ##########################
  #### cohort size
  # population in England at vaccination age * vaccine_coverage
  cohort_size <- Pop %>% filter(age == vaccination_age) %>% pull(Pop) * vaccine_coverage
  
  ## Attach VE
  sims_baseline <- sims0 %>% 
    left_join(bind_rows(lapply(1:N_Iter, function(i) {
      sample_ve(VE, vaccination_age = vaccination_age) %>% mutate(ID = i) %>% rename(Age = age)
    }))) %>% 
    filter(Age >= vaccination_age)
  
  
  sims <- sims_baseline %>% 
    sim_cases(cohort_size, vaccination_age, cost_vac_pp, 
              discount_rate_effects, discount_rate_costs)
  
  
  tab <- sims %>% 
    select(-ID) %>% 
    group_by(Stat, Group) %>% 
    summarise_all(
      list(
        Mean = function(x) mean(x, na.rm = T),
        STD = function(x) sd(x, na.rm = T),
        Q025 = function(x) quantile(x, 0.025, na.rm = T),
        Q25 = function(x) quantile(x, 0.25, na.rm = T),
        Q5 = function(x) quantile(x, 0.5, na.rm = T),
        Q75 = function(x) quantile(x, 0.75, na.rm = T),
        Q975 = function(x) quantile(x, 0.975, na.rm = T)
      )
    ) %>% 
    mutate(
      Scenario = scenario
    )
  
  results[[scenario]] <- tab
  write_csv(tab, file = folder_temp(scenario + ".csv"))
  
}


results <- bind_rows(results)

write_csv(results, file = folder_temp(sprintf("CEA_dy_%s_%s.csv", vaccine, IC_status)))

save(results, file = folder_temp(sprintf("CEA_dy_%s_%s.rdata", vaccine, IC_status)))

