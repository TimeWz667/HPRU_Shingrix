library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", x)


### costs GP (outpatient direct cost per HZ episode)- excludes hospitalisations. 
# Gauthier et al. 2009 Epidemiology and cost of herpes zoster and post-herpetic neuralgia in the United Kingdom. As used by AJ in his last model.
# AJ costs
Cost_GP_per_HZ <- read_csv(folder_raw("AJ_p_PHN.csv")) %>% 
  mutate(
    GP_cost_pp_non_PHN_HZ_inf = 75.63,
    GP_cost_pp_PHN_inf = 340.04
  )


save(Cost_GP_per_HZ, file = folder_data("Cost_GP_AJ.rdata"))



### costs of hospitalisation per HZ case
## from AJ's model (based on average length of stay per age group)
# Hospitalisation_costs_HZ<-read.csv(file=paste(Data,"hospital_costs_HZ_old_AJ_model.csv", sep=""))
# names(Hospitalisation_costs_HZ)<-c("age", "Cost_per_hospitalisation_HZ")
## from Peter Hobbelen's work 2004-05 to 2012-13 


### costs of hospitalisation per HZ case, Non-IC
#### nonIC read data & function to get hospitalisation costs from coefficients
index_2016_17<-302.3
index_2012_13<-287.3
index_2005_06<-240.9


Cost_Hospitalisation_HZ <- read_csv(folder_raw("Cost HZ hospitalisation sim_coef_agelm_rr_100000.csv"))[-1] %>% 
  rename(beta_age = age) %>% 
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Hospitalisation_costs_pp_HZ = Intercept + beta_age * age,
    Hospitalisation_costs_pp_HZ = ifelse(age >= 50 & age < 90, Hospitalisation_costs_pp_HZ, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(Hospitalisation_costs_pp_HZ, .direction = "updown") %>% 
  select(Key, age, Hospitalisation_costs_pp_HZ) %>% 
  ungroup() %>% 
  mutate(
    Hospitalisation_costs_pp_HZ_inf = Hospitalisation_costs_pp_HZ * (index_2016_17 / index_2012_13)
  )

save(Cost_Hospitalisation_HZ, file = folder_data("Cost_Hospitalisation_NIC.rdata"))


#### IC read data & function to get hospitalisation costs from coefficients
Cost_Hospitalisation_HZ <- read_csv(folder_raw("Cost HZ hospitalisation IC sim_coef_agelm_rr_100000.csv"))[-1] %>% 
  rename(beta_age = age) %>% 
  mutate(Key = 1:n()) %>% 
  crossing(tibble(age = 0:100)) %>% 
  arrange(Key, age) %>% 
  mutate(
    Hospitalisation_costs_pp_HZ = Intercept + beta_age * age,
    Hospitalisation_costs_pp_HZ = ifelse(age >= 50 & age < 90, Hospitalisation_costs_pp_HZ, NA)
  ) %>% 
  group_by(Key) %>% 
  fill(Hospitalisation_costs_pp_HZ, .direction = "updown") %>% 
  select(Key, age, Hospitalisation_costs_pp_HZ) %>% 
  ungroup() %>% 
  mutate(
    Hospitalisation_costs_pp_HZ_inf = Hospitalisation_costs_pp_HZ * (index_2016_17 / index_2012_13)
  )

save(Cost_Hospitalisation_HZ, file = folder_data("Cost_Hospitalisation_IC.rdata"))


# AJ costs
# AJ_p_PHN<-read.csv(file=paste(Data,"AJ_p_PHN.csv", sep=""))
# Cost_GP_per_HZ<-data.frame(age=60:99,
#                            p_PHN=AJ_p_PHN$p_PHN,
#                            Cost_GP_per_non_PHN_HZ=75.63,
#                            Cost_GP_per_PHN=340.04)


## outpatient costs Gauthier et al 2009 (in 2006 GBP): 
# 75.63 per non PHN HZ episode (95% CI 74.68-76.58)
mean_non_PHN_HZ <- (76.58 + 74.68) / 2 #75.63
SE_m <- (75.63 - 74.68) / 1.96 # same as (76.58-75.63)/1.96
SD <- SE_m * (sqrt(21587)) # this is incorrect
# simulate 100,000 costs
GP_cost_pp_non_PHN_HZ <- rnorm(mean = 75.63, sd = SE_m, n = 100000)
#check
# quantile(GP_cost_pp_non_PHN_HZ,probs = c(0,5,50,95,100)/100)
# 340.04 for PHN episode (95% CI 319.23-360.85) defined as 3m
mean_PHN <- (319.23 + 360.85) / 2 #340.04
SE_m <- (340.04 - 319.23) / 1.96
GP_cost_pp_PHN <- rnorm(mean = 340.04, sd = SE_m, n = 100000)
#check
# quantile(GP_cost_pp_PHN,probs = c(0,5,50,95,100)/100)
# put data in list


Cost_GP <- tibble(Key = 1:1e5) %>% 
  mutate(
    mean_non_PHN_HZ = (76.58 + 74.68) / 2,
    SE_m = (75.63 - 74.68) / 1.96,
    GP_cost_pp_non_PHN_HZ = rnorm(n(), mean = mean_non_PHN_HZ, sd = SE_m),
    mean_PHN = (319.23 + 360.85) / 2, #340.04
    SE_m = (340.04 - 319.23) / 1.96,
    GP_cost_pp_PHN = rnorm(n(), mean = mean_PHN, sd = SE_m),
    GP_cost_pp_non_PHN_HZ_inf = GP_cost_pp_non_PHN_HZ * (index_2016_17 / index_2005_06),
    GP_cost_pp_PHN_inf = GP_cost_pp_PHN * (index_2016_17 / index_2005_06)
  ) %>% 
  select(Key, starts_with("GP_cost"))


save(Cost_GP, file = folder_data("Cost_GP_Gauthier.rdata"))


