library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_ce", x)


### costs GP (outpatient direct cost per HZ episode)- excludes hospitalisations. 
# Gauthier et al. 2009 Epidemiology and cost of herpes zoster and post-herpetic neuralgia in the United Kingdom. As used by AJ in his last model.
# AJ costs
Cost_GP_per_HZ <- read_csv(folder_raw("AJ_p_PHN.csv")) %>% 
  mutate(
    cost_GP_pp_non_PHN_inf = 75.63,
    cost_GP_pp_PHN_inf = 340.04
  )


Cost_Hospitalisation_HZ <- read_csv(folder_raw("hospital_costs_HZ_old_AJ_model.csv")) %>% 
  rename(age = Age, cost_Hospitalisation_pp_inf = hospital_costs)

save(Cost_Hospitalisation_HZ, Cost_GP_per_HZ, file = folder_data("Cost_AJ.rdata"))
