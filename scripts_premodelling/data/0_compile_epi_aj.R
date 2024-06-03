library(tidyverse)


folder_raw <- function(x) here::here("data", "raw", x)
folder_data <- function(x) here::here("data", "processed_epi", x)



### Incidence/Mortality HZ, IC ----
#### AJs model ----
Incidence_HZ <- local({
  df <- read_csv(file = folder_raw("Incidence_HZ_old_AJ_model.csv")) %>% 
    rename(Incidence_HZ = Incidence)
  
  # false positives when determining incidence
  p_Incidence_false_positives <- 0.05
  # proportion of HZ cases in IC
  p_Incidence_IC <- 0.089523809524
  
  df %>% 
    full_join(tibble(Age = 50:100)) %>% 
    arrange(Age) %>%
    fill(Incidence_HZ, .direction = "updown") %>% 
    mutate(
      Incidence_HZ_nIC = Incidence_HZ * (1-p_Incidence_false_positives)*(1-p_Incidence_IC)
    )
}) %>% 
  select(Age, p_HZ = Incidence_HZ_nIC) %>% 
  mutate(
    p_HZ_GP_only = p_HZ
  )


# AJ's old mortality
Mortality_HZ <- read_csv(folder_raw("CFR_HZ_old_AJ_model.csv")) %>% 
  rename(r_death_hz = CFR) %>% 
  mutate(p_death_hz = 0)


P_PHN <- read_csv(folder_raw("AJ_p_PHN.csv")) %>% 
  rename(Age = age) %>% 
  full_join(tibble(Age=0:100)) %>% 
  arrange(Age) %>% 
  fill(p_PHN, .direction = "updown")


### hospitalisation rate for HZ
## from AJs model (based on first diagnoses per age group), from AJ's model (THIS IS THE PROPORTION OF HZ CASES HOSPITALISED AT FIRST DIAGNOSIS)
Hospitalisation_HZ <- read_csv(folder_raw("hospitalisation_rates_old_AJ_model.csv")) %>% 
  rename(r_hospitalisation_hz = Hospitalisation_rate)


Epi_HZ <- Incidence_HZ %>% 
  full_join(Mortality_HZ) %>% 
  full_join(P_PHN) %>% 
  full_join(Hospitalisation_HZ)


save(Incidence_HZ, Mortality_HZ, P_PHN, Hospitalisation_HZ, Epi_HZ, file = folder_data("Epi_HZ_AJ.rdata"))
