library(tidyverse)
library(betareg)
library(readxl)


dat_burden <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), sheet = 1) %>% 
  select(Age, Immunocompromised, N = `Person- years`, IncR = `Incidence per 1,000 pyrs`) %>% 
  mutate(IncR = IncR / 1000) %>% 
  full_join(
    read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), sheet = 2) %>% 
      select(Age, Immunocompromised, IncR_GP = `Incidence per 1,000 pyrs`) %>% 
      mutate(IncR_GP = IncR_GP / 1000)
  ) %>% 
  full_join(
    read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), sheet = 3) %>%
      select(Age, Immunocompromised, p_phn = `Prop events with PHN`)
  ) %>% 
  filter(!is.na(Age) & Age != "101+") %>% 
  mutate(Age = as.numeric(Age))


save(dat_burden, file = here::here("data", "hz_burden_22.rdata"))
