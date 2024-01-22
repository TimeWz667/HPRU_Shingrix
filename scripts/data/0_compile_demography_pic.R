library(tidyverse)
library(readxl)


raw <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), sheet = 1)


PrIC <- raw %>% 
  select(Age, IC = Immunocompromised, N = `Person- years`) %>% 
  pivot_wider(names_from = IC, values_from = N, names_prefix = "IC_") %>% 
  mutate(
    PrIC = IC_1 / (IC_0 + IC_1) 
  ) %>% 
  select(Age, PrIC)


PrIC 

save(PrIC, file = here::here("data", "processed_demography", "PrIC_GDPR.rdata"))

