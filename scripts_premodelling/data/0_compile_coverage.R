library(tidyverse)



coverage <- read_csv(here::here("data", "raw_coverage", "Coverage.csv")) %>% 
  mutate(
    Cov_2018 = ifelse(Cov_2018 == "Not eligible", NA, Cov_2018),
    Cov_2018 = as.numeric(Cov_2018)
  ) %>% 
  pivot_longer(-Age, 
               names_pattern = "Cov_(\\d+)", names_to = "Year") %>% 
  mutate(value = value / 100) 


save(coverage, file = here::here("data", "processed_vaccine", "coverage.rdata"))
