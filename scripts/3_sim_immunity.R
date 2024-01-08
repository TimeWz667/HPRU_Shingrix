library(tidyverse)

theme_set(theme_bw())


load(here::here("data", "fit_coverage.csv"))

coverage <- read_csv(here::here("data", "Coverage.csv")) %>% 
  mutate(
    Cov_2018 = ifelse(Cov_2018 == "Not eligible", NA, Cov_2018),
    Cov_2018 = as.numeric(Cov_2018)
  ) %>% 
  pivot_longer(-Age, 
               names_pattern = "Cov_(\\d+)", names_to = "Year", values_to = "Coverage") %>% 
  mutate(Coverage = Coverage / 100) 



coverag

