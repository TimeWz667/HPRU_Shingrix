library(tidyverse)



coverage <- read_csv(here::here("data", "Coverage.csv")) %>% 
  mutate(
    Cov_2018 = ifelse(Cov_2018 == "Not eligible", NA, Cov_2018),
    Cov_2018 = as.numeric(Cov_2018)
  ) %>% 
  pivot_longer(-Age, 
               names_pattern = "Cov_(\\d+)", names_to = "Year") %>% 
  mutate(value = value / 100) 

coverage %>% 
  ggplot() +
  geom_line(aes(x = Age, y = value, colour = Year))







p_vi <- 0.45
p_catchup <- 0.15


ds <- tibble(
  Age = 50:80,
  Year = 2012,
  Coverage = 0,
  P_Vi = p_vi
)

d <- ds

for (i in 1:20) {
  d <- d %>% 
    mutate(
      Year = Year + 1,
      P_Vi = P_Vi * 0.99,
      Coverage = c(0, Coverage[-length(Coverage)]),
      Coverage = case_when(
        Age == 70 ~ P_Vi,
        Age > 70 ~ Coverage + (1 - Coverage) * p_catchup,
        T ~ Coverage
      )
    )
  ds <- bind_rows(ds, d)
}


ds %>% 
  filter(Age >= 70) %>% 
  filter(Year > 2017 & Year < 2024) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = Coverage, colour = as.character(Year))) +
  geom_point(data = coverage, aes(x = Age, y = value, colour = as.character(Year)))



