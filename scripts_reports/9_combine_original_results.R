library(tidyverse)


ds <- dir("outputs/temp")

ds <- ds[startsWith(ds, "2023-12-19")]


results <- bind_rows(lapply(ds, function(d) {
  read_csv(here::here("outputs", "temp", d)) %>% 
    mutate(
      Scenario = gsub(".csv", "", gsub("2023-12-19_CEA_", "", d))
    )
}))



summ <- results %>% 
  select(Scenario, total_QALYs_gained_d, total_saved_costs_d, total_costs_intervention, total_net_cost, ICER) %>% 
  pivot_longer(-Scenario, names_to = "Index") %>% 
  group_by(Scenario, Index) %>%
  summarise(
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    q025 = quantile(value, 0.025, na.rm = T),
    q250 = quantile(value, 0.25, na.rm = T),
    q500 = quantile(value, 0.5, na.rm = T),
    q750 = quantile(value, 0.75, na.rm = T),
    q975 = quantile(value, 0.975, na.rm = T),
  ) %>% 
  ungroup() %>% 
  extract(Scenario, "Vaccination_Age", "age(\\d+)", convert = T, remove = F) %>% 
  arrange(Index, vaccination_age)


write_csv(summ, file = here::here("output", "Summary_CEA_shingrix_original_IC.csv"))
