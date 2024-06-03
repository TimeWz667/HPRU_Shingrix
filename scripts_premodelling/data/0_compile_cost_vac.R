library(tidyverse)

cost_vac <- tibble(
  Vaccine = c("Zostavax", "Shingrix"),
  cost_vac_per_dose = c(100, 75),
  cost_admin_per_dose = 10,
  number_courses = c(1, 2)
) %>% 
  mutate(
    cost_vac_pp = (cost_vac_per_dose + cost_admin_per_dose) * number_courses
  )


cost_vac

write_csv(cost_vac, file = here::here("data", "processed_ce", "Cost_Vac.csv"))

