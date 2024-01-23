library(tidyverse)
library(readxl)



epi <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 1)
epi_gp <- read_xlsx(here::here("data", "raw", "Zoster_Burden_Results_Nov19_Trimmed.xlsx"), 2)


epi %>% 
  select(Age, IC = Immunocompromised, X = `Num zoster events`, N = `Person- years`) %>% 
  left_join(epi_gp %>% select(Age, IC = Immunocompromised, X_gp = `Num zoster events`)) %>% 
  mutate(X_hosp = X - X_gp) %>% 
  ggplot() +
  geom_point(aes(x = Age, y = X_hosp / N)) +
  facet_grid(IC~.)


