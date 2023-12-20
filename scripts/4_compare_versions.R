library(tidyverse)

theme_set(theme_bw())


ss <- bind_rows(
  read_csv(here::here("outputs", "tabs", "Summary_CEA_shingrix_IC.csv")) %>% mutate(Version = "Modified"),
  read_csv(here::here("outputs", "tabs", "Summary_CEA_shingrix_original_IC.csv")) %>% mutate(Version = "Original")
)


ss %>% pull(Index) %>% unique()

ss %>% 
  filter(Index == "ICER") %>% 
  ggplot() + 
  geom_linerange(aes(x = Vaccination_Age, ymin = q025, ymax = q975, colour = Version)) +
  geom_pointrange(aes(x = Vaccination_Age, y = q500, ymin = q250, ymax = q750, colour = Version))


ss %>% 
  filter(Index == "ICER") %>% 
  ggplot() + 
  geom_ribbon(aes(x = Vaccination_Age, ymin = q025, ymax = q975), alpha = 0.2) +
  geom_ribbon(aes(x = Vaccination_Age, ymin = q250, ymax = q750), alpha = 0.4) +
  geom_line(aes(x = Vaccination_Age, y = q500)) +
  facet_grid(.~Version)


ss %>% 
  filter(Index == "total_QALYs_gained_d") %>% 
  ggplot() + 
  geom_ribbon(aes(x = Vaccination_Age, ymin = q025, ymax = q975), alpha = 0.2) +
  geom_ribbon(aes(x = Vaccination_Age, ymin = q250, ymax = q750), alpha = 0.4) +
  geom_line(aes(x = Vaccination_Age, y = q500)) +
  facet_grid(.~Version)


ss %>% 
  filter(Index == "total_net_cost") %>% 
  ggplot() + 
  geom_ribbon(aes(x = Vaccination_Age, ymin = q025, ymax = q975), alpha = 0.2) +
  geom_ribbon(aes(x = Vaccination_Age, ymin = q250, ymax = q750), alpha = 0.4) +
  geom_line(aes(x = Vaccination_Age, y = q500)) +
  facet_grid(.~Version)



ds <- dir("outputs/temp")
ds <- ds[endsWith(ds, "70.csv")]



ss <- bind_rows(
  read_csv(here::here("outputs", "temp", "CEA_shingrix_IC_70.csv")) %>% mutate(Version = "Modified"),
  read_csv(here::here("outputs", "temp", "2023-12-19_CEA_shingrix_IC_age70.csv")) %>% mutate(Version = "Original")
)

ss %>% 
  mutate(value = ICER ) %>% 
  ggplot() +
  geom_density(aes(x = value, fill = Version), alpha = 0.2) 

