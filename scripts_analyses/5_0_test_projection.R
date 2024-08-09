library(tidyverse)

theme_set(theme_bw())
 

source(here::here("models", "misc.R"))
source(here::here("models", "m_dy.R"))
source(here::here("models", "strategies.R"))

# Load inputs
load(file = here::here("pars", "parset_t_c35q35y24n1k_realworld.rdata"))

k = 10
pars <- get_pars(pars_set, k)



yss <- bind_rows(
  run(m_dy, pars, strategy = strategy_null) %>% mutate(Scenario = "Null"),
  run(m_dy, pars, strategy = strategy_zvl) %>% mutate(Scenario = "S0"),
  run(m_dy, pars, strategy = strategy_changeonly) %>% mutate(Scenario = "S1"),
  run(m_dy, pars, strategy = strategy_scheduled65) %>% mutate(Scenario = "S2"),
  run(m_dy, pars, strategy = strategy_scheduled) %>% mutate(Scenario = "S3"),
  run(m_dy, pars, strategy = strategy_scheduled_e85) %>% mutate(Scenario = "S4"),
  run(m_dy, pars, strategy = strategy_scheduled_e85b80) %>% mutate(Scenario = "S5")
)
  

yss %>% 
  filter(Age >= 60) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    Inc = sum(N_HZ) / sum(N)
  ) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Inc, colour = Scenario)) +
  geom_vline(xintercept = c(2023, 2028, 2033) - 1, linetype = 2) +
  geom_text(x = 2023 - 1, y = 0, label = "Phase 1", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2028 - 1, y = 0, label = "Phase 2", angle = -90, hjust = 1, vjust = -1) + 
  geom_text(x = 2033 - 1, y = 0, label = "Continuation", angle = -90, hjust = 1, vjust = -1) +
  expand_limits(y = 0)

