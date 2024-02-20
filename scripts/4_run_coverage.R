library(tidyverse)


theme_set(theme_bw())

load(file = here::here("outputs", "temp", "sims_coverage.rdata"))


epi <- bind_rows(
  local({
    load(folder_data("Epi_IC.rdata"))
    
    Epi_HZ %>% 
      select(- Key) %>% 
      group_by(Age) %>% 
      summarise_all(mean) %>% 
      mutate(IC = "IC")
  }),
  local({
    load(folder_data("Epi_NIC.rdata"))
    
    Epi_HZ %>% 
      select(- Key) %>% 
      group_by(Age) %>% 
      summarise_all(mean) %>% 
      mutate(IC = "NonIC")
  })
)

runs <- bind_rows(
  sims_Stay,
  sims_Plan,
  sims_PlanAndOlder
) %>% 
  left_join(epi) %>% 
  group_by(Year, Scenario) %>% 
  summarise(
    IncHZ = sum(N * Pr * (1 - VE) * r_inc_hz) / sum(N),
    MorHZ = sum(N * Pr * (1 - VE) * r_mor_hz) / sum(N),
    N = sum(N)
  )

runs_ic <- bind_rows(
  sims_Stay,
  sims_Plan,
  sims_PlanAndOlder
) %>% 
  left_join(epi) %>% 
  group_by(Year, Scenario, IC) %>% 
  summarise(
    IncHZ = sum(N * Pr * (1 - VE) * r_inc_hz) / sum(N),
    MorHZ = sum(N * Pr * (1 - VE) * r_mor_hz) / sum(N),
    N = sum(N)
  )



g <- runs %>% 
  filter(Year >= 2022) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = IncHZ, colour = Scenario)) +
  scale_y_continuous("Incidence, HZ, per 100k", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)

ggsave(g, filename = here::here("outputs", "figs", "g_proj_trend.png"), width = 6, height = 4)


g <- runs_ic %>% 
  filter(Year >= 2022) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = IncHZ, colour = Scenario)) +
  scale_y_continuous("Incidence, HZ, per 100k", labels = scales::number_format(scale = 1e5)) +
  facet_grid(.~IC) +
  expand_limits(y = 0)


ggsave(g, filename = here::here("outputs", "figs", "g_proj_trend_ics.png"), width = 10, height = 4)



g <- runs %>% 
  filter(Year >= 2022) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = MorHZ, colour = Scenario)) +
  scale_y_continuous("Mortality, HZ, per 100k", labels = scales::number_format(scale = 1e5)) +
  expand_limits(y = 0)

ggsave(g, filename = here::here("outputs", "figs", "g_proj_trend_mor.png"), width = 6, height = 4)


g <- runs_ic %>% 
  filter(Year >= 2022) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = MorHZ, colour = Scenario)) +
  scale_y_continuous("Mortality, HZ, per 100k", labels = scales::number_format(scale = 1e5)) +
  facet_grid(.~IC) +
  expand_limits(y = 0)


ggsave(g, filename = here::here("outputs", "figs", "g_proj_trend_mor_ics.png"), width = 10, height = 4)

