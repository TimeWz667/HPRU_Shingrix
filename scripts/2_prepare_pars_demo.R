library(tidyverse)


load(here::here("data", "processed_demography", "Population_ONS.rdata"))


pars_demo <- lapply(c(England = "England", UK = "UK"), function(loc) {
  demo_fr <- demo_ons %>% 
    filter(Location == loc) %>% 
    select(Year0 = Year, Age0 = Age, dr = mortality, N0 = N)
  
  demo_to <- demo_ons %>% 
    filter(Location == loc) %>% 
    select(Year1 = Year, Age1 = Age, N1 = N) %>% 
    mutate(
      Year0 = Year1 - 1,
      Age0 = Age1 - 1
    )
  
  list(
    N = demo_fr %>% 
      select(Year = Year0, Age = Age0, N = N0),
    DeathIm = demo_fr %>% 
      left_join(demo_to) %>% 
      mutate(
        N1 = ifelse(is.na(N1), 0, N1),
        dN = N1 - N0,
        r_immigration = dN / N0 + dr
      ) %>% 
      select(Year = Year0, Age = Age0, r_death = dr, r_immigration),
    Birth = demo_ons %>% 
      filter(Location == loc) %>%
      filter(Age == 0) %>% 
      select(Year, N_Birth = N) %>% 
      mutate(
        Year = Year - 1
      )
  )
})


save(pars_demo, file = here::here("pars", "pars_demo.rdata"))




# initialise
theme_set(theme_bw())

source(here::here("models", "m_dy_population.R"))


ys_eng <- simulate_demo(pars = pars_demo$England)


dat_sel <- demo_ons %>% 
  filter(Location == "England") %>% 
  filter(Year %in% c(2015, 2020, 2025, 2030)) %>% 
  mutate(
    AgeGrp = cut(Age, seq(0, 100, 10), right = F)
  ) %>% 
  filter(!is.na(AgeGrp)) %>% 
  group_by(Year, AgeGrp) %>% 
  summarise(N = sum(N))


g_pop_py <- ys_eng %>% 
  filter(Year %in% c(2015, 2020, 2025, 2030)) %>% 
  mutate(
    AgeGrp = cut(Age, seq(0, 100, 10), right = F)
  ) %>% 
  filter(!is.na(AgeGrp)) %>% 
  group_by(Year, AgeGrp) %>% 
  summarise(N = sum(N)) %>% 
  ggplot() +
  geom_bar(aes(x = N, y = AgeGrp), stat = "identity", alpha = 0.3) +
  geom_point(data = dat_sel, aes(x = N, y = AgeGrp)) +
  scale_x_continuous("Population Million", labels = scales::number_format(scale = 1e-6)) +
  facet_wrap(.~Year) +
  expand_limits(x = 10e6)
g_pop_py


dat_sel <- demo_ons %>% 
  filter(Location == "England") %>% 
  group_by(Year) %>% 
  summarise(N = sum(N)) %>% 
  filter(Year %in% seq(2015, 2040, 5))


g_pop_n <- ys_eng %>% 
  group_by(Year) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = Year, y = N)) +
  geom_point(data = dat_sel, aes(x = Year, y = N)) +
  expand_limits(y = 0)
g_pop_n


ggsave(g_pop_py, filename = here::here("outputs", "figs", "processed", "g_pop_all_pu.png"), width = 6, height = 4)
ggsave(g_pop_n, filename = here::here("outputs", "figs", "processed", "g_pop_all_n.png"), width = 6, height = 4)

