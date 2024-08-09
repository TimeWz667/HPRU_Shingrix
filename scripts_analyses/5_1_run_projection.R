library(tidyverse)

options(dplyr.summarise.inform = FALSE)


source(here::here("models", "misc.R"))
source(here::here("models", "m_dy.R"))
source(here::here("models", "strategies.R"))

# Load inputs
load(file = here::here("pars", "parset_t_c35q35y24n1k_realworld.rdata"))


## Simulation -----
year1 <- 2050


yss_zvl <- list()
yss_change <- list()
yss_p1 <- list()
yss_sch <- list()
yss_sch85 <- list()


keys <- 1:pars_set$N_Sims
keys <- keys[1:200]


pb <- txtProgressBar(min = 1, max = length(keys), style = 3,  width = 50, char = "=") 

for(i in 1:length(keys)) {
  k <- keys[i]
  pars <- get_pars(pars_set, k)
  
  yss_zvl[[i]] <- run(m_dy, pars, strategy = strategy_zvl, year1 = year1) %>% mutate(Key = k)
  
  yss_change[[i]] <- run(m_dy, pars, strategy = strategy_changeonly, year1 = year1) %>% mutate(Key = k)
  
  yss_p1[[i]] <- run(m_dy, pars, strategy = strategy_scheduled65, year1 = year1) %>% mutate(Key = k)
  
  yss_sch[[i]] <- run(m_dy, pars, strategy = strategy_scheduled, year1 = year1) %>% mutate(Key = k)
  
  yss_sch85[[i]] <- run(m_dy, pars, strategy = strategy_scheduled_e85b80, year1 = year1) %>% mutate(Key = k)
  
  setTxtProgressBar(pb, k)
}


## Outputs -----
yss_zvl <- bind_rows(yss_zvl) %>% mutate(Scenario = "No new programme")
yss_change <- bind_rows(yss_change) %>% mutate(Scenario = "Switch to RZV only")
yss_p1 <- bind_rows(yss_p1) %>% mutate(Scenario = "Expand to 65+")
yss_sch <- bind_rows(yss_sch) %>% mutate(Scenario = "Scheduled")
yss_sch85 <- bind_rows(yss_sch85) %>% mutate(Scenario = "Scheduled + [80, 85)")


save(yss_zvl, file = here::here("out", "yss_zvl.rdata"))
save(yss_change, file = here::here("out", "yss_change.rdata"))
save(yss_p1, file = here::here("out", "yss_p1.rdata"))
save(yss_sch, file = here::here("out", "yss_sch.rdata"))
save(yss_sch85, file = here::here("out", "yss_sch85.rdata"))



stats_epi <- bind_rows(
  yss_zvl,
  yss_change,
  yss_p1,
  yss_sch,
  yss_sch85
) %>% 
  group_by(Scenario, Year, Key) %>% 
  summarise(
    N_All = sum(N),
    N_60u = sum(N * (Age >= 60)),
    N_80u = sum(N * (Age >= 80)),
    IncHZ_All = sum(N_HZ) / N_All,
    IncHZ_60u = sum(N_HZ * (Age >= 60)) / N_60u,
    IncHZ_80u = sum(N_HZ * (Age >= 80)) / N_80u,
    IncPHN_All = sum(N_HZ_PHN) / N_All,
    IncPHN_60u = sum(N_HZ_PHN * (Age >= 60)) / N_60u,
    IncPNH_80u = sum(N_HZ_PHN * (Age >= 80)) / N_80u
  ) %>% 
  select(-Key) %>% 
  group_by(Scenario, Year) %>% 
  summarise_all(
    list(
      L = \(x) quantile(x, 0.25),
      M = \(x) quantile(x, 0.5),
      U = \(x) quantile(x, 0.75)
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    Scenario = factor(Scenario, levels = c(
      "No new programme", "Switch to RZV only", "Expand to 65+", "Scheduled", "Scheduled + [80, 85)"   
    ))
  ) %>% 
  pivot_longer(-c(Scenario, Year)) %>% 
  extract(name, c("Index", "Group", "Stats"), "(\\S+)_(All|60u|80u)_(M|L|U)") %>% 
  pivot_wider(names_from = Stats)


write_csv(stats_epi, file = here::here("docs", "tabs", "stats_epi.csv"))


stats_avt <- local({
  sa <- bind_rows(
    yss_zvl,
    yss_change,
    yss_p1,
    yss_sch,
    yss_sch85
  ) %>% 
    filter(Age >= 60) %>% 
    group_by(Scenario, Year, Key) %>% 
    summarise(
      N_HZ = sum(N_HZ)
    ) %>% 
    ungroup()
  
  sa %>% 
    left_join(
      sa %>% 
        filter(Scenario == "No new programme") %>% 
        select(Year, Key, N_HZ0 = N_HZ)
    ) %>% 
    group_by(Scenario, Key) %>% 
    mutate(
      Cum_HZ = cumsum(N_HZ),
      Cum_HZ0 = cumsum(N_HZ0),
      Avt_HZ = 1 - Cum_HZ / Cum_HZ0
    ) %>% 
    group_by(Scenario, Year) %>% 
    summarise(
      M = mean(Avt_HZ),
      L = quantile(Avt_HZ, 0.25),
      U = quantile(Avt_HZ, 0.75)
    ) %>% 
    filter(Year >= 2022)
}) %>% 
  mutate(
    Scenario = factor(Scenario, levels = c(
      "No new programme", "Switch to RZV only", "Expand to 65+", "Scheduled", "Scheduled + [80, 85)"  
    ))
  )
  
  
write_csv(stats_avt, file = here::here("docs", "tabs", "stats_avt.csv"))


g_inc <- stats_epi %>% 
  filter(Index == "IncHZ") %>% 
  filter(Group != "All") %>% 
  filter(Year >= 2020) %>% 
  ggplot() + 
  geom_line(aes(x = Year, y = M, colour = Scenario)) +
  scale_y_continuous("Incidence of Shingles, per 100,000", labels = scales::number_format(scale = 1e5)) +
  facet_wrap(.~Group, 
             labeller = labeller(Group = c("60u"="60 years-old and above", "80u"="80 years-old and above"))) +
  expand_limits(y = 0)

ggsave(g_inc, filename = here::here("docs", "figs", "g_inc.png"), width = 10, height = 4.5)



g_avt <- stats_avt %>% 
  ggplot() +
  geom_line(aes(x = Year, y = M, colour = Scenario)) +
  scale_x_continuous("Year", breaks = c(2023, 2028, 2033, 2040, 2045, 2050)) +
  scale_y_continuous("Averted cases, %", labels = scales::percent) +
  guides(colour = guide_legend(reverse = T))

g_avt

ggsave(g_avt, filename = here::here("docs", "figs", "g_avt.png"), width = 7, height = 4)



stats_avt %>% 
  arrange(Scenario) %>% 
  filter(Year == 2050)



